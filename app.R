#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(leaflet)
library(DT)
library(collapse)
library(olctools)
library(shinyFiles)
library(stringr)
library(vembedr)

# clear out persisted data
laps <<- NULL
lapdf <<- NULL
tf <<- NULL
dtf <<- NULL
metadf <<- NULL

# Metadata Choices
other <- "Other-see comments"
SessionTypeChoice <- c("Track Day","Autocross","Race","Qualifying","Time Trial","Dragstrip","Drift")
SessionLevelChoice <- c("Free Passing","Restricted Passing","Point-by","Novice","Solo")
DriverExperienceChoice <- c("Advanced", "Intermediate", "Novice (less than 5 track days)", "First Time", "Instructor")
CourseConditionChoice <- c("Dry Sunny", "Dry Overcast", "Damp", "Wet")
ModelChoice <- c("3","Y")
SpecificationChoice <- c("Dual Motor Performance", "Dual Motor Long Range", "Long Range", "Mid Range", "Standard Range")
WheelChoice <- c("Sport 20", "ZeroG 20", "Sport 19", "PowerSports 19", "Aero 18", other)
TireSizeChoice <- c("235/35-20", "235/40-19", "235/45-18", other)
TireTypeChoice <- c("Michelin Pilot Sport 4S-20", "Continental Procontact RX-19", "Michelin MXM4-18", other)
BrakePadChoice <- c("Stock", "Carbotech RP2", "Racing Brake XT910", "Racing Brake XT970", "Unpluigged Performance Street/Track", "Unplugged Performance Track Only", other)
BrakeRotorChoice <- c("Stock", "Racing Brake", "Mountain Pass Performance", other)
BrakeCaliperChoice <- c("Performance", "Standard", other)
BrakeFluidChoice <- c("Stock", "Motul RBF600", "Castrol SRF", other)
CoiloverChoice <- c("Stock", "Motion Control Systems Eibach", "Unplugged Performance Ohlins", "Mountain Pass Performance", other)
FocusChoice <- c("Performance","Temperature","Plots","Turns","Driver")

ylimits <<- c(0,0) # track the limits of the data across all selected sources
xlimits <<- c(0,0) # plot limits uses just the first data set picked to start with
turns <<- data.frame() # initialize and clear for ui reference
bp <- data.frame(Longitude..decimal.=0, Latitude..decimal.=0) # so that addCircles doesn't get null
vidid <- "35TXBtYDOA4" # sample youtube id to start with
vidstart <- "5s"

# process a telemetry file
ptf <- function(trackfile, all=FALSE) {
    rtf <<- try(read.csv(trackfile))
    if (length(names(rtf)) != 29) {
        stop("Expected 29 columns in tesla telemetry csv file")
    }
    # collapse data by averaging repeated location points, reduces size to about a fifth
    tf <<- collapv(rtf, c(1,5,4))
    # re-sort by lap and time
    tf <<- tf[order(tf[,1],tf[,2]),]
    # remove incomplete laps
    latspan <-  max(tf[4]) - min(tf[4])
    longspan <- max(tf[5]) - min(tf[5])
    lapmax <<- max(tf$Lap) # max lap count in file (which starts at Lap 0)
    lapcnt <<- 0    # number of good laps in the file
    laps <<- list() # reset global empty list
    for (i in 1:lapmax) { # ignore lap 0
        l <- tf[tf$Lap==i,] # get the data for one lap
        ln <- length(l$Lap) # get the number of data points
        # does the lap go all the way across the circuit and start end nearly the same place? 
        if (all || (abs(max(l[,4]) - min(l[,4]))/latspan > 0.95) &&
            (abs(max(l[,5]) - min(l[,5]))/longspan > 0.95) &&
            (abs(l[1,4] - l[ln,4]) < 0.0001) && (abs(l[1,5] - l[ln,5]) < 0.0001)) {
                lapcnt <<- lapcnt+1
                # add distance as a derived column for plotting instead of time
                # difference time from time dropping the first one and duplicating last in the column
                # multiply time deltas by speed and convert from milliseconds to hours to get miles
                # save as cululative sum of deltas to get distance in the lap
                d <- cumsum(l[,3]*(c(l[-1,2],l[length(l[,2]),2])-l[,2])/3600000) # distance along track
                l <- cbind(l, Distance=d)
                laps[[lapcnt]] <<- l # only append good laps to the global list
        }
    }
    if (lapcnt==0)
        return(FALSE)
    lapl <- lapply(laps, plap) # run plap against the list of laps and accumulate results
    lapdf <<- data.frame(t(sapply(lapl,c))) # convert to data frame - looked this up on stack overflow...
    # sort the list of laps and summary data to have fastest first
    o <-order(as.numeric(lapdf$seconds))
    laps <<- laps[o]
    lapdf <<- lapdf[o,]
    row.names(lapdf) <<- lapdf$lapnum  # otherwise it numbers sequentially
    telemetrylatlong <<- paste0(laps[[1]][1,4], ";",laps[[1]][1,5])
    # read in locations of turn apexes for a specific track, use rounded off open location code to identify
    trackdir <<- paste0("tracks", .Platform$file.sep, encode_olc(laps[[1]][1,4], laps[[1]][1,5], 8))
    fn <- paste0(trackdir, .Platform$file.sep, "turns.csv")
    turns <<- data.frame(row.names="Start", lat=laps[[1]][1,4], lng=laps[[1]][1,5], radius=10) # start line default
    #if (!dir.exists(trackdir)) {
    #    dir.create(trackdir)
    #}
    #if (!file.exists(fn)) {
    #    file.create(fn)
    #    write.csv(turns, file = fn, quote = TRUE) # write new
    #}
    #tryCatch(turns <<- read.csv(fn,row.names=1), error=write.csv(turns, file = fn, quote = TRUE)) # overwrite bad file
    mn <<- file.path(telemetrydir, metadata)
    if (file.exists(mn)) {
        metadf <<- try(read.csv(mn))
    } else {
        metadf <<- NULL
    }
    return(TRUE)
}

# process a lap
plap <- function(lap) {
    laplen <- length(lap[,2])
    # take the last sample for the lap from the raw data, not the collapsed average
    lt <- max(rtf[rtf$Lap==lap[1,1],2])/1000.0
    #lt <- lap[laplen,2]/1000.0
    KW <- max(lap[,13])
    gms <<- 9.80665
    # lap number, time in seconds, time converted to MM:SS.mmm format
    lapinfo <- data.frame(lapnum=lap[1,1], seconds=lt, minutes=sprintf("%d:%02d.%03d", lt%/%60, trunc(lt%%60), round((1000*lt)%%1000)),
                          # speed and power
                          maxMph=max(lap[,3]), maxKW=round(KW), maxBhp=round(KW/0.745699872),
                          # acceleration and braking
                          maxLateralG=round(max(lap[6])/gms, 2), maxAccelG=round(max(lap[7])/gms, 2),
                          maxBrakeG=-round(min(lap[7])/gms, 2), maxBrakePedal=round(max(lap[9]),1),
                          # charge at start of lap, max temp for all brakes
                          startCharge=round(lap[1,14]), maxBrakeTemp=round(max(lap[,19:22])*100),
                          # inverter and battery max temp percentages
                          maxFrontInverter=round(max(lap[, 23])*100), maxRearInverter=round(max(lap[,24])*100),
                          maxBatteryTemp=round(max(lap[,25])*100),
                          # averages and extra stats added later - keeping column numbers consistent
                          minKW=round(min(lap[,13])), avgThrottle=round(mean(lap$Throttle.Position....)),
                          avgKW=round(mean(lap[,13])), maxSteerRate=round(max(abs(lap$Steering.Angle.Rate..deg.s.))),
                          maxThrottle=round(sum((lap$Throttle.Position....>99.9))*100/length(lap$Throttle.Position....))
                          )
    return(lapinfo)
}


# Define UI for application to summarize Tesla track data
ui <- fluidPage(
    # Application title
    sidebarLayout(
        sidebarPanel(width=6,
            titlePanel("Shiny Tesla Telemetry Analyzer"),
            p("Open source at: ", tags$a(href="github.com/adrianco/rs-tesla-telemetry", "github.com/adrianco/rs-tesla-telemetry")),
            leafletOutput("sidemap", height=500),
            hr(),
            splitLayout(cellWidths="25%",
                        shinyFilesButton('tfile', label='Load Local File', title='Please select a telemetry csv file',
                                         multiple=FALSE, buttonType="primary"),
                        tags$b(textOutput("strack"), textOutput("telemetrydate"), "  ", textOutput("telemetrytime")),
                        selectInput("mappedlap", "Mapped Lap", NULL)
            ),
            h4("Pick complete laps to compare, sorted fastest first from:"),
            p(textOutput("filename")),
            DTOutput("laplist")
        ),
        mainPanel(width=6,
            br(),
            tabsetPanel(
                tabPanel("Metadata",
                         h3("Metadata about the session"),
                         splitLayout(cellWidths=c("20%","35%"),
                                     actionButton("save", "Save", class = "btn-primary"),
                                     verbatimTextOutput("metadatafile", TRUE),
                                     tags$head(tags$style(HTML(".shiny-split-layout > div {overflow: visible;}")))), #need this once per panel to make select menus not clip
                         hr(), # Output section
                         splitLayout(cellWidths=c("35%","32%","18%","15%"),
                                     verbatimTextOutput("telemetry", TRUE),
                                     verbatimTextOutput("latlong", TRUE),
                                     verbatimTextOutput("date", TRUE),
                                     verbatimTextOutput("time", TRUE)
                         ),
                         hr(), # Input section
                         textAreaInput("comments", "Comments and customizations", placeholder="Excuses for not being faster..."),
                         splitLayout(cellWidths="25%",
                                     textInput("track", "Track Name", ""),
                                     textInput("drivername", "Driver Name", ""),
                                     numericInput("carnumber", "Car Number", 44, 1), #Hamilton default
                                     numericInput("passengers", "Passengers", 0, 0)),
                         splitLayout(cellWidths="25%",
                                     p("Track mode settings"),
                                     numericInput("handlingbalance", "Handling Balance", 50),
                                     numericInput("stabilityassist", "Stability Assist", 0),
                                     numericInput("regenerativebraking", "Regenerative Braking", 100)),
                         splitLayout(cellWidths="25%",
                                     textInput("sessionorganizer", "Session Organizer", ""),
                                     selectInput("driverexperience", "Driver Experience", DriverExperienceChoice),
                                     selectInput("sessiontype", "Session Type", SessionTypeChoice),
                                     selectInput("sessionlevel", "Session Level", SessionLevelChoice)),
                         splitLayout(cellWidths="25%",
                                     selectInput("units", "Temperature Units", c("Fahrenheit", "Celsius")),
                                     numericInput("ambienttemperature", "Ambient Temperature", 0),
                                     numericInput("surfacetemperature", "Track Surface Temperature", 0),
                                     selectInput("coursecondition", "Course Condition", CourseConditionChoice)),
                         splitLayout(cellWidths="25%",
                                     selectInput("model", "Model", ModelChoice),
                                     selectInput("specification", "Specification", SpecificationChoice),
                                     numericInput("modelyear", "Model Year", 2020, 2018),
                                     textInput("color", "Color", "Blue")),
                         splitLayout(cellWidths="25%",
                                     selectInput("wheel", "Wheel", WheelChoice),
                                     selectInput("tiresize", "Tire Size", TireSizeChoice),
                                     selectInput("tiretype", "Tire Type", TireTypeChoice),
                                     numericInput("coldpressure", "Cold Pressure", 40)),
                         splitLayout(cellWidths="25%",
                                     selectInput("brakepad", "Brake Pad", BrakePadChoice),
                                     selectInput("brakerotor", "Brake Rotor", BrakeRotorChoice),
                                     selectInput("brakecaliper", "Brake Caliper", BrakeCaliperChoice),
                                     selectInput("brakefluid", "Brake Fluid", BrakeFluidChoice)),
                         splitLayout(cellWidths="25%",
                                     numericInput("frontcamber", "Front Camber (-1.0 to -0.1 std)", -1),
                                     numericInput("rearcamber", "Rear Camber (-2.0 to 0.0 std)", -2),
                                     selectInput("coilovers", "Coilovers", CoiloverChoice),
                                     textInput("controlarms", "Control Arms", "Stock"))
                ),
                tabPanel("Temperature",
                         h3("Battery temperature green, orange, red point by point, hover for details"),
                         # Show a map of the track with lap highlighted
                         leafletOutput("tempmap", height=500),
                         hr(),
                         # temperature oriented summary of selected laps
                         DTOutput("temptab"),
                         # speed oriented summary of selected laps
                         DTOutput("speedtab")
                ),
                tabPanel("Plots",
                         plotOutput("speedplot", height='600px',
                                    dblclick = "plot_dblclick",
                                    brush = brushOpts(id = "plot_brush", clip=FALSE, resetOnNew=TRUE)),
                         splitLayout(cellWidths=c("15%","15%", "70%"), # some option buttons
                                     actionButton("zoom_out", "Zoom Out", class="btn-primary"),
                                     actionButton("reset_plot", "Reset Axes", class="btn-primary"),
                                     h4(" Select and move region with mouse and double-click to zoom in")
                         ),                         
                         hr(),
                         DTOutput("plottab")
                ),
                tabPanel("Turn Analysis",
                         h4("Turn by turn analysis (work in progress)"),
                         plotOutput("turnplot", height='600px',
                                    dblclick = "turn_dblclick",
                                    brush = brushOpts(id = "turn_brush", clip=FALSE, resetOnNew=TRUE)),                         splitLayout(cellWidths=c("20%"),
                                    selectInput("select_turn", "Select Turn", c("Whole Lap", row.names(turns)))
                         ),
                         splitLayout(cellWidths=c("15%","15%","50%"),
                                     actionButton("zoom_out_turn", "Zoom Out", class="btn-primary"),
                                     actionButton("reset_plot_turn", "Reset Axes", class="btn-primary"),
                                     h4(" Select and move region with mouse and double-click to zoom in")
                         ),
                         hr(),
                         DTOutput("turnstab")
                         ),
                    tabPanel("Video",
                        uiOutput("vidid"),
                        splitLayout(cellWidths=c("15%","15%","50%"),
                                    textInput("vididInput", "YouTube ID", vidid),
                                    textInput("vidStartInput", "Start Offset", vidstart),
                                    h4(br()," ")
                            )
                        ),
                    tabPanel("Turn Editor",
                        h3("Setup Turns for a Track (work in progress)"),
                        splitLayout(cellWidths=c("25%"),
                                    actionButton("save_turns", "Save to file", class = "btn-primary"),
                                    verbatimTextOutput("turnsfile", TRUE)),
                        splitLayout(cellWidths=c("25%"),
                                    actionButton("new_turn", "Add new turn", class = "btn-primary"),
                                    textInput("new_turn_name", "New Turn Name")),                       
                        hr(),
                        splitLayout(cellWidths=c("40%", "70%"),
                            DTOutput("turnslist"),
                            leafletOutput("turnmap", height='500px')
                        )
                    ) #tabpanel
                ) # tabsetpanel
            ) # mainpanel
        ) # sidebarlayout
    ) # fluidpage


# return vector of colors
accelcolor <- function(accel) {
    ifelse(accel > 0, "red", "blue")
}

# Define server logic for viewing trackfile
server <- function(input, output, session) {
    # initialize paths so that changes are reflected in the UI
    paths <- reactiveValues(
        filename="",
        telemetry="",
        telemetrydir="",
        telemetrydate="",
        telemetrytime="",
        telemetrylatlong="",
        namebase="",
        metadata=""
    )
    # create reactive links for data sources
    rdata <- reactiveValues(
        laps=NULL,
        lapdf=NULL,
        metadf=NULL,
        turns=NULL
    )
    # Generate all the reactive directory and file paths needed for this run, extract date/time from filename
    # Users username path... telemetry-v1-2020-10-09-11_56_58.csv
    makeFilePaths <- function(fn, rp) {
        filename <<- fn
        rp$filename <- fn
        filepath <<- unlist(strsplit(fn, .Platform$file.sep)) # separate using / or \
        rp$telemetry <- filepath[length(filepath)] # telemetry-v1-2020-10-09-11_56_58.csv
        telemetrydir <<- paste(filepath[-length(filepath)], collapse=.Platform$file.sep)
        telemetrysplit <- unlist(strsplit(rp$telemetry, "-|_|[.]")) # telemetry v1 2020 10 09 11 56 58 csv
        rp$telemetrydate <- paste(telemetrysplit[3:5], collapse='-') # "2020-10-09"
        rp$telemetrytime <- paste(telemetrysplit[6:8], collapse=':') # "11:56:58"
        # construct filename base for additional export files
        namebase <<- sprintf("-v1-%s-%s-%s-%s_%s_%s.csv", telemetrysplit[3], telemetrysplit[4], telemetrysplit[5],
                               telemetrysplit[6], telemetrysplit[7], telemetrysplit[8])
        # metadata saved and loaded matching each telemetry file
        metadata <<- paste0("metadata", namebase) # metadata-v1-2020-10-09-11_56_58.csv
        rp$metadata <- metadata
    }
    # where to get telemetry file from
    volumes <- c(Home = fs::path_home(), getVolumes()())
    # load file from popup button
    observe({
        req(input$tfile)
        fp <<- parseFilePaths(roots=volumes,selection=input$tfile)
        #str(fp)
        req(fp$datapath)
        makeFilePaths(fp$datapath, paths)
        if (!ptf(filename, all=FALSE)) {  # default filter out bad laps
            if (!ptf(filename, all=TRUE)) # try again without filter
                stop("No laps found in telemetry file")
        }
        rdata$laps <- laps
        rdata$lapdf <- lapdf
        rdata$metadf <- metadf
        rdata$turns <- turns
    })
    
    # refresh displayed paths
    output$filename <- renderText({
        req(paths$filename)
        paths$filename
    })
    output$telemetrydate <- renderText({
        req(paths$telemetrydate)
        paths$telemetrydate
    })
    output$telemetrytime <- renderText({
        req(paths$telemetrytime)
        paths$telemetrytime
    })
    
    # sidebar lap selector
    output$laplist <- renderDT({
        req(rdata$lapdf)
        datatable(lapdf[,c(3,4,6,7,9,20,18,11)], selection=list(selected=1, mode='multiple'),
                  options=list(pageLength=20, ordering=FALSE, initComplete=htmlwidgets::JS(
                      "function(settings, json) {",
                      "$(this.api().table().container()).css({'font-size': '80%'});",
                      "}")
                  )
            ) %>% formatStyle(1, target="row", fontWeight="bold")
        
    })
    
    #sidebar map
    output$sidemap <- renderLeaflet({
        # show plotting brush when it exists
        cs <- input$plottab_cells_selected
        if (length(cs) > 0) {
            lap <- laps[[input$laplist_rows_selected[cs[1,1]]]]
            if (!is.null(lap)) {
                bp <<- brushedPoints(lap, input$plot_brush, "Distance", names(lap)[unpick(cs[1,2])])
            }
        }
        ml <- as.numeric(input$mappedlap)
        if (length(input$laplist_rows_selected) > 0 && !is.na(ml)) {
            leaflet(laps[[as.numeric(ml)]]) %>% addTiles() %>%
                fitBounds(min(tf[5]),  min(tf[4]), max(tf[5]), max(tf[4])) %>%
                addCircles(lng=turns$lng, lat=turns$lat, popup=row.names(turns), color="black", radius=turns$radius) %>%
                addCircles(lng=~Longitude..decimal., lat=~Latitude..decimal., radius=1,
                           color=~accelcolor(Longitudinal.Acceleration..m.s.2.),
                           label=~paste(Speed..MPH., "mph ", round(Lateral.Acceleration..m.s.2./gms, 2), "G")) %>%
                addCircles(data=bp, lng= ~Longitude..decimal., lat= ~Latitude..decimal., radius=3, color="yellow")
        }
    })
    
    # sidebar map selector
    observeEvent(input$laplist_rows_selected, {
        x <- setNames(as.list(input$laplist_rows_selected), lapdf[input$laplist_rows_selected,1])
        updateSelectInput(session, "mappedlap", choices=x)
    })
    
    # pop a chooser to pick a local file to display
    shinyFileChoose(input, 'tfile', root=volumes, filetypes=c('csv'), session=session)
    
    # Speed summary table
    output$speedtab <- renderDT({
        req(lapdf)
        datatable(lapdf[input$laplist_rows_selected, c(2:5, 16, 7:9)], selection=list(selected=1, mode='single'),
                  options=list(pageLength=5, ordering=FALSE, initComplete=htmlwidgets::JS(
                      "function(settings, json) {",
                      "$(this.api().table().container()).css({'font-size': '80%'});",
                      "}")
                  )
        ) %>% formatStyle(1, target="row", fontWeight="bold")
    })

    # Temperature summary table and map
    output$temptab <- renderDT({
        req(rdata$lapdf)
        datatable(lapdf[input$laplist_rows_selected, c(10:15,19)], selection=list(selected=1, mode='single'),
                  options=list(pageLength=5, ordering=FALSE, initComplete=htmlwidgets::JS(
                      "function(settings, json) {",
                      "$(this.api().table().container()).css({'font-size': '80%'});",
                      "}")
                  )
        ) %>% formatStyle(1, target="row", fontWeight="bold")
    })
    
    #gyr <- hcl.colors(100, palette="RdYlGn",rev=TRUE)
    # colour the array of circles according to the temperature array
    tempcolor <- function(x) {
        ifelse(x > 1.0, "red", ifelse(x > 0.8, "orange", "green"))
    }
    
    
    output$tempmap <- renderLeaflet({
        if (length(input$laplist_rows_selected) > 0 && length(input$temptab_rows_selected) > 0) {
        leaflet(laps[[input$laplist_rows_selected[input$temptab_rows_selected]]]) %>% addTiles() %>%
            fitBounds(min(tf[5]),  min(tf[4]), max(tf[5]), max(tf[4])) %>%
            addCircles(lng=turns$lng, lat=turns$lat, popup=row.names(turns), color="black", radius=turns$radius) %>%
            addCircles(lng= ~Longitude..decimal., lat= ~Latitude..decimal., radius=1,
                       color= ~tempcolor(Battery.Temp....),
                       label= ~paste(" Battery:", round(Battery.Temp....,2),
                                    "Brakes:", round(Brake.Temperature.Front.Left....est..,2), round(Brake.Temperature.Front.Right....est..,2),
                                    round(Brake.Temperature.Rear.Left....est..,2), round(Brake.Temperature.Rear.Right....est..,2),
                                    " InvertersFR:", round(Front.Inverter.Temp....,2), round(Rear.Inverter.Temp....,2)
                                    )
            )
        }
    })
    
    # map the columns in lapdf that we use in the plot table back to underlying columns for plotting
    colmap <- matrix(c(3,4, 5,7,8,10,12,14,15,
                       3,3,13,6,7, 9,19,24,25), nrow=2, ncol=9, byrow=TRUE)
    
    # take a picked column and return the index into the lap data
    unpick <- function(col) {
        colmap[2,col]
    }
    
    # plotting tab
    output$plottab <- renderDT({
        req(rdata$lapdf)
        datatable(lapdf[input$laplist_rows_selected, colmap[1,]], selection=list(selected=matrix(c(1,2),1), mode='multiple', target='cell'),
                  options=list(pageLength=5, ordering=FALSE, initComplete=htmlwidgets::JS(
                      "function(settings, json) {",
                      "$(this.api().table().container()).css({'font-size': '80%'});",
                      "}")
                  )
        ) %>% formatStyle(1, target="row", fontWeight="bold") #, color = colpal(input$laplist_rows_selected))
    })
    
    output$plot_domain <- renderPrint({
        cat("Domain:\n")
        str(input$plot_brush$domain)
    })
    output$plot_range <- renderPrint({
        cat("Range\n")
        str(input$plot_brush$range)
    })
    output$plot_brushinfo <- renderPrint({
        cat("Brush coordinates:\n")
        str(input$plot_brush$coords_img)
    })
    
    plot.range <- reactiveValues(x = NULL, y = NULL)
    
    observeEvent(input$plot_dblclick, {
        brush <- input$plot_brush
        if (!is.null(brush)) {
            plot.range$x <- c(brush$xmin, brush$xmax)
            plot.range$y <- c(brush$ymin, brush$ymax)
        } else {
            plot.range$x <- NULL
            plot.range$y <- NULL
        }
    })
    
    # set axes lmits to the over-all data limits
    observeEvent(input$reset_plot, {
        plot.range$x <- xlimits 
        plot.range$y <- ylimits
        bp <<- data.frame(Longitude..decimal.=0, Latitude..decimal.=0)
    })
    
    # double the size of the zoomed area
    observeEvent(input$zoom_out, {
        xspan <- plot.range$x[2] - plot.range$x[1]
        yspan <- plot.range$y[2] - plot.range$y[1]
        plot.range$x <- c(plot.range$x[1]-xspan/2, plot.range$x[2]+xspan/2)
        plot.range$y <- c(plot.range$y[1]-yspan/2, plot.range$y[2]+yspan/2)
        bp <<- data.frame(Longitude..decimal.=0, Latitude..decimal.=0)
    })
    
    output$speedplot <- renderPlot({
        cs <- input$plottab_cells_selected #matrix of the lap [,1] and value [,2] to plot in order
        ylimits <<- c(0,0) # track the limits of the data across all selected sources
        xlimits <<- c(0,0) # plot limits uses just the first data set picked to start with
        if (length(input$laplist_rows_selected) > 0 && length(cs) > 0) {
            for (i in 1:length(cs[,1])) {
                # cs[i,1] is the row in the table, index into the selected rows to find which lap
                lap <- laps[[input$laplist_rows_selected[cs[i,1]]]]
                xlimits <<- c(min(xlimits[1], min(lap[,30])), max(xlimits[2], max(lap[,30])))
                ylimits <<- c(min(ylimits[1], min(lap[,unpick(cs[i,2])])), max(ylimits[2], max(lap[,unpick(cs[i,2])])))
                if (i==1) {
                    # create the plot, pick axes limits etc. column 2 is time in milliseconds, 30 is distance
                    plot(lap[,30], lap[,unpick(cs[i,2])],type='l',col=i,
                         xlab="Miles", ylab=names(lap)[unpick(cs[i,2])],
                         # ordered list of cells to plot, lookup which rows in laplist, and lap numbers from lapdf column 1
                         main="", xlim = plot.range$x, ylim = plot.range$y)
                } else {
                    # add more lines to the plot
                    lines(lap[,30], lap[,unpick(cs[i,2])],type='l',col=i)
                }
                # label the lap at the first data point, in the same color, with the right lap number
                text(x=0, y=lap[1,unpick(cs[i,2])], labels=paste("Lap", lapdf[input$laplist_rows_selected[cs[i,1]],1],
                    # if the line is different to the original y axis, label its data type
                    ifelse(cs[1,2]==cs[i,2], "", names(lap)[unpick(cs[i,2])])), col=i, pos=4)
            }
        }
    })
    
    # Video tab
    output$vidid <- renderUI({
        embed_youtube(input$vididInput, height=500, query="autoplay=1") %>% use_start_time(input$vidStartInput)
    })
    
    # Turn Analysis tab
    # start with same picker as plot, since we want to compare multiple laps
    output$turnstab <- renderDT({
        req(lapdf)
        datatable(lapdf[input$laplist_rows_selected, colmap[1,]], selection=list(selected=matrix(c(1,2),1), mode='multiple', target='cell'),
                  options=list(pageLength=5, ordering=FALSE, initComplete=htmlwidgets::JS(
                      "function(settings, json) {",
                      "$(this.api().table().container()).css({'font-size': '80%'});",
                      "}")
                  )
        ) %>% formatStyle(1, target="row", fontWeight="bold") #, color = colpal(input$laplist_rows_selected))
    })
    
    # plot the line through the turn
    output$turnplot <- renderPlot({
        cs <<- input$turnstab_cells_selected #matrix of the lap [,1] and value [,2] to plot in order
        ylimits <<- c(0,0) # track the limits of the data across all selected sources
        xlimits <<- c(0,0) # plot limits uses just the first data set picked to start with
        if (length(input$laplist_rows_selected) > 0 && length(cs) > 0) {
            for (i in 1:length(cs[,1])) {
                # cs[i,1] is the row in the table, index into the selected rows to find which lap
                lap <- laps[[input$laplist_rows_selected[cs[i,1]]]]
                xlimits <<- c(min(xlimits[1], min(lap[,30])), max(xlimits[2], max(lap[,30])))
                ylimits <<- c(min(ylimits[1], min(lap[,unpick(cs[i,2])])), max(ylimits[2], max(lap[,unpick(cs[i,2])])))
                if (i==1) {
                    # create the plot, pick axes limits etc. column 2 is time in milliseconds, 30 is distance
                    plot(lap[,30], lap[,unpick(cs[i,2])],type='l',col=i,
                         xlab="Miles", ylab=names(lap)[unpick(cs[i,2])],
                         # ordered list of cells to plot, lookup which rows in laplist, and lap numbers from lapdf column 1
                         main="", xlim = plot.range$x, ylim = plot.range$y)
                } else {
                    # add more lines to the plot
                    lines(lap[,30], lap[,unpick(cs[i,2])],type='l',col=i)
                }
                # label the lap at the first data point, in the same color, with the right lap number
                text(x=0, y=lap[1,unpick(cs[i,2])], labels=paste("Lap", lapdf[input$laplist_rows_selected[cs[i,1]],1],
                            # if the line is different to the original y axis, label its data type
                            ifelse(cs[1,2]==cs[i,2], "", names(lap)[unpick(cs[i,2])])), col=i, pos=4)
            }
        }
    })
    
    # Metadata tab
    # input$ fields to save/restore in metadata
    textFieldsInput <- c("track", "drivername", "sessionorganizer", "color", "controlarms")
    numericFieldsInput <- c("carnumber", "passengers", "handlingbalance", "stabilityassist", "regenerativebraking",
                         "ambienttemperature", "surfacetemperature", "modelyear", "coldpressure", "frontcamber",
                         "rearcamber")
    selectFieldsInput <- c("driverexperience", "sessiontype", "sessionlevel", "units",
                         "coursecondition", "model", "specification", "wheel",
                         "tiresize", "tiretype", "brakepad", "brakerotor", "brakecaliper", "brakefluid", "coilovers")
    textAreaFieldsInput <- "comments"
    fieldsInput <- c(numericFieldsInput, textFieldsInput, selectFieldsInput, textAreaFieldsInput)
    # build a list of name=value pairs and transpose to columns
    formData <- reactive({
        data <- sapply(fieldsInput, function(x) input[[x]])
        data <- c(telemetry=cat(paths$telemetry), telemetrydate=cat(paths$telemetrydate), telemetrytime=cat(paths$telemetrytime), telemetrylatlong=cat(paths$telemetrylatlong),
                  data, timestamp = as.character(as.integer(Sys.time())))
        data <- t(data)
        data
    })
    
    # save the metadata to the same directory as the telemetry file, and update the UI to confirm
    saveData <- function(data) {
        write.csv(x = data, file = file.path(telemetrydir, metadata),
                  row.names = FALSE)
        output$metadatafile <- renderText({ metadata })
    }
    
    # action to take when save button is pressed
    observeEvent(input$save, {
        saveData(formData())
    })
    
    # update when metadata changes
    observeEvent(rdata$metadf, {
        # fixed output values added to metadata file
        output$telemetry <- renderText({ paths$telemetry })
        output$date <- renderText({ paste("Date:", paths$telemetrydate) })
        output$time <- renderText({ paste("Time:", paths$telemetrytime) })
        output$latlong <- renderText({ paste("Lat;Long:", telemetrylatlong) })
        # put any values that were read at startup back into the display
        if (!is.null(rdata$metadf)) {
            output$strack <- renderText({metadf$track[1]})
            sapply(textFieldsInput, function(x) updateTextInput(session, x, value=metadf[1,x]))
            sapply(numericFieldsInput, function(x) updateNumericInput(session, x, value=metadf[1,x]))
            sapply(selectFieldsInput, function(x) updateSelectInput(session, x, selected=metadf[1,x]))
            sapply(textAreaFieldsInput, function(x) updateTextAreaInput(session, x, value=metadf[1,x]))
        }
    })
    
    # Turn setup tab
    output$turnslist <- renderDT({
        req(rdata$turns)
        datatable(turns, selection=list(selected=matrix(c(1,2),1), mode='single', target='cell'),
                  options=list(pageLength=15, ordering=FALSE, initComplete=htmlwidgets::JS(
                      "function(settings, json) {",
                      "$(this.api().table().container()).css({'font-size': '80%'});",
                      "}")
                  )
        ) %>% formatStyle(1, target="row", fontWeight="bold") #, color = colpal(input$laplist_rows_selected))
    })
} #end of server

# Run the application
# initialization section
# if started from the command line with 'Rscript app.R filename', use the filename given
if (length(commandArgs(TRUE)) > 0) {
    cmdname <<- commandArgs(TRUE)
}
shinyApp(ui = ui, server = server)
