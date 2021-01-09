#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)      # user interface
library(leaflet)    # mapping
library(DT)         # data tables
library(collapse)   # sub sample data to GPS frequency
library(shinyFiles) # local file read popup
library(stringr)    # string processing
library(vembedr)    # video embedding

# clear out persisted data that is saved in R when the app isn't running for post run exploration
laps <<- NULL       # list of complete lap data frames sorted by fastest first, collapsed to GPS resolution
lapdf <<- NULL      # summary data frame, one row per lap
tf <<- NULL         # raw data frame as read from telemetry file
metadf <<- NULL     # metadata about the session, data frame with one row
starttimes <<- array() # times that each lap starts in seconds, offset from start of file


# Metadata Choices mostly taken from Tesla Motors Club Model 3 road course modification guide thread
# https://teslamotorsclub.com/tmc/threads/master-thread-comprehensive-road-course-modification-guide-—-optimizing-the-3-for-the-track.173484/
other <- "Other-see comments"
SessionTypeChoice <- c("Track Day","Autocross","Race","Qualifying","Time Trial","Dragstrip","Drift")
SessionLevelChoice <- c("Free Passing","Restricted Passing","Point-by","Novice","Solo")
DriverExperienceChoice <- c("Advanced", "Intermediate", "Novice (less than 5 track days)", "First Time", "Instructor")
CourseConditionChoice <- c("Dry Sunny", "Dry Overcast", "Damp", "Wet")
ModelChoice <- c("3","Y")
SpecificationChoice <- c("Dual Motor Performance", "Dual Motor Long Range", "Long Range", "Mid Range", "Standard Range")
WheelChoice <- c("UberTurbine 20", "Turbine20", "ZeroG 20", "Sport 19", "PowerSports 19", "Aero 18", other)
TireSizeChoice <- c("235/35-20", "235/40-19", "235/45-18", other)
TireTypeChoice <- c("Pirelli P-Zero PZ4-20","Michelin Pilot Sport 4S-20", "Continental Procontact RX-19", "Michelin MXM4-18", other)
BrakePadChoice <- c("Stock", "Carbotech RP2", "Racing Brake XT910", "Racing Brake XT970", "Unpluigged Performance Street/Track", "Unplugged Performance Track Only", other)
BrakeRotorChoice <- c("Stock", "Racing Brake", "Mountain Pass Performance", other)
BrakeCaliperChoice <- c("Performance", "Standard", other)
BrakeFluidChoice <- c("Stock", "Motul RBF600", "Castrol SRF", other)
CoiloverChoice <- c("Stock", "Motion Control Systems Eibach", "Unplugged Performance Ohlins", "Mountain Pass Performance", other)
FocusChoice <- c("Performance","Temperature","Plots","Turns","Driver")

# global data initialization
ylimits <- c(0,0) # track the limits of the data across all selected sources
xlimits <- c(0,0) # plot limits uses just the first data set picked to start with
turns <- data.frame() # list of turns - initialize and clear for ui reference
bpreset <- data.frame(Elapsed.Time..ms.=0, Longitude..decimal.=0, Latitude..decimal.=0, Speed..MPH.=0, Lateral.Acceleration..m.s.2.=0) # so that addCircles doesn't get null
bp <- bpreset     # brushed points from plot, used to annotate map
error <- ""       # error message
gms <- 9.80665    # convert meters pers second to G force
bpsi <- 14.50377  # convert bar to psi
render <- FALSE   # incoming file format, telemetry (29 column) or render (33 column with cumulative time)


# process a telemetry file - no dependencies on Shiny UI. Select only complete laps unless all=TRUE
ptf <- function(trackfile, all=FALSE) {
    render <<- FALSE
    rtf <<- try(read.csv(trackfile, check.names=FALSE))
    if (length(names(rtf)) != 29) {
        if (length(names(rtf)) != 33) {
            error <<- "Expected 29 or 33 columns in tesla telemetry csv file"
            return(FALSE)
        } else {
            rtf <<- rtf[,c(1:18,23:33)] # remove extra tire pressure in lbs columns
            render <<- TRUE # assume time has been changed to render import format
        }
    }
    # save the original names of the columns as raw.names, and update with valid R syntax names
    raw.names <<- names(rtf)
    names(rtf) <<- make.names(names(rtf))
    # collapse data by averaging repeated location points, reduces size from 42Hz to 10Hz
    tf <<- collapv(rtf, c(1,5,4))
    # re-sort by lap and time
    tf <<- tf[order(tf[,1],tf[,2]),]
    # remove incomplete laps
    latspan <-  max(tf[4]) - min(tf[4]) # east west distance for whole data set
    longspan <- max(tf[5]) - min(tf[5]) # north south distance for whole data set
    lapmax <<- max(tf$Lap) # max lap count in file (which starts at Lap 0)
    lapcnt <<- 0    # number of good laps in the file
    laps <<- list() # reset global empty list
    starttimes[1] <<- tail(tf[tf$Lap==0,2],1) # last timestamp (column 2) in lap 0 is the start time for lap 1
    for (i in 1:lapmax) { # ignore lap 0
        l <- tf[tf$Lap==i,] # get the data for one lap
        if (render) {# culmulative lap times
            starttimes[i+1] <<- tail(l[,2],1) # last timestamp in previous lap
            l[,2] <- l[,2] - starttimes[i]    # change timestamps to restart each lap
        } else {        # lap times reset for each lap
            starttimes[i+1] <<- starttimes[i] + tail(l[,2],1)
        }
        ln <- length(l$Lap) # get the number of data points
        # does the lap go over 95% of the way across the circuit and start end nearly the same place? 
        if (all | (abs(max(l[,4]) - min(l[,4]))/latspan > 0.95) &
            (abs(max(l[,5]) - min(l[,5]))/longspan > 0.95) &
            (abs(l[1,4] - l[ln,4]) < 0.0001) & (abs(l[1,5] - l[ln,5]) < 0.0001)) {
                lapcnt <<- lapcnt+1
                # add distance as a derived column for plotting instead of time
                # difference time from time dropping the first one and duplicating last in the column
                # multiply time deltas by speed and convert from milliseconds to hours to get miles
                # save as cululative sum of deltas to get distance in the lap
                d <- cumsum(l[,3]*(c(l[-1,2],l[length(l[,2]),2])-l[,2])/3600000) # distance along track
                l <- cbind(l, Distance=d)
                laps[[lapcnt]] <<- l # only append good laps to the global list unless all=TRUE
        }
    }
    if (lapcnt==0) {
        error <<- "No laps found in telemetry file"
        return(FALSE)
    }
    lapl <- lapply(laps, plap) # apply plap function against the list of laps and accumulate results
    lapdf <<- data.frame(t(sapply(lapl,c))) # transpose and convert to data frame - looked this up on stack overflow...
    # sort the list of laps and summary data to have fastest first
    o <-order(as.numeric(lapdf$seconds))
    laps <<- laps[o]
    lapdf <<- lapdf[o,]
    row.names(lapdf) <<- lapdf$lapnum  # overwrite the row names otherwise it numbers sequentially
    telemetrylatlong <<- paste0(laps[[1]][1,4], ";",laps[[1]][1,5]) # position of start line for best lap
    # read in saved locations of turns, usually around the braking point entering the turn
    fn <- file.path(telemetrydir, paste0("turns", namebase))
    if (file.exists(fn)) {
        try(turns <<- read.csv(fn,row.names=1)) # read saved turns if they exist, column 1 as names
    } else {
        # default is to set the start of the lap as the initial turn
        turns <<- data.frame(row.names="Start", miles=0.0, dist=round(tail(laps[[1]]$Distance,1),3),
                    time=round(tail(laps[[1]][,2],1)/1000.0, 3), lat=laps[[1]][1,4], lng=laps[[1]][1,5]) # start line default
    }
    # read in metadata about the session if present
    mn <- file.path(telemetrydir, metadata)
    if (file.exists(mn)) {
        metadf <<- try(read.csv(mn))
    } else {
        metadf <<- NULL
    }
    error <<- ""
    return(TRUE)
}

# process a lap
plap <- function(lap) {
    laplen <- length(lap[,2])
    lt <- lap[laplen,2]/1000.0
    KW <- max(lap[,13])
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
            p("Documentation and open source at: ", tags$a(href="github.com/adrianco/rs-tesla-telemetry", "github.com/adrianco/rs-tesla-telemetry")),
            leafletOutput("sidemap", height=500), # main map display
            hr(),
            splitLayout(cellWidths=c("14%","18%","20%","20%"),
                        shinyFilesButton('tfile', label='Load Local File', title='Please select a telemetry csv file',
                                         multiple=FALSE, buttonType="primary"),
                        actionButton("rlsave", "...", class = "btn-primary"),
                        tags$b(textOutput("strack"), textOutput("telemetrydate"), "  ", textOutput("telemetrytime")),
                        selectInput("mappedlap", "Mapped Lap", NULL)
            ),
            h4("Pick complete laps to compare, sorted fastest first from:"),
            p(textOutput("filename")),
            DTOutput("laplist") # main list of laps to choose from
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
                         splitLayout(cellWidths=c("50%","25%", "25%"),
                            textAreaInput("comments", "Comments and customizations", placeholder="Excuses for not being faster...", resize="vertical", width="180%"),
                            textInput("youtube", "YouTube ID", ""),
                            numericInput("youtubeOffset", "Video start time offset", 0)
                         ),
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
                                     numericInput("modelyear", "Model Year", format(Sys.Date(), "%Y")),
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
                         hr(),
                         # speed oriented summary of selected laps
                         DTOutput("speedtab")
                ),
                tabPanel("Plots",
                         h3("Video, Turns and Plots"),
                         splitLayout(cellWidths=c("70%","30%"),
                                    uiOutput("vidid"),
                                    DTOutput("turnslist")
                         ),
                         splitLayout(cellWidths=c("8%", "14%","14%", "2%", "11%", "14%", "2%", "11%"),
                                     actionButton("replay", "Replay", class="btn-primary"),
                                     numericInput("vidStartInput", "Start time for lap", 0, min=0),
                                     numericInput("vidStartOffset", "Offset within lap", 0),
                                     br(),
                                     actionButton("saveturn", "Save as Turn", class="btn-primary"),
                                     textInput("turnname", "Turn Name", placeholder="Brush area first"),
                                     br(),
                                     actionButton("deleteturn", "Delete Turn", class="btn-primary"),
                                     actionButton("fileturns", "...", class="btn-primary")
                         ),
                         plotOutput("speedplot", height='400px',
                                    dblclick = "plot_dblclick",
                                    brush = brushOpts(id = "plot_brush", clip=FALSE, resetOnNew=TRUE)),
                         splitLayout(cellWidths=c("12%","12%", "70%"), # some option buttons
                                     actionButton("zoom_out", "Zoom Out", class="btn-primary"),
                                     actionButton("reset_plot", "Reset Axes", class="btn-primary"),
                                     h4(" Select and move brush region with mouse and double-click to zoom in")
                         ),                         
                         DTOutput("plottab")
                    ) #tabpanel
                ) # tabsetpanel
            ) # mainpanel
        ) # sidebarlayout
    ) # fluidpage

#
# Define server functions that drive the UI components via input$ and output$ reactive lists
# Everything after this is a function defined in scope of the server
#
server <- function(input, output, session) {
    #
    # initialization and data loading section
    #
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
        turns=NULL,
        error=NULL
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
        rp$metadata <- ""   # clear this until a metadata file is written
    }
    # where to get telemetry files from - telemetrydir is persisted across invocations of the app
    if (exists("telemetrydir")) {
        volumes <- c(wd=telemetrydir, Home = fs::path_home(), getVolumes()())
    } else {
        volumes <- c(Home = fs::path_home(), getVolumes()())
    }
    
    # load file using file chooser popup button
    observe({
        req(input$tfile) # require that this exists, and trigger this function when it changes
        fp <- parseFilePaths(roots=volumes,selection=input$tfile)
        req(fp$datapath) # require a non null result
        makeFilePaths(fp$datapath, paths)
        if (!ptf(filename, all=FALSE)) {  # default filter out bad laps
            ptf(filename, all=TRUE) # try again without filter
        }
        # update the reactive data that will trigger the UI update
        rdata$laps <- laps
        rdata$lapdf <- lapdf
        rdata$metadf <- metadf
        rdata$turns <- turns
        rdata$error <- error
        if (!render) { # only save if it wasn't read as render format
            updateActionButton(session, "rlsave", label="Save Render Format")
        }
    })
    
    #
    # sidebar related display functions
    #
    # refresh displayed paths - show the current filename, or an error message if it wasn't a good one
    output$filename <- renderText({
        req(paths$filename)
        if (rdata$error != "") {
            rdata$error
        } else {
            paths$filename
        }
    })
    
    # display the date of the session
    output$telemetrydate <- renderText({
        req(paths$telemetrydate)
        paths$telemetrydate
    })
    
    # display the time of the session
    output$telemetrytime <- renderText({
        req(paths$telemetrytime)
        paths$telemetrytime
    })
    
    # sidebar lap selector using a Data Table
    output$laplist <- renderDT({
        req(rdata$lapdf) # require that it is not null
        # pick columns from the summary lap data frame to show - names(lapdf)
        # [1] "lapnum"           "seconds"          "minutes"          "maxMph"           "maxKW"           
        # [6] "maxBhp"           "maxLateralG"      "maxAccelG"        "maxBrakeG"        "maxBrakePedal"   
        # [11] "startCharge"      "maxBrakeTemp"     "maxFrontInverter" "maxRearInverter"  "maxBatteryTemp"  
        # [16] "minKW"            "avgThrottle"      "avgKW"            "maxSteerRate"     "maxThrottle"     
        datatable(lapdf[,c(3,4,6,7,9,20,18,11)], selection=list(selected=1, mode='multiple'),
                  options=list(pageLength=20, ordering=FALSE, searching=FALSE, initComplete=htmlwidgets::JS(
                      "function(settings, json) {",
                      "$(this.api().table().container()).css({'font-size': '80%'});",
                      "}")
                  )
            ) %>% formatStyle(1, target="row", fontWeight="bold")
        
    })
    
    # return colors used to annotate sidebar map
    accelcolor <- function(accel) {
        ifelse(accel > 0, "red", "blue")
    }
    
    #sidebar map
    output$sidemap <- renderLeaflet({
        # show plotting brush when it exists, as an overlay on the map in yellow
        cs <- input$plottab_cells_selected # get the selection of lines being plotted
        if (length(cs) > 0) {
            lap <- laps[[input$laplist_rows_selected[cs[1,1]]]] # lap for the first (black line) plot data
            if (!is.null(lap)) {
                # when the user brushes an area on the plot it appears as input
                # find the points from the lap that are inside the brushed area
                # X axis is Distance, Y axes is whatever was selected by the users cells
                bp <<- brushedPoints(lap, input$plot_brush, "Distance", names(lap)[unpick(cs[1,2])])
                if (length(bp$Elapsed.Time..ms.) > 1) # is a real brush in use
                    # if there's a non-empty brush use it to set the video start time in seconds
                    updateNumericInput(session, "vidStartOffset", value=trunc(bp$Elapsed.Time..ms.[1]/1000.0))
            }
        }
        # figure out which map is selected to be mapped
        ml <- as.numeric(input$mappedlap)
        req(rdata$turns) # make sure this updates when turns changes
        if (length(input$laplist_rows_selected) > 0 & !is.na(ml) & as.numeric(ml) < lapcnt) {
            # we have a valid lap to map
            leaflet(laps[[as.numeric(ml)]]) %>% addTiles() %>% # add the background tiles
                fitBounds(min(tf[5]),  min(tf[4]), max(tf[5]), max(tf[4])) %>% # set the size/location
                # add each turn position as a large black circle on the map
                addCircles(lng=turns$lng, lat=turns$lat, popup=row.names(turns), color="black", radius=10) %>%
                # color the map accel/decel and set mouse-over label to show mph and G. radius 1 is about the size of a car
                addCircles(lng=~Longitude..decimal., lat=~Latitude..decimal., radius=1,
                           color=~accelcolor(Longitudinal.Acceleration..m.s.2.),
                           label=~paste(Speed..MPH., "mph ", round(Lateral.Acceleration..m.s.2./gms, 2), "G")) %>%
                # add the brushed data to the map as a yellow overlay
                addCircles(data=bp, lng= ~Longitude..decimal., lat= ~Latitude..decimal., radius=3, color="yellow",
                           label=~paste(Speed..MPH., "mph ", round(Lateral.Acceleration..m.s.2./gms, 2), "G"))
        }
    })
    
    # sidebar map selector dropdown menu driven by which rows are selected from the table
    observeEvent(input$laplist_rows_selected, {
        x <- setNames(as.list(input$laplist_rows_selected), lapdf[input$laplist_rows_selected,1])
        updateSelectInput(session, "mappedlap", choices=x)
    })
    
    # lookup video start time and change it when new lap is chosen
    observeEvent(input$mappedlap, {
        ml <- laps[[as.numeric(input$mappedlap)]]$Lap[1]
        updateNumericInput(session, "vidStartInput",
                        label=paste("Start time for lap", ml),
                        value=trunc(starttimes[ml]/1000.0))
    })
    
    # create a pop up chooser to pick a local file to display
    shinyFileChoose(input, 'tfile', root=volumes, filetypes=c('csv'), session=session)
    
    # save in render format - add PSI columns and change the time to be cumulative
    observeEvent(input$rlsave, {
        if (!render) { # only save if it wasn't read as render
            rlnames <- c(raw.names[1:18], "FL PSI", "FR PSI", "RL PSI", "RR PSI", raw.names[19:29])
            rl <- cbind(rtf[,1:18], (rtf[,15:18] * bpsi), rtf[,19:29])
            names(rl) <- rlnames # put the raw names on to be written
            # time offset
            for (ts in 1:max(rl$Lap)) {
                lf <- (rl$Lap==ts) # vector of TRUE for this lap
                rl[lf,2] <- rl[lf,2] + starttimes[ts] # add the start time to all the times for each lap
            }
            options(scipen=10) # avoid scientific notation in the written values
            write.csv(rl, file=file.path(telemetrydir, paste0("render", namebase)), row.names=FALSE)
            updateActionButton(session, "rlsave", label="...") # confirm saved
        }
    })
    
    #
    # Metadata tab
    #
    # input$ fields to save/restore in metadata, need to add to these lists if new metata types are created
    textFieldsInput <- c("youtube", "track", "drivername", "sessionorganizer", "color", "controlarms")
    numericFieldsInput <- c("youtubeOffset", "carnumber", "passengers", "handlingbalance", "stabilityassist", "regenerativebraking",
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
        data <- c(telemetry=paste(paths$telemetry), telemetrydate=paste(paths$telemetrydate), telemetrytime=paste(paths$telemetrytime), telemetrylatlong=paste(paths$telemetrylatlong),
                  data, timestamp = as.character(as.integer(Sys.time())))
        data <- t(data)
        data
    })
    
    # save the metadata to the same directory as the telemetry file, and update the UI to confirm
    saveData <- function(data) {
        write.csv(data, file = file.path(telemetrydir, metadata),
                  row.names = FALSE)
    }
    
    # action to take when save button is pressed
    observeEvent(input$save, {
        saveData(formData())
        paths$metadata <- metadata
    })
    
    # update when it changes
    output$metadatafile <- renderText({
        req(paths$metadata)
        paths$metadata
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
    
    
    #
    # speed and temperature tab related display functions
    #
    # Speed summary table
    output$speedtab <- renderDT({
        req(lapdf)
        # pick columns from the summary lap data frame to show - names(lapdf)
        # [1] "lapnum"           "seconds"          "minutes"          "maxMph"           "maxKW"           
        # [6] "maxBhp"           "maxLateralG"      "maxAccelG"        "maxBrakeG"        "maxBrakePedal"   
        # [11] "startCharge"      "maxBrakeTemp"     "maxFrontInverter" "maxRearInverter"  "maxBatteryTemp"  
        # [16] "minKW"            "avgThrottle"      "avgKW"            "maxSteerRate"     "maxThrottle"     
        datatable(lapdf[input$laplist_rows_selected, c(2:5, 16, 7:9)], selection="none",
                  options=list(pageLength=5, ordering=FALSE, searching=FALSE, paging=FALSE, info=FALSE, initComplete=htmlwidgets::JS(
                      "function(settings, json) {",
                      "$(this.api().table().container()).css({'font-size': '80%'});",
                      "}")
                  )
        ) %>% formatStyle(1, target="row", fontWeight="bold")
    })

    # Temperature summary table
    output$temptab <- renderDT({
        req(rdata$lapdf)
        # pick columns from the summary lap data frame to show - names(lapdf)
        # [1] "lapnum"           "seconds"          "minutes"          "maxMph"           "maxKW"           
        # [6] "maxBhp"           "maxLateralG"      "maxAccelG"        "maxBrakeG"        "maxBrakePedal"   
        # [11] "startCharge"      "maxBrakeTemp"     "maxFrontInverter" "maxRearInverter"  "maxBatteryTemp"  
        # [16] "minKW"            "avgThrottle"      "avgKW"            "maxSteerRate"     "maxThrottle"     
        datatable(lapdf[input$laplist_rows_selected, c(10:15,19)], selection=list(selected=1, mode='single'),
                  options=list(pageLength=5, ordering=FALSE, searching=FALSE, paging=FALSE, info=FALSE, initComplete=htmlwidgets::JS(
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
    
    # draw a map that shows temperature during the lap - same pattern as sidebar map
    output$tempmap <- renderLeaflet({
        if (length(input$laplist_rows_selected) > 0 & length(input$temptab_rows_selected) > 0) {
        leaflet(laps[[input$laplist_rows_selected[input$temptab_rows_selected]]]) %>% addTiles() %>%
            fitBounds(min(tf[5]),  min(tf[4]), max(tf[5]), max(tf[4])) %>%
            addCircles(lng=turns$lng, lat=turns$lat, popup=row.names(turns), color="black", radius=10) %>%
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
    
    #
    # plot tab related functions
    #
    #
    # map the columns in lapdf that we use in the plot table back to underlying columns for plotting
    # click on the summary value, and get a plot of the underlying data for that value
    # to add a new plot metric, first add the summary data to lapdf then change the mapping here
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
                  options=list(pageLength=5, ordering=FALSE, searching=FALSE, paging=FALSE, info=FALSE, initComplete=htmlwidgets::JS(
                      "function(settings, json) {",
                      "$(this.api().table().container()).css({'font-size': '80%'});",
                      "}")
                  )
        ) %>% formatStyle(1, target="row", fontWeight="bold") #, color = colpal(input$laplist_rows_selected))
    })
    
    # reactive needed so that changes trigger updates
    plot.range <- reactiveValues(x = NULL, y = NULL)
    
    # zoom into the brushed area when it's double clicked
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
    
    # save turn info to turn table
    observeEvent(input$saveturn, {
        if (!is.na(bp[1,4])) {
            turns <<- rbind(turns, data.frame(row.names=input$turnname, miles=round(bp$Distance[1],2),
                        dist=round(tail(bp$Distance, 1) - bp$Distance[1],3), time=round((tail(bp[,2],1)-bp[1,2])/1000.0,3),
                        lat=bp[1,4], lng=bp[1,5]))
            updateActionButton(session, "fileturns", label="Save Turns to File") # indicate unsaved data
            rdata$turns <- turns #trigger the UI update
        }
    })
    
    # delete selected turn, but not if its the start line
    observeEvent(input$deleteturn, {
        if (length(input$turnslist_rows_selected) == 1 & as.numeric(input$turnslist_rows_selected) > 1) {
            turns <<-turns[-as.numeric(input$turnslist_rows_selected),]
            updateActionButton(session, "fileturns", label="Save Turns to File") # indicate unsaved data
            rdata$turns <- turns #trigger the UI update
        }
    })
    
    # save turns to file for next time. Change button label to show it's complete
    observeEvent(input$fileturns, {
        write.csv(turns, file = file.path(telemetrydir, paste0("turns", namebase)))
        updateActionButton(session, "fileturns", label="...")
    })
    
    # set axes lmits to the over-all data limits
    observeEvent(input$reset_plot, {
        plot.range$x <- xlimits 
        plot.range$y <- ylimits
        bp <<- bpreset
    })
    
    # double the size of the zoomed area
    observeEvent(input$zoom_out, {
        xspan <- plot.range$x[2] - plot.range$x[1]
        yspan <- plot.range$y[2] - plot.range$y[1]
        plot.range$x <- c(plot.range$x[1]-xspan/2, plot.range$x[2]+xspan/2)
        plot.range$y <- c(plot.range$y[1]-yspan/2, plot.range$y[2]+yspan/2)
        bp <<- bpreset
    })
    
    # generate the plot
    output$speedplot <- renderPlot({
        cs <- input$plottab_cells_selected #matrix of the lap [,1] and value [,2] to plot in order
        ylimits <<- c(0,0) # track the limits of the data across all selected sources
        xlimits <<- c(0,0) # plot limits uses just the first data set picked to start with
        if (length(input$laplist_rows_selected) > 0 & length(cs) > 0) {
            for (i in 1:length(cs[,1])) {
                # cs[i,1] is the row in the table, index into the selected rows to find which lap
                lap <- laps[[input$laplist_rows_selected[cs[i,1]]]]
                xlimits <<- c(min(xlimits[1], min(lap[,30])), max(xlimits[2], max(lap[,30])))
                ylimits <<- c(min(ylimits[1], min(lap[,unpick(cs[i,2])])), max(ylimits[2], max(lap[,unpick(cs[i,2])])))
                if (i==1) {
                    # create the plot, pick axes limits etc. column 2 is time in milliseconds, 30 is distance
                    plot(lap[,30], lap[,unpick(cs[i,2])],type='l',col=i,
                         xlab="Miles", ylab=raw.names[unpick(cs[i,2])],
                         # ordered list of cells to plot, lookup which rows in laplist, and lap numbers from lapdf column 1
                         main="Add laps from the table on the left and pick metrics from the table below to add lines to the plot",
                         xlim = plot.range$x, ylim = plot.range$y)
                } else {
                    # add more lines to the plot
                    lines(lap[,30], lap[,unpick(cs[i,2])],type='l',col=i)
                }
                # label the lap at the first data point, in the same color, with the right lap number
                #text(x=0, y=lap[1,unpick(cs[i,2])], labels=paste("Lap", lapdf[input$laplist_rows_selected[cs[i,1]],1],
                    # if the line is different to the original y axis, label its data type
                #    ifelse(cs[1,2]==cs[i,2], "", raw.names[unpick(cs[i,2])])),
                #    col=i, pos=4)
                
            }
            # add a legend to the corner of the plot
            legend("bottomleft", legend=paste("Lap", lapdf[input$laplist_rows_selected[cs[,1]],1],
                                raw.names[unpick(cs[,2])]), col=1:20, lty=1, box.lty=0)
        }
    })
    
    # Video player
    # https://developers.google.com/youtube/iframe_api_reference documents the options available
    output$vidid <- renderUI({
        req(input$youtube)
        if (req(input$vidStartInput, input$vidStartOffset, input$youtubeOffset)) {
            st <- input$vidStartInput + input$vidStartOffset + input$youtubeOffset
        } else {
            st <- 0
        }
        input$replay # depend on this changing to reload video
        embed_youtube(input$youtube, height=400, query="autoplay=1") %>%
            use_start_time(st)
    })

    # Turns list
    output$turnslist <- renderDT({
        req(rdata$turns)
        datatable(turns[,1:3], selection=list(selected=matrix(c(1,2),1), mode='single', target='row'),
                  options=list(pageLength=10, ordering=FALSE, searching=FALSE, paging=FALSE, info=FALSE, initComplete=htmlwidgets::JS(
                      "function(settings, json) {",
                      "$(this.api().table().container()).css({'font-size': '80%'});",
                      "}")
                  )
        ) %>% formatStyle(1, target="row", fontWeight="bold") #, color = colpal(input$laplist_rows_selected))
    })
    
    # turn selector - set video to play from that point
    observeEvent(input$turnslist_rows_selected, {
        if (length(input$turnslist_rows_selected) == 1) {
            m <- turns[input$turnslist_rows_selected,1]
            l <- laps[[as.numeric(input$mappedlap)]]
            # need to find the time in the lap where the distance just reaches m
            # difference so that this is decreasing and abs so it reaches zero and stays there
            # return the index of the first zero
            i <- which.min(abs(m - l$Distance))
            updateNumericInput(session, "vidStartOffset", value=floor(l[i,2]/1000))
        }
    })
} #end of server

# Run the application
# initialization section
# if started from the command line with 'Rscript app.R filename', use the filename given
if (length(commandArgs(TRUE)) > 0) {
    cmdname <<- commandArgs(TRUE)
}
shinyApp(ui = ui, server = server)
