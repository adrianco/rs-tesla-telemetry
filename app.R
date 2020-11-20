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

# initialization section
# if started from the command line with 'Rscript app.R filename', use the filename given
if (length(commandArgs(TRUE)) > 0) {
    filename <- commandArgs(TRUE)
} else {
    filename <- file.choose()
}
# Generate all the directory and file paths needed for this run, extract date/time from filename
# Users username path... telemetry-v1-2020-10-09-11_56_58.csv
filepath <- unlist(strsplit(filename, .Platform$file.sep)) # separate using / or \
telemetry <- filepath[length(filepath)] # telemetry-v1-2020-10-09-11_56_58.csv
telemetrydir <- paste(filepath[-length(filepath)], collapse=.Platform$file.sep)
telemetrysplit <- unlist(strsplit(telemetry, "-|_|[.]")) # telemetry v1 2020 10 09 11 56 58 csv
telemetrydate <- paste(telemetrysplit[3:5], collapse='-') # "2020-10-09"
telemetrytime <- paste(telemetrysplit[6:8], collapse=':') # "11:56:58"
# construct filename base for additional export files
namebase <- sprintf("-v1-%s-%s-%s-%s_%s_%s.csv", telemetrysplit[3], telemetrysplit[4], telemetrysplit[5],
                    telemetrysplit[6], telemetrysplit[7], telemetrysplit[8])
# metadata saved and loaded matching each telemetry file
metadata <- paste0("metadata", namebase) # metadata-v1-2020-10-09-11_56_58.csv

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

# process a telemetry file
ptf <- function(trackfile) {
    rtf <<- read.csv(trackfile)
    if (length(names(rtf)) != 29) {
        stop("Expected 29 columns in tesla telemetry file")
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
        if (((max(l[,4]) - min(l[,4]))/latspan > 0.95) &&
            ((max(l[,5]) - min(l[,5]))/longspan > 0.95) &&
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
    lapl <- lapply(laps, plap) # run plap against the list of laps and accumulate results
    lapdf <<- data.frame(t(sapply(lapl,c))) # convert to data frame - looked this up on stack overflow...
    # sort the list of laps and summary data to have fastest first
    o <-order(as.numeric(lapdf$seconds))
    laps <<- laps[o]
    lapdf <<- lapdf[o,]
    row.names(lapdf) <<- lapdf$lapnum  # otherwise it numbers sequentially
    telemetrylatlong <<- paste0(laps[[1]][1,4], ";",laps[[1]][1,5])
    # read in locations of turn apexes for a specific track, use rounded off location to identify
    trackdir <<- paste0("tracks/", round(laps[[1]][1,4],1), ";", round(laps[[1]][1,5],1))
    fn <- paste0(trackdir,"/turns.csv")
    if (dir.exists(trackdir)) {
        if (file.exists(fn)) {
            turns <<- read.csv(fn,row.names=1)
        } else {
            turns <<- data.frame(lat=0, lng=0, radius=10) # empty for now
        }
    } else {
        dir.create(trackdir)
        file.create(fn)
    }
    mn <- file.path(telemetrydir, metadata)
    if (file.exists(mn)) {
        metadf <<- read.csv(mn)
    } else {
        metadf <<- NULL
    }
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
                          startChargePct=round(lap[1,14]), maxBrakeTempPct=round(max(lap[,19:22])*100),
                          # inverter and battery max temp percentages
                          maxFrontInverter=round(max(lap[, 23])*100), maxRearInverter=round(max(lap[,24])*100),
                          maxBatteryTemp=round(max(lap[,25])*100))
    return(lapinfo)
}


# Define UI for application to summarize Tesla track data
ui <- fluidPage(
    # Application title
    sidebarLayout(
        sidebarPanel(
            titlePanel("Shiny Tesla Telemetry Analyzer"),
            tags$a(href="github.com/adrianco/rs-tesla-telemetry", "github.com/adrianco/rs-tesla-telemetry"),
            h4("Pick complete laps to compare sorted fastest first from:"),
            p(filename),
            DTOutput("laplist")
        ),
        mainPanel(
            tabsetPanel(
                tabPanel("Speed",
                        h3("Acceleration (red) and deceleration (blue) point by point, hover for speed and lateral G"),
                        # speed oriented summary of selected laps
                        DTOutput("speedtab"),
                        # Show a map of the track with lap highlighted
                        leafletOutput("map", height=600)
                ),
                tabPanel("Temperature",
                         h3("Battery temperature green, orange, red point by point, hover for details"),
                         # temperature oriented summary of selected laps
                         DTOutput("temptab"),
                         # Show a map of the track with lap highlighted
                         leafletOutput("tempmap", height=600)
                ),
                tabPanel("Plots",
                         h3("Comparison plots"),
                         # temperature oriented summary of selected laps
                         DTOutput("plottab"),
                         plotOutput("speedplot", height='600px'),
                         #plotOutput("tempplot")
                ),
                tabPanel("Turns"),
                tabPanel("Metadata",
                        h3("Metadata about the session"),
                        splitLayout(cellWidths=c("25%"),
                            actionButton("save", "Save", class = "btn-primary"),
                            verbatimTextOutput("metadatafile", TRUE),
                            tags$head(tags$style(HTML(".shiny-split-layout > div {overflow: visible;}")))), #need this once per panel to make select menus not clip
                        hr(), # Output section
                        splitLayout(cellWidths="25%",
                             verbatimTextOutput("telemetry", TRUE),
                             verbatimTextOutput("date", TRUE),
                             verbatimTextOutput("time", TRUE),
                             verbatimTextOutput("latlong", TRUE)),
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
                    ) #tabpanel
                ) # tabsetpanel
            ) # mainpanel
        ) # sidebarlayout
    ) # fluidpage


# color the circles
accelcolor <- function(accel) {
    ifelse(accel > 0, "red", "blue")
}

# color cycle for plots
colpal <- function(x) {
    # get nine different colors and pick one
    palette.colors(9)[x%%9]
}

# Define server logic for viewing trackfile
server <- function(input, output, session) {
    # sidebar lap selector
    output$laplist <- renderDT({
        datatable(lapdf[,c(3,4,6,7,9)], selection=list(selected=1, mode='multiple'),
                  options=list(pageLength=20, ordering=FALSE, initComplete=htmlwidgets::JS(
                      "function(settings, json) {",
                      "$(this.api().table().container()).css({'font-size': '80%'});",
                      "}")
                  )
        ) %>% formatStyle(1, target="row", fontWeight="bold")
    })
    
    # Speed tab table and map
    output$speedtab <- renderDT({
        datatable(lapdf[input$laplist_rows_selected, 2:11], selection=list(selected=1, mode='single'),
                  options=list(pageLength=5, ordering=FALSE, initComplete=htmlwidgets::JS(
                      "function(settings, json) {",
                      "$(this.api().table().container()).css({'font-size': '80%'});",
                      "}")
                  )
        ) %>% formatStyle(1, target="row", fontWeight="bold")
    })
    
    output$map <- renderLeaflet({
        if (length(input$laplist_rows_selected) > 0 && length(input$speedtab_rows_selected) > 0) {
        leaflet(laps[[input$laplist_rows_selected[input$speedtab_rows_selected]]]) %>% addTiles() %>%
            fitBounds(min(tf[5]),  min(tf[4]), max(tf[5]), max(tf[4])) %>%
            addCircles(lng=turns$lng, lat=turns$lat, popup=row.names(turns), color="black", radius=turns$radius) %>%
            addCircles(lng=~Longitude..decimal., lat=~Latitude..decimal., radius=1,
                       color=~accelcolor(Longitudinal.Acceleration..m.s.2.),
                       label=~paste(Speed..MPH., "mph ", round(Lateral.Acceleration..m.s.2./gms, 2), "G"))
        }
    })

    # Temperature tab table and map
    output$temptab <- renderDT({
        datatable(lapdf[input$laplist_rows_selected, c(5,10:15)], selection=list(selected=1, mode='single'),
                  options=list(pageLength=5, ordering=FALSE, initComplete=htmlwidgets::JS(
                      "function(settings, json) {",
                      "$(this.api().table().container()).css({'font-size': '80%'});",
                      "}")
                  )
        ) %>% formatStyle(1, target="row", fontWeight="bold")
    })
    
    # colour the circles
    tempcolor <- function(pct) {
        ifelse(pct > 1.0, "red", ifelse(pct > 0.8, "orange", "green"))
    }
    
    
    output$tempmap <- renderLeaflet({
        if (length(input$laplist_rows_selected) > 0 && length(input$temptab_rows_selected) > 0) {
        leaflet(laps[[input$laplist_rows_selected[input$temptab_rows_selected]]]) %>% addTiles() %>%
            fitBounds(min(tf[5]),  min(tf[4]), max(tf[5]), max(tf[4])) %>%
            addCircles(lng=turns$lng, lat=turns$lat, popup=row.names(turns), color="black", radius=turns$radius) %>%
            addCircles(lng=~Longitude..decimal., lat=~Latitude..decimal., radius=1,
                       color=~tempcolor(Battery.Temp....),
                       label=~paste(" Battery:", round(Battery.Temp....,2),
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
        datatable(lapdf[input$laplist_rows_selected, colmap[1,]], selection=list(selected=matrix(c(1,2),1), mode='multiple', target='cell'),
                  options=list(pageLength=5, ordering=FALSE, initComplete=htmlwidgets::JS(
                      "function(settings, json) {",
                      "$(this.api().table().container()).css({'font-size': '80%'});",
                      "}")
                  )
        ) %>% formatStyle(1, target="row", fontWeight="bold") #, color = colpal(input$laplist_rows_selected))
    })
    
    output$speedplot <- renderPlot({
        cs <<- input$plottab_cells_selected #matrix of the lap [,1] and value [,2] to plot in order
        if (length(input$laplist_rows_selected) > 0 && length(cs) > 0) {
            for (i in 1:length(cs[,1])) {
                # cs[i,1] is the row in the table, index into the selected rows to find which lap
                lap <- laps[[input$laplist_rows_selected[cs[i,1]]]]
                if (i==1) {
                    # create the plot, pick axes limits etc. column 2 is time in milliseconds
                    plot(lap[,30], lap[,unpick(cs[i,2])],type='l',col=i,
                         xlab="Miles", ylab=names(lap)[unpick(cs[i,2])],
                         # ordered list of cells to plot, lookup which rows in laplist, and lap numbers from lapdf column 1
                         main="")
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
                         "coursecondition", "model", "specification", "color", "wheel",
                         "tiresize", "tyretype", "coldpressure", "brakepad", "brakerotor", "brakecaliper", "brakefluid", "coilovers")
    textAreaFieldsInput <- "comments"
    fieldsInput <- c(numericFieldsInput, textFieldsInput, selectFieldsInput, textAreaFieldsInput)
    # build a list of name=value pairs and transpose to columns
    formData <- reactive({
        data <- sapply(fieldsInput, function(x) input[[x]])
        data <- c(telemetry=telemetry, telemetrydate=telemetrydate, telemetrytime=telemetrytime, telemetrylatlong=telemetrylatlong,
                  data, timestamp = as.integer(Sys.time()))
        data <- t(data)
        data
    })
    
    # save the metadata to the same directory as the telemetry file, and update the UI to confirm
    saveData <- function(data) {
        write.csv(x = data, file = file.path(telemetrydir, metadata),
                  row.names = FALSE, quote = TRUE)
        output$metadatafile <- renderText({ metadata })
    }
    
    # action to take when save button is pressed
    observeEvent(input$save, {
        saveData(formData())
    })
    
    # fixed output values added to metadata file
    output$telemetry <- renderText({ telemetry })
    output$date <- renderText({ paste("Date:", telemetrydate) })
    output$time <- renderText({ paste("Time:", telemetrytime) })
    output$latlong <- renderText({ paste("Lat;Long:", telemetrylatlong) })
    # put any values that were read at startup back into the display
    if (!is.null(metadf)) {
        sapply(textFieldsInput, function(x) updateTextInput(session, x, value=metadf[1,x]))
        sapply(numericFieldsInput, function(x) updateNumericInput(session, x, value=metadf[1,x]))
        sapply(selectFieldsInput, function(x) updateSelectInput(session, x, selected=metadf[1,x]))
        sapply(textAreaFieldsInput, function(x) updateTextAreaInput(session, x, value=metadf[1,x]))
    }
} #end of server

# Run the application
ptf(filename)
shinyApp(ui = ui, server = server)
