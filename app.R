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

# process a telemetry file
ptf <- function(trackfile) {
    tf <<- read.csv(trackfile)
    if (length(names(tf)) != 29) {
        stop("Expected 29 columns in tesla telemetry file")
    }
    #Remove lap 0
    tf <<- tf[(tf[,1]!=0),]
    # create list of separate laps
    laps <<- split(tf, tf[,1])
    lapl <- lapply(laps, plap)
    lapdf <<- data.frame(t(sapply(lapl,c)))
}

# process a lap
plap <- function(lap) {
    laplen <- length(lap[,2])
    # take the difference between the first and last sample for the lap, should really take the time from the next lap
    lt <- (lap[laplen,2] - lap[1,2])/1000.0
    KW <- max(lap[,13])
    gms <<- 9.80665
    # lap number, time in seconds, time converted to MM:SS.mmm format
    lapinfo <- data.frame(lapnum=lap[1,1], seconds=lt, minutes=sprintf("%d:%02d.%03d", lt%/%60, round(lt%%60), (1000*lt)%%1000),
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
    titlePanel("Shiny Tesla Telemetry Analyzer"),
    DTOutput("sumtab"),
    # Show a map of the track
    leafletOutput("map", height=600)
)

# colur the circles
accelcolor <- function(accel) {
    ifelse(accel > 0, "red", "blue")
}

# Define server logic for viewing trackfile
server <- function(input, output) {
    output$map <- renderLeaflet({
        lapc <- input$sumtab_rows_selected
        if (length(lapc) == 0) {
            lapc <- 1:length(laps)
        }
        tfl <- tf[tf$Lap %in% lapc,]
        leaflet(tfl) %>% addTiles() %>%
            fitBounds(min(tf[5]),  min(tf[4]), max(tf[5]), max(tf[4])) %>%
            addCircles(lng=turns$lng, lat=turns$lat, popup=row.names(turns), color="black", radius=6) %>%
            addCircles(lng=~Longitude..decimal., lat=~Latitude..decimal., radius=2,
                       color=~accelcolor(Longitudinal.Acceleration..m.s.2.),
                       label=~paste(Speed..MPH., "mph ", round(Lateral.Acceleration..m.s.2./gms, 2), "G"))
    })
        
    output$sumtab <- renderDT({
        datatable(lapdf, selection=list(selected=1, mode='single'))
    })
}

# Run the application 
ptf(file.choose())
shinyApp(ui = ui, server = server)
