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
    # remove lap 0
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
    lapinfo <- data.frame(lapnum=lap[1,1], seconds=lt, minutes=sprintf("%d:%02d.%03d", lt%/%60, round(lt%%60), round((1000*lt)%%1000)),
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
            h3("Pick laps to analyze"),
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
                tabPanel("Plots"),
                tabPanel("Turns")
            )
        )
    )
)

# colur the circles
accelcolor <- function(accel) {
    ifelse(accel > 0, "red", "blue")
}

# Define server logic for viewing trackfile
server <- function(input, output) {
    # sidebar lap selector
    output$laplist <- renderDT({
        datatable(lapdf[,c(3,4,6,7,9)], selection=list(selected=1, mode='multiple'),
                  options=list(pageLength=20, ordering=FALSE, initComplete=htmlwidgets::JS(
                      "function(settings, json) {",
                      "$(this.api().table().container()).css({'font-size': '80%'});",
                      "}")
                  )
        )       
    })
    
    # Speed tab table and map
    output$speedtab <- renderDT({
        datatable(lapdf[input$laplist_rows_selected, 2:11], selection=list(selected=1, mode='single'),
                  options=list(pageLength=5, ordering=FALSE, initComplete=htmlwidgets::JS(
                      "function(settings, json) {",
                      "$(this.api().table().container()).css({'font-size': '80%'});",
                      "}")
                  )
        )
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
        )
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
}

# Run the application
# read in locations of turn apexes for a specific track - default Laguna Seca
turns <- read.csv("turns.csv",row.names=1)
ptf(file.choose())
shinyApp(ui = ui, server = server)
