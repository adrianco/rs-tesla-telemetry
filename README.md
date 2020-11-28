# rs-tesla-telemetry
RStudio Shiny viewer for Tesla Telemetry Track Mode files - personal spare time project.

Files are saved to the USB data logger port that is also used for Sentry videos. There's a silent video file of the track mode session, and a CSV format file named telemetry-v1-date-time.csv, there are 29 columns of data.

Running the app, the telemetry file needs to be on the same machine, specified on the command line or from a chooser popup when run from RStudio. After the file is read it starts the user interface. This removes incomplete laps, sorts by fastest lap time, shows a summary of the performance data lap by lap, and summary tabs with a map view of the selected laps, and comparison plots of selected metrics vs. distance (rather than time, to align better). Plots can be zoomed in and out. Metadata about the session can be entered, and can be saved to a metadata file named and located the same as the telemetry file, which is loaded if it exists at startup. For each track, turn locations can be specified, for now this was manually created for Laguna Seca.

To run the app from the command line without RStudio give the track file path like this:
```
% Rscript app.R /Users/anc/Dropbox/cars/TeslaTrackMode/telemetry-v1-2020-10-09-11_56_58.csv

Attaching package: ‘DT’

The following objects are masked from ‘package:shiny’:

    dataTableOutput, renderDataTable

collapse 1.4.0, see ?`collapse-package` or ?`collapse-documentation`
Note: stats::D  ->  D.expression, D.call, D.name

Attaching package: ‘collapse’

The following object is masked from ‘package:stats’:

    D


Listening on http://127.0.0.1:6344
```
Then navigate to the URL and port shown on your machine using a web browser. Use ^C to stop the Rscript server.
If anything goes wrong, a log of the execution is written to app.Rout.

On MacOS, you can use $ zsh runme.sh /Users/anc/Dropbox/cars/TeslaTrackMode/telemetry-v1-2020-10-09-11_56_58.csv which will open the browser for you.


This is the header line for the csv
```
Lap,Elapsed Time (ms),Speed (MPH),Latitude (decimal),Longitude (decimal),Lateral Acceleration (m/s^2),Longitudinal Acceleration (m/s^2),Throttle Position (%),Brake Pressure (bar),Steering Angle (deg),Steering Angle Rate (deg/s),Yaw Rate (rad/s),Power Level (KW),State of Charge (%),Tire Pressure Front Left (bar),Tire Pressure Front Right (bar),Tire Pressure Rear Left (bar),Tire Pressure Rear Right (bar),Brake Temperature Front Left (% est.),Brake Temperature Front Right (% est.),Brake Temperature Rear Left (% est.),Brake Temperature Rear Right (% est.),Front Inverter Temp (%),Rear Inverter Temp (%),Battery Temp (%),Tire Slip Front Left (% est.),Tire Slip Front Right (% est.),Tire Slip Rear Left (% est.),Tire Slip Rear Right (% est.)
```

Here are the names after reading into R
```
> names(laps[[1]])
 [1] "Lap"                                   
 [2] "Elapsed.Time..ms."                     
 [3] "Speed..MPH."                           
 [4] "Latitude..decimal."                    
 [5] "Longitude..decimal."                   
 [6] "Lateral.Acceleration..m.s.2."          
 [7] "Longitudinal.Acceleration..m.s.2."     
 [8] "Throttle.Position...."                 
 [9] "Brake.Pressure..bar."                  
[10] "Steering.Angle..deg."                  
[11] "Steering.Angle.Rate..deg.s."           
[12] "Yaw.Rate..rad.s."                      
[13] "Power.Level..KW."                      
[14] "State.of.Charge...."                   
[15] "Tire.Pressure.Front.Left..bar."        
[16] "Tire.Pressure.Front.Right..bar."       
[17] "Tire.Pressure.Rear.Left..bar."         
[18] "Tire.Pressure.Rear.Right..bar."        
[19] "Brake.Temperature.Front.Left....est.." 
[20] "Brake.Temperature.Front.Right....est.."
[21] "Brake.Temperature.Rear.Left....est.."  
[22] "Brake.Temperature.Rear.Right....est.." 
[23] "Front.Inverter.Temp...."               
[24] "Rear.Inverter.Temp...."                
[25] "Battery.Temp...."                      
[26] "Tire.Slip.Front.Left....est.."         
[27] "Tire.Slip.Front.Right....est.."        
[28] "Tire.Slip.Rear.Left....est.."          
[29] "Tire.Slip.Rear.Right....est.."  
```
 
The app also persists a list of laps, with the structure shown above, that have been collapsed so that each GPS point has averaged data (6-10 points) to reduce the size, and a summary data frame lapdf as shown below.

```
> names(lapdf)
 [1] "num"              "seconds"          "minutes"          "maxMph"          
 [5] "maxKW"            "maxBhp"           "maxLateralG"      "maxAccelG"       
 [9] "maxBrakeG"        "maxBrakePedal"    "startChargePct"   "maxBrakeTempPct" 
[13] "maxFrontInverter" "maxRearInverter"  "maxBatteryTemp"  
 ```
 
 The code drops lap 0 (driving in the hot pit), splits the rest of the data into laps, filters out laps that don't start and end in the same place and cover the whole circuit, and calls plap on each to generate a row for each lap in the data frame, which is then sorted to have the fastest lap first. This is summarized in two tabs, one for speed and the other for temperature, and allows you to pick a lap to show on the map, and a plotting tab that lets you choose multiple laps and add more lines to the plot.
 
 ![Screenshot](screenshots/rs-tesla-telemetry-screenshot.png)
 
 ![Screenshot](screenshots/rs-tesla-telemetry-screenshot-temp.png)
 
 ![Screenshot](screenshots/rs-tesla-telemetry-distanceplot.png)
 
 ![Screenshot](screenshots/rs-tesla-telemetry-plotzoom.png)
 
 ![Screenshot](screenshots/rs-tesla-telemetry-screenshot-temp2.png)
 
 ![Screenshot](screenshots/rs-tesla-telemetry-metadata.png)
 
