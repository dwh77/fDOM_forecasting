# Forecasting fDOM in VA reservoirs 
Repository for development and analysis of fluorescent dissolved organic matter (fDOM) forecasts in Southwest VA reservoirs

## Instructions to recreate figures
1.  Download or clone repository to your local computer
2.  Run './Scripts/1a_Install_Packages.R' to download needed packages
3.  Data for analysis and figure generation is already available in the github, if you want to recreated forecast summary files, see section below title "Instructions to recreate forecasts and scores"
4.  Run './Scripts/4_Analyze_4casts.Rmd' this script will generate figure and summary stats presented in the paper
5.  If you're interested in running an example forecast to see how the code works see section below titled "Running an Example Forecast"


## Instructions to recreate forecast and scores 
**NOTE: it will take several days to rerun all forecasts and require running multiple scripts and restarting functions in this time period**

1.  Run `1a_Install_Packages.R` and `1b_Download_Data.R` located in `./Scripts/`. These scripts will download all need packages and data to rerun the entire analysis.
2.  Next run the Scripts in the folder `./Scripts/` in the following order: 2a through 2f. These scripts will run fDOM forecasts from 12dec22 through 11aug24 using 6 different forecast models. Supporting functions to run these models can be found in `./Scripts/Functions` folder. Each function scripts contains one to multiple functions that support the generation of different forecast models. These scripts also contain code at the bottom that can be un-commented to run an example forecast. **NOTE**: each script labeled 2a - 2f can take < 1 -  > 10 hours to run depending on forecast model, and some for loops will break when missing data is present, so the loop will have to be restarted.
3.  To compile generated forecasts run the script `./Scripts/3_Compile_4cast_outputs.Rmd`, this script will compile all forecasts into a csv with forecast scores to allow easier analysis.
4.  Finally run script `./Scripts/4_Analyze_4casts.Rmd`, to analyze forecast performance.

**NOTE** on data folder: This folder is used to both hold generated data from script `./Scripts/1b_Download_Data.R` that is to large to reload each forecast run, and hold data from forecast outputs. Do to large size of these folders and csv, not all data is pushed to github, but if you want to recreate all analysis steps these folders are used to hold outputs. 

## Instructions to run an example forecast

1.  Run `1a_Install_Packages.R` and `1b_Download_Data.R` located in `./Scripts/`. These scripts will download all need packages and data to rerun the entire analysis.
2.  Each script in the `./Scripts/Functions` folder contains example code at the bottom of the script to run an example forecast for one day. Make sure you have run scripts 1a and 1b. Then uncomment the 'Test Function' code at the bottom of the script and run the entire script to generate an example forecast using each model type. 

