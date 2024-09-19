# Forecasting fDOM in VA reservoirs 
Repository for development and analysis of fluorescent dissolved organic matter (fDOM) forecasts in Southwest VA reservoirs

## Instructions to recreate figures NOTE THIS WILL TAKE ~HOUR
1.  Download or clone repository to your local computer 
be clear that you just need packages and script 4 - BUT it just reproduces results; not reruns 4casts
ADD note about how to run an example forecast here as well



## Instructions to recreate forecast and scores NOTE THIS WILL TAKE DAYS
1.  Run `1a_Install_Packages.R` and `1b_Download_Data.R` located in `./Scripts/`. These scripts will download all need packages and data to rerun the entire analysis.

2a.  Next run the Scripts in the folder `./Scripts/` in the following order: 2a through 2f. These scripts will run fDOM forecasts from 12dec22 through 11aug24 using 6 different forecast models. Supporting functions to run these models can be found in `./Scripts/Functions` folder. Each function scripts contains one to multiple functions that support the generation of different forecast models. These scripts also contain code at the bottom that can be un-commented to run an example forecast. **NOTE**: each script labeled 2a - 2f can take < 1 -  > 10 hours to run depending on forecast model, and some for loops will break when missing data is present, so the loop will have to be restarted.

3.  To compile generated forecasts run the script `./Scripts/3_Compile_4cast_outputs.Rmd`, this script will compile all forecasts into a csv with forecast scores to allow easier analysis.

4.  Finally run script `./Scripts/4_Analyze_4casts.Rmd`, to analyze forecast performance.

**NOTE** on data folder: This folder is used to both hold generated data from script `./Scripts/1b_Download_Data.R` that is to large to reload each forecast run, and hold data from forecast outputs. Do to large size of these folders and csv, not all data is pushed to github, but if you want to recreate steps from the middle of the workflow using generated data, data can be accessed at XYZ zenodo repository OR if you want to recreate all analysis steps these folders can be used to hold outputs. 
