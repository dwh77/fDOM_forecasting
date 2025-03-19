#### Install needed packages 

install.packages('tidyverse')
install.packages('arrow')
install.packages('devtools')
install.packages('feasts')
install.packages('imputeTS') # Met regression model
install.packages('zoo') # Rain Lag model 
install.packages('fable') # needed for NNETAR
install.packages('urca') # needed for NNETAR
install.packages('aws.s3') 
install.packages('tsibble') 
install.packages('fable') 
install.packages('patchwork') 
install.packages('ggpubr') 
install.packages('plotly') 
install.packages('plyr') 

devtools::install_github("eco4cast/score4cast")
