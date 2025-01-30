#### Install needed packages 

install.packages('tidyverse')
install.packages('arrow')
install.packages('devtools')
install.packages('feasts')
install.packages('imputeTS') # got a notice that this was needed for Met regression model function script - is that actually true?
install.packages('zoo') # got a notice that this was needed for RainLag model function script - is that actually true?
install.packages('fable') # needed for NNETAR
install.packages('urca') # needed for NNETAR
install.packages('aws.s3') 
install.packages('tsibble') 
install.packages('fable') 


devtools::install_github("eco4cast/score4cast")
