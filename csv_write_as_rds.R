# ONLY NEEDS TO BE RUN WHEN CSV DATA IS UPDATED
# Often we receive data as CSV rather than SQL query piped into R
#write as RDS TO BE read in global.R --------

library(tidyverse)
library(zipcode)
library(readr)

# excluded dataset -----------------------------------------------------------------

excl <- fread('C:/Users/F404668/Documents/data/asg/excl_dataset_SRS.csv')
excl <- sample_n(excl, 1000)
data(zipcode)

drop.cols <-c('city','state.y')
excl <- left_join(excl,zipcode,by="zip") %>%
  dplyr::select(-one_of(drop.cols)) %>%
  rename(state = state.x)
write_rds(excl, "C:/Users/F404668/Desktop/Doudou project/Shiny/excluded/excl.rds")

# dataset -----------------------------------------------------------------