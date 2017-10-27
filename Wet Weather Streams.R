## Wet weather streams report
# Daniel Nidzgorski
# July 6, 2017

# Libraries
library(tidyverse)
library(lubridate)
library(stringr)
library(scales)

# Clear desktop & variables
rm (list=ls())
# Set working directory
setwd("G:/Share/Bouchard/Final King County Streams Monitoring/Daniel/R")

wwmetals<-read_csv("Metals wet weather - MDL replaced.csv",
                   col_types=cols(
                     .default = col_character(),
                     X1 = col_integer(),
                     Date = col_date(format = ""),
                     Year = col_integer(),
                     Month = col_integer(),
                     Value = col_double(),
                     MDL = col_double(),
                     RDL = col_double(),
                     BelowMDL = col_logical(),
                     Replacement = col_double(),
                     RValue = col_double()
                   )
) %>%
  select(-X1)

rmetals<-read_csv("Metals routine - MDL replaced.csv",
                  col_types=cols(
                    .default = col_character(),
                    X1 = col_integer(),
                    Date = col_date(format = ""),
                    Year = col_integer(),
                    Month = col_integer(),
                    Value = col_double(),
                    MDL = col_double(),
                    RDL = col_double(),
                    BelowMDL = col_logical(),
                    Replacement = col_double(),
                    RValue = col_double()
                  )
) %>%
  select(-X1)

