## Wet weather streams report
## Initial data processing
# Daniel Nidzgorski
# June 2, 2017

# Libraries
library(tidyverse)
library(lubridate)
library(stringr)
library(scales)

# Clear desktop & variables
rm (list=ls())
# Set working directory
setwd("G:/Share/Bouchard/Final King County Streams Monitoring/Daniel/R")


# Extract the streams data from the raw LIMS data.
raw.streams<-read_csv("rawdata.csv",
              col_types=cols(
                .default = col_character(),
                ROWID = col_integer(),
                TIMESPAN = col_double(),
                SAMPLE_DEPTH = col_double(),
                TOTAL_SOLIDS = col_double(),
                NUMVALUE = col_double(),
                MDL = col_double(),
                RDL = col_double()
              )) %>%
  filter(SITE=="STREAMS")
# write.csv(raw.streams,"raw data - streams only.csv")

# Use Martin's final data to make some translation tables:
# Note that one 2/8/96 sample in the original was listed as "stormwater" -- edited in csv file.
final<-read_csv("Streams Final Data Set.csv",
                col_types=cols(
                  .default = col_character(),
                  YEAR = col_integer(),
                  MONTH = col_integer(),
                  DAY = col_integer(),
                  SAMPLE_DEPTH = col_double(),
                  NUMVALUE = col_double(),
                  MDL = col_double(),
                  RDL = col_double()
                  # `Calculated Hardness (mg/L)` = col_double(),
                  # `Acute Criteria ug/L` = col_double(),
                  # `Conc of Acute Exceedance` = col_double(),
                  # `Chronic ug/L` = col_double(),
                  # `Conc of Chronic Excedance` = col_double(),
                  # `Criteria Source` = col_double(),
                  # `Stand Agains Acute` = col_double(),
                  # `Stand Against Chronic` = col_double()
                )) %>%
  filter(PROJECT!="zdummy")

# Location
all.locations<-select(final,LOCATOR,STREAM,`POSITION ON STREAM`,`Reporting Group`)
locator<-unique(all.locations) %>%
  # Make "B" or "C" stream names so non-FDL sites have unique stream names
  mutate(STREAM=ifelse(str_detect(`Reporting Group`,"Other 1") | 
                         str_detect(`Reporting Group`,"OTHER 1"),
                       sprintf("%s B",STREAM),STREAM),
         STREAM=ifelse(str_detect(`Reporting Group`,"Other 2") | 
                         str_detect(`Reporting Group`,"OTHER 2"),
                       sprintf("%s C",STREAM),STREAM)
  )
# write.csv(locator,"trans-locator.csv")

# Parameters
all.parameters<-select(final,PARMNAME,`GROUP 1`,`GROUP 2`)
parameter<-unique(all.parameters)
# write.csv(parameter,"trans-parameter.csv")
# Add in my list of parameters that Martin hadn't classified
parameter<-read_csv("trans-unclassified parameters.csv",
                    col_names=c("PARMNAME","GROUP 1","GROUP 2"),
                    col_types=cols(
                      PARMNAME = col_character(),
                      `GROUP 1` = col_character(),
                      `GROUP 2` = col_character()
                    )) %>%
  bind_rows(parameter)

# Routine vs. wet-weather
final$Date<-as.Date(mdy(final$COLLECTDATE))
all.dates<-select(final,Date,Routine=`SAMPLE TYPE (ROUTINE/STORM)`)
dates<-unique(all.dates)
# write.csv(dates,"trans-dates.csv")

# Merge raw streams data with translation tables
# First get a clean date field in raw.streams
raw.streams$Date<-raw.streams$COLLECTDATE %>%
  mdy_hm() %>%
  as.Date()

merge<-raw.streams %>%
  left_join(locator) %>%
  left_join(parameter) %>%
  left_join(dates) 

merge$Year<-year(merge$Date)
merge$Month<-month(merge$Date)

# Make a clean streams dataframe, limited by years, with only the useful columns (renamed as needed)
streams<-merge %>%
  filter(Year>=1993 & Year<=2010) %>%
  select(Sample=LABSAMPLENUM,
         Date,Year,Month,
         Routine,
         Locator=LOCATOR,
         Stream=STREAM,
         Position=`POSITION ON STREAM`,
         LocGroup=`Reporting Group`,
         MetalOrg=`GROUP 1`,
         Parameter=`GROUP 2`,
         Parmname=PARMNAME,
         Value=NUMVALUE,
         Units=UNITS,
         Qual=QUALIFIER,
         MDL,RDL,TEXTVALUE
  )

# The lab didn't calculate hardness for all samples, so it needs to be 
# recalculated from calcium + magnesium, expressed as CaCO3. 
# Hardness calculations - use total Ca & Mg
hardness<-streams %>%
  filter(Parameter %in% c("Calcium, Total","Magnesium, Total",
                          "Calcium, Dissolved","Magnesium, Dissolved")) %>%
  select(Sample,Date,Parameter,Value) %>%
  spread(key=Parameter,value=Value) %>%
  mutate(Hardness=(`Calcium, Total`*2.497 + `Magnesium, Total`*4.118)/1000)
# There are some samples where the lab calculated hardness, but didn't report Ca or Mg. Include those
# hardness values
hardness.LIMS<-streams %>%
  filter(str_detect(Parmname,"Hardness")) %>%
  select(Sample,Hardness.LIMS=Value)

hardness<-hardness %>%
  full_join(hardness.LIMS) %>%
  mutate(Hardness=ifelse(is.na(Hardness) & !is.na(Hardness.LIMS),
                         Hardness.LIMS,Hardness)
  )

streams<-left_join(streams,select(hardness,Sample,Hardness))

write_csv(streams,"Streams - processed including bad data.csv")

## Look for (and deal with) samples where dissolved is higher than total
dissolved2<-streams %>%
  filter(MetalOrg=="Metal",
         !is.na(Value)) %>%
  select(Date,Locator,Stream,Sample,
         Parameter.D=Parameter,
         Value.D=Value,
         Qual.D=Qual,
         Text.D=TEXTVALUE) %>%
  filter(str_detect(Parameter.D,"Dissolved")) %>%
  mutate(Metal=str_extract(Parameter.D,"\\w+"))

total2<-streams %>%
  filter(MetalOrg=="Metal",
         !is.na(Value)) %>%
  select(Date,Locator,Stream,Sample,
         Parameter.T=Parameter,
         Value.T=Value,
         Qual.T=Qual,
         Text.T=TEXTVALUE) %>%
  filter(str_detect(Parameter.T,"Total")) %>%
  mutate(Metal=str_extract(Parameter.T,"\\w+"))

all2<-inner_join(dissolved2,total2,
                 by = c("Date", "Locator", "Stream","Sample","Metal")) %>%
  mutate(PctDissolved=Value.D/Value.T*100)

over2<-all2 %>%
  filter(PctDissolved>100) %>%
  select(Metal,Stream,Locator,Date,Sample,
         Dissolved=Value.D,
         Total=Value.T,
         PctDissolved,
         Qual.D,Text.D,Qual.T,Text.T) %>%
  arrange(Metal,Stream,Date) %>%
  mutate(BothBelowRDL=str_detect(Qual.D,"<RDL") & str_detect(Qual.T,"<RDL"),
         BothBelowRDL=ifelse(is.na(BothBelowRDL),FALSE,BothBelowRDL))

# write_csv(over2,"Dissolved higher than total.csv")

bad<-tribble(
  ~Sample, ~Parameter,
  "L47748-4", "Nickel, Dissolved",
  "L44956-10", "Copper, Dissolved",
  "L47748-8", "Zinc, Total",
  "L20769-2", "Zinc, Dissolved",
  "L19079-6", "Magnesium, Dissolved",
  "L15085-12", "Copper, Dissolved",
  "L19079-8", "Magnesium, Dissolved",
  "L19079-6", "Calcium, Dissolved",
  "L20695-2", "Mercury, Dissolved",
  "L19079-8", "Calcium, Dissolved"
)

streams.filtered<-anti_join(streams,bad,by = c("Sample", "Parameter"))

# write_csv(streams.filtered,"Streams - processed data.csv")



   
                