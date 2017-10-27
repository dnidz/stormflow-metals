## Wet weather streams - Trend analyses
# Daniel Nidzgorski
# July 6, 2017

# Libraries
library(tidyverse)
library(lubridate)
library(stringr)
library(scales)
library(NADA)



# Clear desktop & variables
rm (list=ls())
# Then read in external functions
source("Heatmap plotting function.R")
source("Faceted regression plotting function.R")



wwmetals<-read_csv("Metals wet weather - MDL replaced.csv",
                   col_types=cols(
                     .default = col_character(),
                     Date = col_date(),
                     Year = col_double(),
                     Month = col_double(),
                     Value = col_double(),
                     MDL = col_double(),
                     RDL = col_double(),
                     Hardness = col_double(),
                     BelowMDL = col_logical(),
                     Replacement = col_double(),
                     RValue = col_double()
                   )
) 
# data<-wwmetals
# param<-"Copper, Total"
# stream<-"Thornton Creek"

cenken.reg<-function(data,param,stream) {
  d<-data %>%
    filter(Parameter==param,
           Stream==stream) %>%
    # Create a censored value that has the MDL if censored
    mutate(CValue=ifelse(BelowMDL,MDL,Value))
  
  # cenken needs numeric x and y -- can't use Date class
  # so consider Year instead of specific date
  r<-cenken(d$CValue,d$BelowMDL,d$Year)
  
  output<-tibble(
    Parameter=param,
    Stream=stream,
    slope=r$slope,
    intercept=r$intercept,
    tau=r$tau,
    p=r$p
  )
  
  output
}


# Function to do a censored regression for all streams in a single parameter
cenkenreg.param<-function(data,param) {
  d<-data %>%
    filter(Parameter==param)
  
  # Only streams sampled in at least three years for that parameter
  streams.list<-d %>%
    group_by(Stream) %>%
    summarize(numYears=length(unique(Year))) %>%
    filter(numYears>=3) %>%
    .$Stream
  
  output<-map_df(streams.list,cenken.reg,data=data,param=param)
  
  output
}

# Map to call all parameters
params.list<-unique(wwmetals$Parameter)
cenkenreg.output<-map_df(params.list,cenkenreg.param,data=wwmetals)

cenkenreg.output<-cenkenreg.output %>%
  arrange(p)


cenkenreg.sig<-cenkenreg.output %>%
  filter(p<0.05) %>%
  arrange(slope)

map(unique(cenkenreg.sig$Parameter),regression.plot,
    data=wwmetals,regdata=cenkenreg.sig,
    x="Year",y="RValue",xlabel="",logy=F)


# Comparing slope values isn't that helpful given the huge range between some metals as 0.1 ug/L and some at 10000 ug/L.
# Normalize slope by each stream's mean for each metal to see if that's more useful.
stream.means<-wwmetals %>%
  group_by(Parameter,Stream) %>%
  summarize(SMean=mean(RValue,na.rm=T))

cenkenreg.norm<-cenkenreg.sig %>%
  left_join(stream.means,by=c("Parameter","Stream")) %>%
  # Percent change, relative to stream mean, per decade
  mutate(DecadalChange=((10*slope)/SMean)*100) %>%
  arrange(DecadalChange)

c.hm<-cenkenreg.norm %>%
  plot.heatmap(method="none",x="Stream",y="Parameter",value="DecadalChange",midpoint=0,reorder.X=T,reorder.Y=T)
ggsave(c.hm,filename="Trend regression heatmap.png",width=15,height=20)

# What if we analyze all streams lumped together?
# However, I don't really think that it makes ecological sense to expect that
# a given metal would necessarily show same/similar trends across all streams.
## WARNING: This takes a loooong time to execute
cenkenreg.allstreams<-function(data,param) {
  d<-data %>%
    filter(Parameter==param) %>%
    # Create a censored value that has the MDL if censored
    mutate(CValue=ifelse(BelowMDL,MDL,Value))
  
  # cenken needs numeric x and y -- can't use Date class
  r<-cenken(d$CValue,d$BelowMDL,d$Year)
  
  output<-tibble(
    Parameter=param,
    slope=r$slope,
    intercept=r$intercept,
    tau=r$tau,
    p=r$p
  )
  
  output
}

# The safely() wrapper is needed b/c one parameter (dissolved aluminum?) throws an error
# cenkenreg.lumped.output<-map(params.list,safely(cenkenreg.allstreams),data=wwmetals) 
# 
# cenkenreg.lumped<-cenkenreg.lumped.output%>%
#   transpose() %>%
#   .$result %>%
#   {map_df(seq_along(.),function(x) .[[x]])}
# 
# cenkenreg.lumped<-cenkenreg.lumped %>%
#   arrange(p)
# View(cenkenreg.lumped)
# lumped.stat<-(0.95^(1/length(cenkenreg.lumped(!is.na(p)))))
# write_csv(cenkenreg.lumped,"Trend analysis - all streams lumped by parameter.csv")

## Plotting: heatmap-like tile plot
trend.plot.data<-cenkenreg.norm %>%
  mutate(fill=NA,
         fill=ifelse(DecadalChange>0,1,fill),
         fill=ifelse(DecadalChange>10,2,fill),
         fill=ifelse(DecadalChange>50,3,fill),
         fill=ifelse(DecadalChange>100,4,fill),
         fill=ifelse(DecadalChange<0,-1,fill),
         fill=ifelse(DecadalChange< -10,-2,fill),
         fill=ifelse(DecadalChange< -50,-3,fill),
         fill=ifelse(DecadalChange< -100,-4,fill),
         fill=as.factor(fill),
         logDC=log10(abs(DecadalChange)),
         logDC=ifelse(DecadalChange<0,-logDC,logDC))
         
trend.tile<-plot.heatmap(trend.plot.data,x="Stream",y="Parameter",value="logDC",method="none",
                         midpoint=0,label="DecadalChange",labelsize=2.5,labeldigits=0,
                         reorder.X=F,reorder.Y=F)

ggsave(trend.tile,filename="Trend analysis - decadal change tilemap.png",
       width=10,height=7.5)
  


