## Wet weather streams - timeseries graphs
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

rmetals<-read_csv("Metals routine - MDL replaced.csv",
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

# # For building/testing the functions
# data<-wwmetals
# stream<-"Pipers Creek"
# param<-"Copper, Total"

# Pass in long-format tibble that rbinds wet-weather and routine data. 
# Single graphs -- one stream, one parameter, save to file
streamplot<-function(data,stream,param){
  
  # Subset data to the desired stream and parameter
  # and make annual means of replaced-values, median MDL, (and eventually add criteria?)
      d<-data %>%
      filter(Stream==stream,Parameter==param,Routine=="wet weather") 
      
      m<-d %>%
      group_by(Year) %>%
      summarize(AnnualMean=mean(RValue,na.rm=T),
                MDL=median(MDL,na.rm=T),
                HighNonDetect=(sum(!str_detect(RVqual,"HighNonDetect"))==0)
      )
     

  ytitle<-sprintf("%s - %s",stream,param)
  ylabel<-sprintf("%s (µg/L)",param)
  
  p<-ggplot(data=d,aes(x=Year,y=RValue))+
    # Individual data points
    geom_point(aes(shape=BelowMDL),alpha=0.25,size=2,fill="black")+
    scale_shape_manual(values=c("FALSE"=19,"TRUE"=23),
                       labels=c("FALSE"="Above MDL","TRUE"="Below MDL"))+
    # Annual means
    geom_line(data=m,aes(x=Year,y=AnnualMean))+
    geom_point(data=m,aes(x=Year,y=AnnualMean),size=3,shape=19,fill="black")+
    # High nondetects
    geom_col(data=filter(m,HighNonDetect),aes(x=Year,y=MDL),
             width=0.01,color="lightblue",fill="lightblue")+
    # MDL marks
    geom_point(aes(data=m,x=Year,y=MDL),shape="-",size=7,color="blue")+
    
    scale_x_continuous(breaks=seq(1993,2010,5),
                       limits=c(1993,2010))+
    coord_cartesian(xlim=c(1992,2010))+
    theme(axis.text.x=element_text(size=11),
          axis.text.y=element_text(size=11),
          axis.title.x=element_blank(),
          axis.title.y=element_text(size=13),
          legend.title=element_blank(),
          legend.justification=c(0,1),
          legend.position=c(0,1),
          legend.background = element_rect(fill=NA)
    )+ guides(shape=guide_legend(nrow=1))+
    labs(title=ytitle,y=ylabel)
  
  filename<-sprintf("%s.png",ytitle)
  
  ggsave(p,file=filename,width=6,height=3)
}

# Faceted graphs -- all streams for a given parameter
allstreamsplot<-function(data,param){
  
  # Subset data to the desired parameter
  # and make annual means of replaced-values, median MDL, (and eventually add criteria?)
  d<-data %>%
    filter(Parameter==param) 
  
  m<-d %>%
    group_by(Stream,Year) %>%
    summarize(AnnualMean=mean(RValue,na.rm=T),
              maxMDL=max(MDL,na.rm=T),
              HighNonDetect=(sum(!str_detect(RVqual,"HighNonDetect"))==0)
    ) %>%
    mutate(Date=as.Date(sprintf("%s-07-01",Year)))
  
  
  ytitle<-param
  ylabel<-sprintf("%s (µg/L)",param)
  start<-as.Date("1993-01-01")
  end<-as.Date("2010-12-31")
  max<-max(d$RValue,na.rm=T)*1.05
  
  p<-ggplot(data=d,aes(x=Date,y=RValue))+
    facet_wrap(~Stream,ncol=3)+
    
    # MDL marks for each sampling date
    geom_point(aes(x=Date,y=MDL),shape="-",size=7,color="blue")+
    
    # Individual data points
    geom_point(aes(shape=BelowMDL),alpha=0.25,size=3,fill="black")+
    scale_shape_manual(values=c("FALSE"=19,"TRUE"=23),
                       labels=c("FALSE"="Above MDL","TRUE"="Below MDL"))+
    # Annual means
    geom_line(data=m,aes(x=Date,y=AnnualMean))+
    # geom_point(data=m,aes(x=Year,y=AnnualMean),size=3,shape=19,fill="black")+
    # High nondetects
    geom_col(data=filter(m,HighNonDetect),aes(x=Date,y=maxMDL),
             width=0.03,color="lightblue",fill="lightblue")+

    
    scale_x_date(breaks=as.Date(c("1993-07-01","1998-07-01","2003-07-01","2008-07-01")),
                 labels=c("7/1/1993","7/1/1998","7/1/2003","7/1/2008"),
                 date_minor_breaks="1 year",
                 limits=c(start,end))+
    coord_cartesian(ylim=c(0,max))+
    theme(axis.text.x=element_text(size=11),
          axis.text.y=element_text(size=11),
          axis.title.x=element_blank(),
          axis.title.y=element_text(size=13),
          legend.title=element_blank(),
          legend.justification=c(0,1),
          legend.position=c(0,1),
          legend.background = element_rect(fill=NA)
    )+ guides(shape=guide_legend(nrow=1))+
    labs(title=ytitle,y=ylabel)
  

  filename<-sprintf("All streams - %s.png",param)
  
  ggsave(p,file=filename,width=15,height=20)
}

allparamsplot<-function(data,stream){
  
  # Subset data to the desired stream
  # and make annual means of replaced-values, median MDL, (and eventually add criteria?)
  d<-data %>%
    filter(Stream==stream)
  
  m<-d %>%
    group_by(Parameter,Year) %>%
    summarize(AnnualMean=mean(RValue,na.rm=T),
              maxMDL=max(MDL,na.rm=T),
              HighNonDetect=(sum(!str_detect(RVqual,"HighNonDetect"))==0)
    ) %>%
    mutate(Date=as.Date(sprintf("%s-07-01",Year)))
  
  ytitle<-stream
  ylabel<-"µg/L"
  start<-as.Date("1993-01-01")
  end<-as.Date("2010-12-31")

  p<-ggplot(data=d,aes(x=Date,y=RValue))+
    facet_wrap(~Parameter,ncol=3,scale="free_y")+

    # Individual data points
    geom_point(aes(shape=BelowMDL),alpha=0.25,size=3,fill="black")+
    scale_shape_manual(values=c("FALSE"=19,"TRUE"=23),
                       labels=c("FALSE"="Above MDL","TRUE"="Below MDL"))+
    # Annual means
    geom_line(data=m,aes(x=Date,y=AnnualMean))+
    # geom_point(data=m,aes(x=Year,y=AnnualMean),size=3,shape=19,fill="black")+

    
    scale_x_date(breaks=as.Date(c("1993-07-01","1998-07-01","2003-07-01","2008-07-01")),
                 labels=c("7/1/1993","7/1/1998","7/1/2003","7/1/2008"),
                 date_minor_breaks="1 year",
                 limits=c(start,end))+
    # coord_cartesian(ylim=c(0,max))+
    theme(axis.text.x=element_text(size=11),
          axis.text.y=element_text(size=11),
          axis.title.x=element_blank(),
          axis.title.y=element_text(size=13),
          legend.title=element_blank(),
          legend.justification=c(0,1),
          legend.position=c(0,1),
          legend.background = element_rect(fill=NA)
    )+ guides(shape=guide_legend(nrow=1))+
    labs(title=ytitle,y=ylabel)
  
  filename<-sprintf("All params - %s.png",stream)
  
  ggsave(p,file=filename,width=15,height=20)
}

# Make faceted plots for each parameter, wet-weather only
params.list<-unique(wwmetals$Parameter)
map(params.list,allstreamsplot,data=wwmetals)


# wwmetals %>%
#   filter(Parameter=="Mercury, Total",
#          Stream=="Eden Creek") %>%
#   View()

# maxima<-allmetals %>%
#   group_by(Parameter) %>%
#   summarize(ymax=max(RValue,na.rm=T)*1.1)
# write.csv(maxima,"Metals maxima.csv")

others<-filter(bind_rows(wwmetals,rmetals),LocGroup!="FDL")
others.list<-unique(others$Stream)
# map(others.list,allparamsplot,data=others)
