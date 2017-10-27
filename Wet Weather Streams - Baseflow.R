## Wet weather streams - Baseflow comparison
# Daniel Nidzgorski
# September 15, 2017

# Libraries
library(tidyverse)
library(lubridate)
library(stringr)
library(scales)
library(npsm)

# Clear desktop & variables
rm (list=ls())
source("Heatmap plotting function.R")

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

# Include parameters that were 100% non-detects
wwmetals.nd<-read_csv("Metals wet weather - all nondetect.csv",
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

rmetals.nd<-read_csv("Metals routine - all nondetect.csv",
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

wwmetals<-bind_rows(wwmetals,wwmetals.nd)
rmetals<-bind_rows(rmetals,rmetals.nd)

# Filter to a common set of dates, streams, parameters
start<-min(rmetals$Date)-months(1)
end<-max(rmetals$Date)+months(1)

wwmetals.filtered<-wwmetals %>%
  filter(Date>=start,Date<=end,
         Stream %in% unique(rmetals$Stream),
         Parameter %in% unique(rmetals$Parameter)) 

rmetals.filtered<-rmetals %>%
  filter(Stream %in% unique(wwmetals.filtered$Stream),
         Parameter %in% unique(wwmetals.filtered$Parameter))

metals<-bind_rows(wwmetals.filtered,rmetals.filtered) %>%
  arrange(Date,Parameter,Routine) 

# Remove parameters that are all non-detects (both wet and routine) within this timeframe
# Appears to be only selenium (both)
params.allnondetects<-metals %>%
  group_by(Parameter) %>%
  summarize(AllNonDetect=all(BelowMDL)) %>%
  filter(AllNonDetect) %>%
  .$Parameter %>%
  unique()

metals<-metals %>%
  filter(!Parameter %in% params.allnondetects)

# Remove streams with only a couple of samples
# Three streams have only 2 wet-weather samples; Eden Creek has 4; others have 6 or more
streams.fewsamples<-metals %>%
  group_by(Stream,Routine) %>%
  summarize(NumSamples=length(unique(Date))) %>%
  filter(NumSamples<3) %>%
  .$Stream %>%
  unique()

metals<-metals %>%
  filter(!Stream %in% streams.fewsamples)

           
           
# Faceted graphs -- all streams for a given parameter
baseflowplot<-function(data,param){
  
  # Subset data to the desired parameter
  # and make annual means of replaced-values, max MDL
  d<-data %>%
    filter(Parameter==param) 
  
  ytitle<-param
  ylabel<-sprintf("%s (µg/L)",param)
  start<-as.Date("2001-04-01")
  end<-as.Date("2003-08-30")
  max<-max(d$RValue,na.rm=T)*1.05
  
  p<-ggplot(data=d,aes(x=Date,y=RValue,shape=Routine,fill=Routine,alpha=BelowMDL))+
    facet_wrap(~Stream,ncol=3)+
    
    # MDL marks for each sampling date
    geom_point(inherit.aes=F,data=d,aes(x=Date,y=MDL),shape="-",size=7,color="blue")+

    # Individual data points
    geom_point(size=4)+
    scale_shape_manual(values=c("wet weather"=21,"Routine"=23),
                       labels=c("wet weather"="Stormflow","Routine"="Baseflow"))+
    scale_fill_manual(values=c("wet weather"="black","Routine"="white"),
                       labels=c("wet weather"="Stormflow","Routine"="Baseflow"))+
    scale_alpha_manual(values=c("TRUE"=0.3,"FALSE"=1),
                      labels=c("TRUE"="Below MDL","FALSE"="Above MDL"))+



    # scale_x_date(breaks=as.Date(c("1993-07-01","1998-07-01","2003-07-01","2008-07-01")),
    #              labels=c("7/1/1993","7/1/1998","7/1/2003","7/1/2008"),
    #              date_minor_breaks="1 year",
    #              limits=c(start,end))+
    scale_x_date(limits=c(start,end))+
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
  
  
  filename<-sprintf("Baseflow - %s.png",param)
  
  ggsave(p,file=filename,width=15,height=20)
  
  filename
}

params.list<-unique(metals$Parameter)
# map(params.list,baseflowplot,data=metals)

PercentBelowMDL<-metals %>%
  group_by(Parameter,Routine) %>%
  summarize(n=n(),
            PercentBelowMDL=sum(BelowMDL)/n*100)

Copper.d<-filter(metals,Parameter=="Copper, Dissolved") %>%
  mutate(FlipVal=ifelse(BelowMDL,100-MDL,100-Value))


# Gehan test
# Note that the Gehan p-value is for a one-sided test
gehan<-function(data,param) {
  d<-data %>%
    filter(Parameter==param)
  
  # Include only streams that have both wet-weather and routine samples (min 3)
  streams.wet<-d %>%
    filter(Routine=="wet weather") %>%
    group_by(Stream) %>%
    summarize(n=n()) %>%
    filter(n>=3) %>%
    .$Stream
  
  streams.routine<-d %>%
    filter(Routine=="Routine") %>%
    group_by(Stream) %>%
    summarize(n=n()) %>%
    filter(n>=3) %>%
    .$Stream
  
  d<-d %>%
    filter(Stream %in% streams.wet,
           Stream %in% streams.routine)
  
  # In some cases with more than one MDL, the highest value can be an MDL value.
  # Make sure the flip value is higher than either.
  flip<-max(c(d$Value,d$MDL),na.rm=T)+10
  
  d<-d %>%
    mutate(FlipVal=ifelse(BelowMDL,flip-MDL,flip-Value),
           Event=!BelowMDL)
  
  # Run the Gehan test
  g<-gehan.test(d$FlipVal,d$Event,d$Routine)
  
  # Mean for wet and routine, using replacement values
  m.wet<-d %>%
    filter(Routine=="wet weather") %>%
    .$RValue %>%
    mean(na.rm=T)
  
  m.routine<-d %>%
    filter(Routine=="Routine") %>%
    .$RValue %>%
    mean(na.rm=T)
  
  # Percent censored for wet and routine
  c.wet<-d %>%
    filter(Routine=="wet weather") %>%
    .$BelowMDL %>%
    {sum(.,na.rm=T)/length(.)*100}
  
  c.routine<-d %>%
    filter(Routine=="Routine") %>%
    .$BelowMDL %>%
    {sum(.,na.rm=T)/length(.)*100}
  
  output<-tibble(
    Parameter=param,
    MeanWet=m.wet,
    MeanRoutine=m.routine,
    GehanStat=g[[1]],
    p=g[[2]],
    CensoredWet=c.wet,
    CensoredRoutine=c.routine
  )
  
  output
}

data<-metals
param<-"Barium, Dissolved"

gehan.output<-map_df(params.list,gehan,data=metals)

# To set an appropriate p-value, adjust by the number of parameters where
# mean wet-weather > mean routine
gehan.count<-gehan.output %>%
  filter(MeanWet>MeanRoutine) %>%
  .$Parameter %>%
  length() 
gehan.stat<- 1-(0.95^(1/gehan.count))
gehan.count
gehan.stat  

gehan.output<-gehan.output %>%
  mutate(Sig=p<gehan.stat & MeanWet>MeanRoutine,
         pseudosig=p<0.05 & MeanWet>MeanRoutine)  %>%
  arrange(Parameter)
write_csv(gehan.output,"Gehan test results.csv")  

sig.params<-filter(gehan.output,Sig==T)$Parameter

# What about running the Gehan test for each stream-parameter combination?
# Expect there's too little data for significance, but let's check
gehan.stream<-function(data,stream) {
  d<-data %>%
    filter(Stream==stream)
  
  params.list<-unique(d$Parameter)
  gehan.output<-map_df(params.list,gehan,data=d)
  
  output<-gehan.output %>%
    mutate(Stream=stream)
  
  output
}
  
streams.list<-unique(metals$Stream)
gehan.by.stream<-map_df(streams.list,gehan.stream,data=metals)


# For estimating significance, only include parameters that are sig. overall
gehan.count.stream<-gehan.by.stream %>%
  filter(MeanWet>MeanRoutine,
         Parameter %in% sig.params,
         !is.na(p)) %>%
  .$Parameter %>%
  length() 
gehan.stat.stream<- 1-(0.95^(1/gehan.count.stream))
gehan.count.stream
gehan.stat.stream
# Still so low that none are significant

gehan.stream.sig<-gehan.by.stream %>%
  mutate(StormBaseRatio=MeanWet/MeanRoutine,
    Sig=p<gehan.stat.stream & MeanWet>MeanRoutine,
         pseudosig=p<0.05 & MeanWet>MeanRoutine)  %>%
  arrange(Parameter,Stream)
write_csv(gehan.stream.sig,"Gehan test - stream by stream.csv")  

gehan.tile.data<-gehan.stream.sig %>%
  filter(pseudosig) %>%
  mutate(fill=NA,
         fill=ifelse(StormBaseRatio>1,1,fill),
         fill=ifelse(StormBaseRatio>2,2,fill),
         fill=ifelse(StormBaseRatio>5,3,fill),
         fill=ifelse(StormBaseRatio>10,4,fill),
         fill=as.factor(fill),
         logSBR=log10(StormBaseRatio))
  
gehan.tile<-plot.heatmap(gehan.tile.data,
                         x="Stream",y="Parameter",value="logSBR",method="none",
                         midpoint=NA,label="StormBaseRatio",labeldigits=2,labelsize=2.5,
                         reorder.X=T,reorder.Y=F)
ggsave(gehan.tile,filename="Storm v Base stream-by-stream.png",width=10,height=7.5)

# Plot all streams individually vs. iron to look for uncorrelated
# If this receives a vector of parameters, flips to lump all streams and facet parameters
norm.baseflow<-function(param,data,valcol="Value",facet=T) {
  names(data)[names(data)==valcol]<-"valcol" 
  
  d<-data %>%
    filter(Parameter %in% c("Iron, Total",param)) %>%
    select(Sample,Date,Stream,Parameter,valcol,Routine) %>%
    spread(key=Parameter,value=valcol)
  
  names(d)[names(d)=="Iron, Total"]<-"X"

  if(length(param)>1) {
    d<-d %>%
      rename(Stream2=Stream) %>%
      gather(-Sample,-Date,-Stream2,-Routine,-X,
             key=Stream,value=Y)
    facet=T
    ylabel<-"Metal (µg/L)"
    title<-"All metals vs. iron"
    filename<-"Baseflow vs. iron -- all metals.png"
    
  } else {
    names(d)[names(d)==param]<-"Y"
    ylabel<-sprintf("%s (µg/L)",param)
    title<-param
    
    if(facet) {
      filename<-sprintf("Baseflow vs. iron - %s.png",param)
    } else {
      filename<-sprintf("Baseflow vs. iron (lumped) - %s.png",param)
    }
    
  }
  
  normplot<-ggplot(d,aes(x=X,y=Y,shape=Routine,fill=Routine))+
    geom_point(size=2.5,alpha=1)+
    scale_shape_manual(values=c("wet weather"=21,"Routine"=23),
                       labels=c("wet weather"="Stormflow","Routine"="Baseflow"))+
    scale_fill_manual(values=c("wet weather"="black","Routine"="white"),
                      labels=c("wet weather"="Stormflow","Routine"="Baseflow"))+
    scale_y_log10()+
    scale_x_log10()+
    labs(y=ylabel,
         title=title,
         x="Iron, Total (µg/L)")+
    theme(legend.position="none")
  
  if(facet) normplot<-normplot+facet_wrap(~Stream,ncol=4)
  
  ggsave(normplot,file=filename,width=15,height=20)
  
  filename
  
}

sig.params.totals<-sig.params[str_detect(sig.params,"Total") & !str_detect(sig.params,"Iron")]

norm.baseflow(param=sig.params.totals,data=metals,facet=T)
map(sig.params.totals,norm.baseflow,data=metals,facet=T)
map(sig.params.totals,norm.baseflow,data=metals,facet=F)

norm.baseflow(param="Molybdenum, Total",data=metals,facet=F)


