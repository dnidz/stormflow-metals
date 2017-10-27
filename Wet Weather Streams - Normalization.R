## Wet weather streams - Normalization
# Daniel Nidzgorski
# July 7, 2017

# Libraries
library(tidyverse)
library(lubridate)
library(stringr)
library(scales)
library(NADA)

# Clear desktop & variables
rm (list=ls())

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
# data<-wwmetals.totals
# param<-"Manganese, Total"
# valcol<-"Value"
# fraction<-"dissolved"

# Plot one parameter at a time, vs iron and aluminum
# Pass in parameter name, long-format dataframe, which column holds the value (or mean, or...),
# and whether to normalize by total or dissolved iron/aluminum.
normalize<-function(param,data,valcol="Value",fraction="total") {
  names(data)[names(data)==valcol]<-"valcol" 
  
  if(fraction=="total") {
    d<-data %>%
      filter(Parameter %in% c("Aluminum, Total","Iron, Total",param)) %>%
      select(Sample,Date,Stream,Parameter,valcol) %>%
      spread(key=Parameter,value=valcol) %>%
      gather(`Aluminum, Total`,`Iron, Total`,key=Norm,value=X)%>%
      mutate(Label=ifelse(str_detect(Stream,"Pipers"),
                          "Pp",
                          str_sub(Stream,1,2)),
      Label=ifelse(str_sub(Stream,-1,-1) %in% c("B","C"),
                   sprintf("%s%s",Label,str_sub(Stream,-1,-1)),
                   Label)
      )
  } else {
    d<-data %>%
      filter(Parameter %in% c("Aluminum, Dissolved","Iron, Dissolved",param)) %>%
      select(Sample,Date,Stream,Parameter,valcol) %>%
      spread(key=Parameter,value=valcol) %>%
      gather(`Aluminum, Dissolved`,`Iron, Dissolved`,key=Norm,value=X)%>%
      mutate(Label=ifelse(str_detect(Stream,"Pipers"),
                          "Pp",
                          str_sub(Stream,1,2)),
             Label=ifelse(str_sub(Stream,-1,-1) %in% c("B","C"),
                          sprintf("%s%s",Label,str_sub(Stream,-1,-1)),
                          Label)
      )
  }
  
  
  names(d)[names(d)==param]<-"Y"
  
  normplot<-ggplot(d)+
    facet_wrap(~Norm,ncol=2,scales="free_x")+
    geom_point(aes(x=X,y=Y,fill=Stream,color=Stream))+
    geom_text(aes(x=X,y=Y,label=Label),size=1)+
    geom_smooth(aes(x=X,y=Y),method="lm",level=0.9)+
    scale_y_log10()+
    scale_x_log10()+
    labs(y=param,title=param,x=element_blank())
  
  filename<-sprintf("Fe-Al norm - %s.png",param)
  ggsave(normplot,file=filename,width=20,height=8)
}

wwmetals.totals<-wwmetals %>%
  select(Sample,Date,Year,Month,Stream,Parameter,Value,RValue) %>%
  filter(str_detect(Parameter,"Total")) 
wwmetals.totals.means<-wwmetals.totals %>%
  group_by(Stream,Parameter) %>%
  summarize(GrandMean=mean(RValue,na.rm=T)) %>%
  filter(!is.na(GrandMean)) 

param.list<-unique(wwmetals.totals$Parameter)
param.list<-param.list[!param.list %in% c("Aluminum, Total","Iron, Total")]

# map(param.list,normalize,data=wwmetals.totals)

# Try plotting dissolved vs. either total or dissolved Fe/Al
wwmetals.dissolved<-wwmetals %>%
  select(Sample,Date,Year,Month,Stream,Parameter,Value,RValue) %>%
  filter(str_detect(Parameter,"Dissolved") | 
           Parameter %in% c("Iron, Total","Aluminum, Total")) 

dissolved.param.list<-unique(wwmetals.dissolved$Parameter)
dissolved.param.list<-dissolved.param.list[!str_detect(dissolved.param.list,"Aluminum") &
                                           !str_detect(dissolved.param.list,"Iron")]

# map(dissolved.param.list,normalize,data=wwmetals.dissolved,fraction="dissolved")


# Plot all streams individually vs. iron to look for uncorrelated
norm.allstreams<-function(param,data,valcol="Value") {
  names(data)[names(data)==valcol]<-"valcol" 
  
  d<-data %>%
    filter(Parameter %in% c("Iron, Total",param)) %>%
    select(Sample,Date,Stream,Parameter,valcol) %>%
    spread(key=Parameter,value=valcol)

  
  names(d)[names(d)=="Iron, Total"]<-"X"
  names(d)[names(d)==param]<-"Y"
  
  normplot<-ggplot(d,aes(x=X,y=Y))+
    facet_wrap(~Stream,ncol=4)+
    geom_point(size=2.5,fill="black",alpha=0.5)+
    scale_y_log10()+
    scale_x_log10()+
    labs(y=sprintf("%s (µg/L)",param),
         title=param,
         x="Iron, Total (µg/L)")+
    theme(legend.position="none")
  
  filename<-sprintf("Fe-norm all streams - %s.png",param)
  ggsave(normplot,file=filename,width=15,height=20)
  
  filename
  
}

iron.correlated<-c("Aluminum, Total",
                   "Arsenic, Total",
                   "Barium, Total",
                   "Cadmium, Total",
                   "Chromium, Total",
                   "Cobalt, Total",
                   "Copper, Total",
                   "Lead, Total",
                   "Manganese, Total",
                   "Mercury, Total",
                   "Nickel, Total",
                   "Vanadium, Total",
                   "Zinc, Total")

# map(iron.correlated,norm.allstreams,data=wwmetals)

## Mill Creek and Fairweather Creek are poorly correlated for several metals.
norm.onestream<-function(data,stream,params) {

  d<-data %>%
    filter(Stream==stream,
           Parameter %in% c("Iron, Total",params)) %>%
    select(Sample,Date,Stream,Parameter,Value) %>%
    spread(key=Parameter,value=Value) %>%
    gather(-Sample,-Date,-Stream,-`Iron, Total`,key=Parameter,value=Value,na.rm=T)
  

  normplot<-ggplot(d,aes(x=`Iron, Total`,y=Value))+
    facet_wrap(~Parameter,ncol=3,scales="free_y")+
    geom_point(size=2.5,fill="black",alpha=0.5)+
    scale_y_log10()+
    scale_x_log10()+
    labs(y="µg/L",
         title=stream,
         x="Iron, Total (µg/L)")+
    theme(legend.position="none")
  
  filename<-sprintf("Iron scatterplots - %s.png",stream)
  ggsave(normplot,file=filename,width=15,height=20)
  
  filename
}


mill.params<-c("Chromium, Total",
  "Copper, Total",
  "Lead, Total",
  "Manganese, Total",
  "Nickel, Total",
  "Vanadium, Total",
  "Zinc, Total")
# norm.onestream(wwmetals,"Mill Creek",mill.params)

# map(unique(wwmetals$Stream),norm.onestream,data=wwmetals,params=iron.correlated)


## Normalize data by iron concentration for those metals that show correlations.

wwmetals.iron<-wwmetals %>%
  select(Sample,Date,Stream,Parameter,Value) %>%
  filter(Parameter %in% c(iron.correlated, "Iron, Total")) %>%
  spread(key=Parameter,value=Value) %>%
  # need to exclude Iron from getting mutated, or the new value will be used for all following columns.
  mutate_at(vars(-Sample,-Date,-Stream,-`Iron, Total`),funs(./`Iron, Total`)) %>%
  gather(-Sample,-Date,-Stream,key=Parameter,value=FeRatio) %>%
  filter(Parameter!="Iron, Total",!is.na(FeRatio))

# Don't normalize each sample and then take the mean -- leaves out <MDL samples
# Better to normalize the site means, which are the best estimates of the means
# including <MDL samples

wwmetals.means<-wwmetals %>%
  group_by(Stream,Parameter) %>%
  summarize(GrandMean=mean(RValue,na.rm=T)) %>%
  filter(!is.na(GrandMean)) 

wwmetals.means.iron<-wwmetals.means %>%
  filter(str_detect(Parameter,"Total")) %>%
  spread(key=Parameter,value=GrandMean) %>%
  ungroup() %>%
  mutate_at(vars(-Stream,-`Iron, Total`),funs(./`Iron, Total`)) %>%
  gather(-Stream,key=Parameter,value=GrandMean) %>%
  filter(Parameter %in% iron.correlated,
         !is.na(GrandMean)) %>%
  ungroup()

## Heatmaps
source("Heatmap plotting function.R")
wwmetals.iron.hm<-wwmetals.means.iron %>%
  select(X=Stream,Y=Parameter,Value=GrandMean) 

# ggsave(plot.heatmap(wwmetals.iron.hm,method="rescale"),
#        file="Heatmap - iron norm - rescale.png",width=10,height=7.5)
# 
# ggsave(plot.heatmap(wwmetals.iron.hm,method="scale"),
#        file="Heatmap - iron norm - scale.png",width=10,height=7.5)

# ## Prep for PCA
# 
# wwmetals.iron.matrix<-wwmetals.means.iron %>%
#   ungroup() %>%
#   spread(key=Parameter,value=GrandMean)
# 
# wwmetals.iron.scaled<-wwmetals.iron.matrix %>%
#   ungroup() %>%
#   select(-Stream) %>%
#   scale()
# row.names(wwmetals.iron.scaled)<-wwmetals.iron.matrix$Stream
# 
# # wwmetals.scaled<-wwmetals.iron.scaled

# Plot stream means to try the 90% regression method for detecting anthropogenic inputs
ironmeans<-function(param,data,valcol="GrandMean") {
  names(data)[names(data)==valcol]<-"valcol" 

    d<-data %>%
      filter(Parameter %in% c("Iron, Total",param)) %>%
      select(Stream,Parameter,valcol) %>%
      spread(key=Parameter,value=valcol) %>%
      gather(`Iron, Total`,key=Norm,value=X)%>%
      mutate(Label=ifelse(str_detect(Stream,"Pipers"),
                          "Pp",
                          str_sub(Stream,1,2)),
             Label=ifelse(str_sub(Stream,-1,-1) %in% c("B","C"),
                          sprintf("%s%s",Label,str_sub(Stream,-1,-1)),
                          Label)
      )
 
  names(d)[names(d)==param]<-"Y"
  
  normplot<-ggplot(d)+
    geom_point(aes(x=X,y=Y,fill=Stream,color=Stream),size=3)+
    geom_text(aes(x=X,y=Y,label=Label),size=3)+
    geom_smooth(aes(x=X,y=Y),method="lm",level=0.9)+
    scale_y_log10()+
    scale_x_log10()+
    labs(y=sprintf("%s (μg/L)",param),
         title=param,
         x="Iron, Total (μg/L)"
    )
  
  filename<-sprintf("Fe-norm - means - %s.png",param)
  ggsave(normplot,file=filename,width=12,height=8)
  
  param
}
# map(iron.correlated,ironmeans,data=wwmetals.means)


## QQ plots to detect anthropogenic impacts
source("multiplot function.R")

plot.qq<-function(param,data,fe.norm=F,dist="norm") {
  
  d<-data %>%
    filter(Parameter==param) %>%
    mutate(Label=ifelse(str_detect(Stream,"Pipers"),
                        "Pp",
                        str_sub(Stream,1,2)),
           Label=ifelse(str_sub(Stream,-1,-1) %in% c("B","C"),
                        sprintf("%s%s",Label,str_sub(Stream,-1,-1)),
                        Label)
    )
  
  metalsqq<-ggplot(d,aes(sample=GrandMean,label=Stream))+
    stat_qq(distribution=sprintf("q%s",dist))
  
  metalsqq.df<-ggplot_build(metalsqq)$data[[1]] %>%
    as.tibble() %>%
    mutate(Label=arrange(d,GrandMean)$Label,
           Stream=arrange(d,GrandMean)$Stream) %>%
    select(Stream,Label,everything())
  
  metalsqq.plot<-ggplot(metalsqq.df,aes(x=theoretical,y=sample,label=Label))+
    geom_text()+
    labs(x=sprintf("%s quantile",dist),
         y=ifelse(fe.norm,"Fe-normalized mean concentration",
                  "Mean concentration (ug/L)"),
         title=param)
  
  metalsqq.plot
}

# Layout matrix for multiplot
mlayout<-matrix(c(1:12),nrow=4,byrow=T)

# Fe-normalized Q-Q plots
# qqplot.list.iron<-wwmetals.means.iron %>%
#   filter(Parameter!="Aluminum, Total") %>%
#   {map(unique(.$Parameter),
#     plot.qq,data=.,fe.norm=T,dist="norm")} 

# ggsave(multiplot(plotlist=qqplot.list.iron,layout=mlayout),
#           filename="Q-Q, Normal, Fe-norm'd.png",
#           width=15,height=20)

# Non-normalized Q-Q plots, all parameters (multiple pages)
qqplot.list<-wwmetals.means %>%
{map(unique(.$Parameter),
     plot.qq,data=.,fe.norm=F,dist="lnorm")}
qqplot.list<-c(qqplot.list,rep(NA,times=48-length(qqplot.list)))
qqplot.dist<-"Lognormal"


# ggsave(multiplot(plotlist=qqplot.list[1:12],layout=mlayout),
#        filename=sprintf("Q-Q, %s p1.png",qqplot.dist),
#        width=15,height=20)
# ggsave(multiplot(plotlist=qqplot.list[13:24],layout=mlayout),
#        filename=sprintf("Q-Q, %s p2.png",qqplot.dist),
#        width=15,height=20)
# ggsave(multiplot(plotlist=qqplot.list[25:36],layout=mlayout),
#        filename=sprintf("Q-Q, %s p3.png",qqplot.dist),
#        width=15,height=20)
# ggsave(multiplot(plotlist=qqplot.list[37:48],layout=mlayout),
#        filename=sprintf("Q-Q, %s p4.png",qqplot.dist),
#        width=15,height=20)

# 
# plot.box<-ggplot(wwmetals.means.iron,aes(x=Parameter,y=GrandMean))+
#   facet_wrap(~Parameter,scales="free")+
#   geom_boxplot()


## Explore DO relationships
DO<-read_csv("Streams - processed data.csv",
             col_types=cols(
               Sample = col_character(),
               Date = col_date(),
               Year = col_double(),
               Month = col_double(),
               Routine = col_character(),
               Locator = col_character(),
               Stream = col_character(),
               Position = col_character(),
               LocGroup = col_character(),
               MetalOrg = col_character(),
               Parameter = col_character(),
               Parmname = col_character(),
               Value = col_double(),
               Units = col_character(),
               Qual = col_character(),
               MDL = col_double(),
               RDL = col_double(),
               Hardness = col_double(),
               TEXTVALUE = col_character()
             )) %>%
  filter(Parmname=="Dissolved Oxygen") %>%
  select(Sample,Date,DO=Value) %>%
  right_join(wwmetals.iron,by=c("Sample","Date"))

# Plot all streams for a single parameter
plot.do<-function(data,param) {
  d<-data %>%
    filter(Parameter==param) %>%
    mutate(Label=ifelse(str_detect(Stream,"Pipers"),
                        "Pp",
                        str_sub(Stream,1,2)),
           Label=ifelse(str_sub(Stream,-1,-1) %in% c("B","C"),
                        sprintf("%s%s",Label,str_sub(Stream,-1,-1)),
                        Label)
    )
  
  doplot<-ggplot(d,aes(x=DO,y=FeRatio)) +
    geom_point(aes(color=Stream),size=3)+
    geom_text(aes(label=Label),size=3)+
    # scale_y_log10()+
    geom_smooth()+
    labs(y=sprintf("%s:Iron",param),
         x="Dissolved Oxygen (mg/L)",
         title=param)
  
  filename<-sprintf("DO - %s.png",param)
  ggsave(doplot,file=filename,width=11,height=7)
    
  filename  
}

# map(iron.correlated,plot.do,data=DO)

### Try things with the K-M mean instead

km<-function(data,stream,param) {
  d<-data %>%
    filter(Stream==stream,
           Parameter==param) %>%
    mutate(CValue=ifelse(BelowMDL,MDL,Value)
           # Group=as.factor(sprintf("%s; %s",Stream,Parameter))
    )
  
  c<-cenfit(obs=d$CValue,censored=d$BelowMDL)
  
  output<-tibble(
    Stream=stream,
    Parameter=param,
    mean=mean(c)[[1]],
    median=median(c),
    sd=sd(c),
    n=length(d$CValue),
    n.censored=sum(d$BelowMDL)
  )
  
  output
}

km.allparams<-function(data,stream) {
  params.list<-data %>%
    filter(Stream==stream) %>%
    .$Parameter %>%
    unique()
  
  output<-map_df(params.list,km,data=data,stream=stream)
  
  output
}

streams.list<-unique(wwmetals$Stream)
km.all<-map_df(streams.list,km.allparams,data=wwmetals)

km.means<-full_join(wwmetals.means,km.all,by = c("Stream", "Parameter"))

qplot(data=km.means,x=mean,y=GrandMean,log="xy")

km.diff<-km.means %>%
  filter(mean!=0,
         mean<0.9*GrandMean | mean>1.1*GrandMean)
