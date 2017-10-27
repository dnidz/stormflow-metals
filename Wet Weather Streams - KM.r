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
# External functions
source("Heatmap plotting function.R")
source("multiplot function.R")

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

### Calculate Kaplan-Meier mean and other summary stats

km.indiv<-function(data,stream,param) {
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
    n.censored=sum(d$BelowMDL),
    all.censored= n==n.censored
  )
  
  output
}

km.allparams<-function(data,stream) {
  params.list<-data %>%
    filter(Stream==stream) %>%
    .$Parameter %>%
    unique()
  
  output<-map_df(params.list,km.indiv,data=data,stream=stream)
  
  output
}

streams.list<-unique(wwmetals$Stream)

# K-M site means for each stream x parameter 
km<-map_df(streams.list,km.allparams,data=wwmetals)

# Iron-normalized means (site mean metal / site mean iron)
km.iron<-km %>%
  filter(str_detect(Parameter,"Total")) %>%
  select(Stream,Parameter,mean) %>%
  spread(key=Parameter,value=mean) %>%
  ungroup() %>%
  mutate_at(vars(-Stream,-`Iron, Total`),funs(./`Iron, Total`)) %>%
  gather(-Stream,key=Parameter,value=mean) %>%
  filter(Parameter %in% iron.correlated,
         !is.na(mean)) %>%
  ungroup() %>%
  arrange(Parameter,Stream) %>%
  left_join(select(km,Stream,Parameter,all.censored),by=c("Stream","Parameter")) %>%
  mutate(mean=ifelse(all.censored,NA,mean)) %>% 
  mutate(logMean=log10(mean))

k<-km %>%
  arrange(Parameter,Stream) %>%
  mutate(mean=ifelse(all.censored,NA,mean)) %>% 
  mutate(logMean=log10(mean))

write_csv(k,"K-M means.csv")

## Heatmaps
ggsave(plot.heatmap(data=k,x="Stream",y="Parameter",value="mean",
                    method="rescale",grey="all.censored"),
       file="Heatmap - rescale.png",height=10,width=7.5)

ggsave(plot.heatmap(data=km.iron,x="Stream",y="Parameter",value="mean",
                    method="rescale",grey="all.censored"),
       file="Heatmap - iron rescale.png",height=7.5,width=10)


## QQ plots to detect high outliers
plot.qq<-function(data,param,fe.norm=F,dist="norm",mean="mean") {
  
  names(data)[names(data)==mean]<-"Mean" 
  
  d<-data %>%
    filter(Parameter==param,
           !is.na(Mean)) %>%
    mutate(Label=ifelse(str_detect(Stream,"Pipers"),
                        "Pp",
                        str_sub(Stream,1,2)),
           Label=ifelse(str_sub(Stream,-1,-1) %in% c("B","C"),
                        sprintf("%s%s",Label,str_sub(Stream,-1,-1)),
                        Label)
    )
  
  metalsqq<-ggplot(d,aes(sample=Mean,label=Stream))+
    stat_qq(distribution=sprintf("q%s",dist))
  
  metalsqq.df<-ggplot_build(metalsqq)$data[[1]] %>%
    as.tibble() %>%
    mutate(Label=arrange(d,Mean)$Label,
           Stream=arrange(d,Mean)$Stream) %>%
    select(Stream,Label,everything())
  
  metalsqq.plot<-ggplot(metalsqq.df,aes(x=theoretical,y=sample,label=Label))+
    geom_text()+
    geom_hline(yintercept=quantile(d$Mean,0.75)+(1.5*IQR(d$Mean)))+
    geom_hline(yintercept=quantile(d$Mean,0.75)+(3*IQR(d$Mean)))+
    labs(x=sprintf("%s quantile",dist),
         y=ifelse(fe.norm,"Fe-normalized mean concentration",
                  "Mean concentration (ug/L)"),
         title=param)
  
  metalsqq.plot
}

# Layout matrix for multiplot
mlayout<-matrix(c(1:12),nrow=4,byrow=T)

# Fe-normalized Q-Q plots
qqplot.list.iron.norm<-km.iron %>%
  filter(Parameter!="Aluminum, Total") %>%
  {map(unique(.$Parameter),
    plot.qq,data=.,fe.norm=T,dist="norm")}

qqplot.list.iron.lnorm<-km.iron %>%
  filter(Parameter!="Aluminum, Total") %>%
  {map(unique(.$Parameter),
       plot.qq,data=.,fe.norm=T,dist="lnorm")}

ggsave(multiplot(plotlist=qqplot.list.iron.norm,layout=mlayout),
          filename="Q-Q, Normal, Fe-norm'd.png",
          width=15,height=20)
ggsave(multiplot(plotlist=qqplot.list.iron.lnorm,layout=mlayout),
       filename="Q-Q, LogNormal, Fe-norm'd.png",
       width=15,height=20)

# Non-normalized Q-Q plots, all parameters (multiple pages)
qqplot.list.norm<-k %>%
{map(unique(.$Parameter),
     plot.qq,data=.,fe.norm=F,dist="norm")}

qqplot.list.lnorm<-k %>%
{map(unique(.$Parameter),
     plot.qq,data=.,fe.norm=F,dist="lnorm")}

qqplot.list.norm.of.logged<-k %>%
  mutate(logmean=log10(mean)) %>%
  {map(unique(.$Parameter),
     plot.qq,data=.,fe.norm=F,dist="norm",mean="logmean")}


plot.fourpage<-function(list,dist) {
  
  list<-c(list,rep(NA,times=48-length(list)))
  
  ggsave(multiplot(plotlist=list[1:12],layout=mlayout),
         filename=sprintf("Q-Q, %s p1.png",dist),
         width=15,height=20)
  ggsave(multiplot(plotlist=list[13:24],layout=mlayout),
         filename=sprintf("Q-Q, %s p2.png",dist),
         width=15,height=20)
  ggsave(multiplot(plotlist=list[25:36],layout=mlayout),
         filename=sprintf("Q-Q, %s p3.png",dist),
         width=15,height=20)
  ggsave(multiplot(plotlist=list[37:48],layout=mlayout),
         filename=sprintf("Q-Q, %s p4.png",dist),
         width=15,height=20)

}

plot.fourpage(qqplot.list.norm,dist="Normal")
plot.fourpage(qqplot.list.lnorm,dist="LogNormal")
plot.fourpage(qqplot.list.norm.of.logged,dist="Normal after log-transformation")


# 
# plot.box<-ggplot(wwmetals.means.iron,aes(x=Parameter,y=GrandMean))+
#   facet_wrap(~Parameter,scales="free")+
#   geom_boxplot()




# What parameters were sampled when?
when<-read_csv("Streams - processed data.csv",
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
  filter(MetalOrg=="Metal") %>%
  group_by(Parameter,Year,Stream) %>%
  summarize(n=n()) %>%
  group_by(Parameter,Year) %>%
  summarize(num.samples=sum(n),
            num.streams=n(),
            n.per.stream=median(n)) %>%
  arrange(Year,Parameter)
plot.heatmap(when,x="Year",y="Parameter",value="num.streams",method="none",reorder.X=F,label="num.streams")

unique(when$Parameter)
