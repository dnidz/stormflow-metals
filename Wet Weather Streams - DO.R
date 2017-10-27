## Wet weather streams -- Dissolved Oxygen relationships
# Daniel Nidzgorski
# October 18, 2017

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

k<-read_csv("K-M means.csv",
            col_types=cols(
              Stream = col_character(),
              Parameter = col_character(),
              mean = col_double(),
              median = col_double(),
              sd = col_double(),
              n = col_integer(),
              n.censored = col_integer(),
              all.censored = col_logical(),
              logMean = col_double()
            )
)

streams<-read_csv("Streams - processed data.csv",
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
                  ))



## Explore DO relationships
DO.raw<-streams %>%
  filter(Parmname=="Dissolved Oxygen, Field")

DO<-DO.raw %>%
  select(Date,Locator,DO=Value) %>%
  right_join(wwmetals,by=c("Locator","Date"))

# Plot all streams for a single parameter
plot.do<-function(data,param,facet=F) {
  d<-data %>%
    filter(Parameter==param) %>%
    mutate(Label=ifelse(str_detect(Stream,"Pipers"),
                        "Pp",
                        str_sub(Stream,1,2)),
           Label=ifelse(str_sub(Stream,-1,-1) %in% c("B","C"),
                        sprintf("%s%s",Label,str_sub(Stream,-1,-1)),
                        Label)
    )
  
 if(facet) {
   doplot<-ggplot(d,aes(x=DO,y=RValue,shape=BelowMDL)) +
     facet_wrap(~Stream,ncol=4)+
     geom_point(size=3)+
     scale_shape_manual(values=c("FALSE"=19,"TRUE"=23),
                        labels=c("FALSE"="Above MDL","TRUE"="Below MDL"))+     
     scale_y_log10()+
     labs(y=param,
          x="Dissolved Oxygen (mg/L)",
          title=param)
   
   filename<-sprintf("DO by stream - %s.png",param)
   ggsave(doplot,file=filename,width=15,height=20)
   
 } else {
   doplot<-ggplot(d,aes(x=DO,y=Value)) +
    geom_point(aes(color=Stream),size=3)+
    geom_text(aes(label=Label),size=3)+
    scale_y_log10()+
    geom_smooth()+
    labs(y=param,
         x="Dissolved Oxygen (mg/L)",
         title=param)
  
  filename<-sprintf("DO - %s.png",param)
  ggsave(doplot,file=filename,width=11,height=7)
 }
  
  filename  
}

dissolved.params<-DO %>%
  filter(str_detect(Parameter,"Dissolved"),
         !str_detect(Parameter,"Selenium")) %>%
  arrange(Parameter) %>%
  .$Parameter %>%
  unique()
# map(dissolved.params,plot.do,data=DO)

DO.params<-c("Arsenic, Dissolved",
             "Chromium, Dissolved",
             "Copper, Dissolved",
             "Magnesium, Dissolved",
             "Molybdenum, Dissolved",
             "Nickel, Dissolved")
# map(DO.params,plot.do,data=DO,facet=T)



# Regression analyses
cenken.DO<-function(data,param,stream) {
  d<-data %>%
    filter(Parameter==param,
           Stream==stream,
           !is.na(DO)) %>%
    # Create a censored value that has the MDL if censored
    mutate(CValue=ifelse(BelowMDL,MDL,Value))
  
  # cenken needs numeric x and y -- can't use Date class
  # so consider Year instead of specific date
  r<-cenken(d$CValue,d$BelowMDL,d$DO)
  
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
cenkenreg.param.DO<-function(data,param) {
  d<-data %>%
    filter(Parameter==param)
  
  # Only streams with at least three samples
  streams.list<-d %>%
    filter(!is.na(DO)) %>%
    group_by(Stream) %>%
    summarize(n=n()) %>%
    filter(n>=3) %>%
    .$Stream
  
  output<-map_df(streams.list,cenken.DO,data=data,param=param)
  
  output
}

# Map to call all parameters
cenkenDO.output<-map_df(dissolved.params,cenkenreg.param.DO,data=DO)

cenkenDO.output<-cenkenDO.output %>%
  arrange(p)

cenkenDO.filtered<-cenkenDO.output %>%
  filter(slope<0,
         p<0.05/136)
# 136 candidates; adjusted p-value would be 
(0.95^126)

cenkenDO.sig<-cenkenDO.output %>%
  filter(p<0.05,
         slope<0) %>%
  arrange(slope) %>%
  left_join(select(k,Stream,Parameter,mean),by=c("Stream","Parameter")) %>%
  mutate(PctChg= -slope/mean,
         logChg=log10(PctChg))

# DO.tile<-plot.heatmap(cenkenDO.sig,x="Stream",y="Parameter",value="logChg",method="none",
#                          reorder.X=F,reorder.Y=F)
# 
# ggsave(DO.tile,filename="DO - proportion change tilemap.png",
# width=10,height=7.5)

# map(unique(cenkenDO.sig$Parameter),regression.plot,
#     data=DO,regdata=cenkenDO.sig,
#     x="DO",y="RValue",xlabel="Dissolved Oxygen (mg/L)",logy=F)

### Dissolved/particulate partitioning
dissolved<-wwmetals %>%
  select(Date,Locator,Stream,
         Sample.D=Sample,
         Parameter.D=Parameter,
         Value.D=Value,
         BelowMDL.D=BelowMDL,
         RValue.D=RValue) %>%
  filter(str_detect(Parameter.D,"Dissolved")) %>%
  mutate(Metal=str_extract(Parameter.D,"\\w+"))

total<-wwmetals %>%
  select(Date,Locator,Stream,
         Sample.T=Sample,
         Parameter.T=Parameter,
         Value.T=Value,
         BelowMDL.T=BelowMDL,
         RValue.T=RValue) %>%
  filter(str_detect(Parameter.T,"Total")) %>%
  mutate(Metal=str_extract(Parameter.T,"\\w+"))

all<-inner_join(dissolved,total,
                by = c("Date", "Locator", "Stream", "Metal")) %>%
  mutate(PctDissolved=RValue.D/RValue.T*100,
         BelowMDL=BelowMDL.D | BelowMDL.T)

all.uncensored<-filter(all,!BelowMDL)

dissolved.plot<-ggplot(data=all.uncensored,aes(x=Stream,y=PctDissolved))+
  facet_wrap(~Metal,ncol=1)+
  geom_boxplot(aes(group=Stream))+
  theme(axis.text.x=element_text(angle=-45,hjust=0))+
  coord_cartesian(ylim=c(0,100))
           
ggsave(dissolved.plot,filename="Percent Dissolved.png",width=7.5,height=30)

DO.dissolved<-DO.raw %>%
  select(Date,Locator,DO=Value) %>%
  right_join(all.uncensored,by=c("Locator","Date")) %>%
  mutate(PctDissolved=ifelse(PctDissolved>100,100,PctDissolved))

# DO.dissolved %>%
#   mutate(Value=PctDissolved,
#          Parameter=Parameter.D) %>%
#          {map(dissolved.params,plot.do,data=.,facet=F)}

metals.params<-c("Arsenic","Barium","Calcium","Chromium","Copper",
                 "Iron","Lead","Magnesium","Nickel","Vanadium","Zinc")


cenken.dissolved.output<-DO.dissolved %>%
  mutate(Value=PctDissolved,
         Parameter=Metal) %>%
         {map_df(metals.params,cenkenreg.param.DO,data=.)}

cenken.dissolved.sig<-cenken.dissolved.output %>%
  filter(p<0.05,
         slope<0) %>%
  arrange(slope)

DO.dissolved %>%
  mutate(Parameter=Metal) %>%
{map(unique(cenken.dissolved.sig$Parameter),
     regression.plot,data=.,regdata=cenken.dissolved.sig,
               x="DO",y="PctDissolved",xlabel="Dissolved Oxygen (mg/L)",
               ylabel="Percent Dissolved",title="Percent Dissolved vs. DO",logy=F)}


# p<-DO.dissolved %>%
#   mutate(Parameter=Metal) %>%
#   regression.plot(regdata=cenken.dissolved.sig,
#                   param="Zinc",
#        x="DO",y="PctDissolved",xlabel="Dissolved Oxygen (mg/L)",
#        ylabel="Percent Dissolved",title="Percent Dissolved vs. DO",logy=F)
# 
# 
# ggsave(p,filename="test2.png",
#                 width=15,height=5.2)




