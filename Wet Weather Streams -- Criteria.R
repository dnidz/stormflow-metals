## Wet weather streams - Criteria
# Daniel Nidzgorski
# July 7, 2017

# Libraries
library(tidyverse)
library(lubridate)
library(stringr)
library(scales)
# library(pcaMethods)


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



criteria<-wwmetals %>%
  mutate(Acute=NA,Chronic=NA,Water=NA,Organisms=NA,EPAwater=NA,EPAorganisms=NA) %>%
  mutate(Acute=ifelse(Parameter=="Aluminum, Total",750,Acute),
         Chronic=ifelse(Parameter=="Aluminum, Total",87,Chronic),
         # Aluminum is from EPA Aquatic Life Criteria, not WAC
    
         Water=ifelse(Parameter=="Antimony, Total",12,Water),
         Organisms=ifelse(Parameter=="Antimony, Total",180,Organisms),
         EPAwater=ifelse(Parameter=="Antimony, Total",6,EPAwater),
         EPAorganisms=ifelse(Parameter=="Antimony, Total",90,EPAorganisms),
    
         Acute=ifelse(Parameter=="Arsenic, Dissolved",360,Acute),
         Chronic=ifelse(Parameter=="Arsenic, Dissolved",190,Chronic),
         Water=ifelse(Parameter=="Arsenic, Total",10,Water),
         Organisms=ifelse(Parameter=="Arsenic, Total",10,Organisms),
         EPAwater=ifelse(Parameter=="Arsenic, Total",0.018,EPAwater),
         EPAorganisms=ifelse(Parameter=="Arsenic, Total",0.14,EPAorganisms),
         
         # Iron only has a chronic criterion, EPA aquatic life
         Chronic=ifelse(Parameter=="Iron, Total",1000,Chronic),
         
         # The mercury acute criterion is for dissolved, but chronic is for total.
         Acute=ifelse(Parameter=="Mercury, Dissolved",2.1,Acute),
         Chronic=ifelse(Parameter=="Mercury, Total",0.012,Chronic),
         Water=ifelse(Parameter=="Mercury, Total",0.14,Water),
         Organisms=ifelse(Parameter=="Mercury, Total",0.15,Organisms),

         Acute=ifelse(Parameter=="Selenium, Total",20,Acute),
         Chronic=ifelse(Parameter=="Selenium, Total",5,Chronic),
         Water=ifelse(Parameter=="Selenium, Total",120,Water),
         Organisms=ifelse(Parameter=="Selenium, Total",480,Organisms),
         EPAwater=ifelse(Parameter=="Selenium, Total",60,EPAwater),
         EPAorganisms=ifelse(Parameter=="Selenium, Total",200,EPAorganisms),
         
         Water=ifelse(Parameter=="Thallium, Total",0.24,Water),
         Organisms=ifelse(Parameter=="Thallium, Total",0.27,Organisms),
         # Don't bother with these EPA criteria since they're higher than WA's
         # EPAwater=ifelse(Parameter=="Thallium, Total",1.7,EPAwater),
         # EPAorganisms=ifelse(Parameter=="Thallium, Total",6.3,EPAorganisms),
         

         Acute=ifelse(Parameter=="Cadmium, Dissolved",
                      (1.136672-(log(Hardness)*0.041838)) * exp(1.128*log(Hardness)-3.828),
                      Acute),
         Chronic=ifelse(Parameter=="Cadmium, Dissolved",
                        (1.101672-(log(Hardness)*0.041838)) * exp(0.7852*log(Hardness)-3.490),
                        Chronic),
         
         # Chromium uses the trivalent criteria
         Acute=ifelse(Parameter=="Chromium, Total",
                      0.316*exp(0.8190*log(Hardness)+3.688),
                      Acute),
         Chronic=ifelse(Parameter=="Chromium, Total",
                        0.860*exp(0.8190*log(Hardness)+1.561),
                        Chronic),
         
         Acute=ifelse(Parameter=="Copper, Dissolved",
                      0.960*exp(0.9422*log(Hardness)-1.464),
                      Acute),
         Chronic=ifelse(Parameter=="Copper, Dissolved",
                        0.960*exp(0.8545*log(Hardness)-1.465),
                        Chronic),
         Water=ifelse(Parameter=="Copper, Total",1300,Water),

         
         Acute=ifelse(Parameter=="Lead, Dissolved",
                      (1.46203-(log(Hardness)*0.145712)) * exp(1.273*log(Hardness)-1.460),
                      Acute),
         Chronic=ifelse(Parameter=="Lead, Dissolved",
                        (1.46203-(log(Hardness)*0.145712)) * exp(1.273*log(Hardness)-4.705),
                        Chronic),
         
         Acute=ifelse(Parameter=="Nickel, Dissolved",
                      0.998*exp(0.8460*log(Hardness)+3.3612),
                      Acute),
         Chronic=ifelse(Parameter=="Nickel, Dissolved",
                        0.997*exp(0.8460*log(Hardness)+1.1645),
                        Chronic),
         Water=ifelse(Parameter=="Nickel, Total",150,Water),
         Organisms=ifelse(Parameter=="Nickel, Total",190,Organisms),
         EPAwater=ifelse(Parameter=="Nickel, Total",80,EPAwater),
         EPAorganisms=ifelse(Parameter=="Nickel, Total",100,EPAorganisms),

         # Silver only has an acute criterion
         Acute=ifelse(Parameter=="Silver, Dissolved",
                      0.85*exp(1.72*log(Hardness)-6.52),
                      Acute),
   
         
         Acute=ifelse(Parameter=="Zinc, Dissolved",
                      0.978*exp(0.8473*log(Hardness)+0.8604),
                      Acute),
         Chronic=ifelse(Parameter=="Zinc, Dissolved",
                        0.986*exp(0.8473*log(Hardness)+0.7614),
                        Chronic),
         Water=ifelse(Parameter=="Zinc, Total",2300,Water),
         Water=ifelse(Parameter=="Zinc, Total",2900,Water),
         EPAwater=ifelse(Parameter=="Zinc, Total",1000,EPAwater),
         EPAorganisms=ifelse(Parameter=="Zinc, Total",1000,EPAorganisms)
         
  ) %>%
  
  mutate(propAcute=RValue/Acute,
         propChronic=RValue/Chronic,
         propWater=RValue/Water,
         propOrganisms=RValue/Organisms,
         propEPAwater=RValue/EPAwater,
         propEPAorganisms=RValue/EPAorganisms)

# For the hardness-dependent criteria, what are the values for our dataset?
hardness.dependent.params<-c("Cadmium, Dissolved",
                             "Chromium, Total",
                             "Copper, Dissolved",
                             "Lead, Dissolved",
                             "Nickel, Dissolved",
                             "Zinc, Dissolved")

criteria.values<-criteria %>%
  filter(!is.na(Acute) | !is.na(Chronic),
         Parameter %in% hardness.dependent.params) %>%
  group_by(Parameter) %>%
  summarize(minAcute=min(Acute,na.rm=T),
            medianAcute=median(Acute,na.rm=T),
            meanAcute=mean(Acute,na.rm=T),
            maxAcute=max(Acute,na.rm=T),
            minChronic=min(Chronic,na.rm=T),
            medianChronic=median(Chronic,na.rm=T),
            meanChronic=mean(Chronic,na.rm=T),
            maxChronic=max(Chronic,na.rm=T)
  )
# or by histograms:


         
# What samples are over the criteria? Exclude all below-MDL samples
over.aquatox<-criteria %>%
  filter(propAcute>1 | propChronic>1,
         !BelowMDL) %>%
  arrange(Parameter,Date,Stream)
write_csv(over.aquatox,"Metals over aquatox.csv")

over.human<-criteria %>%
  filter(propWater>1 | propOrganisms>1,
         !BelowMDL) %>%
  arrange(Parameter,Date,Stream)
write_csv(over.human,"Metals over human-health.csv")

over.EPA<-criteria %>%
  filter(propEPAwater>1 | propEPAorganisms>1,
         !BelowMDL) %>%
  arrange(Parameter,Date,Stream)
write_csv(over.EPA,"Metals over EPA human-health.csv")

# What percent of samples are over the criteria? 
# For <MDL samples, only include them in the count if the MDL is below the criterion.
# First overall, then stream-by-stream
pct.over<-criteria %>%
  group_by(Parameter) %>%
  summarize(Acute.n=sum(MDL<Acute | !BelowMDL,na.rm=T),
            Acute.over=sum(propAcute>1 & !BelowMDL,na.rm=T),
            Acute.pct=Acute.over/Acute.n*100,
            
            Chronic.n=sum(MDL<Chronic | !BelowMDL,na.rm=T),
            Chronic.over=sum(propChronic>1 & !BelowMDL,na.rm=T),
            Chronic.pct=Chronic.over/Chronic.n*100,
            
            Water.n=sum(MDL<Water | !BelowMDL,na.rm=T),
            Water.over=sum(propWater>1 & !BelowMDL,na.rm=T),
            Water.pct=Water.over/Water.n*100,
            
            Organisms.n=sum(MDL<Organisms | !BelowMDL,na.rm=T),
            Organisms.over=sum(propOrganisms>1 & !BelowMDL,na.rm=T),
            Organisms.pct=Organisms.over/Organisms.n*100,
            
            EPAwater.n=sum(MDL<EPAwater | !BelowMDL,na.rm=T),
            EPAwater.over=sum(propEPAwater>1 & !BelowMDL,na.rm=T),
            EPAwater.pct=EPAwater.over/EPAwater.n*100,
            
            EPAorganisms.n=sum(MDL<EPAorganisms | !BelowMDL,na.rm=T),
            EPAorganisms.over=sum(propEPAorganisms>1 & !BelowMDL,na.rm=T),
            EPAorganisms.pct=EPAorganisms.over/EPAorganisms.n*100
  ) %>%
  filter(Acute.over+Chronic.over+Water.over+Organisms.over+EPAwater.over+EPAorganisms.over>0)

pct.over.streams<-criteria %>%
  group_by(Parameter,Stream) %>%
  summarize(Acute.n=sum(MDL<Acute | !BelowMDL,na.rm=T),
            Acute.over=sum(propAcute>1 & !BelowMDL,na.rm=T),
            Acute.pct=Acute.over/Acute.n*100,
            
            Chronic.n=sum(MDL<Chronic | !BelowMDL,na.rm=T),
            Chronic.over=sum(propChronic>1 & !BelowMDL,na.rm=T),
            Chronic.pct=Chronic.over/Chronic.n*100,
            
            Water.n=sum(MDL<Water | !BelowMDL,na.rm=T),
            Water.over=sum(propWater>1 & !BelowMDL,na.rm=T),
            Water.pct=Water.over/Water.n*100,
            
            Organisms.n=sum(MDL<Organisms | !BelowMDL,na.rm=T),
            Organisms.over=sum(propOrganisms>1 & !BelowMDL,na.rm=T),
            Organisms.pct=Organisms.over/Organisms.n*100,
            
            EPAwater.n=sum(MDL<EPAwater | !BelowMDL,na.rm=T),
            EPAwater.over=sum(propEPAwater>1 & !BelowMDL,na.rm=T),
            EPAwater.pct=EPAwater.over/EPAwater.n*100,
            
            EPAorganisms.n=sum(MDL<EPAorganisms | !BelowMDL,na.rm=T),
            EPAorganisms.over=sum(propEPAorganisms>1 & !BelowMDL,na.rm=T),
            EPAorganisms.pct=EPAorganisms.over/EPAorganisms.n*100
  ) %>%
  filter(Acute.over+Chronic.over+Water.over+Organisms.over+EPAwater.over+EPAorganisms.over>0)


            
            


# Heatmap
source("Heatmap plotting function.R")

criteria.means<-criteria %>%
  filter(!is.na(propChronic)) %>%
  group_by(Stream,Parameter) %>%
  summarize(MeanProp=mean(propChronic,na.rm=T)) 

crit.hm<-criteria.means %>%
  select(X=Stream,Y=Parameter,Value=MeanProp) %>%
  plot.heatmap(method="none")

# ggsave(crit.hm,filename="Heatmap by chronic criteria.png",width=10,height=6)


## Data over time
# Can be for single stream or multiple streams (faceted, all/only streams with exceedances)
# If for a single stream, subset the data before passing to this function
# Expanded to do aquatox (Acute/Chronic) or human or EPA (Water/Organisms) criteria
criteriaplot<-function(data,param,type="aquatox"){
  
  # Create two new criteria Crit1 and Crit2 that can be either Acute/Chronic or Organisms/Water
  
  if(type=="aquatox") {
    data2<-data %>%
      rename(Crit1=Acute,Crit2=Chronic)
    toxtitle<-"Aquatic life criteria"
    
  } else if(type=="human") {
    data2<-data %>%
      rename(Crit1=Organisms,Crit2=Water)
    toxtitle<-"Human health criteria"
    
  } else if(type=="EPA") {
    data2<-data %>%
      rename(Crit1=EPAorganisms,Crit2=EPAwater)
    toxtitle<-"EPA human health criteria"
  }
  
  d<-data2 %>%
    filter(Parameter==param,
           Routine=="wet weather",
           # Only include below-MDL samples if the MDL is less than one/both of the criteria
           MDL<Crit1 | MDL<Crit2 | !BelowMDL) %>%
    mutate(prop1=RValue/Crit1,
           prop2=RValue/Crit2,
           AboveCriteria=prop1>1 | prop2>1,
           AboveCriteria=ifelse(is.na(AboveCriteria),F,AboveCriteria),
           AboveCriteria=ifelse(BelowMDL,F,AboveCriteria) # no below-MDL sample marked above criteria
    )
  
  streams<-d %>%
    filter(AboveCriteria) %>%
    .$Stream %>%
    unique()
  
  d<-filter(d,Stream %in% streams)


  ymax<-max(d$RValue,na.rm=T)*1.1 # to prevent high criteria marks from tweaking the plot
  
  p<-ggplot(data=d,aes(x=Date,y=RValue,shape=AboveCriteria))+
    # Individual data points
    geom_point(size=3,color="black")+
    scale_shape_manual(values=c("FALSE"=5,"TRUE"=16),
                       labels=c("TRUE"="Above Criterion","FALSE"="Below Criterion"))+
    # Criteria marks
    geom_point(inherit.aes=F,aes(x=Date,y=Crit2),shape="-",size=7,color="gold")+
    geom_point(inherit.aes=F,aes(x=Date,y=Crit1),shape="-",size=7,color="red")+
    
    # scale_x_date(breaks=seq(1993,2010,5),
    #                    limits=c(1993,2010))+
    coord_cartesian(xlim=as.Date(c("1993-01-01","2010-12-31")),ylim=c(0,ymax))+
    theme(axis.text.x=element_text(size=11),
          axis.text.y=element_text(size=11),
          axis.title.x=element_blank(),
          axis.title.y=element_text(size=13),
          legend.title=element_blank(),
          legend.justification=c(0,1),
          legend.position=c(0,1),
          legend.background = element_rect(fill=NA)
    )+ guides(shape=guide_legend(nrow=1))
  
  loglist<-c("Aluminum, Total",
             "Iron, Total")
  if(param %in% loglist) {
    ymin<-min(d$RValue,na.rm=T)*0.9
    p<-p+scale_y_log10()+
      coord_cartesian(xlim=as.Date(c("1993-01-01","2010-12-31")),ylim=c(ymin,ymax))
  }
  
  
  numstreams<-length(unique(d$Stream))
  if(numstreams>1) { # if multi-stream plot
    ytitle<-sprintf("%s - %s",toxtitle,param)
    filename<-sprintf("Criteria (%s) - %s.png",type,param)
    p<-p+facet_wrap(~Stream,ncol=3)+
      labs(title=ytitle,y=param)
    # If only a few rows, change height to keep from stretching plots
    ht<-ifelse(numstreams>21,20,(floor(numstreams/3)*2.5)+2)
    ggsave(p,file=filename,width=15,height=ht)
  } else { # if only one stream
    streamname<-unique(d$Stream)[[1]]
    ytitle<-sprintf("%s - %s - %s",toxtitle,param,streamname)
    filename<-sprintf("Criteria (%s) - %s - %s.png",type,param,streamname)
    p<-p+labs(title=ytitle,y=param)
    ggsave(p,file=filename,width=8,height=5)
  }
  filename
}

# Plot all parameters with exceedances
params.aquatox<-unique(over.aquatox$Parameter)
map(params.aquatox,criteriaplot,data=criteria,type="aquatox")

params.human<-unique(over.human$Parameter)
map(params.human,criteriaplot,data=criteria,type="human")

params.EPA<-unique(over.EPA$Parameter)
map(params.EPA,criteriaplot,data=criteria,type="EPA")

# criteriaplot(criteria,"Arsenic, Total","EPA")
# 
# # Plot a single creek, single parameter
# criteria %>%
#   filter(Stream=="Cedar River B") %>%
#   criteriaplot(param="Lead, Total")


criteria %>%
  filter(Parameter=="Mercury, Total") %>%
  arrange(-RValue) %>%
  View()









# PCA
# criteria.matrix<-criteria.means %>%
#   ungroup() %>%
#   spread(key=Parameter,value=MeanProp)
# 
# criteria.scaled<-criteria.matrix %>%
#   ungroup() %>%
#   select(-Stream) %>%
#   as.data.frame()
# row.names(criteria.scaled)<-criteria.matrix$Stream
# 
# dopca<-function() { # wrapped in function to prevent PCA running every time
# 
# resBPCA<-pca(criteria.scaled,method="bpca",center=F,nPcs=5)
# resPPCA<-pca(criteria.scaled,method="ppca",center=F,nPcs=5)
# resNIPALS<-pca(criteria.scaled,method="nipals",center=F,nPcs=5)
# resSVDI<-pca(criteria.scaled,method="svdImpute",center=F,nPcs=5)
# resNLPCA<-pca(criteria.scaled,method="nlpca",center=F,nPcs=5,maxSteps=300)
# 
# q2BPCA <- Q2(resBPCA,criteria.scaled,fold=10)
# q2PPCA <- Q2(resPPCA,criteria.scaled,fold=10)
# q2NIPALS <- Q2(resNIPALS,criteria.scaled,fold=10)
# q2SVDI <- Q2(resSVDI,criteria.scaled,fold=10)
# # q2NLPCA <- Q2(resNLPCA,wwmetals.scaled,fold=10)
# q2BPCA
# q2PPCA
# q2NIPALS
# q2SVDI
# # q2NLPCA
# }
# 
# plot.pca<-function() {
#   slplot(resBPCA)
#   slplot(resPPCA)
#   slplot(resNIPALS)
#   slplot(resSVDI)
#   slplot(resNLPCA)
#   
# }
# 
