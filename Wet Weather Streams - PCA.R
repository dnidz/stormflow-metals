## Wet weather streams - PCA
# Daniel Nidzgorski
# July 7, 2017

# Libraries
library(tidyverse)
library(lubridate)
library(stringr)
library(scales)
library(pcaMethods)

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

## First PCA: Purely descriptive (no scaling to toxicity criteria)
# Use means of all samples (all years)

wwmetals.means<-wwmetals %>%
  group_by(Stream,Parameter) %>%
  summarize(GrandMean=mean(RValue,na.rm=T)) %>%
  filter(!is.na(GrandMean))

wwmetals.matrix<-wwmetals.means %>%
  spread(key=Parameter,value=GrandMean) %>%
  # Dissolved K and Na both have few measurements; remove
  select(-`Potassium, Dissolved`,-`Sodium, Dissolved`)

wwmetals.totals<-wwmetals %>%
  filter(str_detect(Parameter,"Total")) %>%
  group_by(Stream,Parameter) %>%
  summarize(GrandMean=mean(RValue,na.rm=T)) %>%
  spread(key=Parameter,value=GrandMean)

wwmetals.scaled<-wwmetals.matrix %>%
  ungroup() %>%
  select(-Stream) %>%
  scale()
row.names(wwmetals.scaled)<-wwmetals.matrix$Stream

dopca<-function() { # wrapped as function so it doesn't all run each time. Don't use as function!

resBPCA<-pca(wwmetals.scaled,method="bpca",center=F,nPcs=5)
resPPCA<-pca(wwmetals.scaled,method="ppca",center=F,nPcs=5)
resNIPALS<-pca(wwmetals.scaled,method="nipals",center=F,nPcs=5)
resSVDI<-pca(wwmetals.scaled,method="svdImpute",center=F,nPcs=5)
resNLPCA<-pca(wwmetals.scaled,method="nlpca",center=F,nPcs=5,maxSteps=300)

q2BPCA <- Q2(resBPCA,wwmetals.scaled,fold=10)
q2PPCA <- Q2(resPPCA,wwmetals.scaled,fold=10)
q2NIPALS <- Q2(resNIPALS,wwmetals.scaled,fold=10)
q2SVDI <- Q2(resSVDI,wwmetals.scaled,fold=10)
# q2NLPCA <- Q2(resNLPCA,wwmetals.scaled,fold=10)
q2BPCA
q2PPCA
q2NIPALS
q2SVDI
# q2NLPCA
}

plot.pca<-function() {
slplot(resBPCA)
slplot(resPPCA)
slplot(resNIPALS)
slplot(resSVDI)
slplot(resNLPCA)

} # end dopca

## Heatmap
source("Heatmap plotting function.R")

wwmetals.hm<-wwmetals.means %>%
  rename(X=Stream,Y=Parameter,Value=GrandMean) 

# ggsave(plot.heatmap(wwmetals.hm,method="scale"),
#        file="Heatmap - wet weather metals.png",width=8.5,height=11)
# 
# ggsave(plot.heatmap(wwmetals.hm,method="rescale"),
#        file="Heatmap - wet weather metals - rescale.png",width=8.5,height=11)

wwmetals.metalmean<-wwmetals.means %>%
  group_by(Parameter) %>%
  summarize(metalmean=mean(GrandMean,na.rm=T)) %>%
  ungroup()

wwmetals.means<-wwmetals.means %>%
  ungroup() %>%
  left_join(wwmetals.metalmean,by="Parameter") %>%
  mutate(ScaleVal=GrandMean/metalmean)

wwmetals.hm.log<-wwmetals.means %>%
  select(X=Stream,Y=Parameter,Value=ScaleVal) 
plot.heatmap(wwmetals.hm.log,method="none")

ggplot(data=wwmetals.means,aes(x=Parameter,y=ScaleVal))+
  geom_boxplot()+
  scale_y_log10()
