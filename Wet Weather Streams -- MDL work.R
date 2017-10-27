## Wet weather streams - MDL investigation and replacement
# Daniel Nidzgorski
# July 6, 2017

# Libraries
library(tidyverse)
library(lubridate)
library(stringr)
library(scales)

# Clear desktop & variables
rm (list=ls())


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

# Exclude some non-FDL sites that only have a few samples
streams<-streams %>%
  filter(!Stream %in% c("Issaquah Creek B",
                        "Pipers Creek B",
                        "Lyon Creek B",
                        "North Creek C",
                        "Sammamish River B",
                        "Kelsey Creek C",
                        "Bear Creek B")
  )

# Identify data < MDL
# Note that str_detect returns NA for NA quals, but we need FALSE
streams<-streams %>%
  mutate(BelowMDL=str_detect(Qual,"MDL"),
         BelowMDL=ifelse(is.na(BelowMDL),FALSE,BelowMDL)
  )

## This looks like a good point to average replicate samples
streams.reps<-streams %>%
  group_by(Date,Year,Month,Routine,
           Locator,Stream,Position,LocGroup,MetalOrg,
           Parameter,Parmname,Units) %>%
  summarize(Sample=n(),
            Value=mean(Value,na.rm=T),
            Qual=first(Qual),
            BelowMDL=all(BelowMDL), # If some <MDL and some >MDL, Value averages the >MDLs, so return False
            MDL=mean(MDL,na.rm=T),
            RDL=mean(RDL,na.rm=T),
            Hardness=mean(Hardness,na.rm=T),
            TEXTVALUE=first(TEXTVALUE)
  ) %>%
  ungroup()


## Split out wet-weather and routine metals
wwmetals<-filter(streams.reps,Routine=="wet weather",
                         MetalOrg=="Metal",
                         !is.na(Stream)
          )
rmetals<-filter(streams.reps,Routine=="Routine",
                MetalOrg=="Metal",
                !is.na(Stream)
)

## Parameters with zero detects -- do wet and routine separately
# Note that these lists aren't used for the MDL replacement; the exclusion is repeated within the function.
params.exclude.ww<-wwmetals %>%
  group_by(Parameter) %>%
  summarize(PercentBelowMDL= sum(BelowMDL,na.rm=T)/sum(!is.na(BelowMDL))*100) %>%
  filter(PercentBelowMDL==100) %>%
  .$Parameter

params.exclude.r<-rmetals %>%
  group_by(Parameter) %>%
  summarize(PercentBelowMDL= sum(BelowMDL,na.rm=T)/sum(!is.na(BelowMDL))*100) %>%
  filter(PercentBelowMDL==100) %>%
  .$Parameter

# Note that I'm using MDL/2 as the RValue here, though it's really a high non-detect and should be left blank
# These data are getting used for different purposes than the main dataset, which is why they're separated out.
ww.excluded<-wwmetals %>%
  filter(Parameter %in% params.exclude.ww) %>%
  mutate(Replacement=MDL/2,
         RValue=MDL/2,
         RVqual="AllNonDetect")

r.excluded<-rmetals %>%
  filter(Parameter %in% params.exclude.r) %>%
  mutate(Replacement=MDL/2,
         RValue=MDL/2,
         RVqual="AllNonDetect")

write_csv(ww.excluded,"Metals wet weather - all nondetect.csv")
write_csv(r.excluded,"Metals routine - all nondetect.csv")



### Iterative MDL replacement

# # for building/testing the function
# param<-"Mercury, Total"
# alldata<-wwmetals
# stream<-"Eden Creek"
# mdls<-mdl.list$`Mercury, Total`
# j<-mdls[[2]]

replace.mdls<-function(data.all) {
  
  # Exclude parameters with no detects
  params.exclude<-data.all %>%
    group_by(Parameter) %>%
    summarize(PercentBelowMDL= sum(BelowMDL,na.rm=T)/sum(!is.na(BelowMDL))*100) %>%
    filter(PercentBelowMDL==100) %>%
    .$Parameter
  
  alldata<-filter(data.all,!Parameter %in% params.exclude)
  
  # Function to generate a vector of mdls for a single parameter
  v.mdls<-function(param,data) {
    d<-filter(data,Parameter==param)
    
    unique(d$MDL) %>%
      sort()
  }
  
  # Function to apply the 40% rule
  # Takes in data (and then filters to this MDL), the MDL, and text for the RVqual
  FortyPercent<-function(data,mdl,qual) {
    d.r1<-filter(data,MDL==mdl)
    pct.below<-sum(d.r1$BelowMDL,na.rm=T)/sum(!is.na(d.r1$BelowMDL))
    
    # Replacement value and RVqualifier
    r1<-ifelse(pct.below<0.4,mdl,mdl/2)
    rqual<-sprintf("%s, %s",qual,ifelse(pct.below<0.4,"<40%",">40%"))
    
    # dataframe to return
    d.rep<-d.r1 %>%
      mutate(Replacement=ifelse(BelowMDL,r1,NA),
             RValue=ifelse(BelowMDL,Replacement,Value),
             RVqual=ifelse(BelowMDL,rqual,NA)
      )
    
    d.rep
   }
  
  # Generate lists of parameters and their mdl-vectors, and streams
  param.list<-unique(alldata$Parameter)
  mdl.list<- map(param.list,v.mdls,data=alldata)
  names(mdl.list)<-param.list
  streams.list<-unique(alldata$Stream)
  
  # For each unique combination of stream, parameter, and MDL,
  # figure out the replacement value
  # I'm using for loops here to keep binding together a dataframe instead of dealing with
  # nested lists. Also, within a parameter I need to use the lower data with replacements.
  for(stream in streams.list) {

    for (param in param.list) {
      
      mdls<-mdl.list[[param]] 
      
      d<-filter(alldata,Stream==stream,Parameter==param)
      
      # First replacement value -- use the 40% rule
      d.replaced<-FortyPercent(d,mdls[[1]],"LowestMDL")
      
      # Subsequent mdls
      if(length(mdls)>1){ 
        
        others<-mdls[2:length(mdls)]
        
        for(j in others) {
          # First test if this is a "high nondetect" MDL, with no uncensored
          # data (at any MDL) higher than this MDL.
          if(j>=max(d$Value,na.rm=T)) { 
            d.replaced<-d %>%
              filter(MDL==j) %>%
              mutate(RValue=ifelse(BelowMDL,NA,Value),
                     RVqual=ifelse(BelowMDL,"HighNonDetect",NA)
              ) %>%
              bind_rows(d.replaced)
            
          } else { # If there is some uncensored data above this MDL
            
            # take mean of values from lower MDLs that are below this MDL
            lower.mean<-d.replaced %>%
              filter(RValue<j) %>%
              {mean(.$RValue,na.rm=T)} 
            
            if(!is.na(lower.mean)) {
              d.replaced<-d %>%
                filter(MDL==j) %>%
                mutate(Replacement=ifelse(BelowMDL,lower.mean,NA),
                       RValue=ifelse(BelowMDL,Replacement,Value),
                       RVqual=ifelse(BelowMDL,"LowerMean",NA)
                ) %>%
                bind_rows(d.replaced)
            } else {
              # lower.mean=NA could be because there are no data at lower MDLs,
              # or because all lower data are >= this MDL.
              # In either case, use the 40% rule, but note with different qualifiers
              d.replaced<-FortyPercent(d,mdl=j,
                                       qual=ifelse(length(d.replaced$RValue)==0,
                                                   "NoLowerData","LowerData>MDL")
              ) %>%
                bind_rows(d.replaced)
            } # end if is.na(lower.mean)
            
            
          } # end if higher.mdls.na
        
        } # end for loop through other mdls
      } # end if-more-than-one-mdl
      
      # Initialize alldata the first time, and add to it subsequently.
      if(which(streams.list==stream)==1 & which(param.list==param)==1) {
        alldata.replaced<-d.replaced
      } else alldata.replaced<-bind_rows(alldata.replaced,d.replaced)
      
    } # end for loop through parameters
    
  } # end for loop through streams
  
  
alldata.replaced
} # end function
    
wwmetals.replaced<-replace.mdls(wwmetals)
rmetals.replaced<-replace.mdls(rmetals)

write_csv(wwmetals.replaced,"Metals wet weather - MDL replaced.csv")
write_csv(rmetals.replaced,"Metals routine - MDL replaced.csv")


# ### MDL listings
# # For each metal and each year, what were the MDLs?
# MDLs<-wwmetals %>%
#   group_by(Parameter,Year) %>%
#   summarize(Lowest=min(MDL,na.rm=T),
#             Highest=max(MDL,na.rm=T),
#             Median=median(MDL,na.rm=T),
#             Count=length(unique(MDL))
#   )
# write_csv(MDLs,"MDLs by year.csv")
# 
# MDLs.range<-MDLs %>%
#   ungroup() %>%
#   group_by(Parameter,Lowest) %>%
#   summarize(From=min(Year,na.rm=T)) %>%
#   arrange(Parameter,From) %>%
#   mutate(To=NA)
# 
# for(r in seq_along(MDLs.range$Parameter)) {
#   s<-MDLs.range %>%
#     filter(Parameter==MDLs.range[[r,1]],
#            Lowest<MDLs.range[[r,2]])
#   MDLs.range[[r,4]]<-min(s$From,na.rm=T)
#   
#   # if no later MDLs, replace Inf with last year in which it was measured
#   latest<-MDLs %>%
#     filter(Parameter==MDLs.range[[r,1]]) %>%
#     .$Year %>%
#     max(na.rm=T)
#   
#   if(MDLs.range[[r,4]]==Inf) MDLs.range[[r,4]]<-latest
#   
# }
# 
# MDLs.range<-MDLs.range %>%
#   filter(To>=From)
# 
# View(MDLs.range)
# 
# write_csv(MDLs.range,"MDLs year range.csv")
# 
# MDLs.range$Parameter<-ordered(MDLs.range$Parameter)
# 
# MDLplot<-ggplot(MDLs.range,aes(y=Parameter,x=From))+
#   geom_tile(aes(x=From+(To-From)/2,width=(To-From+1)),fill="white",color="black")+
#   scale_y_discrete(limits = rev(levels(MDLs.range$Parameter)))+
#   geom_text(aes(x=From+(To-From)/2,label=Lowest,hjust=0.5))+
#   scale_x_continuous(breaks=seq(1993,2010,1))+
#   labs(y=NULL,x=NULL)
# 
# ggsave(MDLplot,filename="MDLs over time.png",width=7.5,height=10)

### MDL investigations
# # Overall 45% of samples are below MDL, but that includes some that are 100% <MDL
# 
# # What percent of each metal's samples are below MDL?
# below<-wwmetals %>%
#   group_by(Parameter) %>%
#   summarize(count = n(),
#             PercentBelowMDL= sum(BelowMDL,na.rm=T)/sum(!is.na(BelowMDL))*100
#   ) %>%
#   arrange(-PercentBelowMDL)
# 
# # Rare detects - investigage parameters with >66% below MDL
# rare.params<-below %>%
#   filter(PercentBelowMDL>66) %>%
#   .$Parameter
# 
# rare.detects<-wwmetals %>% 
#   filter(!is.na(Value),Parameter %in% rare.params) %>%
#   arrange(Parameter,Stream,Date)
# # write.csv(rare.detects,"Rare detects.csv")
# 
# below.stream<-wwmetals %>%
#   group_by(Parameter,Stream,MDL) %>%
#   summarize(count=n(),
#             PercentBelowMDL= sum(BelowMDL,na.rm=T)/sum(!is.na(BelowMDL))*100
#   )
# below.mdl<-wwmetals %>%
#   group_by(Parameter,MDL) %>%
#   summarize(count=n(),
#             PercentBelowMDL= sum(BelowMDL,na.rm=T)/sum(!is.na(BelowMDL))*100
#   ) %>%
#   arrange(-PercentBelowMDL,Parameter)
# 
# below.mdl.n<-below.stream %>%
#   select(-PercentBelowMDL) %>%
#   spread(key=Stream,value=count) %>%
#   arrange(Parameter,MDL)
# # write.csv(below.mdl.n,"Number of samples by metal MDL and stream.csv")
# 
# below.mdl.pct<-below.stream %>%
#   select(-count) %>%
#   spread(key=Stream,value=PercentBelowMDL) %>%
#   arrange(Parameter,MDL)
# # write.csv(below.mdl.pct,"Percent below MDL by metal MDL and stream.csv")

# 
# linedata<-tibble(x=seq(0.001,35,0.01),
#                  y=x,
#                  y2=x/2)
# 
# lowermeanplot.data<-wwmetals.replaced %>%
#   filter(str_detect(RVqual,"LowerMean"),MDL<1000) #%>%
#   # mutate(MDL=as.factor(log10(MDL)),
#   #        Replacement=log10(Replacement))
#   
# lowermeanplot<-ggplot(lowermeanplot.data,aes(x=MDL,y=Replacement))+
#   geom_point(alpha=0.05,size=4)+
#   # geom_boxplot()
#   geom_line(data=linedata,aes(x=x,y=y),color="blue")+
#   geom_line(data=linedata,aes(x=x,y=y2),color="blue")+
#   scale_x_log10()+scale_y_log10()
  

  


  