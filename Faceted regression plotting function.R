# Faceted regression plot

# The regression.plot function plots a faceted set of streams
# Includes all streams in the regression data, (e.g. only p<0.05)
# and filters the base data down to that set.

# Regression data must have columns "Stream", "slope", and "intercept"

# Needs to know what x and y columns in the base data to use
# Also supply an x-axis label
# Y-axis label and title (reused as filename) are optional. 
# Title gets the parameter automatically appended; useful for mapping over function.

regression.plot<-function(data,regdata,param,x,y,xlabel,ylabel=NA,title=NA,logy=F) {
  reg<-filter(regdata,Parameter==param) %>%
    arrange(Stream)
  
  streams.list<-unique(reg$Stream)
  num.streams<-length(streams.list)
  if(num.streams<4) num.streams<-4 # to keep graphs tall enough
  
  if(is.na(ylabel)) ylabel<-sprintf("%s (µg/L)",param)
  if(is.na(title)) title<-sprintf("%s regressions",x)
  title<-sprintf("%s - %s",title,param)
  
  names(data)[names(data)==y]<-"ycol" 
  names(data)[names(data)==x]<-"xcol" 
  
  d<-filter(data,Parameter==param,
            Stream %in% streams.list) %>%
    arrange(Stream)
  
  max.x<-max(d$xcol,na.rm=T)
  min.x<-min(d$xcol,na.rm=T)
  
  max.y<-max(d$ycol,na.rm=T)
  
  reg.df<-map_df(streams.list,make.reg.df,reg=reg,max=max.x,min=min.x)
  
  p<-ggplot(data=d,aes(x=xcol,y=ycol,shape=BelowMDL)) +
    facet_wrap(~Stream,ncol=4)+
    geom_point(size=3)+
    scale_shape_manual(values=c("FALSE"=19,"TRUE"=23),
                       labels=c("FALSE"="Above MDL","TRUE"="Below MDL"))+   
    geom_line(data=reg.df,aes(x=x,y=y),inherit.aes=F)+
    coord_cartesian(ylim=c(0,max.y*1.1))+
    labs(y=ylabel,
         x=xlabel,
         title=title)+
    theme(axis.text.x=element_text(size=11),
          axis.text.y=element_text(size=11),
          axis.title.y=element_text(size=13),
          legend.title=element_blank(),
          legend.justification=c(0,1),
          legend.position=c(0,1),
          legend.background = element_rect(fill=NA)
    )+ guides(shape=guide_legend(nrow=1))
  
 if(logy) p<-p+scale_y_log10()

    
  ggsave(p,filename=sprintf("%s.png",title),
         width=15,height=2+(num.streams/4*2.2))
  
 title
  
  
}




make.reg.df<-function(reg,stream,max,min) {
  r<-filter(reg,Stream==stream)
  
  output<-tibble(Stream=stream,
                 x=seq(from=min, to=max,by=0.1),
                 y=x*r$slope+r$intercept
  )
  
  output
  
}