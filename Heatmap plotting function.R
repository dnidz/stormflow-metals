## Heatmap plotting function
# Daniel Nidzgorski
# Last updated October 4, 2017

# This creates a "plot.heatmap()" function that returns a ggplot2 object.

# Required arguments:
# data = a data frame in long format (each observation in its own row)
# X = the name (in quotes) of the column with the categories to go on the X axis (along the top)
# Y = same, for y-axis (down the left side)
# Value = name (in quotes) of the column with values corresponding to the fill color

# Optional arguments:

# reorder.X or .Y = reorder the X or Y categories from high to low
# (defaults to reorder X but not Y)

# method = method to use for scaling the data
# (understands "scale", "rescale", "categoric", or "none"; default = scale)
# Note that "scale" and "sescale" scale each Y category independently, so a given color
# means different values in different rows (useful for parameters with different ranges).

# Fill: defaults to a light-blue (low) to dark-red (high) scale
# If midpoint is supplied (i.e. meaningful zero) that becomes white
# in a dark-blue to dark-red scale.
# For method = "categoric", the Value column is a factor for
# manual control of the colors. Use 1 to 4 for light to dark red, and
# -1 to -4 for light to dark blue. (integers only)
# For any fill option, set bluehigh=T to reverse blue and red

# NA values (or implicit missing) are not plotted, leaving visible the background 
# of light grey with white gridlines. To distinguish missing values (e.g. implicit v. explicit)
# pass in a logical "grey" column where TRUE values get plotted with dark grey.
# For example: unmeasured cells are blank whereas 100% <MDL are dark grey.


# Labels: To label the tiles, label = name (in quotes) of the label column
# (can be Value or a different column, esp. if Value is a categoric).
# Also can take label size (labelsize = 2) or the number of digits to round to (labeldigits = 0)



plot.heatmap<-function(data,x="X",y="Y",value="Value",method="scale",
                       midpoint=NA,bluehigh=F,grey=NA,
                       label=NA,labeldigits=0,labelsize=2,
                       reorder.X=T,reorder.Y=F) {
  ## Rename X, Y, and Value columns
  names(data)[names(data)==x]<-"X" 
  names(data)[names(data)==y]<-"Y" 
  names(data)[names(data)==value]<-"Value" 
  # and optionally Label
  if(!is.na(label)) {
    # In case Value is being used for both the fill and the label
    if(label==value) data<-mutate(data,Label=Value)
    
    names(data)[names(data)==label]<-"Label" 
  }
  if(!is.na(grey)) names(data)[names(data)==grey]<-"Grey" 


  
  # Default is blue = low, red = high. To reverse this:
  if(bluehigh) data<-mutate(data,Value = -Value)

  
  ## Convert to matrix-like format, scale, and reassemble
  d.matrix<-data %>%
    ungroup() %>%
    select(X,Y,Value) %>%
    spread(key=Y,value=Value)
  
  # Scale data according to the chosen method
  if(method=="scale") {
    d<-d.matrix %>%
      select(-X) %>%
      scale() %>%
      as.tibble() %>%
      mutate(X=d.matrix$X) %>%
      gather(key=Y,value=Value,-X) %>%
      filter(!is.na(Value))
    if(!is.na(label)) d<-left_join(d,select(data,X,Y,Label),by=c("X","Y"))
  
  } else if(method=="rescale") {
    d<-d.matrix %>%
      select(-X) %>% # since X might be numeric (e.g., year)
      mutate_if(is.numeric,rescale,to=c(0,1)) %>%
      mutate(X=d.matrix$X) %>%
      gather(key=Y,value=Value,-X) %>%
      filter(!is.na(Value))
    if(!is.na(label)) d<-left_join(d,select(data,X,Y,Label),by=c("X","Y"))

  } else {
    d<-data
  } # end methods if/else
  
  ## Y and X need to become factors, reordered or not
  if(reorder.X) {
    # Total for each X of all Ys' scaled values
    total.X<-d %>%
      group_by(X) %>%
      summarize(total=sum(Value,na.rm=T)) %>%
      arrange(-total)
    
    d$X<-ordered(d$X,levels=total.X$X)
  } else {
    d$X<-as.factor(d$X)
  } # end if/else to reorder X
  
  if(reorder.Y) {
    # Total for each Y of all Xs' scaled values
    total.Y<-d %>%
      group_by(Y) %>%
      summarize(total=sum(Value,na.rm=T)) %>%
      arrange(-total)
    
    d$Y<-ordered(d$Y,levels=total.Y$Y)
  } else {
    d$Y<-as.factor(d$Y)
  } # end if/else to reorder Y
  

  ## Plot
  hm<-ggplot(data=d,aes(x=X,y=Y,fill=Value))+
    geom_tile()+
    scale_x_discrete(position="top")+
    scale_y_discrete(limits = rev(levels(d$Y)))+
    theme(legend.position="none",
          axis.text.x=element_text(angle=45,hjust=0))+
    labs(y=NULL,x=NULL)
  
  
  # Fill options:
  if(is.na(midpoint) & method!="categoric") { # default option
    hm<-hm+scale_fill_gradient(low = "lightblue",high = "darkred")
    
  } else if(method=="categoric") { # Value must be on a 1-4 scale, positive or negative
    hm<-hm+scale_fill_manual(values=c("-1"='#9CB1FF', # light blue
                               "-2"='#7290FF',
                               "-3"='#2453FF',
                               "-4"='#121799', # dark blue
                               "4"='#991712', # dark red
                               "3"='#E53027',
                               "2"='#FF8A86',
                               "1"='#FFB9B7')) # light red
    
  } else { # if a midpoint is supplied
    hm<-hm+scale_fill_gradient2(low = "darkblue",high = "darkred",
                                mid="white",midpoint=midpoint)
  }
  
  # Optional label
  if(!is.na(label)) {
    hm<-hm+geom_label(aes(label=round(Label,digits=labeldigits)),fill="white",size=labelsize,
                      label.padding=unit(0.1,"lines"))
    
  }

  if(!is.na(grey)) {
    hm<-hm+geom_tile(data=filter(data,Grey),aes(x=X,y=Y),fill="grey60")
  }

  
  hm
}

