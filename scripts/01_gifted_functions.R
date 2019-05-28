## Functions for gifted mapping app


gg_state_plot<-function(df,var,groupvar,axis_label){
  ## gg state_plot:
  ## takes options
  ## df: data
  ## var: variable
  ## groupvar: grouping variable
  ## axis_label: x axis label
  
  select_vars<-c(groupvar,var)
  
  df<-df%>%select_at(select_vars)%>%drop_na()
  
  df$groupvar<-unlist(df[groupvar])
  
  df$v<-unlist(df[var])
  
  #Number of levels to cut by
  n.levels<-10
  
  my_accuracy=.01
  
  ## Cuts 
  
  mymax<-max(df$v,na.rm=-TRUE)
  
  mymin<-min(df$v,na.rm=TRUE)
  
  mylevels<-cbreaks(range=c(mymin,mymax),
                    pretty_breaks(n.levels),
                    labels=comma_format(accuracy = my_accuracy))
  
  ##Change those labels into ranges
  i<-2:length(mylevels$labels)
  
  mynicelevels<-NULL
  
  mynicelevels[i-1]<-paste0(mylevels$labels[i-1],"-",mylevels$labels[i])
  
  ##Take all the "v" data and create nice groups for it.
  
  ##Apply ranges as defined above and add to the existing data
  df$vcut<-findInterval(unlist(df$v),vec=mylevels$breaks)
  
  df$vcut<-factor(df$vcut,
                  levels=(i-1),
                  labels=mynicelevels,
                  ordered=TRUE)
  
  ## Create palette, might want to match with plot above    
  pal<- (brewer.pal(length(mylevels$breaks), 'RdYlGn'))
  
  fpal <- colorFactor(pal = pal,
                      domain = df$vcut,
                      ordered = TRUE)
  myval<-fpal(df$vcut)
  
  gg<-ggplot(df,aes(text=paste0(df$State,"= ",round(df$v,2))))
  gg<-gg+geom_bar(aes(
    x=fct_reorder(.f=as_factor(groupvar),
                  .x=v),
    y=v,
    fill=vcut),
    width=.75,
    stat="identity")
  gg<-gg+scale_fill_manual(values =pal)
  gg<-gg+coord_flip()
  gg<-gg+xlab("")+ylab(axis_label)
  gg<-gg+theme_minimal()
  gg<-gg+theme(axis.text.y=element_text(size=7,angle=15))
  gg<-gg+theme(legend.position="none")
  outplot<-ggplotly(gg,tooltip="text")
  outplot
}


## Mapping Function

map_gen<-function(geo_df,var,legend_label){
  # This is a function to generate a map linked to plots 
  ## of data, it takes one argument "v" which specifies 
  ## the variable to use
  ## geo_df= geographic data frame
  ## legend_label: title of legend
  
  select_vars<-c("State",var)
  
  geo_df<-geo_df%>%select_at(select_vars)
  
  geo_df$v<-geo_df[var][[1]]
  ## Top percent used to set range
  #Number of levels to cut by
  n.levels<-10
  
  my_accuracy=.01
  
  ## Cuts 
  
  mymax<-max(geo_df$v,na.rm=-TRUE)
  
  mymin<-min(geo_df$v,na.rm=TRUE)
  
  mylevels<-cbreaks(range=c(mymin,mymax),
                    pretty_breaks(n.levels),
                    labels=comma_format(accuracy = my_accuracy))
  
  ##Change those labels into ranges
  i<-2:length(mylevels$labels)
  
  mynicelevels<-NULL
  
  mynicelevels[i-1]<-paste0(mylevels$labels[i-1],"-",mylevels$labels[i])
  
  ##Take all the "v" data and create nice groups for it.
  
  ##Apply ranges as defined above and add to the existing data
  geo_df$vcut<-findInterval(unlist(geo_df$v),vec=mylevels$breaks)
  
  geo_df$vcut<-factor(geo_df$vcut,
                  levels=(i-1),
                  labels=mynicelevels,
                  ordered=TRUE)
  
  ## Create palette, might want to match with plot above    
  pal<- (brewer.pal(n.levels, 'RdYlGn'))
  
  fpal <- colorFactor(pal = pal,
                      domain = geo_df$vcut,
                      ordered = TRUE)
  
  ## Create a label for each state that links to
  ## external plots
  state_pop<-paste0(
    geo_df$`State`,
    ": ",
    prettyNum((geo_df$v),digits=4)
    )
  
  ## Set line weights
  myweight=1
  myopacity=.5
  
  ## Set projection
  epsg2163 <- leafletCRS(
    crsClass = "L.Proj.CRS",
    code = "EPSG:2163",
    proj4def = "+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs",
    resolutions = 2^(16:7))
  
  ## Create leaflet map
  out_map<-leaflet(data = geo_df,
                   options = leafletOptions(crs = epsg2163)) %>% 
    addPolygons(
      color = 'black',
      weight = myweight,
      opacity=myopacity,
      fillColor = fpal(geo_df$vcut),
      fillOpacity = 0.75,
      popup=state_pop)%>%
    addLegend('bottomright',
              title=legend_label,
              pal = fpal,
              values = geo_df$vcut
    )%>%
    setView(lng = -98.35, lat = 39.50, zoom = 3)
  

  
} # End function


## Pull text function
pull_text<-function(var,df){
  my_text<-df$description[df$var_title==var]
  my_text
}
