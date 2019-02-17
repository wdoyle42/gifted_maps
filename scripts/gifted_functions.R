## Functions for gifted mapping app


gg_state_plot<-function(df,var,groupvar,axis_label){
  ## gg state_plot:
  ## takes options
  ## df: data
  ## var: variable
  ## groupvar: grouping variable
  ## axis_label: x axis label
  
  df$groupvar<-unlist(df[groupvar])
  
  df$v<-unlist(df[var])
  
  top.percent<-1
  bottom.percent<-0
  
  #Number of levels to cut by
  n.levels<-10
  
  ## Cuts 
  ## Using max makes bad ranges, use top percentile (set above as constant) instead. 
  
  mymax<-quantile(df$v                
                  ,top.percent,na.rm=TRUE)
  
  mymin<-quantile(unlist(df$v)               
                  ,bottom.percent,na.rm=TRUE)
  
  mylevels<-cbreaks(range=c(mymin,mymax),
                    pretty_breaks(n.levels,high.u.bias=5),
                    labels=comma_format())
  
  ##Change those labels into ranges
  i<-2:length(mylevels$labels)
  
  mynicelevels<-NULL
  
  mynicelevels[i-1]<-paste0(mylevels$labels[i-1],"-",mylevels$labels[i])
  
  ##Take all the "v" data and create nice groups for it.
  
  ##Apply ranges as defined above and add to the existing data
  df$vcut<-findInterval(unlist(df[var]),vec=mylevels$breaks)
  
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
  
  df$`State`<-paste0(df$state,"= ",round(df$v,1))
  
  gg<-ggplot(df,aes(label=`State`))
  gg<-gg+geom_bar(aes(
    x=fct_reorder(.f=as_factor(groupvar),
                  .x=v),
    y=v,
    fill=vcut),
    stat="identity")
  gg<-gg+scale_fill_manual(values =pal)
  gg<-gg+coord_flip()
  gg<-gg+xlab("")+ylab(axis_label)
  gg<-gg+theme(axis.text.y=element_text(size=8,angle=15))
  gg<-gg+theme(legend.position="none")
  outplot<-ggplotly(gg,tooltip="label")
  outplot
}


## Mapping Function

map_gen<-function(v,geo_df,legend_label){
  # This is a function to generate a map linked to plots 
  ## of data, it takes one argument "v" which specifies 
  ## the variable to use
  ## geo_df= geographic data frame
  ## legend_label: title of legend
  
  geo_df$v<-geo_df[v][[1]]
  ## Top percent used to set range
  
  top.percent<-1
  bottom.percent<-0
  
  #Number of levels to cut by
  n.levels<-10
  
  ## Cuts 
  ## Using max makes bad ranges, use top percentile (set above as constant) instead. 
  
  mymax<-quantile(geo_df$v,
                  top.percent,na.rm=TRUE)
  
  mymin<-quantile(geo_df$v,
                  bottom.percent,na.rm=TRUE)
  
  mylevels<-cbreaks(range=c(mymin,mymax),
                    pretty_breaks(n.levels,high.u.bias=5),
                    labels=comma_format())
  
  
  ##Change those labels into ranges
  i<-2:length(mylevels$labels)
  
  mynicelevels<-NULL
  
  mynicelevels[i-1]<-paste0(mylevels$labels[i-1],"-",mylevels$labels[i])
  
  ##Take all the "v" data and create nice groups for it.
  
  ##Apply ranges as defined above and add to the existing data
  geo_df$vcut<-findInterval(geo_df$v,vec=mylevels$breaks)
  
  geo_df$vcut<-factor(geo_df$vcut,
                      levels=(i-1),
                      labels=mynicelevels,
                      ordered=TRUE)
  
  
  ## Create palette, might want to match with plot above    
  pal<- (brewer.pal(length(mylevels$breaks), 'RdYlGn'))
  
  fpal <- colorFactor(pal = pal,
                      domain = geo_df$vcut,
                      ordered = TRUE)
  
  
  ## Create a label for each state that links to
  ## external plots
  state_pop<-paste0(
    geo_df$name,"</a><b>",
    '<br/> ',
    v,
    ": ",
    prettyNum((geo_df$v),digits=1)
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
