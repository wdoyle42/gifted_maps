################################################################################
## Functions for gifted mapping app
## Key functions for plotting and mapping state-level data
## AU: Will Doyle
## Init: May, 2019
## Rev: Sep, 2019
###############################################################################

################################################################################
## Global Parameters
################################################################################

#Decimal Point Accuracy
my_accuracy=.01

## Breaks for percents
break_vector_percents<-c(seq(0,100,by=10))

break_vector_short_percents<-c(seq(0,30,by=5))

## breaks for ratios
break_vector_ratios<-c(seq(0,1,by=.1),
                       seq(1,5,by=.5))

## Hex codes as specified for percent variables
percent_hex_codes <- c(
  "#f21811",
  "#f86631",
  "#fc955b",
  "#ffbc8c",
  "#fc8d0a",
  "#f1aa2c",
  "#e7c250",
  "#e0d876",
  "#95c045",
  "#77b839"
)

## Hex codes as specified for percent variables
short_percent_hex_codes <- c(
  "#f21811",
  "#fc955b",
  "#fc8d0a",
  "#f1aa2c",
  "#e0d876",
  "#95c045",
  "#77b839"
)


## Hex codes as specified for ratios
ratio_hex_codes <- c(
  "#f21811",
  "#ffbc8c",
  "#f86631",
  "#fc955b",
  "#ffa456",
  "#fc8d0a",
  "#16a82b", 
  "#00c281", 
  "#00cda7", 
  "#00d8c9", 
  "#00e1e6", 
  "#00eaff", 
  "#00cdfc", 
  "#00aff7", 
  "#0090ee", 
  "#0070dd",
  "#004dc4",
  "#3422a1",
  "#6237af", 
  "#864fbe"
)

################################################################################
# Specify Decimal Function
################################################################################

## @param x= variable
## @param k= number of decimal points
specify_decimal <- function(x, k) trimws(format(round(x, k), nsmall=k))

################################################################################
## Factor Levels function
################################################################################

## @ param df= data frame
## @ param var= specified variable
## returns factor variable with nice looking levels for either percent or ratio
## variabls

factor_level<-function(df, var){
  ## move var into $v for convenience
  df$v<-unlist(df[var][[1]])
  
  ## Determine whether ratio or percent
  
  if(var=="Percent of Students with Access to GT Identification"){
    mylevels<-cbreaks(range=c(0,100),
                      breaks=break_vector_percents,
                      labels=comma_format(accuracy = my_accuracy))
  } else  if(grepl("Percent",var)) {
    mylevels<-cbreaks(range=c(0,31),
                      breaks=break_vector_short_percents,
                      labels=comma_format(accuracy = my_accuracy))
  }  
  else{
    mylevels<-cbreaks(range=c(0,5),
                      breaks=break_vector_ratios,
                      labels=comma_format(accuracy = my_accuracy))
  }
  
  ##Change those labels into ranges
  i<-2:length(mylevels$labels)
  
  mynicelevels<-NULL
  
  mynicelevels[i-1]<-paste0(mylevels$labels[i-1],"-",mylevels$labels[i])
  
  ##Take all the "v" data and create nice groups for it.
  
  ##Apply ranges as defined above and add to the existing data
  df$vcut<-findInterval(unlist(df$v),vec=mylevels$breaks)
  
  ## Turn result into ordered factor
  df$vcut<-factor(df$vcut,
                  levels=(i-1),
                  labels=mynicelevels,
                  ordered=TRUE)
  df$vcut
} ## End level factor function
  
################################################################################
## Color Mapping function
################################################################################


color_mapping <- function(df, var) {
  ## This is a function to map one of two sets
  ## of hex codes to variable levels,
  ## with one set of levels for ratio variables
  ## and another set of levels for percent variables
  
  df$v <- unlist(df[var][[1]])
  
  ## Determine whether ratio or percent
  
  if (var == "Percent of Students with Access to GT Identification") {
    mylevels <- cbreaks(
      range = c(0, 100),
      breaks = break_vector_percents,
      labels = comma_format(accuracy = my_accuracy)
    )
  }  else  if (grepl("Percent", var)) {
    mylevels <- cbreaks(
      range = c(0, 31),
      breaks = break_vector_short_percents,
      labels = comma_format(accuracy = my_accuracy)
    )
  }
  else{
    mylevels <- cbreaks(
      range = c(0, 5),
      breaks = break_vector_ratios,
      labels = comma_format(accuracy = my_accuracy)
    )
  }
  
  ## Apply factor level to df
  df$vcut <- factor_level(df, var)
  
  ## Color mapping function for leaflet
  ## Apply different functions to ratios or percents
  ## Ratios only go to 5
  if (var == "Percent of Students with Access to GT Identification") {
    fpal <- colorFactor(pal = percent_hex_codes,
                        domain = df$vcut,
                        ordered = TRUE)
  }  else  if (grepl("Percent", var)) {
    fpal <- colorFactor(pal = short_percent_hex_codes,
                        domain = df$vcut,
                        ordered = TRUE)
  }
  else {
    fpal <- colorFactor(pal = ratio_hex_codes,
                        domain = df$vcut,
                        ordered = TRUE)
  }
  
  ## Color mapping function for ggplot
  ## All percent variables are named percent
  if (var == "Percent of Students with Access to GT Identification") {
    bar_colors <- setNames(percent_hex_codes, levels(df$vcut))
  } else  if (grepl("Percent", var)) {
    bar_colors = setNames(short_percent_hex_codes, levels(df$vcut))
  }
  else {
    bar_colors <- setNames(ratio_hex_codes, levels(df$vcut))
  }
  
  list(fpal, df$vcut, bar_colors)
  
} # End color mapping function

################################################################################
## Bar Plot Function
################################################################################

gg_state_plot<-function(df,var,groupvar,axis_label){
  ## gg state_plot:
  ## takes options
  ## df: data frame name
  ## var: variable name, string variable
  ## groupvar: grouping variable, string variable
  ## axis_label: x axis label, string variable
  
  select_vars<-c(groupvar,var)
  
  df<-df%>%select_at(select_vars)%>%drop_na()
  
  df$groupvar<-unlist(df[groupvar])
  
  df$v<-unlist(df[var])
  
  ## Color mapping: cuts
  df$vcut<-unlist(color_mapping(df,var)[[2]])
  
  ## Color mapping: values
  bar_colors<-color_mapping(df,var)[[3]]
  
  ## Percent=% in legend label
  axis_label<-str_replace(axis_label,"Percent","%")
  
  gg<-ggplot(df,aes(text=paste0(df$State,"= ",specify_decimal(df$v,2))))
  gg<-gg+geom_bar(aes(
    x=fct_reorder(.f=as_factor(groupvar),
                  .x=v),
    y=v,
    fill=vcut),
    width=.75,
    stat="identity")
  gg<-gg+scale_fill_manual(values=bar_colors)
  gg<-gg+coord_flip()
  gg<-gg+xlab("")+ylab(axis_label)
  gg<-gg+theme_minimal()
  gg<-gg+theme(axis.text.y=element_text(size=7,angle=15))
  gg<-gg+theme(legend.position="none")
  outplot<-ggplotly(gg,tooltip="text")
  outplot
} # End State Plot Function

################################################################################
## Mapping Function
################################################################################

map_gen<-function(geo_df,var,legend_label){
  # This is a function to generate a map linked to plots 
  ## of data, it takes one argument "v" which specifies 
  ## the variable to use
  ## geo_df= geographic data frame
  ## legend_label: title of legend
  
  
legend_label<-str_replace(legend_label,"Percent","%")
  
  select_vars<-c("State",var)
  
  geo_df<-geo_df%>%select_at(select_vars)
  
  geo_df$v<-geo_df[var][[1]]
  
  fpal<-color_mapping(geo_df,var)[[1]]
  
  geo_df$vcut<-color_mapping(geo_df,var)[[2]]
  
  ## Create a label for each state that links to
  ## external plots
  state_pop<-paste0(
    geo_df$`State`,
    ": ",
    specify_decimal(geo_df$v,2),
    " ",
    legend_label
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
                   options = leafletOptions(crs = epsg2163,
                   minZoom=3,
                   maxZoom=3,
                   dragging=FALSE,
                   zoomControl=FALSE
                   ) )%>% 
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
    setView(lng = -98.35, lat = 39.50,zoom=3)%>%
    setMaxBounds(lng1=-125,lng2=-40,lat1=10, lat2=60)
  out_map

  
} # End Mapping function

################################################################################
## Pull text function
################################################################################

## Used to grab codebook information and present it
pull_text<-function(var,df){
  my_text<-df$description[df$var_title==var]
  my_text
} # End function






