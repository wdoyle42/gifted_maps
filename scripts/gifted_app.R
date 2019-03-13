library(ggplot2)
library(shiny)
library(plotly)
library(readxl)
library(usmap)
library(leaflet)
library(sp)
library(sf)
library(tigris)
library(albersusa)
library(htmlwidgets)
library(scales)
library(RColorBrewer)
library(tidyverse)


source("./gifted_functions.r")

load("gm.Rdata")
load("gm_states.Rdata")

list_of_vars<-names(gm)[c(1,5,7:37)]

gm_sub<-gm[list_of_vars]


shinyApp(
  ui = fluidPage(
    titlePanel("TITLE OF PROJECT"),
    sidebarPanel(
    varSelectInput("variable", "Variable:", select(gm_sub,-state),selected = "access"),
    helpText("Lorem ipsum dolor sit amet, id eum diceret probatus scriptorem, probo delenit repudiandae vim te. Sit utinam regione propriae ei, in alia erant interpretaris quo. Mundi omnes te pro. Eam recteque suavitate liberavisse ex, ea sit populo corpora maluisset, ei vim quis omnium reprehendunt. Commune apeirian cu sit, et adhuc salutandi vix.")
  ),
  mainPanel(plotlyOutput("barplot"),leafletOutput("map") ),
  tags$head(
    tags$style(HTML(".leaflet-container { background: #FFF; }"))
  )
  ),
 
   server = function(input, output) {
      output$barplot <- renderPlotly({
      gg_state_plot(df=gm_sub,var=deparse(input$variable),groupvar="state",axis_label=deparse(input$variable))
    })
      output$map<-renderLeaflet({
        map_gen(v=deparse(input$variable), geo_df = gm_states,legend_label  =deparse(input$variable) )
      })
  }
)
