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


source("./01_gifted_functions.r")

load("gm.Rdata")
load("gm_States.Rdata")
load("gm_codebook.Rdata")

list_of_vars<-names(gm)[c(2:34)]

gm_sub<-gm[list_of_vars]


shinyApp(
  ui = fluidPage(
    titlePanel(
      "Students with Gifts and Talents Across the United States: Access, Equity, and Missing Youth"
    ),
    fluidRow(
      ## First Section, 
      column(2,
            
             ## Variaable Selection Widget 
            fluidRow(
                      varSelectInput("variable", "Choose a Variable:", gm_sub, selected = "access")
               ),
            ## Reactive Text based on variables
                      textOutput("description")
               ),
      
    ## Second Section: Plot Output
               column(10,
                      ## Barplot
                      fluidRow(
                               plotlyOutput("barplot")
                        ),
                       ## Map
                      fluidRow(
                                leafletOutput("map",height = 600)
                      ) 
               )# Close Column
    ), # Close overall fluidrow
                      
    
    tags$style(type='text/css', '#description {background-color: #FFF; color: black;}'),
    
    ## Background for leaflet
      tags$head(tags$style(
        HTML(".leaflet-container { background: #FFF; }")))
    
    ),
  
    
   server = function(input, output) {
    
     ## Barplot
       output$barplot <- renderPlotly({
      gg_state_plot(df=gm,var=deparse(input$variable),groupvar="State",axis_label=deparse(input$variable))
    })
      ## Map 
      output$map<-renderLeaflet({
        map_gen(v=deparse(input$variable), geo_df = gm_states,legend_label  =deparse(input$variable) )
      })
    ## Text output
      output$description<-renderText(pull_text(var=input$variable,df=gm_cb))
  }
)
