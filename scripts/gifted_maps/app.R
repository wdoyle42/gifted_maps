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


source("./01_gifted_functions.R")

load("gm.Rdata")
load("gm_states.Rdata")
load("gm_codebook.Rdata")

## Drop state from list of variables
gm_sub<-gm%>%select(-1)

shinyApp(
  ui = fluidPage(
    
    ## Title Section
    titlePanel(
      "Students with Gifts and Talents Across the United States: Access, Equity, and Missing Youth"
    ),
    
    fluidRow(
      ## First Section: Variable Selection and Description, 
      column(2,
             fluidRow(
               ## Variable Selection Widget 
               varSelectInput("variable", "Choose a Variable:", gm_sub, selected = "access"),
               ## Reactive Text based on variables
               textOutput("description")
             )
      ), # Close First Section
      
      ## Second Section: Plot Output
      column(10,
             ## Barplot
             fluidRow(
               plotlyOutput("barplot",height = 500)
             ),
             ## Map
             fluidRow(
               leafletOutput("map",height = 600)
             )
      )# Close Second Section
      
    ), # Close overall fluidrow
    
    ## Background for text description
    tags$style(type='text/css', '#description {background-color: #FFF; color: black;}'),
    
    ## Background for leaflet
    tags$head(tags$style(
        HTML(".leaflet-container {background: #FFF; }"))),
    
    helpText("                                        ",
             "                                        ",
             "Copyright, Citation, Contact information here
           Notes: Rhode Island, Massachussetts, Vermont and DC have been dropped from all variables
                      except Access to Identification and Percent Identified as Gifted/Talented
           as they have fewer than 5% of their students having access to identification.")
  ),
    
 # End fluidpage
  
    
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
