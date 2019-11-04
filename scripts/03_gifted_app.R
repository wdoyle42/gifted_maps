################################################################################
# Shiny App to Display Data
# <Init> 2/1/2019
# <Rev> 5/30/2019
# <AU> Doyle
# Reactive display: for each variable provide description map and barplot
################################################################################

library(shiny)
library(shinythemes)
library(plotly)
library(leaflet)
library(sf)
library(albersusa)
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
    theme = shinytheme("slate"),
    
        
    navbarPage("Students with Gifts and Talents Across the United States: Access, Equity, and Missing Youth",
      
      tabPanel(
        "Plot",
        
        ## First Section: Variable Selection and Description,
        column(2, offset = 1,
               fluidRow(
                 ## Variable Selection Widget
                 varSelectInput("variable", "Choose a Variable:", gm_sub, selected = "access"),
                 ## Reactive Text based on variables
                 htmlOutput("description")),
               fluidRow(
                 img(src='vu06br.jpg', align = "bottom",height=150,width=150),
                 img(src='Purdue-Sig-Black-Gold-rgb.png', align = "bottom",height=150,width=150)
               )),
        # Close First Section
        
        ## Second Section: Plot Output
        column(9,
               ## Map
               fluidRow(leafletOutput("map", height = 600)),
               ## Barplot
               fluidRow(plotlyOutput("barplot", height = 500))),
        # Close Second Section
        
        ## Background for text description
        #tags$style(type='text/css', '#description {background-color: #FFF; color: black;}'),
        
        ## Background for leaflet
        tags$head(tags$style(
          HTML(".leaflet-container {background-color:rgba(255,0,0,0)}" 
              ))),
        
        helpText(
          "                                        ",
          "                                        ",
          "Notes: Rhode Island, Massachusetts, Vermont and DC have been dropped from all variables
            except Access to Identification and Percent Identified as Gifted/Talented
            as they have fewer than 5% of their students having access to identification."
        )
      ),  #Close tab panel
      
      tabPanel(
        "About",
        
        ## Title Section
        titlePanel(
          "Students with Gifts and Talents Across the United States: Access, Equity, and Missing Youth"
        ),
        
        helpText(
          HTML(' <a href="https://www.dropbox.com/sh/8zs94wxcd2cwi1r/AACDZiMMtxq53rqdJtNsrW9Ia?dl=0">Please click here for links to report cards for all 50 states.</a> ')
        )
      ) # Close tabset
    ) # Close navbar
  ), # End fluidpage
  
  
  server = function(input, output) {
    ## Barplot
    output$barplot <- renderPlotly({
      gg_state_plot(
        df = gm,
        var = deparse(input$variable),
        groupvar = "State",
        axis_label = deparse(input$variable)
      )
    })
    ## Map
    output$map <- renderLeaflet({
      map_gen(
        v = deparse(input$variable),
        geo_df = gm_states,
        legend_label  = deparse(input$variable)
      )
    })
    ## Text output
    output$description <-
      renderText(pull_text(var = input$variable, df = gm_cb))
  }
)
