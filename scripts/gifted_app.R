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


source("gifted_functions.r")
load("../data/gm.Rdata")

list_of_vars<-names(gm)[c(1,5,7:10)]#:37)]
gm_sub<-gm[list_of_vars]


# single selection
shinyApp(
  ui = fluidPage(
    radioButtons("plot_type", "Plot Type:",
                 c("Bar Plot" = "bar",
                   "Map" = "map")),
    varSelectInput("variable", "Variable:", gm_sub),
    plotOutput("data")
  ),
  server = function(input, output) {
    output$data <- renderPlot({
      gg_state_plot(df=gm_sub,var=deparse(input$variable),groupvar="state",axis_label="TEXT HERE")
      ##ggplot(mtcars, aes(!!input$variable)) + geom_histogram()
    })
  }
)