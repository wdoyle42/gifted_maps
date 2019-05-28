################################################################################
# Load in data 
# <Init> 2/1/2019
# <AU> Doyle
# Load in gifted and mapping data, write to Rdata for easy access from app
################################################################################

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
library(readxl)


ddir<-"../data/"


## Read in codebook
gm_cb<-read_xlsx(paste0(ddir,"codebook.xlsx"))

save(gm_cb,file="gm_codebook.Rdata")

## Read in data file
gm<-read_xlsx(paste0(ddir,"gifted_data_2.xlsx"))


##Select only relevant rows and columns
gm%>%slice(-c(1,53:dim(gm)[1]))%>%select(-c(2:5,39:dim(gm)[2]))->gm

## Name variables with descriptive labels
names(gm)<-gm_cb$var_title

pct_vars<-names(gm)[c(2,4:6)]

mult_100<-function(x){x*100}

gm%>%mutate_at(.vars=pct_vars,.funs=mult_100)->gm

gm$`National Rank in Access to Identification`<-as.numeric(gm$`National Rank in Access to Identification`)

gm$stabbr<-gm$State

save(gm,file="gm.Rdata")

spdf <- usa_sf()

spdf$stabbr<-spdf$iso_3166_2

## Join data and shapefile

gm_states<-left_join(spdf,gm,by="stabbr")

save(gm_states,file="gm_states.Rdata")

