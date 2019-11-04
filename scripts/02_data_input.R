################################################################################
# Load in data 
# <Init> 2/1/2019
# <Rev> 5/30/2019
# <AU> Doyle
# Load in gifted and mapping data, write to Rdata for easy access from app
################################################################################
library(tidyverse)
library(readxl)
library(scales)
library(noncensus)
library(albersusa)

ddir<-"../data/"

## Get state data for names
data(states)

states<-states%>%select(state,name)%>%slice(1:51)

states$stabbr<-states$state

## Read in codebook
gm_cb<-read_xlsx(paste0(ddir,"codebook.xlsx"))

save(gm_cb,file="gm_codebook.Rdata")

## Read in data file
gm<-read_xlsx(paste0(ddir,"gifted_data_2.xlsx"))

##Select only relevant rows and columns
gm%>%slice(-c(1,53:dim(gm)[1]))%>%select(-c(2:5,7,39:dim(gm)[2]))->gm

## Name variables with descriptive labels
names(gm)<-gm_cb$var_title

pct_vars<-names(gm)[c(2:5)]

mult_100<-function(x){x*100}

## Mutate pct variables
gm%>%mutate_at(.vars=pct_vars,.funs=mult_100)->gm

## For every variable after access, replace MA,DC,VT,RI

out_states<-c("RI", "MA", "VT", "DC")

drop_function<-function(x){ifelse(gm$`State`%in%out_states,NA,x)}

gm<-gm%>%mutate_at(vars(c(4:dim(gm)[2])),.funs=drop_function)

gm$stabbr<-gm$State

gm<-left_join(gm,states,by="stabbr")

gm$State<-gm$name

gm<-gm%>%select(-stabbr,-state,-name)

save(gm,file="gm.Rdata")

spdf <- usa_sf()

spdf$State<-spdf$name

## Join data and shapefile

gm_states<-left_join(spdf,gm,by="State")

save(gm_states,file="gm_states.Rdata")

## Text for About Page
