``
################################################################################
# Load in data 
# <Init> 2/1/2019
# <AU> Doyle
# Load in gifted and mapping data, write to Rdata for easy access from app
################################################################################

library(tidyverse)
library(readxl)

ddir<-"../data/"

gm<-read_xlsx(paste0(ddir,"gifted_data.xlsx"))

gm%>%slice(-c(1:2,54:59))->gm

labels<-names(gm)

names(gm)<-names(gm)%>%
  str_replace_all(c(" "="_",
                    "-"="_",
                    ","=""
  ))%>%
  tolower()

pct_vars<-names(gm)[c(6,8,9,10,12:38)]

mult_100<-function(x){x*100}

gm%>%mutate_at(.vars=pct_vars,.funs=mult_100)->gm

gm$equity_between_non_title_1_and_title_1_schools[gm$state=="VT"]<-NA

gm$stabbr<-gm$state

gm$national_rank_in_access<-as.numeric(as.character(gm$national_rank_in_access))

names(gm)[5:37]<-labels[5:37]

save(gm,file="gm.Rdata")

spdf <- usa_sf()
spdf$stabbr<-spdf$iso_3166_2

## Join data and shapefile

gm_states<-left_join(spdf,gm,by="stabbr")

save(gm_states,file="gm_states.Rdata")

#gm_states<-left_join(gm,spdf,by="stabbr")
#names(gm_states)[19:56]<-names(gm)[5:42]