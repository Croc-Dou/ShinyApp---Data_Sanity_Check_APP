# What is global.R? -------------------------------------------------------

# a script executed before app launches
# the objects generated here can be used both in user and sever
# these are actions that can be done once per session
# such as library calls, data source loading and custom function sourcing


# PACKAGES ---------------------------------------------------------------

library(data.table)
library(dplyr)
library(leaflet)
library(leaflet.extras)
library(zipcode)
library(tidyverse)
library(htmltools)
library(scales)
library(sf)
library(RColorBrewer)
library(tigris)
library(acs)
library(lubridate)
library(plotly)

# DATA INPUT -----------------------------------------------------------------
excl <- read_rds("excl.rds")
excl <- sample_n(excl, 5000)
data(zipcode)
drop.cols <-c('city','state.y')
excl <- left_join(excl,zipcode,by="zip") %>%
  dplyr::select(-one_of(drop.cols)) %>%
  rename(state = state.x)
# DATA PREPROCESSING -----------------------------------------------------------------
## extract date and set new variables year and date
excl$date<-as.Date(paste(excl$ntorgdte),"%m/%d/%Y")
excl$year <- year(excl$date)
excl <- subset( excl, select = -ntorgdte )

## remove'$'in the price
excl$price <- gsub("[\\$,]","",excl$price) 
excl$price <- as.numeric(excl$price)

## remove ovservations from Virgin Island(VI)
excl <- excl %>%
  filter(!state %in% "VI")  

# FIPS COUNTY MATCHING -----------------------------------------------------------------
## Using County-FIPS information from US Census Bureau
fips <- read.table("national_county.txt", 
                   header = FALSE,
                   sep=",", 
                   col.names=c("state", "state code","county code","county","H"), 
                   fill= TRUE, 
                   strip.white=FALSE)
fips$county <- gsub("County$","",fips$county) 

fips <- subset(fips, select = -H)

##counties of PR
PR_Info <- read.table(text = "
                      PR,72,001,Adjuntas 
                      PR,72,003,Aguada
                      PR,72,005,Aguadilla
                      PR,72,007,Aguas Buenas
                      PR,72,009,Aibonito 
                      PR,72,011,Anasco
                      PR,72,013,Arecibo
                      PR,72,015,Arroyo
                      PR,72,017,Barceloneta
                      PR,72,019,Barranquitas
                      PR,72,021,Bayamon
                      PR,72,023,Cabo Rojo
                      PR,72,025,Caguas
                      PR,72,027,Camuy
                      PR,72,029,Canovanas
                      PR,72,031,Carolina
                      PR,72,033,Catano
                      PR,72,035,Cayey
                      PR,72,037,Ceiba
                      PR,72,041,Cidra
                      PR,72,043,Coamo
                      PR,72,045,Comerio
                      PR,72,047,Corozal 
                      PR,72,049,Culebra 
                      PR,72,051,Dorado 
                      PR,72,053,Fajardo 
                      PR,72,054,Florida 
                      PR,72,055,Guanica 
                      PR,72,057,Guayama 
                      PR,72,059,Guayanilla 
                      PR,72,061,Guaynabo 
                      PR,72,063,Gurabo 
                      PR,72,065,Hatillo 
                      PR,72,067,Hormigueros 
                      PR,72,071,Isabela 
                      PR,72,073,Jayuya 
                      PR,72,075,Juana Diaz 
                      PR,72,077,Juncos 
                      PR,72,079,Lajas 
                      PR,72,081,Lares 
                      PR,72,083,Las Marias 
                      PR,72,085,Las Piedras 
                      PR,72,087,Loiza 
                      PR,72,089,Luquillo 
                      PR,72,091,Manati 
                      PR,72,093,Maricao 
                      PR,72,095,Maunabo 
                      PR,72,097,Mayaguez 
                      PR,72,099,Moca 
                      PR,72,101,Morovis 
                      PR,72,103,Nabuabo 
                      PR,72,105,Naranjito 
                      PR,72,107,Orocovis 
                      PR,72,109,Patillas 
                      PR,72,111,Penuelas 
                      PR,72,113,Ponce 
                      PR,72,115,Quebradillas 
                      PR,72,117,Rincon 
                      PR,72,119,Rio Grande 
                      PR,72,121,Sabana Grande 
                      PR,72,123,Salinas 
                      PR,72,125,San German 
                      PR,72,127,San Juan 
                      PR,72,129,San Lorenzo 
                      PR,72,131,San Sabastian 
                      PR,72,133,Santa Isabel 
                      PR,72,135,Toa Alta 
                      PR,72,137,Toa Baja 
                      PR,72,139,Trujillo Alto 
                      PR,72,141,Utuado 
                      PR,72,143,Vega Alta 
                      PR,72,145,Vega Baja 
                      PR,72,147,Vieques 
                      PR,72,149,Villalba 
                      PR,72,151,Yabucoa 
                      PR,72,153,Yauco",
                      header = FALSE,
                      sep=",",
                      col.names=c("state", "state code","county code","county"),
                      fill= TRUE, 
                      strip.white=TRUE)


fips <-rbind(fips, PR_Info)
excl <- left_join(excl, fips, by = c("state"="state","fips_co"="county.code"))

# mAPPING SETUP  -----------------------------------------------------------------
##criteria1: ice lower than 5000
c1 <-
  excl %>%
  filter(!is.na(price),
         price <= 5000) 
##criteria2: price larger than 1.5M
c2 <-
  excl %>%
  filter(!is.na(price),
         price >= 1500000) 

##criteria3: property type not in S,C,P
c3 <- excl %>%
  filter(!is.na(prop_typ),
         !prop_typ %in% c("S", "C", "P"))     

typ4 <- c3 %>%
  filter(prop_typ == "4")   

typ5 <- c3 %>%
  filter(prop_typ == "5")   

typ6 <- c3 %>%
  filter(prop_typ == "6") 

typ9 <- c3 %>%
  filter(prop_typ == "9")   


## map for criteria 4: excluded if nunit not equal to "1"
c4 <- excl %>%
  filter(!is.na(prop_typ),
         !nunits %in% "1")  

nunits2 <- c4 %>%
  filter(nunits == "2")   

nunits3 <- c4 %>%
  filter(nunits == "3")   

nunits4 <- c4 %>%
  filter(nunits == "4")   

nunitsdot <- c4 %>%
  filter(nunits == ".") 

# US POLYGON --------------------------------------------------------------
## Source: US Census Bureau, Geography Division
### load US shapefile (to identify state boundaries)
### Used for choropleth map
#/Users/zhangdoudou/Desktop/data
by_states <- read_sf("tl_2018_us_state.shp") %>%
  st_zm() 
as_tibble(by_states)

# CHOROPLETH MAP --------------------------------------------------------------
## criteria 1
c1_count <- c1 %>%
  group_by(state) %>%
  summarize(total=n()) 

state_c1 <- geo_join(by_states, c1_count, "STUSPS", "state")
state_c1 <- subset(state_c1, !is.na(total))

## criteria 2
c2_count <- c2 %>%
  group_by(state) %>%
  summarize(total=n()) 

state_c2 <- geo_join(by_states, c2_count, "STUSPS", "state")
state_c2 <- subset(state_c2, !is.na(total))

## criteria 3
c3_count <- c3 %>%
  group_by(state) %>%
  summarize(total=n()) 

state_c3 <- geo_join(by_states, c3_count, "STUSPS", "state")
state_c3 <- subset(state_c3, !is.na(total))

## criteria 4
c4_count <- c4 %>%
  group_by(state) %>%
  summarize(total=n()) 

state_c4 <- geo_join(by_states, c4_count, "STUSPS", "state")
state_c4 <- subset(state_c4, !is.na(total))

criteria_list <- c("Property price < $5000","Property price > $1.5M","Property Type not in S,C,P","Number of unit not equal to 1")
map_list <-c("Choropleth Map", "Cluster Map")


##for time series
yearly_count1 <-  c1 %>%
  filter(year < 2019 & year >= 1975) %>%
  drop_na() %>%
  group_by(year) %>%
  summarise(count=n())

yearly_count2 <-  c2 %>%
  filter(year < 2019 & year >= 1975) %>%
  drop_na() %>%
  group_by(year) %>%
  summarise(count=n())

yearly_count3 <-  c3 %>%
  filter(year < 2019 & year >= 1975) %>%
  drop_na() %>%
  group_by(year, prop_typ) %>%
  summarise(count=n())

yearly_count4 <-  c4 %>%
  filter(year < 2019 & year >= 1975) %>%
  drop_na() %>%
  group_by(year, nunits) %>%
  summarise(count=n())