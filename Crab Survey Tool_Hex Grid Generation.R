library(tidyverse)
library(dplyr)
library(lubridate)
library(sf)
library(tmap)
library(mapview)
library(RColorBrewer)
library(ggplot2)

# Read crab management spatial files.
## Read in Puget Sound shapefile
ps <- st_read( "./Input/Puget_Sound_WDFW_Boundary.shp")%>%
  st_make_valid()

ps <- ps %>%
  dplyr::select(AreaName, geometry)

## Read in crab management regions shapefile
### This file has the 26A E and W designations. 
ps_crab_mfsf <- st_read("./Input/Puget_Sound_Crab_MFSF_CA_Modifications.shp")%>%
  st_make_valid()

## Read in MFSF Catch Areas shapefile 
ps_mfsf <- st_read("./Input/Puget_Sound_MFSF_CatchAreas.shp")%>%
  st_make_valid()

### Combine all MFSF CA's and the modified crab ones
ps_mfsf <- rbind(ps_mfsf, ps_crab_mfsf)

ps_mfsf$CatchArea <- ps_mfsf$AreaName 

ps_mfsf <- ps_mfsf %>%
  dplyr::select(CatchArea, geometry) %>%
  filter(CatchArea != "26A")

## Create a master PS Crab management spatial layers.
ps.management <- ps_mfsf %>%
  mutate(Region = case_when(CatchArea %in% c("26D", "28A", "28B", "28C", "28D") ~ "6", 
                            CatchArea %in% c("26B", "26C") ~ "4", 
                            CatchArea %in% c("25C", "27A", "27B", "27C") ~ "5",
                            CatchArea %in% c("25B", "25D", "26A-W") ~ "2W",
                            CatchArea %in% c("24A", "24B", "24C", "24D", "26A-E") ~ "2E",
                            CatchArea %in% c("23D", "25A", "25E") ~ "3",
                            CatchArea %in% c("23A", "23B", "23C", "23D", 29) ~ "3",
                            CatchArea %in% c("20A", "20B", "21A", "21B", "22A", "22B") ~ "1"))

# Make hex grid in footprint of Puget Sound Management layer shapefile 
ps_hex_2000 <- st_make_grid(ps, 
                            cellsize = 2000,
                            square = FALSE,
                            flat_topped = TRUE) %>%
  st_sf()

ps_hex_2000 <- ps_hex_2000 %>%
  mutate(ID = row_number())

## Join together the management information with the hex cells.
## Roll together from the largest spatial unit to the smallest.
ps_crab_sg <- st_join(ps_hex_2000, ps.management) %>%
  filter(is.na(Region) == FALSE)

ps_crab_sg <- ps_crab_sg %>%
  filter(is.na(ID) == FALSE)

st_write(ps_crab_sg, "./Output/Puget Sound_Hexagonal Survey Grid_2000ft.shp")
saveRDS(ps_crab_sg, "./Output/Puget Sound_Hexagonal Survey Grid_2000ft.rds")

ps.map <- mapview(ps_crab_sg,
                  zcol =  "Region",
                  cex = NULL,
                  alpha = 0.25,
                  legend = TRUE,
                  label = "ID",
                  pane = NULL)

## Filter down to 2E for survey tool exercise
crab_2e_sg <- ps_crab_sg %>%
  filter(Region == "2E")

st_write(crab_2e_sg, "./Output/Region 2E_Hexagonal Survey Grid_2000ft.shp")

Reg.2E.map <- mapview(crab_2e_sg,
        zcol =  "CatchArea",
        cex = NULL,
        alpha = 0.25,
        legend = TRUE,
        label = "ID",
        pane = NULL)
