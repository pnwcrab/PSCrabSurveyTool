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
ps_cmr <- st_read("./Input/Puget_Sound_Crab_Regions.shp")%>%
  st_make_valid()

ps_cmr$Region <- ps_cmr$AreaName 
ps_cmr$Subregion <- ps_cmr$SubareaNam

ps_cmr <- ps_cmr %>%
  dplyr::select(Region, Subregion, geometry)

## Read in crab management regions shapefile
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

## Bring in the crab exclusion areas for the State Commercial Fleet.
# ps_crab_ex <- st_read("./Input/Puget_Sound_Crab_Exclusion_Areas.shp") %>%
#   st_make_valid()
# 
# ps_crab_ex$ExclusionArea <- ps_crab_ex$AreaName
# 
# ps_crab_ex <- ps_crab_ex %>%
#   dplyr::select(ExclusionArea, geometry)

## Create a master PS Crab management spatial layers.
ps.management <- st_join(ps_mfsf, ps_cmr)


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
ps_crab_sg <- st_intersection(ps_hex_2000, ps.management)

ps_crab_sg <- ps_crab_sg %>%
  filter(is.na(ID) == FALSE)

st_write(ps_crab_sg, "./Output/Puget Sound_Hexagonal Survey Grid_2000ft.shp")

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