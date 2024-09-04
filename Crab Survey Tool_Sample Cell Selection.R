# This script imports the 2000ft hexagonal grid cells, performs some minor filtering 
# of the data, summarizes the proportional area of MFSF catch areas in 2-West, 
# defines sample depth strata and 
# chooses primary, secondary, and tertiary sample sites.

# Load packages
library(tidyverse)
library(sf)
library(mapview)

## Import survey grid

raw_grid <- st_read("./Input/PS Crab 2000ftHexGrid_ZonalSummary.shp")%>%
  st_make_valid() %>%
  distinct()

# mapview(raw_grid)  

dim(raw_grid)

## Filter grid cells to exclude those cells that are entirely on land. 
grid <- raw_grid %>%
  filter(MIN < 0 & MAX < 0)

dim(grid)
# Convert depths to absolute values for easier use. 
grid <- grid %>%
  mutate(MinDepth = abs(MAX),
         MaxDepth = abs(MIN), 
         MedianDepth = abs(MEDIAN),
         MeanDepth = abs(MEAN)) %>%
  select(-c(MAX, MIN, MEDIAN, MEAN))

# mapview(grid)  

dim(grid)
### potential survey cells dropped from 17385 to 12044


## Create a subset of sample cells in 2-west.
grid_2w <- grid %>%
  filter(Region == "2W")

dim(grid_2w)
# mapview(grid_2w, 
#         zcol = "CatchArea")  

# calculate proportional sample cells by catch area
catch_areas <- table(grid_2w$CatchArea)

prop_ca <- prop.table(catch_areas)

# Subset features to depth bins
## Cells that match shallow criteria
shallow_2w <- grid_2w %>%
  # filter(MinDepth >= 15) %>%
  filter(MinDepth <= 60) %>%
  mutate(STRATA = "SHALLOW") %>%
  distinct(ID, .keep_all = TRUE)

# mapview(shallow_2w)

## Cells that match mid criteria. 
mid_2w <- grid_2w %>% 
  filter(MinDepth >= 15) %>%
  filter(MinDepth <= 95) %>%
  mutate(STRATA = "MID") %>%
  distinct(ID, .keep_all = TRUE)

# mapview(mid_2w)

## Cells that match deep criteria. 
deep_2w <- grid_2w %>% 
  filter(MinDepth > 95) %>%
  filter(MinDepth < 200) %>%
  mutate(STRATA = "DEEP") %>%
  distinct(ID, .keep_all = TRUE)

# mapview(deep_2w)

## Combine subsets back into one dataframe for plotting
grid_2w_binned <- bind_rows(shallow_2w, mid_2w, deep_2w)

mapview(list(shallow_2w, mid_2w, deep_2w),
        zcol = "STRATA")

# Choose sample cells.
set.seed(159)

## 25B 
sample_cells_25B <- grid_2w_binned %>% 
  filter(CatchArea == "25B") %>% # subset to CA 25B
  group_by(STRATA) %>% # group sample cells in CA 25B by depth strata
  slice_sample(n = round((3*(60 * 0.5)))) %>% # sample primary, secondary, and tertiary cells within 25B 
                                              # based on proportion of all potential 
                                              # samples in 2W that fall within 25B. 
  mutate(SamplePriority = rep(1:3, length.out = n()))

sample_cells_25B$SamplePriority <- as.factor(sample_cells_25B$SamplePriority) 

table(sample_cells_25B$SamplePriority)

## 25D
sample_cells_25D <- grid_2w_binned %>%
  filter(CatchArea == "25D") %>% # sample primary, secondary, and tertiary cells within 25D 
                                  # based on proportion of all potential 
                                  # samples in 2W that fall within 25D.
  group_by(STRATA) %>%
  slice_sample(n = round((3*(60 * 0.06))))  %>%
  mutate(SamplePriority = rep(1:3, length.out = n()))

sample_cells_25D$SamplePriority <- as.factor(sample_cells_25D$SamplePriority) 

table(sample_cells_25D$SamplePriority)

## 26A-W
sample_cells_26AW <- grid_2w_binned %>%
  filter(CatchArea == "26A-W") %>% # sample primary, secondary, and tertiary cells within 26AW 
                                  # based on proportion of all potential 
                                  # samples in 2W that fall within 26AW.
  group_by(STRATA) %>%
  slice_sample(n = round((3*(60 * 0.44)))) %>%
  mutate(SamplePriority = rep(1:3, length.out = n()))

sample_cells_26AW$SamplePriority <- as.factor(sample_cells_26AW$SamplePriority)

table(sample_cells_26AW$SamplePriority)


## Combine selected cells back together
selected <- bind_rows(sample_cells_25B, sample_cells_25D, sample_cells_26AW )
table(selected$CatchArea)

mapview(selected,
        zcol = c("STRATA", "SamplePriority"),
        layer.name = "Selected Sample Cells by Depth Strata")

selected_1 <- selected[selected$SamplePriority == "1", ]
table(selected_1$CatchArea)
xtabs( ~ CatchArea + STRATA, data = selected_1)

mapview(selected_1,
        zcol = c("STRATA"),
        layer.name = "Selected Sample Cells by Depth Strata")

## Identify cells that are sampled more than once. 
selected_count <- aggregate()