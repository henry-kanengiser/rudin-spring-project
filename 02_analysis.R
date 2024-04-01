# 2. Analysis

# Author: Henry Kanengiser
# Purpose: To analyze the solar generation dat for the report


# 0. Packages -----------------------------------------------------------------

library(tidyverse)
library(janitor)
library(sf)


# 1. Read in data -------------------------------------------------------------

# solar energy potential - developed using ArcGIS tools and exported as .shp
solar_bldg <- st_read("dat/electricity production/Suitable_buildings.shp") %>%
  clean_names() 

solar_prkg <- st_read("dat/electricity production/Suitable_parking.shp") %>%
  clean_names()

# BBL key - created manually to identify relevant sites
bblkey <- read_csv("dat/Bus depots BBL.csv") %>% 
  clean_names() %>%
  separate_longer_delim(bbl, ";") %>%
  mutate(bbl = as.numeric(bbl)) %>%
  filter(!is.na(bbl))


# 2. Connect buildings and parking lots to depots -----------------------------

# first, combine solar files (geometry is no longer required, so drop it now)
solar <- bind_rows(solar_bldg, solar_prkg) %>% 
  st_drop_geometry() %>%
  mutate(type = ifelse(is.na(source_id), "building", "parking lot"),
         bldg = type == "building",
         prkg = type == "parking lot")

# join with the bblkey file to generate depot name info
depot_solar <- bblkey %>%
  select(borough, depot, bbl) %>%
  full_join(solar, by = "bbl")

# one record from the bblkey is not in the electicity production file, probably too small
depot_solar %>%
  filter(is.na(elc_prd_m_wh))


# 3. Summarize solar information to the depot site level ----------------------
depot_solar_sum <- depot_solar %>%
  group_by(borough, depot) %>%
  summarise(bldg_elc_prd_MWh = sum(bldg * elc_prd_m_wh),
            prkg_elc_prd_MWh = sum(prkg * elc_prd_m_wh),
            bldg_elc_prd_kW = sum(bldg * elc_prd_k_w),
            prkg_elc_prd_kW = sum(prkg * elc_prd_k_w)) %>%
  ungroup()


# 4. Save shapefile with complete information ---------------------------------
# first, combine solar files (geometry is no longer required, so drop it now)
solar_shp <- bind_rows(solar_bldg, solar_prkg) %>% 
  # st_drop_geometry() %>%
  mutate(type = ifelse(is.na(source_id), "building", "parking lot"),
         bldg = type == "building",
         prkg = type == "parking lot")

# join with the bblkey file to generate depot name info
depot_solar_shp <- bblkey %>%
  select(borough, depot, bbl) %>%
  full_join(solar_shp, by = "bbl")



# 5. Write to permanent file --------------------------------------------------

write_csv(depot_solar_sum, "dat/OUT/depot_solar_summary.csv")

# save shapefile for mapping
st_write(depot_solar_shp, "dat/out/depot_solar.shp")

            