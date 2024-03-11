# 1. Getting Data

# Author: Henry Kanengiser
# Purpose: To read in datasets necessary for the spatial analysis of this project


# 0. Packages -----------------------------------------------------------------

library(tidyverse)
library(janitor)
library(sf)
library(tmap)


# 1. Read in data -------------------------------------------------------------

# BBL key - created manually to identify relevant sites
bblkey <- read_csv("dat/Bus depots BBL.csv") %>% 
  clean_names() %>%
  separate_longer_delim(bbl, ";")

bbllist <- bblkey %>%
  filter(!is.na(bbl)) %>%
  mutate(bbl_clean = trimws(bbl, which = "both")) %>%
  pull(bbl_clean)


# Building footprints - read in from Open Data & filtered using bbllist from above
## source: https://data.cityofnewyork.us/Housing-Development/Building-Footprints/nqwf-w8eh

fp_query <- URLencode(paste0("https://data.cityofnewyork.us/resource/qb5r-6dgf.csv?$query=
                      SELECT the_geom, bin, mpluto_bbl
                      WHERE mpluto_bbl IN ('",
                      paste(bbllist, collapse = "' , '"),
                      "')"))

fp <- read_csv(fp_query)
# fp <- read_csv("https://data.cityofnewyork.us/resource/qb5r-6dgf.csv")


# Parking lots - read in from Open Data & filtered using bbllist from above
## 

pk_query <- URLencode("https://data.cityofnewyork.us/resource/7cgt-uhhz.csv?$query=
                      SELECT the_geom, source_id
                      LIMIT 100000")

pk <- read_csv(pk_query)


# MapPLUTO (reading it in without having the entire file saved on this computer)
temp <- tempfile()
temp2 <- tempfile()

mp_url <- "https://s-media.nyc.gov/agencies/dcp/assets/files/zip/data-tools/bytes/nyc_mappluto_23v3_1_shp.zip"
download.file(mp_url, temp)

unzip(zipfile = temp, exdir = temp2)

mp <- st_read(temp2)

mp2 <- mp %>%
  clean_names() %>%
  select(bbl, owner_name, geometry) %>%
  filter(bbl %in% c(bbllist))


# 2. Restructure and turn to spatial format -----------------------------------


# Building footprints - convert to shapefile
fp_shp <- fp %>%
  st_as_sf(wkt = "the_geom", crs = st_crs(4326)) %>%
  st_transform(st_crs(2263))


# check this process visually
tmap_mode("view")

tm_shape(fp_shp) + 
  tm_polygons("bin")


# Parking lots - restrict to lots that fall within tax lots of interest
pk2 <- pk %>%
  st_as_sf(wkt = "the_geom", crs = st_crs(4326)) %>%
  st_transform(st_crs(2263)) %>%
  st_join(mp2, join = st_intersects) %>%
  filter(!is.na(bbl))

# get_dupes(pk, source_id)

# note that parking lots sometimes intersect several tax lots and are being duplicated here
# we may later need to deduplicate this but for now it's okay
get_dupes(pk2, source_id) %>%
  st_drop_geometry() %>% 
  print(n = 100)


# 3. Write to permament files -------------------------------------------------

## Building footprints
fp_shp %>%
  rename(bbl = mpluto_bbl) %>% 
  st_write("dat/OUT/building footprints/fp.shp", delete_dsn = T)

## Parking lots 
pk2 %>%
  select(-owner_name) %>%
  st_write("dat/OUT/parking lots/pk.shp", delete_dsn = T)

## MapPLUTO tax lots
mp2 %>%
  rename(name = owner_name) %>%
  st_write("dat/OUT/mappluto/mp.shp", delete_dsn = T)


