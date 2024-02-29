# Exploratory Analysis

# Author: Henry Kanengiser
# Purpose: To explore various datasets and understand what data is available for
## this research project



# 0. Packages -----------------------------------------------------------------

library(tidyverse)
library(janitor)
library(clipr)
library(sf)
# library(rgdal)
library(tmap)


# 1. Read in data -------------------------------------------------------------

pluto <- read_csv("https://data.cityofnewyork.us/resource/64uk-42ks.csv")

pluto_url <- URLencode("https://data.cityofnewyork.us/resource/64uk-42ks.csv?$query=
                        SELECT borough, block, lot, bbl, ownername, bct2020, bldgclass, landuse
                        LIMIT 1000000")

pluto <- read_csv(pluto_url)

# use ownername filtering criteria developed below to restrict the giant file
## of MapPLUTO to make this run much faster
mp <- st_read("dat/nyc_mappluto_23v3_1_fgdb/MapPLUTO23v3_1.gdb",
              layer = "MapPLUTO_23v3_1_clipped",
              query = "SELECT BBL, OwnerName 
                       FROM \"MapPLUTO_23v3_1_clipped\" 
                       WHERE OwnerName LIKE 'MTA%' OR
                             OwnerName LIKE 'LIRR%' OR
                             OwnerName LIKE 'NYC T%' OR 
                             OwnerName LIKE '%TRANSIT%' OR
                             OwnerName LIKE 'METROPOLITAN TRANSPORTATION AUTHORITY'")



# # export this to look at it spatially
# st_write(mp, dsn = "dat/exploratory/mp_mtaowner.shp")


# 2. Identify potential owner name information --------------------------------

# It is notoriously difficult to identify all properties owned by the MTA,
## because properties are registered with many different agency names
## and because the MTA has many departments/agencies within it that have different roles

pluto_owner <- pluto %>% select(bbl, ownername)
distinct_owners <- pluto_owner %>%
  distinct(ownername, .keep_all = T)

# # checking the sensitivity of string detection
# distinct_owners %>%
#   filter(str_detect(ownername, "MTA |^LIRR|NYC T|Transit")) %>%
#   left_join(distinct_owners %>%
#               filter(str_detect(ownername, "^MTA |^LIRR|NYC T|Transit")) %>%
#               rename(ownername2 = ownername) %>%
#               mutate(in_list2 = 1),
#             by = "bbl") %>%
#   arrange(desc(in_list2)) %>%
#   print(n = 30)

# Final filtering method that produces only MTA-relevant ownername information
distinct_owners %>%
  filter(str_detect(ownername, "^MTA |^LIRR|NYC T|Transit|METROPOLITAN TRANSPORTATION AUTHORITY") & 
           str_detect(ownername, "HYLTON|TOWNHOUSE|COLUMBIA|TAXI|DOVE|HOLDINGS|TOPPING", negate = T)) 

pluto_owner %>%
  filter(str_detect(ownername, "^MTA |^LIRR|NYC T|Transit|METROPOLITAN TRANSPORTATION AUTHORITY") & 
           str_detect(ownername, "HYLTON|TOWNHOUSE|COLUMBIA|TAXI|DOVE|HOLDINGS|TOPPING", negate = T)) %>%
  count(ownername) %>%
  adorn_totals(where = "row", name = "TOTAL")

  
# Note that this includes some but not all elevated/above-ground stations
##  Example: J/Z elevated stations and 7 trains with stations in the ROW do not have tax lot status
##   so they aren't included in PLUTO
# Doesn't seem like that data is going to be published according to the MTA Open Data catalog, nor does it exist anywhere


# 3. Save permanent file ------------------------------------------------------

mp %>%
  rename(name = OwnerName) %>%
  st_write("dat/mappluto_mtaowned/mappluto_mtaowned.shp", delete_dsn = T)




