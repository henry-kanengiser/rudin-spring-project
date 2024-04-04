# 3. Mapping Layers

# The purpose of this script is to pull spatial layers that will be used
#  in report maps. These layers are not for data analysis, but will sit in the
#  basemap

# 0. Packages
library(tidyverse)
library(tidytransit)
library(tigris)
library(sf)
library(tmap)
library(magrittr)

tmap_mode("view")

# 1. Read in county shapefiles ------------------------------------------------

counties <- counties(state = c("NJ", "NY"), cb = TRUE, resolution = "500k")

# # check output looks right (commented out for now)
# tm_shape(counties) + 
#   tm_polygons()

# 2. Read in bus route GTFS ---------------------------------------------------

# This data comes from the MTA developers website, and is used to create a 
#  simple map of all bus routes across all 5 boroughs
#  source: https://new.mta.info/developers

# Read in all 6 GTFS files
bx <- read_gtfs("http://web.mta.info/developers/data/nyct/bus/google_transit_bronx.zip")
bk <- read_gtfs("http://web.mta.info/developers/data/nyct/bus/google_transit_brooklyn.zip")
mn <- read_gtfs("http://web.mta.info/developers/data/nyct/bus/google_transit_manhattan.zip")
si <- read_gtfs("http://web.mta.info/developers/data/nyct/bus/google_transit_staten_island.zip")
bc <- read_gtfs("http://web.mta.info/developers/data/busco/google_transit.zip")

# using queens bus network redesign for the borough (https://new.mta.info/project/queens-bus-network-redesign)
qn <- read_gtfs("qns-redesign-gtfs.zip")


## Create distinct route and stops shapefiles ----

## Pick simplified filtering dataframe that contains
##  - One trip per route 
##  - All stops associated with that trip
##  - The shape_id & direction_id associated with that trip (for matching to shapes data)
##  - The route names that we use outside of GTFS data

## Then create three output datasetes for each provider:
##  - Stops (distinct to each route in each direction)
##  - Routes (distinct to each route in each direction)



### bx ----
bx_sf <- gtfs_as_sf(bx)

bx_stopstripsroutes <- bx$trips %>%
  select(route_id, trip_id, shape_id, direction_id) %>%
  group_by(route_id, direction_id) %>%
  summarise(ntrips = n_distinct(trip_id),
            trip_id = first(trip_id),
            shape_id = first(shape_id),
            direction_id = first(direction_id)) %>%
  ungroup() %>%
  # add stop_id
  left_join(select(bx$stop_times, trip_id, stop_id), by = "trip_id") %>%
  # add route_short_name
  left_join(select(bx$routes, route_id, route_short_name, route_long_name), by = "route_id")

## Filter spatial data and join on ID vars
bx_stops <- filter_feed_by_trips(bx_sf, pull(bx_stopstripsroutes, trip_id)) %>% 
  extract2('stops') %>%
  left_join(bx_stopstripsroutes, by = "stop_id")

bx_routes <- filter_feed_by_trips(bx_sf, pull(bx_stopstripsroutes, trip_id)) %>% 
  extract2('shapes') %>%
  #join to get useful IDs (note that this file is deduplicated first to avoid duplicating shapes)
  left_join(distinct(bx_stopstripsroutes, shape_id, .keep_all = T), by = "shape_id") %>%
  select(-trip_id, -stop_id)

# Plot routes
tm_shape(bx_routes) + 
  tm_lines(lwd = 2)


### bk ----
bk_sf <- gtfs_as_sf(bk)

bk_stopstripsroutes <- bk$trips %>%
  select(route_id, trip_id, shape_id, direction_id) %>%
  group_by(route_id, direction_id) %>%
  summarise(ntrips = n_distinct(trip_id),
            trip_id = first(trip_id),
            shape_id = first(shape_id),
            direction_id = first(direction_id)) %>%
  ungroup() %>%
  # add stop_id
  left_join(select(bk$stop_times, trip_id, stop_id), by = "trip_id") %>%
  # add route_short_name
  left_join(select(bk$routes, route_id, route_short_name, route_long_name), by = "route_id")

## Filter spatial data and join on ID vars
bk_stops <- filter_feed_by_trips(bk_sf, pull(bk_stopstripsroutes, trip_id)) %>% 
  extract2('stops') %>%
  left_join(bk_stopstripsroutes, by = "stop_id")

bk_routes <- filter_feed_by_trips(bk_sf, pull(bk_stopstripsroutes, trip_id)) %>% 
  extract2('shapes') %>%
  #join to get useful IDs (note that this file is deduplicated first to avoid duplicating shapes)
  left_join(distinct(bk_stopstripsroutes, shape_id, .keep_all = T), by = "shape_id") %>%
  select(-trip_id, -stop_id)

# Plot these
tm_shape(bk_routes) + 
  tm_lines(lwd = 2) 


### mn ----
mn_sf <- gtfs_as_sf(mn)

mn_stopstripsroutes <- mn$trips %>%
  select(route_id, trip_id, shape_id, direction_id) %>%
  group_by(route_id, direction_id) %>%
  summarise(ntrips = n_distinct(trip_id),
            trip_id = first(trip_id),
            shape_id = first(shape_id),
            direction_id = first(direction_id)) %>%
  ungroup() %>%
  # add stop_id
  left_join(select(mn$stop_times, trip_id, stop_id), by = "trip_id") %>%
  # add route_short_name
  left_join(select(mn$routes, route_id, route_short_name, route_long_name), by = "route_id")

## Filter spatial data and join on ID vars
mn_stops <- filter_feed_by_trips(mn_sf, pull(mn_stopstripsroutes, trip_id)) %>% 
  extract2('stops') %>%
  left_join(mn_stopstripsroutes, by = "stop_id")

mn_routes <- filter_feed_by_trips(mn_sf, pull(mn_stopstripsroutes, trip_id)) %>% 
  extract2('shapes') %>%
  #join to get useful IDs (note that this file is deduplicated first to avoid duplicating shapes)
  left_join(distinct(mn_stopstripsroutes, shape_id, .keep_all = T), by = "shape_id") %>%
  select(-trip_id, -stop_id)

# Plot these
tm_shape(mn_routes) + 
  tm_lines(lwd = .5) 


### qn ----

# for some reason, the lat, lon, and sequence vars are read in as characters in 
#  the shapes dataframe. convert them to numeric before doing any more steps
#  for the gtfs_as_sf() function to work
qn$shapes <- qn$shapes %>%
  mutate(across(starts_with("shape_pt"), as.numeric))

qn_sf <- gtfs_as_sf(qn)

qn_stopstripsroutes <- qn$trips %>%
  select(route_id, trip_id, shape_id, direction_id) %>%
  #align direction_id format with other files
  mutate(direction_id = as.integer(direction_id)) %>% 
  group_by(route_id, direction_id) %>%
  summarise(ntrips = n_distinct(trip_id),
            trip_id = first(trip_id),
            shape_id = first(shape_id),
            direction_id = first(direction_id)) %>%
  ungroup() %>%
  # add stop_id
  left_join(select(qn$stop_times, trip_id, stop_id), by = "trip_id") %>%
  # add route_short_name
  left_join(select(qn$routes, route_id, route_short_name, route_long_name), by = "route_id")

## Filter spatial data and join on ID vars
qn_stops <- filter_feed_by_trips(qn_sf, pull(qn_stopstripsroutes, trip_id)) %>% 
  extract2('stops') %>%
  left_join(qn_stopstripsroutes, by = "stop_id")

qn_routes <- filter_feed_by_trips(qn_sf, pull(qn_stopstripsroutes, trip_id)) %>% 
  extract2('shapes') %>%
  #join to get useful IDs (note that this file is deduplicated first to avoid duplicating shapes)
  left_join(distinct(qn_stopstripsroutes, shape_id, .keep_all = T), by = "shape_id") %>%
  select(-trip_id, -stop_id)


# Plot these
tm_shape(qn_routes) + 
  tm_lines(lwd = 1) 


### si ----
si_sf <- gtfs_as_sf(si)

si_stopstripsroutes <- si$trips %>%
  select(route_id, trip_id, shape_id, direction_id) %>%
  group_by(route_id, direction_id) %>%
  summarise(ntrips = n_distinct(trip_id),
            trip_id = first(trip_id),
            shape_id = first(shape_id),
            direction_id = first(direction_id)) %>%
  ungroup() %>%
  # add stop_id
  left_join(select(si$stop_times, trip_id, stop_id), by = "trip_id") %>%
  # add route_short_name
  left_join(select(si$routes, route_id, route_short_name, route_long_name), by = "route_id")

## Filter spatial data and join on ID vars
si_stops <- filter_feed_by_trips(si_sf, pull(si_stopstripsroutes, trip_id)) %>% 
  extract2('stops') %>%
  left_join(si_stopstripsroutes, by = "stop_id")

si_routes <- filter_feed_by_trips(si_sf, pull(si_stopstripsroutes, trip_id)) %>% 
  extract2('shapes') %>%
  #join to get useful IDs (note that this file is deduplicated first to avoid duplicating shapes)
  left_join(distinct(si_stopstripsroutes, shape_id, .keep_all = T), by = "shape_id") %>%
  select(-trip_id, -stop_id)

# Plot these
tm_shape(si_routes) + 
  tm_lines(lwd = 1) 


### bc (MTA Bus Company) ----
bc_sf <- gtfs_as_sf(bc)

bc_stopstripsroutes <- bc$trips %>%
  select(route_id, trip_id, shape_id, direction_id) %>%
  group_by(route_id, direction_id) %>%
  summarise(ntrips = n_distinct(trip_id),
            trip_id = first(trip_id),
            shape_id = first(shape_id),
            direction_id = first(direction_id)) %>%
  ungroup() %>%
  # add stop_id
  left_join(select(bc$stop_times, trip_id, stop_id), by = "trip_id") %>%
  # add route_short_name
  left_join(select(bc$routes, route_id, route_short_name, route_long_name), by = "route_id")

## Filter spatial data and join on ID vars
bc_stops <- filter_feed_by_trips(bc_sf, pull(bc_stopstripsroutes, trip_id)) %>% 
  extract2('stops') %>%
  left_join(bc_stopstripsroutes, by = "stop_id")

bc_routes <- filter_feed_by_trips(bc_sf, pull(bc_stopstripsroutes, trip_id)) %>% 
  extract2('shapes') %>%
  #join to get useful IDs (note that this file is deduplicated first to avoid duplicating shapes)
  left_join(distinct(bc_stopstripsroutes, shape_id, .keep_all = T), by = "shape_id") %>%
  select(-trip_id, -stop_id)

# Plot these
tm_shape(bc_routes) + 
  tm_lines(lwd = 1) 


## Combine bus routes and stops into citywide shapefiles ----------------------
mta_routes <- bind_rows(
  bx_routes %>% mutate(source = "bx"),
  bk_routes %>% mutate(source = "bk"),
  mn_routes %>% mutate(source = "mn"),
  qn_routes %>% mutate(source = "qn"),
  si_routes %>% mutate(source = "si"),
  bc_routes %>% mutate(source = "bus company")
)

mta_stops <- bind_rows(
  bx_stops %>% mutate(source = "bx"),
  bk_stops %>% mutate(source = "bk"),
  mn_stops %>% mutate(source = "mn"),
  qn_stops %>% mutate(source = "qn"),
  si_stops %>% mutate(source = "si"),
  bc_stops %>% mutate(source = "bus company")
)

# Plot these
tm_shape(mta_routes) + 
  tm_lines("source", lwd = 1.5)


# 3. Save permanent files -----------------------------------------------------

# counties 
st_write(counties, "dat/counties/counties.shp", delete_dsn = T)

# bus routes
st_write(mta_routes, "dat/bus_routes/mta_routes.shp", delete_dsn = T)
st_write(mta_stops,  "dat/bus_routes/mta_stops.shp",  delete_dsn = T)





