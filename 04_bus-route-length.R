# 4. Bus Route Length

# The PURPOSE of this program is to identify the average bus route length
# across all non-express bus routes in NYC and the number of bus trips on 
# an average weekday. These numbers will be used in the narrative and help to
# illustrate the amount of bus travel that can be powered by solar panels.

# 0. Packages -----------------------------------------------------------------
library(tidyverse)
library(tidytransit)
library(sf)
library(clipr)


# 1. Read in GTFS data --------------------------------------------------------

bx <- read_gtfs("http://web.mta.info/developers/data/nyct/bus/google_transit_bronx.zip")
bk <- read_gtfs("http://web.mta.info/developers/data/nyct/bus/google_transit_brooklyn.zip")
mn <- read_gtfs("http://web.mta.info/developers/data/nyct/bus/google_transit_manhattan.zip")
qn <- read_gtfs("http://web.mta.info/developers/data/nyct/bus/google_transit_queens.zip")
si <- read_gtfs("http://web.mta.info/developers/data/nyct/bus/google_transit_staten_island.zip")
bc <- read_gtfs("http://web.mta.info/developers/data/busco/google_transit.zip")

# using queens bus network redesign for the borough (https://new.mta.info/project/queens-bus-network-redesign)
# qn <- read_gtfs("qns-redesign-gtfs.zip")


# 2. Pull number of trips per route & distance traveled -----------------------

gtfslist <- list(bx, bk, mn, qn, si, bc)

# do this functionally across all 6 programs, but set it up for one first
make_routesum <- function(df) {
  
  gtfs2 <- set_servicepattern(df) %>%
    gtfs_as_sf()
  
  gtfs2$shapes$length <- st_length(st_transform(gtfs2$shapes))
  
  shape_lengths <- gtfs2$shapes %>% 
    as.data.frame() %>% 
    select(shape_id, length, -geometry)
  
  service_pattern_summary <- gtfs2$trips %>%
    left_join(gtfs2$.$servicepatterns, by="service_id") %>% 
    left_join(shape_lengths, by="shape_id") %>%
    left_join(gtfs2$stop_times, by="trip_id") %>% 
    group_by(servicepattern_id) %>% 
    summarise(
      trips = n(), 
      routes = n_distinct(route_id),
      total_distance_per_day_km = sum(as.numeric(length), na.rm=TRUE)/1e3,
      route_avg_distance_km = (sum(as.numeric(length), na.rm=TRUE)/1e3)/(trips*routes),
      stops=(n_distinct(stop_id)/2))
  
  top_svc_pattern <- service_pattern_summary %>%
    arrange(desc(trips)) %>%
    head(n = 1) %>%
    pull(servicepattern_id)
  
  # choose the servicepattern_id with the greatest number of trips
  ## that is: s_3cb3c9a
  
  gtfs2$trips %>%
    left_join(gtfs2$.$servicepatterns, by="service_id") %>% 
    filter(servicepattern_id == top_svc_pattern) %>%
    left_join(shape_lengths, by="shape_id") %>%
    left_join(gtfs2$stop_times, by="trip_id") %>% 
    group_by(route_id) %>% 
    summarise(
      trips = n_distinct(trip_id), 
      total_distance_per_day_mi = (mean(as.numeric(length), na.rm=TRUE)*0.000621371)*trips,
      route_avg_distance_mi = (mean(as.numeric(length), na.rm=TRUE)*0.000621371),
      stops=(n_distinct(stop_id)/2)) # half point if uneven stops in each direction

}

# Run this across all 6 gtfs files and collapse it into one final dataframe
route_summary <- map_dfr(gtfslist, make_routesum)


# 3. Summary statistics -------------------------------------------------------

# add flag for express buses which we won't be using
route_summary2 <- route_summary %>%
  mutate(sbs = ifelse(str_detect(route_id, "\\+"), 1, 0),
         expressbus = ifelse(str_detect(route_id, "(?i)(?<!^)M"), 1, 0),
         buscat = case_when(
           sbs == 1 ~ "sbs",
           expressbus == 1 ~ "express",
           TRUE ~ "conventional"
         ))

# now create weighted means for distance for different types of routes
overall_length <- route_summary2 %>%
  summarise(
    sample = "all buses (incl SBS & express)",
    total_distance_per_day_mi = sum(total_distance_per_day_mi),
    ntrips = sum(trips),
    avg_distance_per_trip_mi = weighted.mean(route_avg_distance_mi, trips)
  )

buscat_length <- route_summary2 %>%
  group_by(sample = buscat) %>%
  summarise(
    total_distance_per_day_mi = sum(total_distance_per_day_mi),
    ntrips = sum(trips),
    avg_distance_per_trip_mi = weighted.mean(route_avg_distance_mi, trips)
  )

# combine these to create a summary table

summary_length_table <- bind_rows(
  buscat_length,
  overall_length
)


# 4. Export final information -------------------------------------------------

summary_length_table %>% write_clip()

route_summary2 %>% write_clip()
