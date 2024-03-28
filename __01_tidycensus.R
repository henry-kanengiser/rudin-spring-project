# Script: tidycensus.r
# Purpose: Download ACS 5-year estimate data for Assignment 3

library(tidyverse)
library(janitor)
library(tidycensus)
library(sf)

# Include census API key
# census_api_key("b79cffc22be2625e69c44505ffb55c60498f5796", install = TRUE, overwrite=TRUE)


## Identify ACS vars ----------------------------------------------------------

# Look at tidycensus documentation
## COMMENTING OUT TO AVOID RE-RUNNING, SLOW STEP
v21 <- load_variables(2021, "acs5")
# v21s <- load_variables(2021, "acs5/subject")

# Interested in:
# - Total population          (B01001_001)
totpopvar <- c(totpop = "B01001_001")

# - Race & ethnicity
racevars <- c(hispa = "B03002_012",
              white = "B03002_003",
              black = "B03002_004",
              asian = "B03002_006",
              nhisp = "B03002_002")
# Hispanic              (B03002_012)
# Non-hispanic white    (B03002_003)
# Non-hispanic black    (B03002_004)
# Non-hispanic asian    (B03002_006)
# Other                 (calculate from B03002_002 minus the non-hispanic categories)

# - Median hh income          (B19013_001)
incvars <- c(mhhi = "B19013_001")
# From table "MEDIAN HOUSEHOLD INCOME IN THE PAST 12 MONTHS 
#              (IN 2021 INFLATION-ADJUSTED DOLLARS)"

# - Limited english proficiency ()
engvars <- c(hhpop      = "S1602_C01_001", # added after I did the analysis, so will duplicate this in 04 Analysis
             limeng     = "S1602_C03_001",
             limeng_sp  = "S1602_C03_002",
             limeng_ie  = "S1602_C03_003",
             limeng_as  = "S1602_C03_004",
             limeng_ot  = "S1602_C03_005",
             limeng_pct = "S1602_C04_001")

acsvars <- c(totpopvar, racevars, incvars, engvars)


## Read in ACS data -----------------------------------------------------------
acs <- get_acs(geography = "tract", 
               variables = c(acsvars),
               state = "NJ",
               year = 2021,
               survey = "acs5")

# We need data from PA and NY too, just the counties in the MSA west of Hudson

acs_pa <- get_acs(geography = "tract", 
                  variables = c(acsvars),
                  state = "PA",
                  county = "Pike",
                  year = 2021,
                  survey = "acs5")

acs_ny <- get_acs(geography = "tract", 
                  variables = c(acsvars),
                  state = "NY",
                  county = c("Rockland", "Orange"),
                  year = 2021,
                  survey = "acs5")


# Read in geometry separately to add onto the file once it's restructured
acs_geom_nj <- get_acs(geography = "tract",
                       variables = "B01001A_001",
                       state = "NJ",
                       year = 2021,
                       survey = "acs5",
                       geometry = T) %>%
  clean_names() %>%
  select(geoid, geometry)

acs_geom_ny <- get_acs(geography = "tract",
                       variables = "B01001A_001",
                       state = "NY",
                       county = "Rockland",
                       year = 2021,
                       survey = "acs5",
                       geometry = T) %>%
  clean_names() %>%
  select(geoid, geometry)

acs_geom_pa <- get_acs(geography = "tract",
                       variables = "B01001A_001",
                       state = "PA",
                       county = "Pike",
                       year = 2021,
                       survey = "acs5",
                       geometry = T) %>%
  clean_names() %>%
  select(geoid, geometry)

acs_geom <- bind_rows(acs_geom_nj, acs_geom_ny, acs_geom_pa)

# join NY and PA county data onto NJ data and analyze it all together
acs_wide <- acs %>%
  bind_rows(acs_pa, acs_ny) %>%
  clean_names() %>%
  select(-moe) %>%
  pivot_wider(id_cols = c(geoid, name),
              names_from = variable,
              values_from = estimate)



## Checks on data, create 'other' vars ----------------------------------------
# race & ethnicity - create non-hispanic other count

# Check that hispa & nhisp sum to total population
acs_wide %>%
  select(name, totpop, hispa, nhisp) %>%
  mutate(ck_totpop = hispa + nhisp,
         ck_same = totpop == ck_totpop) %>%
  count(ck_same)

# Create final dataset with other categories & spatial data
acs_final <- acs_wide %>%
  mutate(re_other = nhisp - (asian + black + white)) %>%
  full_join(acs_geom, by = "geoid")

# Check that the geometry isn't missing for any census tracts
acs_final %>%
  mutate(ck_geom = ifelse(is.na(geometry), 0, 1)) %>%
  st_drop_geometry() %>%
  count(ck_geom)

## Write shp ------------------------------------------------------------------

# Mega-shapefile with all the data
acs_final %>% 
  select(-nhisp) %>%
  st_write("acs/acs.shp", delete_dsn = T)


