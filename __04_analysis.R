# Analysis script

# The purpose of this script is to analyze the differences in Census data
#  among tracts with commuter bus access and tracts without it

# 0. Packages -----------------------------------------------------------------

library(tidyverse)
library(tidycensus)
library(janitor)
library(sf)
library(clipr)

# 1. Read in ACS file ---------------------------------------------------------

acs_spatial <- st_read("acs/tracts_pctbus.shp")


# Read in additional variable necessary for calculating % values for the 

varlist <- c(
  hhpop = "S1602_C01_001",
  hhinc_lt10    = "B19001_002",
  hhinc_10t15   = "B19001_003",
  hhinc_15t20   = "B19001_004",
  hhinc_20t25   = "B19001_005",
  hhinc_25t30   = "B19001_006",
  hhinc_30t35   = "B19001_007",
  hhinc_35t40   = "B19001_008",
  hhinc_40t45   = "B19001_009",
  hhinc_45t50   = "B19001_010",
  hhinc_50t60   = "B19001_011",
  hhinc_60t75   = "B19001_012",
  hhinc_75t100  = "B19001_013",
  hhinc_100t125 = "B19001_014",
  hhinc_125t150 = "B19001_015",
  hhinc_150t200 = "B19001_016",
  hhinc_ge200   = "B19001_017"
  )

acs_add_nj <- get_acs(
  geography = "tract",
  variables = c(varlist),
  state = "NJ",
)

acs_add_ny <- get_acs(
  geography = "tract",
  variables = varlist,
  state = "NY",
  county = c("Rockland", "Orange"),
)

acs_add_pa <- get_acs(
  geography = "tract",
  variables = varlist,
  state = "PA",
  county = "Pike",
)

acs_add <- acs_add_nj %>%
  bind_rows(acs_add_ny, acs_add_pa) %>%
  clean_names() %>%
  select(-moe) %>%
  pivot_wider(id_cols = c(geoid, name),
              names_from = variable,
              values_from = estimate) %>%
  #collapse some income variables to match PA's categories
  mutate(hhinc_lt15 = hhinc_lt10 + hhinc_10t15,
         hhinc_15t25 = hhinc_15t20 + hhinc_20t25,
         hhinc_25t50 = hhinc_25t30 + hhinc_30t35 + hhinc_35t40 + hhinc_40t45 + hhinc_45t50,
         hhinc_50t75 = hhinc_50t60 + hhinc_60t75,
         hhinc_100t150 = hhinc_100t125 + hhinc_125t150,
         .keep = "unused") %>%
  select(geoid, name, hhpop, hhinc_lt15:hhinc_50t75, 
         hhinc_75t100, hhinc_100t150, hhinc_150t200, hhinc_ge200)


# 2. Setup --------------------------------------------------------------------

acs <- acs_spatial %>%
  st_drop_geometry() %>%
  mutate(bus_flag = ifelse(pct_bus > 25, 1, 0),
         bus_er_flag = ifelse(pct_bus_er > 25, 1, 0),
         bus_cat = case_when(
           bus_flag == 1 & bus_er_flag == 1 ~ "bus access only",
           bus_flag == 1 & bus_er_flag == 0 ~ "bus & train access",
           bus_flag == 0 & bus_er_flag == 0 ~ "no bus access"
         )) %>%
  # remove 6 census tracts with 0 population in South Jersey
  filter(!is.na(bus_flag))
  

acs %>% count(bus_cat, bus_flag, bus_er_flag)

# remove tract data from counties with bus tracts
acs_county <- acs %>%
  select(bus_flag, name) %>%
  separate(name, into = c("tract", "county", "state"), sep = ", ") %>%
  count(county, bus_flag) %>%
  group_by(county) %>% 
  mutate(pct = n/sum(n)) %>%
  ungroup()

# remove counties where there are 0 tracts with bus access
counties_to_drop <- acs_county %>%
  filter(bus_flag == 0 & pct == 1) %>%
  pull(county) %>%
  paste(collapse = "|")

# 3. Analysis -----------------------------------------------------------------

# group by bus_cat and calculate weighted means by population
acs_summary <- acs %>%
  # remove counties with 0 tracts with bus access
  filter(!str_detect(name, counties_to_drop)) %>%
  #join additional vars (left join keeps only the counties we want in the analysis)
  left_join(acs_add, by = c("geoid", "name")) %>%
  # rearrange variables for mass summarizing 
  select(totpop:hispa, re_other, limeng:limeng_ot, starts_with("hhinc_"), hhpop, everything()) %>%
  group_by(bus_cat) %>%
  # summarise(totpop = sum(totpop))
  summarise(across(totpop:hhpop, ~ sum(.x, na.rm = T)),
            mhhi = median(mhhi, na.rm = T))

# calculate total value
acs_summary_all <- acs %>%
  # remove counties with 0 tracts with bus access
  filter(!str_detect(name, counties_to_drop)) %>%
  #join additional vars (left join keeps only the counties we want in the analysis)
  left_join(acs_add, by = c("geoid", "name")) %>%
  # rearrange variables for mass summarizing 
  select(totpop:hispa, re_other, limeng:limeng_ot, starts_with("hhinc_"), hhpop, everything()) %>%
  # summarise(totpop = sum(totpop))
  summarise(across(totpop:hhpop, ~ sum(.x, na.rm = T)),
            mhhi = median(mhhi, na.rm = T)) %>%
  bind_rows(acs_summary)

acs_summary_all %>% write_clip()

# group by bus_cat and county to calculate weighted means by population
acs_county_summary_cat <- acs %>%
  # remove counties with 0 tracts with bus access
  filter(!str_detect(name, counties_to_drop)) %>%
  #join additional vars (left join keeps only the counties we want in the analysis)
  left_join(acs_add, by = c("geoid", "name")) %>%
  separate(name, into = c("tract", "county", "state"), sep = ", ") %>%
  # rearrange variables for mass summarizing 
  select(totpop:hispa, re_other, limeng:limeng_ot, starts_with("hhinc_"), hhpop, everything()) %>%
  group_by(bus_flag, county) %>%
  # summarise(totpop = sum(totpop))
  summarise(across(totpop:hhpop, ~ sum(.x, na.rm = T)),
            mhhi = median(mhhi, na.rm = T))

# group by county to calculate weighted means by population
acs_county_summary <- acs %>%
  # remove counties with 0 tracts with bus access
  filter(!str_detect(name, counties_to_drop)) %>%
  #join additional vars (left join keeps only the counties we want in the analysis)
  left_join(acs_add, by = c("geoid", "name")) %>%
  separate(name, into = c("tract", "county", "state"), sep = ", ") %>%
  # rearrange variables for mass summarizing 
  select(totpop:hispa, re_other, limeng:limeng_ot, starts_with("hhinc_"), hhpop, everything()) %>%
  group_by(county) %>%
  # summarise(totpop = sum(totpop))
  summarise(across(totpop:hhpop, ~ sum(.x, na.rm = T)),
            mhhi = median(mhhi, na.rm = T)) %>%
  mutate(bus_flag = 9)

# combine to collect county-level data
acs_county_summary2 <- bind_rows(acs_county_summary_cat, acs_county_summary) %>%
  arrange(county)

acs_county_summary2 %>% write_clip()


# 4. Mapping layer & misc output ----------------------------------------------

# create a census tract layer that only includes tracts from the counties 
#  included in this analysis

acs_spatial %>%
  select(geoid, name, geometry) %>%
  filter(!str_detect(name, counties_to_drop)) %>%
  st_write("mapping layers/county_tracts.shp")

acs %>%
  # remove counties with 0 tracts with bus access
  filter(!str_detect(name, counties_to_drop)) %>%
  #join additional vars (left join keeps only the counties we want in the analysis)
  left_join(acs_add, by = c("geoid", "name")) %>%
  # rearrange variables for mass summarizing 
  select(totpop:hispa, re_other, limeng:limeng_ot, starts_with("hhinc_"), hhpop, everything()) %>%
  write_csv("tracts_finall_alldata.csv")




