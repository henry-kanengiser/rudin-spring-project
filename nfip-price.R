## OpenFEMA - NFIP Policies

# The purpose of this script is to read in flood insurance price file 
## to gauge the costs of flood insurance for residents in the relocated area

# Data source: https://www.fema.gov/openfema-data-page/fima-nfip-redacted-policies-v2
## This also contains the data dictionary

# 0. Install Packages ---------------------------------------------------------

library(tidyverse) 
library(data.table)
library(clipr)
# library(jsonlite)
# library(httr)
# library(data.table)
# library(sqldf)
# library(sqldf)


# 1. Read in data -------------------------------------------------------------

nfip_ny <- read_csv("nfip_ny.csv") %>%
  mutate(year = year(policyEffectiveDate))

nfip_ny %>%
  filter(reportedZipCode == "11249") %>%
  group_by(reportedZipCode, year) %>%
  summarise(avg_policyCost = mean(policyCost),
            n = n()) %>%
  write_clip()




nfip <- fread("https://www.fema.gov/about/reports-and-data/openfema/FimaNfipPolicies.csv", sep = ",")

nfip <- fread(cmd="grep 'NY' https://www.fema.gov/about/reports-and-data/openfema/FimaNfipPolicies.csv",
              select = c("censusTract", "reportedZipCode", "id", "policyCost", 
                         "policyCount", "propertyState", "policyEffectiveDate"))

nfip <- fread(cmd=paste("grep", " 'NY' ","https://www.fema.gov/about/reports-and-data/openfema/FimaNfipPolicies.csv"),
              select = c("censusTract", "reportedZipCode", "id", "policyCost", 
                         "policyCount", "propertyState", "policyEffectiveDate"))

nfip <- fread("https://www.fema.gov/about/reports-and-data/openfema/FimaNfipPolicies.csv", sep = ",", 
              select = c("censusTract", "reportedZipCode", "id", "policyCost", 
                         "policyCount", "propertyState", "policyEffectiveDate"))

nfip <- fread("https://www.fema.gov/about/reports-and-data/openfema/FimaNfipPolicies.csv", sep = ",", 
              select = c("censusTract", "reportedZipCode", "id", "policyCost", 
                         "policyCount", "propertyState", "policyEffectiveDate"))[reportedZipCode = "11249"]

nfip_ny <- nfip[nfip$propertyState == "NY"]

nfip_canarsie <- nfip[nfip$reportedZipCode == "11249"]

nfip_tract

write_csv(nfip_ny, "nfip_ny.csv")
write_csv(nfip_canarsie, "nfip_canarsie.csv")

# nfip <- read.csv.sql("FimaNfipPolicies.csv",
#                      "select * from file
#                      where reportedZipCode eq 11249",
#                      nrows = 99999)
# 
# nfip <- read_csv("FimaNfipPolicies.csv", n_max = 100000)
# 
# f <- function(x, pos) subset(x, reportedZipCode == 11249)
# nfip1 <- read_csv_chunked("FimaNfipPolicies.csv", DataFrameCallback$new(f), chunk_size = 100)

nfip_head <- read_csv("FimaNfipPolicies.csv", n_max = 10)

nfip <- read_csv("https://www.fema.gov/about/reports-and-data/openfema/FimaNfipPolicies.csv", lazy = TRUE)

nfip_select <- nfip %>%
  select(censusTract, reportedZipCode, id, policyCost, policyCount, propertyState, policyEffectiveDate)

# nfip_select %>%
#   slice_sample(n = 100)

nfip_100 <- nfip_select %>%
  slice_sample(prop = 0.0001)
  

nfip_100 %>% count(propertyState)

nfip_filtered <- nfip_select[propertyState == "NY", ]

nfip_select %>%
  filter(propertyState == "NY")

# filter to the zip code that contains most of Canarsie (11249)


api_url <- "https://www.fema.gov/api/open/v2/FimaNfipPolicies"
api_url <- URLencode("https://www.fema.gov/api/open/v2/FimaNfipPolicies?filter=propertyState eq 'NY'")

result <- GET(api_url)

# set filters
filters <- list(match = "and",
                rules = data.frame(
                  field = "reportedZipCode",
                  operator = "equals",
                  value = "11249"
                ))

# Convert filters list to JSON and URL encode
filters_string <- toJSON(filters, auto_unbox = TRUE, pretty = TRUE)
api_url <-
  paste0(api_url,
         '?rows_per_page=1000&filters=',
         URLencode(filters_string))

# Send the GET request
result <- GET(
  api_url
)

# Retrieve the result
data <- fromJSON(content(result, as = "text"))

nfip <- data$FimaNfipPolicies

nfip %>% count(reportedZipCode)


# none of this code runs because it's way too big
properties <- read_csv("https://www.fema.gov/api/open/v2/FimaNfipPolicies.csv")

policies <- read_csv("https://www.fema.gov/about/reports-and-data/openfema/FimaNfipPolicies.csv")

bk %>% count(floodEvent) %>% mutate(pct = n/sum(n)) %>% arrange(desc(n))
