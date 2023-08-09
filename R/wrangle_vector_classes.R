# Grab some new data for MUSA 7950 Bootcamp

library(tidyverse)
library(tidycensus)
library(sf)

census_api_key("e79f3706b6d61249968c6ce88794f6f556e5bf3d", overwrite = TRUE)

# 2020 Census data - blockgroups with population
# Turn that into blockgroups with only FID and a dbf
# Philly 311 Calls
# Philly council districts

# Do we want to create a new site selection analysis to replace Gettysburg?

acs_variable_list <- load_variables(2020, #year
                                         "acs5", #five year ACS estimates
                                         cache = TRUE)

# Philly 2020 blockgroups with population

## Set up a list of variables to grab
acs_vars <- c("B01001_001E", # ACS total Pop estimate
              "C02003_004E", # One race black
              "C02003_003E", # One race white
              "B03001_003E", # Hispanic (all)
              "B02001_005E", # AAPI
              "B11012_001E", # n Households
              "B08137_003E", # Renter hh
              "B08137_002E", # owner hh
              "B06011_001E") # Median income in past 12 months

## Grab some data for Phila County

acs2020 <- get_acs(geography = "tract",
                   year = 2020,
                   variables = acs_vars,
                   geometry = TRUE,
                   state = "PA",
                   county = "Philadelphia",
                   output = "wide") %>%
  select(GEOID, NAME, acs_vars) %>%
  rename(pop = B01001_001E,
         med_inc = B06011_001E,
         blk_tot = C02003_004E,
         wht_tot = C02003_003E,
         hsp_tot = B03001_003E,
         aapi_tot = B02001_005E,
         hhs = B11012_001E,
         renter_hh = B08137_003E,
         owner_hh = B08137_002E) %>%
  mutate(year = 2020,
         pct_wht = 100*(wht_tot/pop),
         pct_blk = 100*(blk_tot/pop),
         pct_hsp = 100*(hsp_tot/pop),
         pct_aapi = 100*(aapi_tot/pop),
         rent_pct = 100*(renter_hh / hhs)) %>%
  mutate(tract = str_sub(GEOID, start= -6))

# Write class 1 shp
st_write(acs2020, "C:/Users/Michael/Documents/Clients/MUSA_Teaching_and_Admin/MUSA_BootCamp_2023/Class_1/philadelphia_tracts_2020.shp")

# Write class 2 dbf
write.table(acs2020 %>% 
              as.data.frame %>%
              select(-geometry) %>%
              mutate(GEOID = as.numeric(GEOID)),
              "C:/Users/Michael/Documents/Clients/MUSA_Teaching_and_Admin/MUSA_BootCamp_2023/Class_2/philadelphia_tracts_2020.dbf")

# Write class 2 shp - e.g. class 1 with nothing but the GEOID and tract name

st_write(acs2020 %>%
           select(tract, GEOID, NAME), 
         "C:/Users/Michael/Documents/Clients/MUSA_Teaching_and_Admin/MUSA_BootCamp_2023/Class_2/philadelphia_tracts_no_data.shp")

# Class 3

# Grab Philly 311 complaints

philly_311 <- read.csv("https://phl.carto.com/api/v2/sql?filename=public_cases_fc&format=csv&skipfields=cartodb_id,the_geom,the_geom_webmercator&q=SELECT%20*%20FROM%20public_cases_fc%20WHERE%20requested_datetime%20%3E=%20%272023-01-01%27%20AND%20requested_datetime%20%3C%20%272023-03-01%27")

# Filter out NA lat/lon and filter for specific type

philly_311_clean <- philly_311 %>%
  filter(is.na(lat) == FALSE) %>%
  filter(subject == "Illegal Dumping") %>%
  mutate(incident = 1) %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
  st_transform(crs = 2272)

st_write(philly_311_clean, "C:/Users/Michael/Documents/Clients/MUSA_Teaching_and_Admin/MUSA_BootCamp_2023/Class_3/philly_311.shp")


# Grab Philly council districts

councilDistricts <- st_read("https://opendata.arcgis.com/datasets/9298c2f3fa3241fbb176ff1e84d33360_0.geojson") %>%
  st_transform(crs = 2272) 

st_write(councilDistricts, "C:/Users/Michael/Documents/Clients/MUSA_Teaching_and_Admin/MUSA_BootCamp_2023/Class_3/council_dist.shp")


# Write out some acs data - with and without a shape

# Write class 2 dbf
write.table(acs2020 %>% 
              as.data.frame %>%
              select(-geometry),
            "C:/Users/Michael/Documents/Clients/MUSA_Teaching_and_Admin/MUSA_BootCamp_2023/Class_3/philadelphia_tracts_2020.dbf")

# Write class 2 shp - e.g. class 1 with nothing but the GEOID and tract name

st_write(acs2020 %>%
           select(tract, GEOID, NAME), 
         "C:/Users/Michael/Documents/Clients/MUSA_Teaching_and_Admin/MUSA_BootCamp_2023/Class_3/philadelphia_tracts_no_data.shp")

