
library("geonames")
options(geonamesUsername = "aelhabr")

.get_geo_info <- function(city, country = "US", verbose = TRUE) {

  if(verbose) {
    msg <- sprintf("Trying to get data for %s, %s.", city, country)
    message(msg)
  }
  # city = "San Francisco"
  res <-
    geonames::GNsearch(name = city, country = country) %>%
    as_tibble() %>%
    arrange(desc(population)) %>% 
    slice(1) %>% 
    select(toponymName, adminCode1, countryCode, lat, lng, population) %>%
    rename(
      city = toponymName,
      state = adminCode1,
      country = countryCode,
      pop = population
    )
}

sf <-
  "San Francisco" %>% 
  .get_geo_info()

nfl_tm_city <-
  nfl_tm %>%
  # distinct(city) %>% 
  mutate(data = purrr::map(city, ~.get_geo_info(.x))) %>% 
  select(-city) %>% 
  unnest()
nfl_tm_city

nfl_tm_city_trim <-
  nfl_tm_city %>% 
  select(tm, city, state, country, lat, lng, pop)

nfl_tm_city_trim[nfl_tm_city_trim$tm == "SFO", c("lat")] <- sf$lat
nfl_tm_city_trim[nfl_tm_city_trim$tm == "SFO", c("lng")] <- sf$lng
nfl_tm_city_trim[nfl_tm_city_trim$tm == "SFO", c("pop")] <- sf$pop

nfl_tm_city_trim %>% teproj::export_path(config$path_tm_nfl_city)
