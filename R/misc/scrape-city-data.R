
options(geonamesUsername = "aelhabr")
library("geonames")
.get_geo_info <- function(city, country = "US", verbose = TRUE) {

  if(verbose) {
    msg <- sprintf("Trying to get data for %s, %s.", city, country)
    message(msg)
  }
  
  res <-
    geonames::GNsearch(name_equals = city, countryCode = country) %>%
    as_tibble() %>%
    # arrange(desc(population)) %>% 
    select(toponymName, adminCode1, countryCode, lat, lng, population) %>%
    rename(
      city = toponymName,
      state = adminCode1,
      country = countryCode,
      pop = population
    ) %>%
    slice(1) 
}


nfl_tm_city <-
  nfl_tm %>%
  distinct(city) %>% 
  mutate(data = purrr::map(city, ~.get_geo_info(.x))) %>% 
  select(-city) %>% 
  unnest()
nfl_tm_city

nfl_tm_city %>% teproj::export_path(config$path_tm_city)
