
library(dplyr)
library(jsonlite)
library(purrr)
library(janitor)
library(readr)
library(tidyr)
library(stringr)

state_strings <- c("alaska", "texas", "minnesota", "michigan", "west-virginia",
  "virginia", "wisconsin", "kentucky", "louisiana", "mississippi",
  "missouri", "north-carolina", "california", "iowa", "maine",
  "florida", "washington", "illinois", "north-dakota", "maryland",
  "georgia", "tennessee", "new-york", "arkansas", "oklahoma", "nebraska",
  "south-carolina", "idaho", "new-hampshire", "ohio", "south-dakota",
  "vermont", "indiana", "pennsylvania", "montana", "kansas", "oregon",
  "arizona", "alabama", "new-jersey", "hawaii", "massachusetts",
  "nevada", "new-mexico", "colorado", "rhode-island", "wyoming",
  "connecticut", "utah", "delaware", "district-of-columbia")


get_nyt_data <- function(x, time_start, race) {
  
  if(race == "special"){
    url_string <- "senate/0/special.json"
    folder_1 <- "senate"
    folder_2 <- "senate/special/states"
  } else if (race == "presidential"){
    url_string <- "president.json"
    folder_1 <- "presidential"
    folder_2 <- "presidential/states"    
  } else if (race == "senate"){
    url_string <- "senate.json"
    folder_1 <- "senate"
    folder_2 <- "senate/states"    
  }
  
  print(x)
  
  Sys.sleep(2)
  
  time <- Sys.time()
  
  json_url <- glue::glue("https://static01.nyt.com/elections-assets/2020/data/api/2020-11-03/race-page/{x}/{url_string}")
  
  res <- jsonlite::fromJSON(json_url)
  
  if(!dir.exists(glue::glue("data/{time_start}/{folder_2}"))){
    dir.create(glue::glue("data/{time_start}/{folder_2}/json"), recursive = T)
    dir.create(glue::glue("data/{time_start}/{folder_2}/csv"), recursive = T)
  }
  
  jsonlite::write_json(res, path = glue::glue("data/{time_start}/{folder_2}/json/{x}.json"))
  
  cleaned <- res[["data"]][["races"]][["counties"]][[1]]  %>%
    rowwise() %>%
    mutate(results = list(as.list(results)),
           results_absentee = list(as.list(results_absentee)),
           state = x,
           retrieved_time = time) %>%
    tidyr::unnest_wider(results, names_sep = "_")  %>%
    tidyr::unnest_wider(results_absentee, names_sep = "_")  %>%
    janitor::clean_names()
  
  data.table::fwrite(cleaned, file = glue::glue("data/{time_start}/{folder_2}/csv/{x}.csv"))
  
  return(cleaned)
  
}

get_nyt_data <- possibly(get_nyt_data, otherwise = NULL, quiet = F)


time_start <- Sys.time() %>% as.character() %>% str_replace_all(":", "-")


###### --- Presidential ###########

election_results <- state_strings %>%
  map_dfr(~{get_nyt_data(.x, time_start, "presidential")})


data.table::fwrite(election_results, file = glue::glue("data/{time_start}/presidential/presidential.csv"))

presidential_json <- read_json("https://static01.nyt.com/elections-assets/2020/data/api/2020-11-03/national-map-page/national/president.json")

jsonlite::write_json(presidential_json, path = glue::glue("data/{time_start}/presidential/presidential.json"))


###### --- Senate Special ###########


election_results <- c("georgia", "arizona") %>%
  map_dfr(~{get_nyt_data(.x, time_start, "special")})


data.table::fwrite(election_results, file = glue::glue("data/{time_start}/senate/special/special.csv"))

###### --- Senate ###########

senate_state_strings <- c("alabama", "alaska", "arkansas", "colorado", "delaware", "georgia", 
                    "idaho", "illinois", "iowa", "kansas", "kentucky", "louisiana", 
                    "maine", "massachusetts", "michigan", "minnesota", "mississippi", 
                    "montana", "nebraska", "new-hampshire", "new-jersey", "new-mexico", 
                    "north-carolina", "oklahoma", "oregon", "rhode-island", "south-carolina", 
                    "south-dakota", "tennessee", "texas", "virginia", "west-virginia", 
                    "wyoming")

election_results <- senate_state_strings %>%
  map_dfr(~{get_nyt_data(.x, time_start, "senate")})


data.table::fwrite(election_results, file = glue::glue("data/{time_start}/senate/senate.csv"))

senate_json <- read_json("https://static01.nyt.com/elections-assets/2020/data/api/2020-11-03/national-map-page/national/senate.json")

jsonlite::write_json(senate_json, path = glue::glue("data/{time_start}/senate/senate.json"))


###### --- House ###########



house_json <- read_json("https://static01.nyt.com/elections-assets/2020/data/api/2020-11-03/national-map-page/national/house.json")


if(!dir.exists(glue::glue("data/{time_start}/house"))){
  dir.create(glue::glue("data/{time_start}/house/races", recursive = T))
}

jsonlite::write_json(house_json, path = glue::glue("data/{time_start}/house/house.json"))

get_house_data <- function(x) {
  x %>% .[["counties"]] %>% 
    map_dfr(~{
      .x %>% keep( ~ !is.null(.)) %>% 
        as.data.frame() %>% as_tibble() 
    }) %>%
    bind_cols(x %>% keep(~length(.x)==1) %>% as_tibble() %>% set_names(paste0(names(.),"_race")))  %>% 
    janitor::clean_names() %>% 
    select_if(Negate(is.list))
}

get_house_data <- possibly(get_house_data, otherwise = NULL, quiet = F)

house_dat <- house_json[["data"]][["races"]] %>%
  map_dfr(~{
    race <- get_house_data(.x)
    
    
    if(nrow(race)!=0){
      
      print(unique(race$race_slug_race))
      
      data.table::fwrite(race, file = glue::glue("data/{time_start}/house/races/{unique(race$race_slug_race)}.csv"))
      
      return(race)      
    }

    
  }) 

data.table::fwrite(house_dat, file = glue::glue("data/{time_start}/house/house.csv"))
