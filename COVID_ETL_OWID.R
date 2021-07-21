library(tidyverse)
library(httr)
library(rvest)
library(jsonlite)
library(lubridate)

################
####GET_OWID####
################

INPUT_OWID <- httr::GET("https://covid.ourworldindata.org/data/owid-covid-data.json") %>% 
  content(.,type="text") %>%
  jsonlite::fromJSON(.)

process_one_country_from_json <- function(country,code){
  #country <- INPUT_OWID$AFG
  
  df <- country$data
  
  df <- mutate(df,
               code = code,
               continent = country$continent, 
               location = country$location,
               population = country$population,
  )
  
  return(df)
}

OWID_DF <- map2(INPUT_OWID,names(INPUT_OWID),~process_one_country_from_json(.x,.y)) %>% reduce(bind_rows)

RBT <- filter(OWID_DF,population>5000000) %>%
  filter(.,!str_detect(code,"OWID"))

saveRDS(RBT,"RBT.Rdata")

#############################
####CREATIG_SUMMARY_TABLE####
#############################

SUMMARY_TABLE <- RBT %>%
  filter(.,max(ymd(RBT$date))-days(8) < ymd(date)) %>%
  arrange(.,location,date) %>%
  group_by(.,location) %>%
  summarise(.,
            Continent=last(continent,date),
            Population_mill=round(max(population)/1000000,1),
            #New_case_count_per_mill = last(new_cases_per_million,date),
            New_case_count_per_mill_smoothed = round(last(new_cases_smoothed_per_million,date),1),
            Weekly_change_of_new_cases_per_mill=round((last(new_cases_smoothed_per_million,date)-first(new_cases_smoothed_per_million,date)),1),
            Weekly_change_of_new_cases_percent=round((last(new_cases_smoothed_per_million,date)/first(new_cases_smoothed_per_million,date))-1,3),
            New_cases_timeline = list(new_cases_smoothed),
            #New_death_count_per_mill = last(new_deaths_per_million,date),
            New_death_count_per_mill_smoothed = round(last(new_deaths_smoothed_per_million,date),1),
            Weekly_change_of_new_deaths_per_mill=round((last(new_deaths_smoothed_per_million,date)-first(new_deaths_smoothed_per_million,date)),1),
            New_deaths_timeline = list(new_deaths_smoothed)
  ) 
# %>%
#   mutate_at(.,is.numeric,round,digits=1)

saveRDS(SUMMARY_TABLE,"SUMMARY_TABLE.Rdata")
saveRDS(max(RBT$date),"SUMMARY_TABLE_DATE.Rdata")

gc()