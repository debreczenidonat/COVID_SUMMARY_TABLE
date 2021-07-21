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

OWID_TEMP <- filter(OWID_DF,population>5000000) %>%
  filter(.,!str_detect(code,"OWID"))

GET_ISO_CONV_TABLE <- read.csv("https://gist.github.com/radcliff/f09c0f88344a7fcef373/raw/2753c482ad091c54b1822288ad2e4811c021d8ec/wikipedia-iso-country-codes.csv") %>%
  select(.,country_region_code=Alpha.2.code,code=Alpha.3.code)

OWID_TEMP <- left_join(OWID_TEMP,GET_ISO_CONV_TABLE)
################
####GET_MOBI####
################

if(dim(filter(OWID_TEMP,is.null(country_region_code)))[1]>0){
  stop("ISO_CONV_TABLE_ERROR!")
}

INPUT_MOBILITY <- read.csv("https://www.gstatic.com/covid19/mobility/Global_Mobility_Report.csv")
INPUT_MOBILITY <- filter(INPUT_MOBILITY,sub_region_1=="" & sub_region_2=="" & metro_area=="")
INPUT_MOBILITY <- select(INPUT_MOBILITY,
                          country_region_code,
                          date,
                          retail_and_recreation_percent_change_from_baseline,
                          grocery_and_pharmacy_percent_change_from_baseline,
                          parks_percent_change_from_baseline,
                          transit_stations_percent_change_from_baseline,
                          workplaces_percent_change_from_baseline,
                          residential_percent_change_from_baseline
                         )

RBT <- left_join(OWID_TEMP,INPUT_MOBILITY,by = c("date", "country_region_code"))

rm(INPUT_MOBILITY)
rm(INPUT_OWID)
gc()

write.table(RBT,"COVID_RBT.csv",sep = ",",na="",row.names = F)
#readRDS()
saveRDS(RBT,"RBT.Rdata")

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

#system("git remote add origin https://github.com/debreczenidonat/COVID_SUMMARY_TABLE.git")


# COVID_POPULATIONS <- distinct(RBT,code,location,population)
# 
# if( ! length(unique(RBT$code)) == dim(COVID_POPULATIONS)[1]){
#   stop("Duplicates in pop")
# }
# 
# write.table(COVID_POPULATIONS,"COVID_POP.csv",sep = ",",na="",row.names = F)
