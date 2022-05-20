library(easypackages)

libraries(c("readxl", "readr", "plyr", "dplyr", "ggplot2", "png", "tidyverse", "scales", 'zoo', 'stats',"rgdal", 'rgeos', "tmaptools", 'sp', 'sf', 'maptools', 'leaflet', 'leaflet.extras', 'spdplyr', 'geojsonio', 'rmapshaper', 'jsonlite', 'grid', 'aweek', 'xml2', 'rvest', 'officer', 'flextable', 'viridis', 'epitools', 'directlabels', 'DT'))

options(scipen = 999)

capwords = function(s, strict = FALSE) {
  cap = function(s) paste(toupper(substring(s, 1, 1)),
                          {s = substring(s, 2); if(strict) tolower(s) else s},sep = "", collapse = " " )
  sapply(strsplit(s, split = " "), cap, USE.NAMES = !is.null(names(s)))}

ph_theme = function(){
  theme(
    plot.title = element_text(colour = "#000000", face = "bold", size = 10),
    plot.subtitle = element_text(colour = "#000000", size = 10),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.background = element_rect(fill = "#FFFFFF"),
    panel.grid.major.y = element_line(colour = "#E7E7E7", size = .3),
    panel.grid.minor.y = element_blank(),
    strip.text = element_text(colour = "#000000", size = 10, face = "bold"),
    strip.background = element_blank(),
    axis.ticks = element_line(colour = "#dbdbdb"),
    legend.position = "bottom",
    legend.title = element_text(colour = "#000000", size = 9, face = "bold"),
    legend.background = element_rect(fill = "#ffffff"),
    legend.key = element_rect(fill = "#ffffff", colour = "#ffffff"),
    legend.text = element_text(colour = "#000000", size = 9),
    axis.text.y = element_text(colour = "#000000", size = 8),
    axis.text.x = element_text(colour = "#000000", angle = 0, hjust = 1, vjust = .5, size = 8),
    axis.title =  element_text(colour = "#000000", size = 9, face = "bold"),
    axis.line = element_line(colour = "#dbdbdb")
  )
}

# github_repo_dir <- "~/Documents/GitHub/wsx_covid_public_mobile_site"
github_repo_dir <- "~/GitHub/wsx_covid_public_mobile_site"
# github_repo_dir <- '~/wsx_covid_public_mobile_site'
output_directory_x <- paste0(github_repo_dir, '/Outputs')

#list.files(output_directory_x)
# 2020 MYE
# Cornwall and Isles of Scilly" "Hackney and City of London"  

mye_total <- read_csv('https://www.nomisweb.co.uk/api/v01/dataset/NM_2002_1.data.csv?geography=2092957699,2013265928,1820327937...1820328318,1816133633...1816133848&date=latest&gender=0&c_age=200&measures=20100&select=date_name,geography_name,geography_type,geography_code,obs_value') %>%
  rename(Population = OBS_VALUE,
         Code = GEOGRAPHY_CODE,
         Name = GEOGRAPHY_NAME,
         Type = GEOGRAPHY_TYPE) %>%
  select(-DATE_NAME) %>%
  unique() %>%
  group_by(Name, Code) %>%
  mutate(Count = n()) %>%
  mutate(Type = ifelse(Count == 2, 'Unitary Authority', ifelse(Type == 'local authorities: county / unitary (as of April 2019)', 'Upper Tier Local Authority', ifelse(Type == 'local authorities: district / unitary (as of April 2019)', 'Lower Tier Local Authority', ifelse(Type == 'regions', 'Region', ifelse(Type == 'countries', 'Country', Type)))))) %>%
  ungroup() %>%
  select(-Count) %>%
  unique()

mye_total <- mye_total %>% 
  mutate(Name = ifelse(Name %in% c('Hackney', 'City of London'), 'Hackney and City of London', Name)) %>% 
  mutate(Code = ifelse(Code %in% c('E09000012', 'E09000001'), 'E09000012', Code)) %>% 
  group_by(Type, Name, Code) %>% 
  summarise(Population = sum(Population, na.rm = TRUE)) %>% 
  ungroup()

if(exists('mye_total') == FALSE) {
  mye_total <- read_csv(paste0(github_repo_dir,'/Source_files/mye2020_ltla.csv'))
}

mye_total %>%
  write.csv(., paste0(github_repo_dir,'/Source_files/mye2020_ltla.csv'), row.names = FALSE)

area_code_names <- mye_total %>%
  select(Code, Name)

mye_total <- mye_total %>%
  select(-Name)

# Cases ####
#
# daily_cases <- read_csv('https://coronavirus.data.gov.uk/downloads/csv/coronavirus-cases_latest.csv') %>%
#   rename(Name = `Area name`) %>%
#   rename(Code = `Area code`) %>%
#   rename(Date = `Specimen date`) %>%
#   rename(New_cases = `Daily lab-confirmed cases`) %>%
#   rename(Cumulative_cases = `Cumulative lab-confirmed cases`) %>%
#   arrange(Name, Date) %>%
#   select(Name, Code, `Area type`, Date, New_cases, Cumulative_cases) %>%
#   group_by(Name, Code, Date) %>%
#   mutate(Count = n()) %>%
#   filter(!(`Area type` == 'ltla' & Count == 2)) %>%
#   select(-c(`Area type`, Count)) %>%
#   left_join(mye_total, by = 'Code') %>%
#   ungroup()

daily_cases_ltla <- read_csv('https://api.coronavirus.data.gov.uk/v2/data?areaType=ltla&metric=newCasesBySpecimenDate&format=csv') %>%
  filter(substr(areaCode, 1,1) == 'E') %>%
  select(-areaType) %>%
  rename(Name = areaName,
         Code = areaCode,
         Date = date,
         New_cases = newCasesBySpecimenDate) %>%
  left_join(mye_total, by = 'Code') %>%
  mutate(New_cases = replace_na(New_cases, 0)) %>%
  group_by(Name) %>%
  arrange(Name, Date) %>%
  mutate(Cumulative_cases = cumsum(New_cases))

daily_cases_utla <- read_csv('https://api.coronavirus.data.gov.uk/v2/data?areaType=utla&metric=newCasesBySpecimenDate&format=csv') %>%
  filter(substr(areaCode, 1,1) == 'E') %>%
  select(-areaType) %>%
  rename(Name = areaName,
         Code = areaCode,
         Date = date,
         New_cases = newCasesBySpecimenDate) %>%
  left_join(mye_total, by = 'Code') %>%
  mutate(New_cases = replace_na(New_cases, 0)) %>%
  group_by(Name) %>%
  arrange(Name, Date) %>%
  mutate(Cumulative_cases = cumsum(New_cases))

daily_cases_nation <- read_csv('https://api.coronavirus.data.gov.uk/v2/data?areaType=nation&metric=newCasesBySpecimenDate&format=csv') %>%
  filter(substr(areaCode, 1,1) == 'E') %>%
  select(-areaType) %>%
  rename(Name = areaName,
         Code = areaCode,
         Date = date,
         New_cases = newCasesBySpecimenDate) %>%
  left_join(mye_total, by = 'Code') %>%
  mutate(New_cases = replace_na(New_cases, 0)) %>%
  group_by(Name) %>%
  arrange(Name, Date) %>%
  mutate(Cumulative_cases = cumsum(New_cases))

daily_cases_region <- read_csv('https://api.coronavirus.data.gov.uk/v2/data?areaType=region&metric=newCasesBySpecimenDate&format=csv') %>%
  filter(substr(areaCode, 1,1) == 'E') %>%
  select(-areaType) %>%
  rename(Name = areaName,
         Code = areaCode,
         Date = date,
         New_cases = newCasesBySpecimenDate) %>%
  left_join(mye_total, by = 'Code') %>%
  mutate(New_cases = replace_na(New_cases, 0)) %>%
  group_by(Name) %>%
  arrange(Name, Date) %>%
  mutate(Cumulative_cases = cumsum(New_cases))

p12_test_df <- daily_cases_ltla %>%
  bind_rows(daily_cases_utla) %>%
  bind_rows(daily_cases_region) %>%
  bind_rows(daily_cases_nation) %>%
  filter(substr(Code, 1,1) == 'E') %>%
  unique()

# query_filters <- c(
#   # "areaType=utla"
#   'areaName=West Sussex'
# )
#
# query_structure <- list(
#   date = "date",
#   name = "areaName",
#   code = "areaCode",
#   daily = "newCasesBySpecimenDate",
#   cumulative = "cumCasesBySpecimenDate"
# )
#
# last_date <- as.Date(last_update(filters = query_filters, structure = query_structure))
# # last_date <- as.Date('2021-04-19')
last_date <- max(p12_test_df$Date) + 1

# PHE say the last four data points are incomplete (perhaps they should not publish them). Instead, we need to make sure we account for this so that it is not misinterpreted.
complete_date <- last_date - 5

first_date <- min(p12_test_df$Date)
last_case_date <- p12_test_df %>%
  filter(New_cases != 0)

last_case_date <- max(last_case_date$Date)

Areas = p12_test_df %>%
  select(Name, Code, Type) %>%
  unique()

Dates = seq.Date(first_date, last_case_date, by = '1 day')

daily_cases_reworked <- data.frame(Name = rep(Areas$Name, length(Dates)), Code = rep(Areas$Code, length(Dates)), Type = rep(Areas$Type, length(Dates)), check.names = FALSE) %>%
  arrange(Name) %>%
  group_by(Name) %>%
  mutate(Date = seq.Date(first_date, last_case_date, by = '1 day'))

p12_test_df <- daily_cases_reworked %>%
  left_join(p12_test_df, by = c('Name', 'Code', 'Type', 'Date')) %>%
  mutate(Period = format(Date, '%d %B %Y')) %>%
  mutate(Data_completeness = ifelse(Date > complete_date, 'Considered incomplete', 'Complete')) %>%
  mutate(Cumulative_per_100000 = (Cumulative_cases / Population) * 100000) %>%
  mutate(New_cases_per_100000 = (New_cases / Population) * 100000) %>%
  ungroup() %>%
  mutate(Name = ifelse(Name == 'South East', 'South East region', Name))  %>%
  group_by(Name) %>%
  arrange(Name, Date) %>%
  mutate(Rolling_7_day_new_cases = rollapply(New_cases, 7, sum, align = 'right', fill = NA)) %>%
  mutate(Rolling_7_day_new_cases_per_100000 = ifelse(is.na(Rolling_7_day_new_cases), NA, (Rolling_7_day_new_cases / Population) * 100000)) %>%
  mutate(Perc_change_on_rolling_7_days_actual = round((Rolling_7_day_new_cases - lag(Rolling_7_day_new_cases, 7))/ lag(Rolling_7_day_new_cases, 7), 2))  %>%
  mutate(Perc_change_on_rolling_7_days_actual = ifelse(Perc_change_on_rolling_7_days_actual == Inf, 1, Perc_change_on_rolling_7_days_actual)) %>%
  mutate(Perc_change_on_rolling_7_days_actual = replace_na(Perc_change_on_rolling_7_days_actual, 0)) %>%
  mutate(Rolling_7_day_average_new_cases = rollapply(New_cases, 7, mean, align = 'right', fill = NA)) %>%
  mutate(Previous_7_days_sum = lag(Rolling_7_day_new_cases, 7)) %>%
  ungroup()

data.frame(Item = 'latest_daily_case', Label = paste0(format(last_case_date, '%A '), ordinal(as.numeric(format(last_case_date, '%d'))), format(last_case_date, ' %B %Y'))) %>%
  add_row(Item = 'daily_case_update_date',Label = paste0(format(last_date, '%A '), ordinal(as.numeric(format(last_date, '%d'))), format(last_date, ' %B %Y'))) %>%
  add_row(Item = 'complete_date', Label = paste0(format(complete_date, '%A '), ordinal(as.numeric(format(complete_date, '%d'))), format(complete_date, ' %B %Y')))%>%
  add_row(Item = 'first_case_period', Label =  format(first_date, '%d %B %Y')) %>%
  add_row(Item = 'last_case_period', Label =  format(last_case_date, '%d %B %Y')) %>%
  add_row(Item = 'previous_week_period', Label =  format(complete_date -7, '%A %d %B %Y')) %>%
  add_row(Item = 'complete_date_actual', Label = format(complete_date, '%d/%m/%y')) %>%
  toJSON() %>%
  write_lines(paste0(output_directory_x, '/case_dates.json'))

Areas <- c('Adur', 'Arun', 'Chichester', 'Crawley', 'Horsham', 'Mid Sussex', 'Worthing', 'West Sussex', 'South East region', 'England')

p12_test_summary_1 <- p12_test_df %>%
  filter(Name %in% Areas) %>%
  filter(Date == last_case_date) %>%
  select(Name, Date, Cumulative_cases, Cumulative_per_100000) %>%
  rename(Cumulative_date = Date)

p12_test_summary_2 <- p12_test_df %>%
  filter(Name %in% Areas) %>%
  filter(Date %in% c(complete_date)) %>%
  select(Name, Date, New_cases, New_cases_per_100000, Rolling_7_day_new_cases, Rolling_7_day_new_cases_per_100000, Rolling_7_day_average_new_cases, Perc_change_on_rolling_7_days_actual) %>%
  mutate(Change_direction = ifelse(Perc_change_on_rolling_7_days_actual <0, 'Down', ifelse(Perc_change_on_rolling_7_days_actual == 0, 'Same', ifelse(Perc_change_on_rolling_7_days_actual > 0, 'Up', NA)))) %>%   rename(Rate_date = Date)

p12_test_summary <- p12_test_summary_1 %>%
  left_join(p12_test_summary_2, by = 'Name')

# p12_test_df %>%
#   filter(Name == 'Arun') %>%
#   write.csv(., paste0(output_directory_x, '/Arun_ltla.csv'),  row.names = FALSE)

# Cumulative to most recent case date, new cases and rolling 7 day incidence up to complete date only.

rm(p12_test_summary_1, p12_test_summary_2)

# Age specific cases ####

mye_ages <- read_csv('https://www.nomisweb.co.uk/api/v01/dataset/NM_2002_1.data.csv?geography=2092957699,2013265921...2013265932,1816133633...1816133848,1820327937...1820328318&date=2020&gender=0&c_age=1,3...18,210&measures=20100&select=geography_name,geography_code,c_age_name,obs_value,geography_type') %>%
  rename(Population = OBS_VALUE,
         Code = GEOGRAPHY_CODE,
         Name = GEOGRAPHY_NAME,
         Type = GEOGRAPHY_TYPE,
         Age = C_AGE_NAME) %>%
  group_by(Code, Name, Age) %>%
  mutate(Count = n()) %>%
  unique() %>%
  mutate(Type = ifelse(Count == 2, 'Unitary Authoritory', ifelse(Type == 'local authorities: county / unitary (as of April 2019)', 'Upper Tier Local Authority', ifelse(Type == 'local authorities: district / unitary (as of April 2019)', 'Lower Tier Local Authority', ifelse(Type == 'regions', 'Region', ifelse(Type == 'countries', 'Country', Type)))))) %>%
  group_by(Name, Code) %>%
  ungroup() %>%
  select(-Count) %>%
  unique() %>%
  mutate(Age = gsub('Aged ', '', Age)) %>%
  mutate(Age = gsub('Age', '', Age)) %>%
  mutate(Age = gsub(' 0 - ', '0-', Age)) %>%
  mutate(Age = paste0(Age, ' years')) %>%
  mutate(Age = ifelse(Age %in% c('80-84 years', '85+ years'), '80+ years', Age)) %>%
  group_by(Name, Code, Age, Type) %>%
  summarise(Population = sum(Population, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(Name = ifelse(Name == 'South East', 'South East region', Name))

mye_ages <- mye_ages %>% 
  mutate(Name = ifelse(Name %in% c('Hackney', 'City of London'), 'Hackney and City of London', Name)) %>% 
  mutate(Code = ifelse(Code %in% c('E09000012', 'E09000001'), 'E09000012', Code)) %>% 
  group_by(Type, Name, Code, Age) %>% 
  summarise(Population = sum(Population, na.rm = TRUE)) %>% 
  ungroup()

if(exists('mye_ages') == FALSE) {
  mye_ages <- read_csv(paste0(github_repo_dir,'/Source_files/mye_ages.csv'))
}

mye_ages %>%
  write.csv(., paste0(github_repo_dir,'/Source_files/mye_ages.csv'), row.names = FALSE)

# age_spec_ltla <- read_csv('https://api.coronavirus.data.gov.uk/v2/data?areaType=ltla&metric=newCasesBySpecimenDateAgeDemographics&format=csv')

# &metric=newVirusTests&metric=uniqueCasePositivityBySpecimenDateRollingSum
library(showtext)
library(httr)

lads <- c("E07000223", "E07000224","E07000225", "E07000226", "E07000227", "E07000228","E07000229")

## build the required structure for the api
# {"date":"date","areaCode":"areaCode","areaName":"areaName","newCasesBySpecimenDateRollingRate":"newCasesBySpecimenDateRollingRate","newCasesBySpecimenDateRollingSum":"newCasesBySpecimenDateRollingSum","uniqueCasePositivityBySpecimenDateRollingSum":"uniqueCasePositivityBySpecimenDateRollingSum","uniquePeopleTestedBySpecimenDateRollingSum":"uniquePeopleTestedBySpecimenDateRollingSum"}

###### read the data
england <- 'https://api.coronavirus.data.gov.uk/v1/data?filters=areaType=nation;areaName=england&structure={"date":"date","areaCode":"areaCode","areaName":"areaName","newCasesBySpecimenDateAgeDemographics":"newCasesBySpecimenDateAgeDemographics"}'

westsussex <- 'https://api.coronavirus.data.gov.uk/v1/data?filters=areaType=utla;areaCode=E10000032&structure={"date":"date","areaCode":"areaCode","areaName":"areaName","newCasesBySpecimenDateAgeDemographics":"newCasesBySpecimenDateAgeDemographics"}'

southeast <- 'https://api.coronavirus.data.gov.uk/v1/data?filters=areaType=region;areaCode=E12000008&structure={"date":"date","areaCode":"areaCode","areaName":"areaName","newCasesBySpecimenDateAgeDemographics":"newCasesBySpecimenDateAgeDemographics"}'

# create api calls for lads:
baseurl <- 'https://api.coronavirus.data.gov.uk/v1/data?filters=areaType=ltla;areaCode=HEREPLEASE&structure={"date":"date","areaCode":"areaCode","areaName":"areaName","newCasesBySpecimenDateAgeDemographics":"newCasesBySpecimenDateAgeDemographics"}'

ltlas <- character()

for(i in lads) {
  new_string <- sub(pattern = "HEREPLEASE", replacement = i, x = baseurl)
  ltlas <- c(ltlas, new_string)
}

# list of apis
urls <- c(england, southeast, westsussex, ltlas)

# empty list
dflist <- list()

# api calls for each geography into list
for(i in urls) {
  response <- httr::GET(url = i)

  if (response$status_code >= 400) {
    err_msg = httr::http_status(response)
    stop(err_msg)
  }
  json_text <- content(response, "text")
  data <- jsonlite::fromJSON(json_text)

  df <- as.data.frame(data$data)
  df$apisource <- i

  dflist[[i]] <- df
}

# bind together (unnest)
age_spec <- bind_rows(dflist) %>%
  unnest(newCasesBySpecimenDateAgeDemographics) %>%
  mutate(Age = ifelse(age == 'unassigned', 'Unknown', paste0(age, ' years'))) %>%
  mutate(Age = gsub('_', '-', Age)) %>%
  mutate(Age = ifelse(Age %in% c('80-84 years', '85-89 years', '90+ years'), '80+ years', Age)) %>%
  mutate(Age = ifelse(Age == '00-04 years', '0-4 years', ifelse(Age == '05-09 years', '5-9 years', Age))) %>%
  rename(Name = areaName,
         Code = areaCode,
         Cases = cases,
         Date = date) %>%
  group_by(Name, Age, Date) %>%
  summarise(Cases = sum(Cases, na.rm = TRUE)) %>%
  ungroup() %>%
  filter(Age != 'Unknown') %>%
  mutate(Age = factor(Age, levels = c('0-4 years', '5-9 years', '10-14 years', '15-19 years', '20-24 years' , '25-29 years', '30-34 years', '35-39 years', '40-44 years', '45-49 years', '50-54 years' ,'55-59 years', '60-64 years' , '65-69 years', '70-74 years', '75-79 years', '80+ years'))) %>%
  mutate(Date = as.Date(Date)) %>%
  mutate(Name = ifelse(Name == 'South East', 'South East region', Name))

# age_spec <- read_csv('https://coronavirus.data.gov.uk/downloads/demographic/cases/specimenDate_ageDemographic-unstacked.csv') %>%
#   filter(areaName %in% c('Adur', 'Arun', 'Chichester', 'Crawley', 'Horsham', 'Mid Sussex', 'Worthing', 'West Sussex', 'South East', 'England')) %>%
#   mutate(areaName = ifelse(areaName == 'South East', 'South East region', areaName)) %>%
#   select(areaCode, areaName, areaType, date, `newCasesBySpecimenDate-0_4`,`newCasesBySpecimenDate-5_9`,`newCasesBySpecimenDate-10_14`,`newCasesBySpecimenDate-15_19`,`newCasesBySpecimenDate-20_24`,`newCasesBySpecimenDate-25_29`,`newCasesBySpecimenDate-30_34`,`newCasesBySpecimenDate-35_39`,`newCasesBySpecimenDate-40_44`,`newCasesBySpecimenDate-45_49`,`newCasesBySpecimenDate-50_54`,`newCasesBySpecimenDate-55_59`,`newCasesBySpecimenDate-60_64`,`newCasesBySpecimenDate-65_69`,`newCasesBySpecimenDate-70_74`,`newCasesBySpecimenDate-75_79`,`newCasesBySpecimenDate-80_84`,`newCasesBySpecimenDate-85_89`,`newCasesBySpecimenDate-90+`,`newCasesBySpecimenDate-unassigned`) %>%
#   pivot_longer(cols = c(`newCasesBySpecimenDate-0_4`,`newCasesBySpecimenDate-5_9`,`newCasesBySpecimenDate-10_14`,`newCasesBySpecimenDate-15_19`,`newCasesBySpecimenDate-20_24`,`newCasesBySpecimenDate-25_29`,`newCasesBySpecimenDate-30_34`,`newCasesBySpecimenDate-35_39`,`newCasesBySpecimenDate-40_44`,`newCasesBySpecimenDate-45_49`,`newCasesBySpecimenDate-50_54`,`newCasesBySpecimenDate-55_59`,`newCasesBySpecimenDate-60_64`,`newCasesBySpecimenDate-65_69`,`newCasesBySpecimenDate-70_74`,`newCasesBySpecimenDate-75_79`,`newCasesBySpecimenDate-80_84`,`newCasesBySpecimenDate-85_89`,`newCasesBySpecimenDate-90+`,`newCasesBySpecimenDate-unassigned`),
#                names_to = 'Age') %>%
#   mutate(Age = gsub('newCasesBySpecimenDate-', '', Age)) %>%
#   mutate(Age = ifelse(Age == 'unassigned', 'Unknown', paste0(Age, ' years'))) %>%
#   mutate(Age = gsub('_', '-', Age)) %>%
#   mutate(Age = ifelse(Age %in% c('80-84 years', '85-89 years', '90+ years'), '80+ years', Age)) %>%
#   filter(areaType != 'overview') %>%
#   mutate(areaType = ifelse(areaType == 'ltla', 'Lower Tier Local Authority', ifelse(areaType == 'utla', 'Upper Tier Local Authority', ifelse(areaType == 'region', 'Region', ifelse(areaType == 'nation' , 'Nation', NA))))) %>%
#   rename(Name = areaName,
#          Code = areaCode,
#          Type = areaType,
#          Cases = value,
#          Date = date) %>%
#   group_by(Name, Age, Date) %>%
#   summarise(Cases = sum(Cases, na.rm = TRUE))

Ages <- data.frame(Age = c('0-4 years', '5-9 years', '10-14 years', '15-19 years', '20-24 years' , '25-29 years', '30-34 years', '35-39 years', '40-44 years', '45-49 years', '50-54 years' ,'55-59 years', '60-64 years' , '65-69 years', '70-74 years', '75-79 years', '80+ years'))
Dates <- seq.Date(min(age_spec$Date), max(age_spec$Date), by = '1 day')

age_df_daily_combined <- data.frame(Name = character(), Age = character(), Date = character())

for(i in 1:length(Areas)){

  area_x = Areas[i]

  df_x <- data.frame(Age = rep(Ages$Age, length(Dates))) %>%
    arrange(Age) %>%
    group_by(Age) %>%
    mutate(Date = seq.Date(min(age_spec$Date), max(age_spec$Date), by = '1 day')) %>%
    mutate(Name = area_x) %>%
    mutate(Date = as.character(Date)) %>%
    ungroup()

  age_df_daily_combined <- age_df_daily_combined %>%
    bind_rows(df_x)

}

case_age_df_daily <- age_df_daily_combined %>%
  mutate(Date = as.Date(Date)) %>%
  left_join(age_spec, by = c('Name', 'Date', 'Age')) %>%
  mutate(Cases = replace_na(Cases, 0)) %>%
  group_by(Name, Age) %>%
  arrange(Name, Age, Date) %>%
  ungroup() %>%
  left_join(mye_ages, by = c('Name', 'Age')) %>%
  group_by(Name, Age) %>%
  arrange(Name, Age, Date) %>%
  mutate(Cumulative_cases = cumsum(Cases)) %>%
  mutate(Rolling_7_day_new_cases = rollapply(Cases, 7, sum, align = 'right', fill = NA, partial = TRUE)) %>%
  mutate(Rolling_7_day_new_cases = replace_na(Rolling_7_day_new_cases, 0)) %>%
  mutate(Perc_change_on_rolling_7_days_actual = round((Rolling_7_day_new_cases - lag(Rolling_7_day_new_cases, 7))/ lag(Rolling_7_day_new_cases, 7), 2)) %>%
  mutate(Perc_change_on_rolling_7_days_actual = ifelse(Perc_change_on_rolling_7_days_actual == Inf, 1, Perc_change_on_rolling_7_days_actual)) %>%
  mutate(Perc_change_on_rolling_7_days_actual = replace_na(Perc_change_on_rolling_7_days_actual, 0)) %>%
  mutate(Rolling_7_day_average_new_cases = rollapply(Cases, 7, mean, align = 'right', fill = NA)) %>%
  ungroup() %>%
  mutate(ASR = pois.exact(Rolling_7_day_new_cases, Population)[[3]]*100000)

age_spec_10 <- case_age_df_daily %>%
  mutate(Age = ifelse(Age %in% c('0-4 years', '5-9 years'), '0-9 years', ifelse(Age %in% c('10-14 years', '15-19 years'), '10-19 years',ifelse(Age %in% c('20-24 years', '25-29 years'), '20-29 years',ifelse(Age %in% c('30-34 years', '35-39 years'), '30-39 years',ifelse(Age %in% c('40-44 years', '45-49 years'), '40-49 years',ifelse(Age %in% c('50-54 years', '55-59 years'), '50-59 years',ifelse(Age %in% c('60-64 years', '65-69 years'), '60-69 years',ifelse(Age %in% c('70-74 years', '75-79 years'), '70-79 years', '80+ years'))))))))) %>%
  group_by(Name, Age, Date) %>%
  summarise(Cases = sum(Cases, na.rm = TRUE),
            Population = sum(Population, na.rm = TRUE)) %>%
  group_by(Name, Age) %>%
  arrange(Name, Age, Date) %>%
  mutate(Cumulative_cases = cumsum(Cases)) %>%
  mutate(Rolling_7_day_new_cases = rollapply(Cases, 7, sum, align = 'right', fill = NA, partial = TRUE)) %>%
  mutate(Rolling_7_day_new_cases = replace_na(Rolling_7_day_new_cases, 0)) %>%
  mutate(Perc_change_on_rolling_7_days_actual = round((Rolling_7_day_new_cases - lag(Rolling_7_day_new_cases, 7))/ lag(Rolling_7_day_new_cases, 7), 2)) %>%
  mutate(Perc_change_on_rolling_7_days_actual = ifelse(Perc_change_on_rolling_7_days_actual == Inf, 1, Perc_change_on_rolling_7_days_actual)) %>%
  mutate(Perc_change_on_rolling_7_days_actual = replace_na(Perc_change_on_rolling_7_days_actual, 0)) %>%   mutate(Rolling_7_day_average_new_cases = rollapply(Cases, 7, mean, align = 'right', fill = NA)) %>%
  ungroup() %>%
  mutate(ASR = pois.exact(Rolling_7_day_new_cases, Population)[[3]]*100000) %>%
  arrange(Date) %>%
  select(Name, Age, Date, Cases, Cumulative_cases, Rolling_7_day_new_cases, Rolling_7_day_average_new_cases, ASR, Perc_change_on_rolling_7_days_actual, Population)

age_spec_10_summary_1 <- age_spec_10 %>%
  filter(Date == max(Date)) %>%
  mutate(New_cases_per_100000 = pois.exact(Cases, Population)[[3]]*100000) %>%
  mutate(Cumulative_per_100000 = pois.exact(Cumulative_cases, Population)[[3]]*100000) %>%
  rename(New_cases = Cases) %>%
  select(Name, Date, Age, Cumulative_cases, Cumulative_per_100000, New_cases, New_cases_per_100000, Rolling_7_day_new_cases, ASR, Rolling_7_day_average_new_cases, Perc_change_on_rolling_7_days_actual) %>%
  rename(Rolling_7_day_new_cases_per_100000 = ASR) %>%
  mutate(Change_direction = ifelse(Perc_change_on_rolling_7_days_actual < 0, 'Down', ifelse(Perc_change_on_rolling_7_days_actual == 0, 'Same', ifelse(Perc_change_on_rolling_7_days_actual > 0, 'Up', NA)))) %>%   rename(Rate_date = Date) %>%
  mutate(Cumulative_date = Rate_date)

age_spec_over_60 <- case_age_df_daily %>%
  filter(Age %in% c('60-64 years', '65-69 years', '70-74 years', '75-79 years', '80+ years')) %>%
  mutate(Age = '60+ years') %>%
  group_by(Name, Age, Date) %>%
  summarise(Cases = sum(Cases, na.rm = TRUE),
            Population = sum(Population, na.rm = TRUE)) %>%
  group_by(Name) %>%
  arrange(Name, Age, Date) %>%
  mutate(Cumulative_cases = cumsum(Cases)) %>%
  mutate(Rolling_7_day_new_cases = rollapply(Cases, 7, sum, align = 'right', fill = NA, partial = TRUE)) %>%
  mutate(Rolling_7_day_new_cases = replace_na(Rolling_7_day_new_cases, 0)) %>%
  mutate(Perc_change_on_rolling_7_days_actual = round((Rolling_7_day_new_cases - lag(Rolling_7_day_new_cases, 7))/ lag(Rolling_7_day_new_cases, 7), 2)) %>%
  mutate(Perc_change_on_rolling_7_days_actual = ifelse(Perc_change_on_rolling_7_days_actual == Inf, 1, Perc_change_on_rolling_7_days_actual)) %>%
  mutate(Perc_change_on_rolling_7_days_actual = replace_na(Perc_change_on_rolling_7_days_actual, 0)) %>%   mutate(Rolling_7_day_average_new_cases = rollapply(Cases, 7, mean, align = 'right', fill = NA)) %>%
  ungroup() %>%
  mutate(ASR = pois.exact(Rolling_7_day_new_cases, Population)[[3]]*100000)

age_spec_60_summary_1 <- age_spec_over_60 %>%
  filter(Date == max(Date)) %>%
  mutate(New_cases_per_100000 = pois.exact(Cases, Population)[[3]]*100000) %>%
  mutate(Cumulative_per_100000 = pois.exact(Cumulative_cases, Population)[[3]]*100000) %>%
  rename(New_cases = Cases) %>%
  select(Name, Date, Age, Cumulative_cases, Cumulative_per_100000, New_cases, New_cases_per_100000, Rolling_7_day_new_cases, ASR, Perc_change_on_rolling_7_days_actual, Rolling_7_day_average_new_cases) %>%
  rename(Rolling_7_day_new_cases_per_100000 = ASR) %>%
  mutate(Change_direction = ifelse(Perc_change_on_rolling_7_days_actual < 0, 'Down', ifelse(Perc_change_on_rolling_7_days_actual == 0, 'Same', ifelse(Perc_change_on_rolling_7_days_actual > 0, 'Up', NA)))) %>%
  rename(Rate_date = Date) %>%
  mutate(Cumulative_date = Rate_date)

rm(age_df_daily_combined, age_spec, Ages, df_x, mye_ages)

p12_test_summary <- p12_test_summary %>%
  mutate(Age = 'All ages')

case_summary <- p12_test_summary %>%
  bind_rows(age_spec_10_summary_1) %>%
  bind_rows(age_spec_60_summary_1) %>%
  mutate(Rate_date = format(Rate_date, '%A %d %B')) %>%
  mutate(Cumulative_date = format(Cumulative_date, '%A %d %B'))

case_summary %>%
  toJSON() %>%
  write_lines(paste0(output_directory_x,'/case_summary.json'))

at_a_glance_1 <- case_summary %>%
  filter(Age %in% c('All ages', '60+ years')) %>%
  select(Age, Name, Rolling_7_day_new_cases, Rolling_7_day_new_cases_per_100000, Change_direction) %>%
  mutate(icon_path = ifelse(Change_direction == 'Down', 'https://raw.githubusercontent.com/psychty/wsx_covid_public_mobile_site/main/images/down-double-arrow-green.svg', ifelse(Change_direction == 'Up', 'https://raw.githubusercontent.com/psychty/wsx_covid_public_mobile_site/main/images/up-double-arrow-red.svg', 'https://raw.githubusercontent.com/psychty/wsx_covid_public_mobile_site/main/images/same-blue.svg')))

at_a_glance_1 %>%
  mutate(Name = factor(Name, levels = c('Adur', 'Arun', 'Chichester', 'Crawley', 'Horsham', 'Mid Sussex', 'Worthing', 'West Sussex','South East region', 'England'))) %>%
  arrange(Name) %>%
  # mutate(index = row_number()) %>%
  toJSON() %>%
  write_lines(paste0(output_directory_x,'/table_summary.json'))

daily_cases_df <- p12_test_df %>%
  filter(Name %in% Areas) %>%
  mutate(Age = 'All ages') %>%
  select(Name, Age, Date, Period, New_cases, Rolling_7_day_new_cases, Perc_change_on_rolling_7_days_actual, Rolling_7_day_new_cases_per_100000, Rolling_7_day_average_new_cases) %>%
  rename(Cases = New_cases,
         ASR = Rolling_7_day_new_cases_per_100000) %>%
  bind_rows(age_spec_over_60) %>%
  rename(Rolling_7_day_new_cases_per_100000 = ASR) %>%
  select(!Population) %>%
  mutate(Rolling_7_day_average_new_cases = ifelse(Date > complete_date, NA, Rolling_7_day_average_new_cases)) %>%
  mutate(Data_completeness = ifelse(Date > complete_date, 'Considered incomplete', 'Complete'))

daily_cases_df %>%
  mutate(Period = format(Date, '%d %B %Y')) %>%
  toJSON() %>%
  write_lines(paste0(output_directory_x, '/daily_cases.json'))

cases_this_week <- daily_cases_df %>%
  filter(Name == 'West Sussex',
         Age == 'All ages',
         Date == complete_date)

cases_last_week <- daily_cases_df %>%
  filter(Name == 'West Sussex',
         Age == 'All ages',
         Date == complete_date - 7)

change_between_weeks <- cases_this_week$Rolling_7_day_new_cases - cases_last_week$Rolling_7_day_new_cases

change_direction_between_weeks <- ifelse(cases_this_week$Rolling_7_day_new_cases - cases_last_week$Rolling_7_day_new_cases == 0, 'SAME', ifelse(cases_this_week$Rolling_7_day_new_cases - cases_last_week$Rolling_7_day_new_cases < 0, 'DOWN',  ifelse(cases_this_week$Rolling_7_day_new_cases - cases_last_week$Rolling_7_day_new_cases > 0, 'UP', NA)))

total_so_far <- p12_test_df %>%
  filter(Name == 'West Sussex',
         Date == max(Date))

# Hospital admissions ####

# Hospital provider trusts do not have geographically defined boundaries for their population nor do they have complete lists of registered patients. However, modeled estimates of the catchment populations for hospital provider trusts in England are provided by Public Health England (PHE). These experimental statistics estimates the number of people who are using each hospital trust or have the potential to do so. Individual acute trusts sometimes use varying methods to define the population they serve, such as patient flow, CCG derived or travel time based estimates. PHE published modelled estimates use the patient flow method.

# Note: interpretation of the figures should take into account the fact that totals by date of death, particularly for recent prior days, are likely to be updated in future releases. For example as deaths are confirmed as testing positive for Covid-19, as more post-mortem tests are processed and data from them are validated. Any changes are made clear in the daily files.

hospital_admissions_1 <- read_csv('https://api.coronavirus.data.gov.uk/v2/data?areaType=nhsRegion&metric=covidOccupiedMVBeds&metric=hospitalCases&metric=newAdmissions&metric=newAdmissionsRollingSum&format=csv')

hospital_admissions_2 <- read_csv('https://api.coronavirus.data.gov.uk/v2/data?areaType=nation&metric=covidOccupiedMVBeds&metric=hospitalCases&metric=newAdmissions&metric=newAdmissionsRollingSum&format=csv')

hospital_admissions_df <- hospital_admissions_1 %>%
  bind_rows(hospital_admissions_2) %>% 
  filter(date <= max(date))

rm(hospital_admissions_1, hospital_admissions_2)

# patients occupying beds as at 8am
admissions_date <- hospital_admissions_df %>%
  filter(!is.na(newAdmissions)) %>%
  filter(date == max(date, na.rm = TRUE)) %>%
  select(date) %>%
  unique() %>%
  mutate(item = 'Admissions')

occupied_date <- hospital_admissions_df %>%
  filter(!is.na(hospitalCases)) %>%
  filter(date == max(date, na.rm = TRUE)) %>%
  select(date) %>%
  unique() %>%
  mutate(item = 'Patients in hospital')

publish_date <- data.frame(date = occupied_date$date,
                           item = 'Publish date')

admissions_date %>%
  bind_rows(occupied_date) %>%
  bind_rows(publish_date) %>%
  mutate(Date_label = format(date, '%A %d %B %Y')) %>%
  toJSON() %>%
  write_lines(paste0(output_directory_x,'/hospital_meta.json'))

format(as.Date(occupied_date$date), '%A %d %B %Y') %>%
  toJSON() %>%
  write_lines(paste0(output_directory_x,'/trust_meta.json'))

hospital_admissions_df %>%
  rename(Patients_occupying_beds = hospitalCases,
         Patients_occupying_mv_beds = covidOccupiedMVBeds,
         Name = areaName,
         Date = date) %>%
  select(Name, Date, Patients_occupying_beds, Patients_occupying_mv_beds) %>%
  group_by(Name) %>%
  arrange(Name, Date) %>%
  mutate(Previous_occupying_beds = lag(Patients_occupying_beds, 7)) %>%
  mutate(Perc_change_on_beds_occupied = (Patients_occupying_beds - lag(Patients_occupying_beds, 7))/ lag(Patients_occupying_beds, 7)) %>%
  mutate(Perc_change_on_beds_occupied = ifelse(Perc_change_on_beds_occupied == Inf, 1, Perc_change_on_beds_occupied)) %>%
  mutate(Perc_change_on_beds_occupied = replace_na(Perc_change_on_beds_occupied, 0)) %>%
  mutate(Change_direction = ifelse(Perc_change_on_beds_occupied <0, 'decreased', ifelse(Perc_change_on_beds_occupied == 0, 'stayed the same', ifelse(Perc_change_on_beds_occupied > 0, 'increased', NA)))) %>%
  mutate(Date_label = format(Date, '%d %b %y')) %>%
  filter(Date == occupied_date$date) %>%
  select(!c(Date)) %>%
  toJSON() %>%
  write_lines(paste0(output_directory_x,'/trust_bed_summary.json'))

se_hospital_df <- hospital_admissions_df %>%
  rename(Patients_occupying_beds = hospitalCases,
         Patients_occupying_mv_beds = covidOccupiedMVBeds,
         Name = areaName,
         Date = date) %>%
  select(Name, Date, Patients_occupying_beds, Patients_occupying_mv_beds) %>%
  group_by(Name) %>%
  arrange(Name, Date) %>%
  mutate(Previous_occupying_beds = lag(Patients_occupying_beds, 7)) %>%
  mutate(Perc_change_on_beds_occupied = (Patients_occupying_beds - lag(Patients_occupying_beds, 7))/ lag(Patients_occupying_beds, 7)) %>%
  mutate(Perc_change_on_beds_occupied = ifelse(Perc_change_on_beds_occupied == Inf, 1, Perc_change_on_beds_occupied)) %>%
  mutate(Perc_change_on_beds_occupied = replace_na(Perc_change_on_beds_occupied, 0)) %>%
  mutate(Change_direction = ifelse(Perc_change_on_beds_occupied <0, 'decreased', ifelse(Perc_change_on_beds_occupied == 0, 'stayed the same', ifelse(Perc_change_on_beds_occupied > 0, 'increased', NA)))) %>%
  mutate(Date_label = format(Date, '%d %b %y')) %>%
  filter(Date == occupied_date$date) %>%
  filter(Name == 'South East')

# mortality ####

# Area lookup

mye_total_raw <- read_csv('http://www.nomisweb.co.uk/api/v01/dataset/NM_2002_1.data.csv?geography=1816133633...1816133848,1820327937...1820328318,2092957697...2092957703,2013265921...2013265932&date=2020&gender=0&c_age=200,209&measures=20100&select=date_name,geography_name,geography_code,geography_type,c_age_name,obs_value') %>%
  rename(Population = OBS_VALUE,
         Area_code = GEOGRAPHY_CODE,
         Name = GEOGRAPHY_NAME,
         Year = DATE_NAME,
         Age = C_AGE_NAME,
         Type = GEOGRAPHY_TYPE) %>%
  unique() %>%
  group_by(Name, Area_code) %>%
  mutate(Count = n()) %>%
  mutate(Type = ifelse(Count == 4, 'Unitary Authority', ifelse(Type == 'local authorities: county / unitary (as of April 2019)', 'Upper Tier Local Authority', ifelse(Type == 'local authorities: district / unitary (as of April 2019)', 'Lower Tier Local Authority', ifelse(Type == 'regions', 'Region', ifelse(Type == 'countries', 'Country', Type)))))) %>%
  ungroup() %>%
  select(-Count) %>%
  unique()

if(exists('mye_total_raw') == FALSE) {
  mye_total_raw <- read_csv(paste0(github_repo_dir,'/Source_files/mye_total_raw.csv'))
}

mye_total_raw %>%
  write.csv(., paste0(github_repo_dir,'/Source_files/mye_total_raw.csv'), row.names = FALSE)

area_code_names <- mye_total_raw %>%
  select(Area_code, Name) %>%
  rename(Code = Area_code) %>%
  unique()

mye_total <- mye_total_raw %>%
  select(-Name) %>%
  spread(Age, Population) %>%
  rename(All_ages = `All Ages`,
         Age_65_plus = `Aged 65+`)

lookup <- read_csv(url("https://opendata.arcgis.com/datasets/3e4f4af826d343349c13fb7f0aa2a307_0.csv")) %>%
  select(-c(FID,LTLA19NM)) %>%
  left_join(read_csv(url('https://opendata.arcgis.com/datasets/3ba3daf9278f47daba0f561889c3521a_0.csv')), by = c('LTLA19CD' = 'LAD19CD')) %>%
  select(-c(FID, LAD19NM)) %>%
  add_row(LTLA19CD ='E06000060', UTLA19CD = 'E06000060', UTLA19NM = 'Buckinghamshire', RGN19CD = 'E12000008', RGN19NM = 'South East') %>%
  filter(RGN19NM == 'South East')

mye_total <- mye_total %>%
  mutate(Area_code = ifelse(Area_code == 'E10000002', 'E06000060', Area_code))

set_week_start('Friday')

week_ending_a <- data.frame(Week_ending= get_date(week = 1:53, year = 2020)) %>%
  mutate(Week_number = paste0(row_number(), ' - 2020'))

week_ending_b <- data.frame(Week_ending = get_date(week = 2:53, year = 2021)) %>%
  mutate(Week_number = paste0(row_number(), ' - 2021'))

week_ending_c <- data.frame(Week_ending = get_date(week = 2:53, year = 2022)) %>%
  mutate(Week_number = paste0(row_number(), ' - 2022'))

week_ending <- week_ending_a %>%
  bind_rows(week_ending_b) %>%
  bind_rows(week_ending_c) %>% 
  mutate(Week_number = factor(Week_number, levels = c("1 - 2020", "2 - 2020",  "3 - 2020", "4 - 2020",  "5 - 2020",  "6 - 2020",  "7 - 2020",  "8 - 2020",  "9 - 2020",  "10 - 2020", "11 - 2020", "12 - 2020", "13 - 2020", "14 - 2020", "15 - 2020", "16 - 2020", "17 - 2020", "18 - 2020", "19 - 2020", "20 - 2020", "21 - 2020", "22 - 2020", "23 - 2020", "24 - 2020", "25 - 2020", "26 - 2020", "27 - 2020", "28 - 2020", "29 - 2020", "30 - 2020", "31 - 2020", "32 - 2020", "33 - 2020", "34 - 2020", "35 - 2020", "36 - 2020", "37 - 2020", "38 - 2020", "39 - 2020", "40 - 2020", "41 - 2020", "42 - 2020", "43 - 2020", "44 - 2020", "45 - 2020", "46 - 2020", "47 - 2020", "48 - 2020", "49 - 2020", "50 - 2020", "51 - 2020", "52 - 2020", "53 - 2020", "1 - 2021", "2 - 2021", "3 - 2021", "4 - 2021",  "5 - 2021",  "6 - 2021",  "7 - 2021", "8 - 2021",  "9 - 2021",  "10 - 2021", "11 - 2021", "12 - 2021", "13 - 2021", "14 - 2021", "15 - 2021", "16 - 2021", "17 - 2021", "18 - 2021", "19 - 2021", "20 - 2021", "21 - 2021", "22 - 2021", "23 - 2021", "24 - 2021", "25 - 2021", "26 - 2021", "27 - 2021", "28 - 2021", "29 - 2021", "30 - 2021", "31 - 2021", "32 - 2021", "33 - 2021", "34 - 2021", "35 - 2021", "36 - 2021", "37 - 2021", "38 - 2021", "39 - 2021", "40 - 2021", "41 - 2021", "42 - 2021", "43 - 2021", "44 - 2021", "45 - 2021", "46 - 2021", "47 - 2021", "48 - 2021", "49 - 2021", "50 - 2021", "51 - 2021", "52 - 2021", "1 - 2022", "2 - 2022", "3 - 2022", "4 - 2022",  "5 - 2022",  "6 - 2022",  "7 - 2022", "8 - 2022",  "9 - 2022",  "10 - 2022", "11 - 2022", "12 - 2022", "13 - 2022", "14 - 2022", "15 - 2022", "16 - 2022", "17 - 2022", "18 - 2022", "19 - 2022", "20 - 2022", "21 - 2022", "22 - 2022", "23 - 2022", "24 - 2022", "25 - 2022", "26 - 2022", "27 - 2022", "28 - 2022", "29 - 2022", "30 - 2022", "31 - 2022", "32 - 2022", "33 - 2022", "34 - 2022", "35 - 2022", "36 - 2022", "37 - 2022", "38 - 2022", "39 - 2022", "40 - 2022", "41 - 2022", "42 - 2022", "43 - 2022", "44 - 2022", "45 - 2022", "46 - 2022", "47 - 2022", "48 - 2022", "49 - 2022", "50 - 2022", "51 - 2022", "52 - 2022"))) %>%
  mutate(week_id = row_number())

rm(week_ending_a, week_ending_b, week_ending_c)

# Deaths are updated each week - https://www.ons.gov.uk/peoplepopulationandcommunity/healthandsocialcare/causesofdeath/datasets/deathregistrationsandoccurrencesbylocalauthorityandhealthboard

# sadly, the file uploaded each week does not have a static url nor is it always sequential (sometimes it is wk1, wk2 and so on but not always)

download.file('https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fhealthandsocialcare%2fcausesofdeath%2fdatasets%2fdeathregistrationsandoccurrencesbylocalauthorityandhealthboard%2f2020/lahbtablesweek01to532020datawk232021.xlsx', paste0(github_repo_dir, '/Source_files/ons_mortality_2020.xlsx'), mode = 'wb')

download.file(paste0('https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fhealthandsocialcare%2fcausesofdeath%2fdatasets%2fdeathregistrationsandoccurrencesbylocalauthorityandhealthboard%2f2021/lahbtables2021.xlsx'),  paste0(github_repo_dir, '/Source_files/ons_mortality_2021.xlsx'), mode = 'wb')

download.file(paste0('https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fhealthandsocialcare%2fcausesofdeath%2fdatasets%2fdeathregistrationsandoccurrencesbylocalauthorityandhealthboard%2f2022/lahbtables2022week18.xlsx'),  paste0(github_repo_dir, '/Source_files/ons_mortality_2022.xlsx'), mode = 'wb')

# # if the download does fail, it wipes out the old one, which we can use to our advantage
# if(!file.exists(paste0(github_repo_dir, '/Source_files/ons_mortality.xlsx'))){
#   download.file(paste0('https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fhealthandsocialcare%2fcausesofdeath%2fdatasets%2fdeathregistrationsandoccurrencesbylocalauthorityandhealthboard%2f2020/lahbtablesweek',substr(as.character(as.aweek(Sys.Date()-12)), 7,8), '.xlsx'),  paste0(github_repo_dir, '/Source_files/ons_mortality.xlsx'), mode = 'wb')
# }

Occurrences_ltla_2022 <- read_excel(paste0(github_repo_dir, '/Source_files/ons_mortality_2022.xlsx'), sheet = 'Occurrences - All data', skip = 2) %>%
  mutate(`Week number` = paste0(`Week number`, ' - 2022'))

Occurrences_ltla_2021 <- read_excel(paste0(github_repo_dir, '/Source_files/ons_mortality_2021.xlsx'), sheet = 'Occurrences - All data', skip = 2) %>%
  mutate(`Week number` = paste0(`Week number`, ' - 2021'))

Occurrences_ltla <- read_excel(paste0(github_repo_dir, '/Source_files/ons_mortality_2020.xlsx'), sheet = 'Occurrences - All data', skip = 2) %>%
  mutate(`Week number` = paste0(`Week number`, ' - 2020')) %>%
  bind_rows(Occurrences_ltla_2021) %>%
  bind_rows(Occurrences_ltla_2022) %>% 
  rename(Name = `Area name`,
         Cause = `Cause of death`,
         Week_number = `Week number`,
         Place_of_death = `Place of death`,
         Deaths = `Number of deaths`) %>%
  filter(Name %in% c('Adur', 'Arun', 'Chichester', 'Crawley', 'Horsham', 'Mid Sussex', 'Worthing')) %>%
  select(Name, Week_number, Cause, Place_of_death, Deaths) %>%
  left_join(week_ending, by = 'Week_number') %>%
  ungroup()

# Occurrences data is produced at ltla level and we would probably find it useful to aggregate to utla and region for our analysis
Occurrences_wsx <- Occurrences_ltla %>%
  group_by(Cause, Week_number, Place_of_death) %>%
  summarise(Deaths = sum(Deaths, na.rm = TRUE)) %>%
  mutate(Name = 'West Sussex') %>%
  select(Name, Week_number, Cause, Place_of_death, Deaths) %>%
  left_join(week_ending, by = 'Week_number') %>%
  ungroup()

Occurrences_england <- read_excel(paste0(github_repo_dir, '/Source_files/ons_mortality_2020.xlsx'), sheet = 'Occurrences - All data', skip = 2) %>%
  mutate(`Week number` = paste0(`Week number`, ' - 2020')) %>%
  bind_rows(Occurrences_ltla_2021) %>%
  bind_rows(Occurrences_ltla_2022) %>% 
  rename(Name = `Area name`,
         Cause = `Cause of death`,
         Week_number = `Week number`,
         Place_of_death = `Place of death`,
         Deaths = `Number of deaths`) %>%
  filter(substr(`Area code`, 1,1) == 'E') %>%
  group_by(Cause, Week_number, Place_of_death) %>%
  summarise(Deaths = sum(Deaths, na.rm = TRUE)) %>%
  mutate(Name = 'England') %>%
  select(Name, Week_number, Cause, Place_of_death, Deaths) %>%
  left_join(week_ending, by = 'Week_number') %>%
  ungroup()

Occurrences_southeast <- read_excel(paste0(github_repo_dir, '/Source_files/ons_mortality_2020.xlsx'), sheet = 'Occurrences - All data', skip = 2) %>%
  mutate(`Week number` = paste0(`Week number`, ' - 2020')) %>%
  bind_rows(Occurrences_ltla_2021) %>%
  bind_rows(Occurrences_ltla_2022) %>% 
  rename(Name = `Area name`,
         Cause = `Cause of death`,
         Week_number = `Week number`,
         Place_of_death = `Place of death`,
         Deaths = `Number of deaths`) %>%
  filter(`Area code` %in% lookup$LTLA19CD) %>%
  group_by(Cause, Week_number, Place_of_death) %>%
  summarise(Deaths = sum(Deaths, na.rm = TRUE)) %>%
  mutate(Name = 'South East region') %>%
  select(Name, Week_number, Cause, Place_of_death, Deaths) %>%
  left_join(week_ending, by = 'Week_number') %>%
  ungroup()

Occurrences_southeast_check <- read_excel(paste0(github_repo_dir, '/Source_files/ons_mortality_2020.xlsx'), sheet = 'Occurrences - All data', skip = 2) %>%
  mutate(`Week number` = paste0(`Week number`, ' - 2020')) %>%
  bind_rows(Occurrences_ltla_2021) %>%
  bind_rows(Occurrences_ltla_2022) %>% 
  rename(Name = `Area name`,
         Cause = `Cause of death`,
         Week_number = `Week number`,
         Place_of_death = `Place of death`,
         Deaths = `Number of deaths`) %>%
  filter(`Area code` %in% lookup$LTLA19CD)

Occurrences<- Occurrences_ltla %>%
  bind_rows(Occurrences_wsx) %>%
  bind_rows(Occurrences_england) %>%
  bind_rows(Occurrences_southeast) %>%
  arrange(week_id) %>%
  mutate(Week_number = factor(Week_number, levels = c("1 - 2020", "2 - 2020",  "3 - 2020", "4 - 2020",  "5 - 2020",  "6 - 2020",  "7 - 2020",  "8 - 2020",  "9 - 2020",  "10 - 2020", "11 - 2020", "12 - 2020", "13 - 2020", "14 - 2020", "15 - 2020", "16 - 2020", "17 - 2020", "18 - 2020", "19 - 2020", "20 - 2020", "21 - 2020", "22 - 2020", "23 - 2020", "24 - 2020", "25 - 2020", "26 - 2020", "27 - 2020", "28 - 2020", "29 - 2020", "30 - 2020", "31 - 2020", "32 - 2020", "33 - 2020", "34 - 2020", "35 - 2020", "36 - 2020", "37 - 2020", "38 - 2020", "39 - 2020", "40 - 2020", "41 - 2020", "42 - 2020", "43 - 2020", "44 - 2020", "45 - 2020", "46 - 2020", "47 - 2020", "48 - 2020", "49 - 2020", "50 - 2020", "51 - 2020", "52 - 2020", "53 - 2020", "1 - 2021", "2 - 2021", "3 - 2021", "4 - 2021",  "5 - 2021",  "6 - 2021",  "7 - 2021", "8 - 2021",  "9 - 2021",  "10 - 2021", "11 - 2021", "12 - 2021", "13 - 2021", "14 - 2021", "15 - 2021", "16 - 2021", "17 - 2021", "18 - 2021", "19 - 2021", "20 - 2021", "21 - 2021", "22 - 2021", "23 - 2021", "24 - 2021", "25 - 2021", "26 - 2021", "27 - 2021", "28 - 2021", "29 - 2021", "30 - 2021", "31 - 2021", "32 - 2021", "33 - 2021", "34 - 2021", "35 - 2021", "36 - 2021", "37 - 2021", "38 - 2021", "39 - 2021", "40 - 2021", "41 - 2021", "42 - 2021", "43 - 2021", "44 - 2021", "45 - 2021", "46 - 2021", "47 - 2021", "48 - 2021", "49 - 2021", "50 - 2021", "51 - 2021", "52 - 2021", "1 - 2022", "2 - 2022", "3 - 2022", "4 - 2022",  "5 - 2022",  "6 - 2022",  "7 - 2022", "8 - 2022",  "9 - 2022",  "10 - 2022", "11 - 2022", "12 - 2022", "13 - 2022", "14 - 2022", "15 - 2022", "16 - 2022", "17 - 2022", "18 - 2022", "19 - 2022", "20 - 2022", "21 - 2022", "22 - 2022", "23 - 2022", "24 - 2022", "25 - 2022", "26 - 2022", "27 - 2022", "28 - 2022", "29 - 2022", "30 - 2022", "31 - 2022", "32 - 2022", "33 - 2022", "34 - 2022", "35 - 2022", "36 - 2022", "37 - 2022", "38 - 2022", "39 - 2022", "40 - 2022", "41 - 2022", "42 - 2022", "43 - 2022", "44 - 2022", "45 - 2022", "46 - 2022", "47 - 2022", "48 - 2022", "49 - 2022", "50 - 2022", "51 - 2022", "52 - 2022")))

rm(Occurrences_ltla, Occurrences_wsx, Occurrences_southeast, Occurrences_england)

deaths_labels <- Occurrences %>%
  arrange(week_id) %>%
  select(Week_ending) %>%
  unique() %>%
  mutate(deaths_label = paste0('w/e ', ordinal(as.numeric(format(Week_ending, '%d'))), format(Week_ending, ' %b %y')))

# calculating release date
Occurrences_meta_1 <- week_ending %>%
  filter(Week_ending == max(Occurrences$Week_ending)) %>%
  mutate(registered_by = Week_ending + 8,
         published_on = Week_ending + 11) %>%
  pivot_longer(cols = c(Week_ending, registered_by, published_on), names_to = 'Item') %>%
  mutate(Label = format(value, '%A %d %B'))

first_week_deaths <- Occurrences %>%
  filter(week_id == min(Occurrences$week_id)) %>%
  select(Week_number) %>%
  unique()

latest_week_deaths <- Occurrences %>%
  filter(week_id == max(Occurrences$week_id)) %>%
  select(Week_number) %>%
  unique()

Occurrences_meta_2 <- data.frame(Week_number = character(), Item = character()) %>%
  add_row(Week_number = as.character(first_week_deaths$Week_number),
          Item = 'First_week') %>%
  add_row(Week_number = as.character(latest_week_deaths$Week_number),
          Item = 'Last_week') %>%
  left_join(week_ending, by = 'Week_number') %>%
  mutate(Label = paste0('w/e ', ordinal(as.numeric(format(Week_ending, '%d'))), format(Week_ending, ' %b %y'))) %>%
  rename(value = Week_ending)

Occurrences_meta_1 %>%
  bind_rows(Occurrences_meta_2) %>%
  toJSON() %>%
  write_lines(paste0(output_directory_x, '/mortality_dates.json'))

weekly_all_place_all_deaths <- Occurrences %>%
  filter(Cause == 'All causes') %>%
  arrange(week_id) %>%
  group_by(Name, Week_ending) %>%
  summarise(Deaths = sum(Deaths, na.rm = TRUE)) %>%
  select(Name, Week_ending, Deaths) %>%
  mutate(Week_ending = factor(paste0('w/e ', ordinal(as.numeric(format(Week_ending, '%d'))), format(Week_ending, ' %b %y')), levels = deaths_labels$deaths_label)) %>%
  rename(All_deaths = Deaths)

All_settings_occurrences <- Occurrences %>%
  group_by(Name, Week_number, Week_ending, Cause) %>%
  summarise(Deaths = sum(Deaths, na.rm = TRUE)) %>%
  group_by(Name, Cause) %>%
  arrange(Cause, Week_number) %>%
  mutate(Cumulative_deaths = cumsum(Deaths))  %>%
  ungroup()

weekly_all_place_deaths <- All_settings_occurrences %>%
  filter(Name %in% Areas) %>%
  arrange(Week_ending) %>%
  select(Name, Cause, Week_ending, Deaths) %>%
  mutate(Week_ending = factor(paste0('w/e ', ordinal(as.numeric(format(Week_ending, '%d'))), format(Week_ending, ' %b %y')), levels = deaths_labels$deaths_label)) %>%
  pivot_wider(id_cols = c(Name, Week_ending),
              names_from = Cause,
              values_from = Deaths) %>%
  mutate(`Non-Covid` = `All causes` - `COVID 19`) %>%
  select(-`All causes`) %>%
  gather(key = 'Cause', value = 'Deaths', `COVID 19`:`Non-Covid`) %>%
  mutate(Cause = factor(Cause, levels = rev(c('Non-Covid', 'COVID 19')))) %>%
  mutate(lab_posit = ifelse(Cause == 'Non-Covid', 1.5, -1)) %>%
  left_join(weekly_all_place_all_deaths, by = c('Name', 'Week_ending'))

deaths_summary_1 <- All_settings_occurrences %>%
  filter(Week_ending == max(Week_ending)) %>%
  select(Name, Week_number, Week_ending, Cause, Deaths) %>%
  mutate(Cause = paste0(Cause, ' deaths this week')) %>%
  pivot_wider(names_from = Cause, values_from = Deaths)

deaths_summary_2 <- All_settings_occurrences %>%
  filter(Week_ending == max(Week_ending)) %>%
  select(Name, Week_number, Cause, Cumulative_deaths) %>%
  mutate(Cause = paste0(Cause, ' deaths so far')) %>%
  pivot_wider(names_from = Cause, values_from = Cumulative_deaths)

# Care home deaths
carehome_deaths_summary <- Occurrences %>%
  filter(Place_of_death %in% 'Care home') %>%
  arrange(Week_ending) %>%
  select(Name, Cause, Week_ending, Week_number, Deaths) %>%
  # mutate(Week_ending = factor(paste0('w/e ', ordinal(as.numeric(format(Week_ending, '%d'))), format(Week_ending, ' %b')), levels = deaths_labels$deaths_label)) %>%
  mutate(Cause = paste0(Cause, ' deaths this week')) %>%
  pivot_wider(id_cols = c(Name, Week_ending, Week_number),
              names_from = Cause,
              values_from = Deaths) %>%
  group_by(Name) %>%
  arrange(Name, Week_ending) %>%
  mutate(`All causes deaths so far` = cumsum(`All causes deaths this week`),
         `COVID 19 deaths so far` = cumsum(`COVID 19 deaths this week`))  %>%
  ungroup() %>%
  filter(Week_ending == max(Week_ending)) %>%
  mutate(Type = 'Care homes')

deaths_summary <- deaths_summary_1 %>%
  left_join(deaths_summary_2, by = c('Name', 'Week_number')) %>%
  mutate(Type = 'All places') %>%
  bind_rows(carehome_deaths_summary) %>%
  mutate(Label = paste0('week ', Week_number, ' (', format(Week_ending - 6, '%d-%B'), ' to ', format(Week_ending, '%d-%B'), ')')) %>%
  rename(COVID_deaths_in_week = `COVID 19 deaths this week`,
         COVID_deaths_cumulative = `COVID 19 deaths so far`,
         All_cause_deaths_in_week = `All causes deaths this week`,
         All_case_cumulative = `All causes deaths so far`) %>%
  select(!c('Week_number', 'Week_ending'))

deaths_summary %>%
  toJSON() %>%
  write_lines(paste0(output_directory_x, '/mortality_summary.json'))

rm(deaths_summary_1, deaths_summary_2)

all_deaths_json_export <- All_settings_occurrences %>%
  ungroup() %>%
  select(Name, Week_number, Week_ending, Cause, Deaths) %>%
  group_by(Name, Week_number, Week_ending) %>%
  spread(Cause, Deaths) %>%
  mutate(`Not attributed to Covid-19` = `All causes` - `COVID 19`) %>%
  rename(`Covid-19` = `COVID 19`) %>%
  mutate(Deaths_in_week_label = paste0('The total number of deaths occurring in the week ending ', format(Week_ending, '%d %B %Y'), ' in ', Name, ' was<b> ', format(`All causes`, big.mark = ',', trim = TRUE), '</b>. Of these, <b>', format(`Covid-19`, big.mark = ',', trim = TRUE), ifelse(`Covid-19` == 1, ' death</b> was', ' deaths</b> were'), ' attributed to Covid-19. This is ',  round((`Covid-19`/`All causes`) * 100, 1), '% of deaths occuring in this week.')) %>%
  group_by(Name) %>%
  mutate(Cumulative_deaths_all_cause = cumsum(`All causes`)) %>%
  mutate(Cumulative_covid_deaths = cumsum(`Covid-19`)) %>%
  mutate(Cumulative_deaths_label = paste0('As at ', format(Week_ending, '%d %B %Y'), ' in ', Name, ' the total cumulative number of deaths for 2020 was<b> ', format(Cumulative_deaths_all_cause, big.mark = ',', trim = TRUE), '</b>. The cumulative number of deaths where Covid-19 is recorded as a cause by this date was ', format(Cumulative_covid_deaths, big.mark = ',', trim = TRUE), '. This is ', round((Cumulative_covid_deaths/Cumulative_deaths_all_cause) * 100, 1), '% of deaths occuring by this week.')) %>%
  mutate(Date_label = factor(paste0('w/e ', ordinal(as.numeric(format(Week_ending, '%d'))), format(Week_ending, ' %b %y')), levels = deaths_labels$deaths_label))

all_deaths_json_export %>%
  select(Name, Date_label, Week_number, `Covid-19`, `Not attributed to Covid-19`, Cumulative_covid_deaths, Cumulative_deaths_all_cause) %>%
  toJSON() %>%
  write_lines(paste0(output_directory_x, '/deaths_all_settings.json'))

all_deaths_json_export %>%
  select(Name, `All causes`) %>%
  group_by(Name) %>%
  filter(`All causes` == max(`All causes`)) %>%
  mutate(Limit = ifelse(`All causes`<= 50, round_any(`All causes`, 5, ceiling), ifelse(`All causes` <= 100, round_any(`All causes`, 10, ceiling), ifelse(`All causes` <= 250, round_any(`All causes`, 25, ceiling), ifelse(`All causes` <= 500, round_any(`All causes`, 50, ceiling), round_any(`All causes`, 100, ceiling)))))) %>%
  ungroup() %>%
  toJSON() %>%
  write_lines(paste0(output_directory_x, '/deaths_limits_by_area.json'))

carehome_deaths_json_export <- Occurrences %>%
  filter(Place_of_death %in% 'Care home') %>%
  arrange(week_id) %>%
  select(Name, Cause, Week_ending, Week_number, Deaths) %>%
  mutate(Date_label = factor(paste0('w/e ', ordinal(as.numeric(format(Week_ending, '%d'))), format(Week_ending, ' %b %y')), levels = deaths_labels$deaths_label)) %>%
  pivot_wider(id_cols = c(Name, Week_ending, Week_number, Date_label),
              names_from = Cause,
              values_from = Deaths) %>%
  mutate(`Not attributed to Covid-19` = `All causes` - `COVID 19`) %>%
  gather(key = 'Cause', value = 'Deaths', `All causes`:`Not attributed to Covid-19`) %>%
  select(Name, Week_number, Week_ending, Cause, Date_label, Deaths) %>%
  group_by(Name, Week_number, Week_ending, Date_label) %>%
  spread(Cause, Deaths) %>%
  rename(`Covid-19` = `COVID 19`) %>%
  mutate(Deaths_in_week_label = paste0('The total number of deaths occurring in care homes in the week ending ', format(Week_ending, '%d %B %Y'), ' in ', Name, ' was<b> ', format(`All causes`, big.mark = ',', trim = TRUE), '</b>. Of these, <b>', format(`Covid-19`, big.mark = ',', trim = TRUE), ifelse(`Covid-19` == 1, ' death</b> was', ' deaths</b> were'), ' attributed to Covid-19. This is ',  round((`Covid-19`/`All causes`) * 100, 1), '% of deaths occuring in care homes in this week.')) %>%
  group_by(Name) %>%
  mutate(Cumulative_deaths_all_cause = cumsum(`All causes`)) %>%
  mutate(Cumulative_covid_deaths = cumsum(`Covid-19`)) %>%
  mutate(Cumulative_deaths_label = paste0('As at ', format(Week_ending, '%d %B %Y'), ' in ', Name, ' the total cumulative number of deaths in care homes for 2020 was<b> ', format(Cumulative_deaths_all_cause, big.mark = ',', trim = TRUE), '</b>. The cumulative number of care home deaths where Covid-19 is recorded as a cause by this date was ', format(Cumulative_covid_deaths, big.mark = ',', trim = TRUE), '. This is ', round((Cumulative_covid_deaths/Cumulative_deaths_all_cause) * 100, 1), '% of deaths occuring by this week.'))

carehome_deaths_json_export %>%
  select(Name, Week_number, Date_label, `Covid-19`, `Not attributed to Covid-19`, Cumulative_covid_deaths, Cumulative_deaths_all_cause) %>%
  toJSON() %>%
  write_lines(paste0(output_directory_x, '/deaths_carehomes.json'))

# PHE deaths any cause among COVID-19 + patients
# area_level <- 'ltla'
#
# phe_mortality_new_deaths <- read_csv(paste0('https://api.coronavirus.data.gov.uk/v2/data?areaType=', area_level,'&metric=newDeathsByDeathDate&metric=newDeaths28DaysByDeathDate&metric=newDeaths28DaysByDeathDateRollingSum&metric=newDeaths28DaysByDeathDateRollingRate&metric=newDeaths60DaysByDeathDate&format=csv')) %>%
#   bind_rows(read_csv(paste0('https://api.coronavirus.data.gov.uk/v2/data?areaType=nation&areaCode=E92000001&metric=newDeathsByDeathDate&metric=newDeaths28DaysByDeathDate&metric=newDeaths28DaysByDeathDateRollingSum&metric=newDeaths28DaysByDeathDateRollingRate&metric=newDeaths60DaysByDeathDate&format=csv'))) %>%
#   bind_rows(read_csv(paste0('https://api.coronavirus.data.gov.uk/v2/data?areaType=region&areaCode=E12000008&metric=newDeathsByDeathDate&metric=newDeaths28DaysByDeathDate&metric=newDeaths28DaysByDeathDateRollingSum&metric=newDeaths28DaysByDeathDateRollingRate&metric=newDeaths60DaysByDeathDate&format=csv'))) %>%
#   bind_rows(read_csv(paste0('https://api.coronavirus.data.gov.uk/v2/data?areaType=utla&areaCode=E10000032&metric=newDeathsByDeathDate&metric=newDeaths28DaysByDeathDate&metric=newDeaths28DaysByDeathDateRollingSum&metric=newDeaths28DaysByDeathDateRollingRate&metric=newDeaths60DaysByDeathDate&format=csv'))) %>%
#   mutate(areaName = ifelse(areaName == 'South East', 'South East region', areaName)) %>%
#   filter(areaName %in% Areas) %>%
#   filter(date != last_date)
#
# phe_mortality_cumdeaths <- read_csv(paste0('https://api.coronavirus.data.gov.uk/v2/data?areaType=', area_level,'&metric=cumDeathsByDeathDate&metric=cumDeaths28DaysByDeathDate&metric&metric=cumDeaths60DaysByDeathDate&format=csv')) %>%
#   bind_rows(read_csv(paste0('https://api.coronavirus.data.gov.uk/v2/data?areaType=nation&areaCode=E92000001&metric=cumDeathsByDeathDate&metric=cumDeaths28DaysByDeathDate&metric&metric=cumDeaths60DaysByDeathDate&format=csv'))) %>%
#   bind_rows(read_csv(paste0('https://api.coronavirus.data.gov.uk/v2/data?areaType=region&areaCode=E12000008&metric=cumDeathsByDeathDate&metric=cumDeaths28DaysByDeathDate&metric&metric=cumDeaths60DaysByDeathDate&format=csv'))) %>%
#   bind_rows(read_csv(paste0('https://api.coronavirus.data.gov.uk/v2/data?areaType=utla&areaCode=E10000032&metric=cumDeathsByDeathDate&metric=cumDeaths28DaysByDeathDate&metric&metric=cumDeaths60DaysByDeathDate&format=csv'))) %>%
#   mutate(areaName = ifelse(areaName == 'South East', 'South East region', areaName)) %>%
#   filter(areaName %in% Areas) %>%
#   filter(date == last_case_date)
#
# phe_mortality_new_deaths %>%
#   mutate(Period = format(date, '%d %B')) %>%
#   rename(Date = date,
#          Name = areaName,
#          Deaths = newDeathsByDeathDate,
#          Deaths_within28 = newDeaths28DaysByDeathDate,
#          Deaths_within28_rolling = newDeaths28DaysByDeathDateRollingSum,
#          Deaths_within28_rolling_rate = newDeaths28DaysByDeathDateRollingRate,
#          Deaths_within60 = newDeaths60DaysByDeathDate) %>%
#   select(Name, Date, Period, Deaths, Deaths_within28, Deaths_within28_rolling, Deaths_within28_rolling_rate, Deaths_within60) %>%
#   toJSON() %>%
#   write_lines(paste0(output_directory_x, '/covid_positive_deaths_all_cause.json'))
#
# phe_mortality_cumdeaths %>%
#   mutate(Period = format(date, '%d %B')) %>%
#   rename(Date = date,
#          Name = areaName,
#          Cumulative_deaths = cumDeathsByDeathDate,
#          Cumulative_deaths_28 = cumDeaths28DaysByDeathDate,
#          Cumulative_deaths_60 = cumDeaths60DaysByDeathDate) %>%
#   select(Name, Date, Period, Cumulative_deaths, Cumulative_deaths_28, Cumulative_deaths_60) %>%
#   toJSON() %>%
#   write_lines(paste0(output_directory_x, '/covid_positive_cumdeaths_all_cause.json'))

# MSOA map ####
if(!file.exists(paste0(github_repo_dir, '/Source_files/msoa_lookup_local.csv'))) {

  oa_region <- read_csv('https://opendata.arcgis.com/datasets/180c271e84fc400d92ca6dcc7f6ff780_0.csv') %>%
    select(OA11CD, RGN11NM)

  msoa_names <- read_csv('https://visual.parliament.uk/msoanames/static/MSOA-Names-1.8.csv') %>%
    select(msoa11cd, msoa11hclnm) %>%
    rename(MSOA11CD = msoa11cd)

  msoa_lookup <- read_csv('https://opendata.arcgis.com/datasets/6ecda95a83304543bc8feedbd1a58303_0.csv') %>%
    left_join(read_csv('https://opendata.arcgis.com/datasets/180c271e84fc400d92ca6dcc7f6ff780_0.csv')[c('OA11CD', 'RGN11NM')], by = 'OA11CD') %>%
    select(MSOA11CD, MSOA11NM, LAD11NM, RGN11NM) %>%
    unique() %>%
    left_join(msoa_names, by = 'MSOA11CD')

  lsoa_to_msoa <- read_csv('https://opendata.arcgis.com/datasets/a46c859088a94898a7c462eeffa0f31a_0.csv') %>%
    select(LSOA11CD, MSOA11CD, MSOA11NM) %>%
    unique()

  msoa_to_utla <- read_csv('https://opendata.arcgis.com/datasets/4c6f3314565e43c5ac7885fd71347548_0.csv') %>%
    left_join(lsoa_to_msoa, by = 'LSOA11CD') %>%
    select(MSOA11CD, UTLA19NM) %>%
    mutate(UTLA19NM = ifelse(UTLA19NM %in% c('City of London', 'Hackney'), 'Hackney and City of London', ifelse(UTLA19NM %in% c('Cornwall', 'Isles of Scilly'), 'Cornwall and Isles of Scilly', UTLA19NM))) %>%
    filter(!is.na(MSOA11CD)) %>%
    unique()

  msoa_lookup %>%
    left_join(msoa_to_utla, by = 'MSOA11CD') %>%
    write.csv(., paste0(github_repo_dir, '/Source_files/msoa_lookup_local.csv'), row.names = FALSE)

}

msoa_lookup <- read_csv(paste0(github_repo_dir, '/Source_files/msoa_lookup_local.csv'))

se_msoas <- msoa_lookup %>%
  filter(RGN11NM == 'South East')

wsx_msoas <- msoa_lookup %>%
  filter(LAD11NM %in% c('Adur', 'Arun', 'Chichester','Crawley', 'Horsham', 'Mid Sussex', 'Worthing'))

# Weekly rolling sums and population-based rates of new cases by specimen date time series data are available to download for English MSOAs via the following links. The data are updated each day, and show the latest 7 days for which near-complete data release date minus 5 days are available, and historic non-overlapping 7-day blocks. Dates are the final day in the relevant 7-day block, and counts between 0 and 2 are blank in the CSV or NULL in the other formats.

# This stopped on the 28th November!!!
# legacy_msoa_case_df <- read_csv('https://coronavirus.data.gov.uk/downloads/msoa_data/MSOAs_latest.csv')

msoa_data <- read_csv('https://api.coronavirus.data.gov.uk/v2/data?areaType=msoa&metric=newCasesBySpecimenDateRollingSum&metric=newCasesBySpecimenDateRollingRate&format=csv') %>%
  select(areaCode, areaName, date, newCasesBySpecimenDateRollingSum, newCasesBySpecimenDateRollingRate)

# I think the team have stopped pushing data for time periods and msoas where cases are supressed. We need to therefore create a dummy dataset which does include a row for every msoa and expected date.
Areas <- unique(msoa_data$areaCode)
Dates <- unique(msoa_data$date)

msoa_cases_dummy <- data.frame(areaCode = character(), date = character())

for(i in 1:length(Areas)){
  area_x = Areas[i]
  df_x <- data.frame(date = Dates, areaCode = area_x) %>%
    mutate(date = as.character(date))

  msoa_cases_dummy <- msoa_cases_dummy %>%
    bind_rows(df_x)
}

msoa_data <- msoa_cases_dummy %>%
  mutate(date = as.Date(date)) %>%
  left_join(msoa_data, by = c('areaCode', 'date')) %>%
  select(-areaName) %>%
  left_join(msoa_lookup[c('MSOA11CD', 'msoa11hclnm')], by = c('areaCode' = 'MSOA11CD')) %>%
  rename(areaName = msoa11hclnm) %>%
  select(areaCode, areaName, date, newCasesBySpecimenDateRollingSum, newCasesBySpecimenDateRollingRate)

msoa_cases_1 <-  msoa_data %>%
  select(areaCode, areaName, date, newCasesBySpecimenDateRollingSum, newCasesBySpecimenDateRollingRate) %>%
  # filter(areaCode %in% msoa_lookup$MSOA11CD) %>%
  filter(date %in% c(max(date))) %>%
  select(areaCode, date, newCasesBySpecimenDateRollingRate) %>%
  rename(Latest_rate = newCasesBySpecimenDateRollingRate) %>%
  mutate(Latest_rate_key = factor(ifelse(is.na(Latest_rate), 'Less than 3 cases', ifelse(Latest_rate <= 50, 'Up to 50 per 100,000', ifelse(Latest_rate <= 100, '51-100 cases per 100,000', ifelse(Latest_rate <= 150, '101-150 cases per 100,000', ifelse(Latest_rate <= 200, '151-200 cases per 100,000', 'More than 200 cases per 100,000'))))), levels = c('Less than 3 cases', 'Up to 50 cases per 100,000', '51-100 cases per 100,000', '101-150 cases per 100,000', '151-200 cases per 100,000', 'More than 200 cases per 100,000')))

msoa_cases_raw <- as.data.frame(msoa_data %>%
                                  select(areaCode, areaName, date, newCasesBySpecimenDateRollingSum, newCasesBySpecimenDateRollingRate) %>%
                                  # filter(areaCode %in% msoa_lookup$MSOA11CD) %>%
                                  group_by(areaCode, areaName) %>%
                                  arrange(areaCode, areaName, date) %>%
                                  filter(date %in% c(max(date), max(date) - 7)) %>%
                                  select(areaCode, areaName, date, newCasesBySpecimenDateRollingSum) %>%
                                  mutate(date = ifelse(date == max(date), 'This_week', ifelse(date == max(date)-7, 'Last_week', NA))) %>%
                                  pivot_wider(names_from = 'date', values_from = 'newCasesBySpecimenDateRollingSum') %>%
                                  mutate(Change_actual = This_week - Last_week) %>%
                                  mutate(Change_label = ifelse(is.na(Last_week) & is.na(This_week), 'Cases below 3 in both weeks', ifelse(is.na(Last_week), 'Cases below 3 in previous week but have risen', ifelse(is.na(This_week), 'Cases below 3 in the latest 7 days but have fallen', ifelse(Change_actual == 0, 'No change in case numbers', ifelse(Change_actual < 0, 'Cases have fallen', ifelse(Change_actual>0, 'Cases have risen', NA))))))) %>%
                                  mutate(Case_key = ifelse(is.na(This_week), '0-2 cases', ifelse(This_week <= 5, '3-5 cases', ifelse(This_week <= 10, '6-10 cases', ifelse(This_week <= 15, '11-15 cases', 'More than 15 cases'))))) %>%
                                  left_join(msoa_cases_1, by = 'areaCode') %>%
                                  left_join(msoa_lookup, by = c('areaCode' = 'MSOA11CD')) %>%
                                  mutate(Label = paste0('<b>', MSOA11NM,' (', msoa11hclnm,')</b><br><br>In the seven days to ', format(date, '%A %d %B'), ' there were ', ifelse(is.na(This_week), ' less than three new cases.', paste0(format(This_week, big.mark = ',', trim = TRUE), ' new cases, this is a rate of ', round(Latest_rate, 1), ' cases per 100,000 population.')), '<br><br>', ifelse(is.na(Last_week) & is.na(This_week), 'Cases have been below 3 in the last two 7 day periods.', ifelse(is.na(Last_week), paste0('Cases were below 3 in the previous 7 days (up to ', format(max(date)-7, '%A %d %B') ,') but have risen this week.'), ifelse(is.na(This_week), paste0('Cases are now below 3 in the latest 7 days but have fallen since the previous 7 day period (up to ', format(max(date)-7, '%A %d %B') ,').'), ifelse(Change_actual == 0, 'There is no change in case numbers over the last two weeks', ifelse(Change_actual < 0, paste0('Cases have fallen in the last 7 days compared to the previous 7 days (up to ', format(max(date)-7, '%A %d %B') ,').'), ifelse(Change_actual >0, paste0('Cases have risen in the last 7 days compared to the previous 7 days (up to ', format(max(date)-7, '%A %d %B') ,').'), NA)))))))) %>%
                                  ungroup())

msoa_cases <- msoa_cases_raw %>%
  select(MSOA11NM, Case_key, Latest_rate_key, Change_label, Label) %>%
  #  filter(MSOA11NM %in% se_msoas$MSOA11NM) %>%
  arrange(MSOA11NM)

# msoa_boundaries_json <- geojson_read("https://opendata.arcgis.com/datasets/e680000c6806493ca126c3ef98fdae19_0.geojson",  what = "sp") %>%
#   filter(MSOA11NM %in% msoa_cases$MSOA11NM) %>%
#   arrange(MSOA11NM)

# download.file("https://opendata.arcgis.com/datasets/23cdb60ee47e4fef8d72e4ee202accb0_0.geojson", paste0(github_repo_dir, '/Source_files/failsafe_msoa_boundary.geojson'), mode = 'wb')

msoa_boundaries_json <- geojson_read(paste0(github_repo_dir, '/Source_files/failsafe_msoa_boundary.geojson'),  what = "sp") %>%
  filter(MSOA11NM %in% msoa_cases$MSOA11NM) %>%
  arrange(MSOA11NM)

df <- data.frame(ID = character())

# Get the IDs of spatial polygon
for (i in msoa_boundaries_json@polygons ) { df <- rbind(df, data.frame(ID = i@ID, stringsAsFactors = FALSE))  }

# and set rowname = ID
row.names(msoa_cases) <- df$ID

# Then use df as the second argument to the spatial dataframe conversion function:
msoa_boundaries_json <- SpatialPolygonsDataFrame(msoa_boundaries_json, msoa_cases)

# geojson_write(ms_simplify(geojson_json(utla_ua_boundaries_rate_geo), keep = 0.2), file = paste0(output_directory_x, '/utla_covid_rate_latest.geojson'))

geojson_write(ms_simplify(geojson_json(msoa_boundaries_json), keep = 0.2), file = paste0(output_directory_x, '/msoa_covid_rate_latest.geojson'))

msoa_cases_raw %>%
  select(MSOA11NM, msoa11hclnm, Latest_rate, LAD11NM, This_week, Last_week, Change_label, date, Change_actual) %>%
  mutate(Change_label = paste0(ifelse(is.na(Last_week) & is.na(This_week), 'Cases have been below 3 in the last two 7 day periods.', ifelse(is.na(Last_week), paste0('Cases were below 3 in the previous 7 days (up to ', format(max(date)-7, '%A %d %B') ,') but have risen this week.'), ifelse(is.na(This_week), paste0('Cases are now below 3 in the latest 7 days but have fallen since the previous 7 day period (up to ', format(max(date)-7, '%A %d %B') ,').'), ifelse(Change_actual == 0, 'There is no change in case numbers over the last two weeks', ifelse(Change_actual < 0, paste0('<b class = "cases_go_down">', Change_actual, '</b> cases compared to the previous 7 days (', Last_week,  ' cases in the 7 days to ', format(max(date)-7, '%A %d %B') ,').'), ifelse(Change_actual >0, paste0('<b class = "cases_go_up">+', Change_actual, '</b> cases compared to the previous 7 days (', Last_week, ' cases in the 7 days to ', format(max(date)-7, '%A %d %B') ,').'), NA)))))))) %>%
  mutate(This_week = ifelse(is.na(This_week), '0-2', format(This_week, big.mark = ',', trim = TRUE)),
         Last_week = ifelse(is.na(Last_week), '0-2', format(Last_week, big.mark = ',', trim = TRUE)),
         Latest_rate = ifelse(is.na(Latest_rate), 'No rate available', Latest_rate)) %>%
  select(!c('date', 'Change_actual')) %>%
  toJSON() %>%
  write_lines(paste0(output_directory_x,'/msoa_summary.json'))

ltla_summary_1 <- p12_test_df %>%
  filter(Type %in% c('Unitary Authority', 'Lower Tier Local Authority')) %>%
  filter(Date == last_case_date) %>%
  select(Name, Date, Cumulative_cases, Cumulative_per_100000) %>%
  rename(Cumulative_date = Date)

ltla_summary_2 <- p12_test_df %>%
  filter(Type %in% c('Unitary Authority', 'Lower Tier Local Authority')) %>%
  filter(Date %in% c(complete_date)) %>%
  select(Name, Date, New_cases, New_cases_per_100000, Rolling_7_day_new_cases, Rolling_7_day_new_cases_per_100000, Rolling_7_day_average_new_cases, Perc_change_on_rolling_7_days_actual, Previous_7_days_sum) %>%
  mutate(Change_direction = ifelse(Perc_change_on_rolling_7_days_actual <0, 'Down', ifelse(Perc_change_on_rolling_7_days_actual == 0, 'Same', ifelse(Perc_change_on_rolling_7_days_actual > 0, 'Up', NA)))) %>%   rename(Rate_date = Date)

ltla_summary <- ltla_summary_1 %>%
  left_join(ltla_summary_2, by = 'Name') %>%
  left_join(read_csv(paste0(github_repo_dir, '/Source_files/ltla_tiers.csv'))[c('Area code', 'Name', 'Tier', 'Primary_schools')], by = 'Name') %>%
  mutate(Change_label = ifelse(Change_direction == 'Down', paste0('decreased by ', format(abs(Previous_7_days_sum - Rolling_7_day_new_cases), big.mark = ',', trim = TRUE), ' cases (', round(abs(Perc_change_on_rolling_7_days_actual) * 100,1), '%)'),ifelse(Change_direction == 'Up', paste0('increased by ',   format(abs(Previous_7_days_sum - Rolling_7_day_new_cases), big.mark = ',', trim = TRUE), ' cases (', round(abs(Perc_change_on_rolling_7_days_actual) * 100,1), '%)'), 'stayed the same.'))) %>%
  unique()

ltla_summary %>%
  toJSON() %>%
  write_lines(paste0(output_directory_x,'/ltla_summary.json'))

ltla_boundaries <- geojson_read('https://opendata.arcgis.com/datasets/3a4fa2ce68f642e399b4de07643eeed3_0.geojson',  what = "sp")

#download.file('https://opendata.arcgis.com/datasets/3a4fa2ce68f642e399b4de07643eeed3_0.geojson', paste0(github_repo_dir, '/Source_files/failsafe_ltla_boundary.geojson'), mode = 'wb')

if(exists('ltla_boundaries') == FALSE) {
  ltla_boundaries <- geojson_read(paste0(github_repo_dir, '/Source_files/failsafe_ltla_boundary.geojson'),  what = "sp")
}

# Positivity ####
positivity_ltla <- read_csv('https://api.coronavirus.data.gov.uk/v2/data?areaType=ltla&metric=uniquePeopleTestedBySpecimenDateRollingSum&metric=uniqueCasePositivityBySpecimenDateRollingSum&metric=newLFDTestsBySpecimenDate&format=csv') %>%
  filter(substr(areaCode, 1,1) == 'E') %>%
  select(-areaType) %>%
  rename(Name = areaName,
         Code = areaCode,
         Date = date)

positivity_utla <- read_csv('https://api.coronavirus.data.gov.uk/v2/data?areaType=utla&metric=uniquePeopleTestedBySpecimenDateRollingSum&metric=uniqueCasePositivityBySpecimenDateRollingSum&metric=newLFDTestsBySpecimenDate&format=csv') %>%
  filter(substr(areaCode, 1,1) == 'E') %>%
  select(-areaType) %>%
  rename(Name = areaName,
         Code = areaCode,
         Date = date)

positivity_region <- read_csv('https://api.coronavirus.data.gov.uk/v2/data?areaType=region&metric=uniquePeopleTestedBySpecimenDateRollingSum&metric=uniqueCasePositivityBySpecimenDateRollingSum&metric=newLFDTestsBySpecimenDate&format=csv') %>%
  filter(substr(areaCode, 1,1) == 'E') %>%
  select(-areaType) %>%
  rename(Name = areaName,
         Code = areaCode,
         Date = date)

positivity_nation <- read_csv('https://api.coronavirus.data.gov.uk/v2/data?areaType=nation&metric=uniquePeopleTestedBySpecimenDateRollingSum&metric=uniqueCasePositivityBySpecimenDateRollingSum&metric=newLFDTestsBySpecimenDate&format=csv') %>%
  filter(substr(areaCode, 1,1) == 'E') %>%
  select(-areaType) %>%
  rename(Name = areaName,
         Code = areaCode,
         Date = date)

positivity_df <- positivity_ltla %>%
  bind_rows(positivity_utla) %>%
  bind_rows(positivity_region) %>%
  bind_rows(positivity_nation) %>%
  unique() %>%
  filter(Name %in% c('Adur', 'Arun', 'Chichester', 'Crawley', 'Horsham', 'Mid Sussex', 'Worthing', 'West Sussex', 'South East', 'England')) %>%
  mutate(Name = ifelse(Name == 'South East', 'South East region', Name)) %>%
  group_by(Name) %>%
  arrange(Name, Date) %>%
  mutate(LFD_7_day_tests = rollapplyr(newLFDTestsBySpecimenDate, 7, sum, align = 'right', partial = TRUE))

positivity_at_a_glance <- positivity_df %>%
  filter(Date == complete_date -1) %>%
  select(Name, uniquePeopleTestedBySpecimenDateRollingSum, uniqueCasePositivityBySpecimenDateRollingSum, LFD_7_day_tests) %>%
  rename(`Number of people receiving a PCR (Polymerase chain reaction) test` = uniquePeopleTestedBySpecimenDateRollingSum,
         `Positivity rate (weekly percentage of individuals tested who test positive for COVID-19)` = uniqueCasePositivityBySpecimenDateRollingSum,
         `Number of LFD tests conducted in last 7 days` = LFD_7_day_tests)

at_a_glance_all <- at_a_glance_1 %>%
  filter(Age == 'All ages') %>%
  select(Name, Rolling_7_day_new_cases ,Rolling_7_day_new_cases_per_100000) %>%
  mutate(Rolling_7_day_new_cases_per_100000 = round(Rolling_7_day_new_cases_per_100000, 1)) %>%
  rename(Cases = Rolling_7_day_new_cases,
         `Rate per 100,000` = Rolling_7_day_new_cases_per_100000)

at_a_glance_60s <- at_a_glance_1 %>%
  filter(Age == '60+ years') %>%
  select(Name, Rolling_7_day_new_cases_per_100000) %>%
  mutate(Rolling_7_day_new_cases_per_100000 = round(Rolling_7_day_new_cases_per_100000, 1)) %>%
  rename(`Rate of cases per 100,000 for people aged 60 and over` = Rolling_7_day_new_cases_per_100000)

public_latest_rates_table <- at_a_glance_all %>%
  left_join(positivity_at_a_glance, by = 'Name') %>%
  left_join(at_a_glance_60s, by = 'Name') %>%
  select(Name, Cases, `Rate per 100,000`,  `Positivity rate (weekly percentage of individuals tested who test positive for COVID-19)`, `Number of people receiving a PCR (Polymerase chain reaction) test`, `Rate of cases per 100,000 for people aged 60 and over`)

public_latest_rates_table %>%
  mutate(Name = factor(Name, levels = c('Adur', 'Arun', 'Chichester', 'Crawley', 'Horsham', 'Mid Sussex', 'Worthing', 'West Sussex', 'South East region', 'England'))) %>%
  arrange(Name) %>%
  mutate(Date = complete_date) %>%
  write.csv(., paste0(output_directory_x, '/public_latest_rates_table.csv'), row.names = FALSE)

# The number of people who received a PCR test in the previous 7 days, and the percentage of people who received a PCR test in the previous 7 days, who had at least one positive COVID-19 PCR test result.

# Polymerase chain reaction (PCR) tests are lab-based and test for the presence of SARS-CoV-2 virus. This data shows the number of people who received a PCR test in the previous 7 days, and the percentage of people who received a PCR test in the previous 7 days who had at least one positive PCR test result.

# If a person has had more than one test result in the 7-day period, they are only counted once. If any of their tests in that period were positive, they count as one person with a positive test result. The positivity percentage is the number of people with a positive test result, divided by the number of people tested and multiplied by 100.

# Individuals tested more than once in the period are only counted once in the denominator, and those with more than one positive test result in the period are only included once in the numerator.

positivity_worked <- positivity_df %>%
  rename(Seven_day_PCR_positivity = uniqueCasePositivityBySpecimenDateRollingSum,
         Seven_day_PCR_tested_individuals = uniquePeopleTestedBySpecimenDateRollingSum) %>%
  group_by(Name) %>%
  arrange(Name, Date) %>%
  mutate(Perc_change_on_individuals_tested = round((Seven_day_PCR_tested_individuals - lag(Seven_day_PCR_tested_individuals, 7))/ lag(Seven_day_PCR_tested_individuals, 7), 2))  %>%
  mutate(Perc_change_on_individuals_tested = ifelse(Perc_change_on_individuals_tested == Inf, 1, Perc_change_on_individuals_tested)) %>%
  mutate(Perc_change_on_individuals_tested = replace_na(Perc_change_on_individuals_tested, 0)) %>%
  mutate(Name = factor(Name, levels = c('Adur', 'Arun', 'Chichester', 'Crawley', 'Horsham', 'Mid Sussex', 'Worthing', 'West Sussex', 'South East region', 'England'))) %>%
  arrange(Name, Date) %>%
  filter(Date >= max(Date) - 90)

# positivity_worked %>%
#   filter(Date == complete_date)

positivity_worked %>%
  select(!Code) %>%
  toJSON() %>%
  write_lines(paste0(output_directory_x, '/positivity_df.json'))

library(lemon)

data_dummy_positivity_worked <- positivity_worked %>%
  rename(dummy_name = Name)

positivity_worked_plotted <- ggplot(positivity_worked,
                                    aes(x = Date,
                                        y = Seven_day_PCR_positivity,
                                        colour = Name)) +
  geom_line(data = data_dummy_positivity_worked,
            aes(x = Date,
                y = Seven_day_PCR_positivity,
                group = dummy_name),
            colour = '#dbdbdb',
            size = .6) +
  geom_line(size = .9) +
  geom_point(size = .5) +
  ph_theme() +
  theme(axis.text.x = element_text(angle = 90, size = 6),
        legend.position = 'none') +
  scale_y_continuous(labels = label_comma(accuracy = 1, suffix = '%'),
                     limits = c(0,30),
                     breaks = seq(0, 30, 5)) +
  scale_x_date(date_labels = "%b %d",
               breaks = seq.Date(max(positivity_worked$Date) -(52*7), max(positivity_worked$Date), by = 7),
               limits = c(min(positivity_worked$Date), max(positivity_worked$Date) + 7),
               expand = c(0.01,1)) +
  labs(x = '',
       y = '7-day rolling PCR case positivity rate',
       title = paste0('7-day PCR Case positivity rate for Covid-19 in the last 90 days; West Sussex, South East region, and England'),
       subtitle = paste0('Pillar 1 and 2 combined; data as at ', format(last_date, '%d %B %Y')))  +
  theme(axis.text.x = element_text(size = 8)) +
  facet_rep_wrap(. ~ Name, ncol = 4, repeat.tick.labels = TRUE)

png(paste0(output_directory_x, '/Figure_7_day_rolling_positivity_rates_latest_faceted.png'),
    width = 1480,
    height = 880,
    res = 130)
print(positivity_worked_plotted)
dev.off()

# Vaccine time series counts ####

lads <- c("E07000223", "E07000224","E07000225", "E07000226", "E07000227", "E07000228","E07000229")

## build the required structure for the api
# {"date":"date","areaCode":"areaCode","areaName":"areaName","newCasesBySpecimenDateRollingRate":"newCasesBySpecimenDateRollingRate","newCasesBySpecimenDateRollingSum":"newCasesBySpecimenDateRollingSum","uniqueCasePositivityBySpecimenDateRollingSum":"uniqueCasePositivityBySpecimenDateRollingSum","uniquePeopleTestedBySpecimenDateRollingSum":"uniquePeopleTestedBySpecimenDateRollingSum"}

###### read the data
england <- 'https://api.coronavirus.data.gov.uk/v1/data?filters=areaType=nation;areaName=england&structure={"date":"date","areaCode":"areaCode","areaName":"areaName","vaccinationsAgeDemographics":"vaccinationsAgeDemographics"}'

westsussex <- 'https://api.coronavirus.data.gov.uk/v1/data?filters=areaType=utla;areaCode=E10000032&structure={"date":"date","areaCode":"areaCode","areaName":"areaName","vaccinationsAgeDemographics":"vaccinationsAgeDemographics"}'

southeast <- 'https://api.coronavirus.data.gov.uk/v1/data?filters=areaType=region;areaCode=E12000008&structure={"date":"date","areaCode":"areaCode","areaName":"areaName","vaccinationsAgeDemographics":"vaccinationsAgeDemographics"}'

# create api calls for lads:
baseurl <- 'https://api.coronavirus.data.gov.uk/v1/data?filters=areaType=ltla;areaCode=HEREPLEASE&structure={"date":"date","areaCode":"areaCode","areaName":"areaName","vaccinationsAgeDemographics":"vaccinationsAgeDemographics"}'

ltlas <- character()

for(i in lads) {
  new_string <- sub(pattern = "HEREPLEASE", replacement = i, x = baseurl)
  ltlas <- c(ltlas, new_string)
}

# list of apis
urls <- c(england, southeast, westsussex, ltlas)

# empty list
dflist <- list()

# api calls for each geography into list
for(i in urls) {
  response <- httr::GET(url = i)

  if (response$status_code >= 400) {
    err_msg = httr::http_status(response)
    stop(err_msg)
  }
  json_text <- content(response, "text")
  data <- jsonlite::fromJSON(json_text)

  df <- as.data.frame(data$data)
  df$apisource <- i

  dflist[[i]] <- df
}

vaccine_age_df <- bind_rows(dflist) %>%
  unnest(vaccinationsAgeDemographics) %>% 
  rename(Date = date,
         Code = areaCode,
         Name = areaName,
         Age_group = age,
         Denominator = VaccineRegisterPopulationByVaccinationDate,
         Cumulative_dose_1 = cumPeopleVaccinatedFirstDoseByVaccinationDate,
         Dose_1 = newPeopleVaccinatedFirstDoseByVaccinationDate, 
         Cumulative_dose_2 = cumPeopleVaccinatedSecondDoseByVaccinationDate,
         Dose_2 = newPeopleVaccinatedSecondDoseByVaccinationDate,
         Cumulative_dose_3_or_booster = cumPeopleVaccinatedThirdInjectionByVaccinationDate,
         Dose_3_or_booster = newPeopleVaccinatedThirdInjectionByVaccinationDate) %>% 
  select(!c(apisource, Code)) %>% 
  mutate(Date = as.Date(Date)) %>% 
  mutate(Age_group = factor(paste0(gsub('_', '-', Age_group), ' years'), levels = c('12-15 years','16-17 years',"18-24 years", "25-29 years", "30-34 years", "35-39 years", "40-44 years", "45-49 years", "50-54 years", "55-59 years", "60-64 years", "65-69 years", "70-74 years", "75-79 years", "80-84 years", "85-89 years", "90+ years"))) %>% 
  group_by(Name, Age_group) %>% 
  arrange(Date) %>% 
  mutate(Seven_day_sum_dose_1 = round(rollapplyr(Dose_1, 7, sum, align = 'right', partial = TRUE),0)) %>%  
  mutate(Seven_day_sum_dose_2 = round(rollapplyr(Dose_2, 7, sum, align = 'right', partial = TRUE),0))  %>% 
  mutate(Seven_day_sum_dose_3_or_booster = round(rollapplyr(Dose_3_or_booster, 7, sum, align = 'right', partial = TRUE),0)) %>% 
  select(Date, Name, Age_group, Denominator, Dose_1, Seven_day_sum_dose_1, Cumulative_dose_1, Dose_2, Seven_day_sum_dose_2, Cumulative_dose_2, Dose_3_or_booster, Seven_day_sum_dose_3_or_booster, Cumulative_dose_3_or_booster)

# create vaccine time series by dose for 18+ by ltla ####

vaccine_ts_18_plus_df <- vaccine_age_df %>%
  filter(Age_group %in% c("18-24 years", "25-29 years", "30-34 years", "35-39 years", "40-44 years", "45-49 years", "50-54 years", "55-59 years", "60-64 years", "65-69 years", "70-74 years", "75-79 years", "80-84 years", "85-89 years", "90+ years")) %>% 
  group_by(Date, Name) %>%
  summarise(Age_group = '18 and over',
            Dose_1 = sum(Dose_1, na.rm = TRUE),
            Dose_2 = sum(Dose_2, na.rm = TRUE),
            Dose_3_or_booster = sum(Dose_3_or_booster, na.rm = TRUE),
            Denominator = sum(Denominator, na.rm = TRUE)) %>%
  group_by(Name) %>%
  arrange(Date) %>%
  mutate(Cumulative_dose_1 = cumsum(Dose_1),
         Cumulative_dose_2 = cumsum(Dose_2),
         Cumulative_dose_3_or_booster = cumsum(Dose_3_or_booster)) %>%
  mutate(Seven_day_sum_dose_1 = round(rollapplyr(Dose_1, 7, sum, align = 'right', partial = TRUE),0)) %>%
  mutate(Seven_day_sum_dose_2 = round(rollapplyr(Dose_2, 7, sum, align = 'right', partial = TRUE),0)) %>%
  mutate(Seven_day_sum_dose_3_or_booster = round(rollapplyr(Dose_3_or_booster, 7, sum, align = 'right', partial = TRUE),0)) %>%
  mutate(Rolling_age_specific_first_dose_rate_per_100000 = pois.exact(Seven_day_sum_dose_1, Denominator)[[3]]*100000) %>%
  mutate(Cumulative_age_specific_first_dose_rate_per_100000 = pois.exact(Cumulative_dose_1, Denominator)[[3]]*100000)  %>%
  mutate(Rolling_age_specific_second_dose_rate_per_100000 = pois.exact(Seven_day_sum_dose_2, Denominator)[[3]]*100000) %>%
  mutate(Cumulative_age_specific_second_dose_rate_per_100000 = pois.exact(Cumulative_dose_2, Denominator)[[3]]*100000) %>%
  mutate(Rolling_age_specific_third_or_booster_dose_rate_per_100000 = pois.exact(Seven_day_sum_dose_3_or_booster, Denominator)[[3]]*100000) %>%
  mutate(Cumulative_age_specific_third_or_booster_dose_rate_per_100000 = pois.exact(Cumulative_dose_3_or_booster, Denominator)[[3]]*100000)

vaccine_ts_18_plus_df %>%
  mutate(Date_label = format(Date, '%d %b %y')) %>%
  select(Date_label, Name, Seven_day_sum_dose_1, Seven_day_sum_dose_2, Seven_day_sum_dose_3_or_booster, Rolling_age_specific_first_dose_rate_per_100000, Rolling_age_specific_second_dose_rate_per_100000, Rolling_age_specific_third_or_booster_dose_rate_per_100000) %>% 
  toJSON() %>%
  write_lines(paste0(output_directory_x, '/vaccine_18_plus_by_dose_daily.json'))
  
# create vaccine at a glance (proportion 12-15 (first dose), 16+ first and second, 18+ first, second, booster) by ltla

vaccine_ts_12_plus_df <- vaccine_age_df %>%
  filter(Age_group %in% c('12-15 years', '16-17 years',"18-24 years", "25-29 years", "30-34 years", "35-39 years", "40-44 years", "45-49 years", "50-54 years", "55-59 years", "60-64 years", "65-69 years", "70-74 years", "75-79 years", "80-84 years", "85-89 years", "90+ years")) %>% 
  group_by(Date, Name) %>%
  summarise(Age_group = '12 and over',
            Dose_1 = sum(Dose_1, na.rm = TRUE),
            Dose_2 = sum(Dose_2, na.rm = TRUE),
            Dose_3_or_booster = sum(Dose_3_or_booster, na.rm = TRUE),
            Denominator = sum(Denominator, na.rm = TRUE)) %>%
  group_by(Name) %>%
  arrange(Date) %>%
  mutate(Cumulative_dose_1 = cumsum(Dose_1),
         Cumulative_dose_2 = cumsum(Dose_2)) %>%
  mutate(Seven_day_sum_dose_1 = round(rollapplyr(Dose_1, 7, sum, align = 'right', partial = TRUE),0)) %>%
  mutate(Seven_day_sum_dose_2 = round(rollapplyr(Dose_2, 7, sum, align = 'right', partial = TRUE),0)) %>%
  mutate(Seven_day_sum_dose_3_or_booster = round(rollapplyr(Dose_3_or_booster, 7, sum, align = 'right', partial = TRUE),0)) %>%
  mutate(Rolling_age_specific_first_dose_rate_per_100000 = pois.exact(Seven_day_sum_dose_1, Denominator)[[3]]*100000) %>%
  mutate(Cumulative_age_specific_first_dose_rate_per_100000 = pois.exact(Cumulative_dose_1, Denominator)[[3]]*100000)  %>%
  mutate(Rolling_age_specific_second_dose_rate_per_100000 = pois.exact(Seven_day_sum_dose_2, Denominator)[[3]]*100000) %>%
  mutate(Cumulative_age_specific_second_dose_rate_per_100000 = pois.exact(Cumulative_dose_2, Denominator)[[3]]*100000)

wsx_doses_seven_days <- vaccine_ts_12_plus_df %>%
  filter(Name == 'West Sussex') %>% 
  filter(Date == max(Date))

wsx_first_doses_seven_days <- as.numeric(wsx_doses_seven_days$Seven_day_sum_dose_1)
wsx_second_doses_seven_days <- as.numeric(wsx_doses_seven_days$Seven_day_sum_dose_2)
wsx_third_doses_seven_days <- as.numeric(wsx_doses_seven_days$Seven_day_sum_dose_3_or_booster)

vaccine_ts_12_plus_df <- vaccine_ts_12_plus_df %>% 
  select(!c(Dose_3_or_booster, Seven_day_sum_dose_3_or_booster))

vaccine_ts_12_17_df <- vaccine_age_df %>%
  filter(Age_group %in% c('12-15 years', '16-17 years')) %>% 
  group_by(Date, Name) %>%
  summarise(Age_group = '12-17 years',
            Dose_1 = sum(Dose_1, na.rm = TRUE),
            Dose_2 = sum(Dose_2, na.rm = TRUE),
            Denominator = sum(Denominator, na.rm = TRUE)) %>%
  group_by(Name) %>%
  arrange(Date) %>%
  mutate(Cumulative_dose_1 = cumsum(Dose_1),
         Cumulative_dose_2 = cumsum(Dose_2)) %>%
  mutate(Seven_day_sum_dose_1 = round(rollapplyr(Dose_1, 7, sum, align = 'right', partial = TRUE),0)) %>%
  mutate(Seven_day_sum_dose_2 = round(rollapplyr(Dose_2, 7, sum, align = 'right', partial = TRUE),0)) %>%
  mutate(Rolling_age_specific_first_dose_rate_per_100000 = pois.exact(Seven_day_sum_dose_1, Denominator)[[3]]*100000) %>%
  mutate(Cumulative_age_specific_first_dose_rate_per_100000 = pois.exact(Cumulative_dose_1, Denominator)[[3]]*100000)  %>%
  mutate(Rolling_age_specific_second_dose_rate_per_100000 = pois.exact(Seven_day_sum_dose_2, Denominator)[[3]]*100000) %>%
  mutate(Cumulative_age_specific_second_dose_rate_per_100000 = pois.exact(Cumulative_dose_2, Denominator)[[3]]*100000)

vaccine_df_ltla <- vaccine_ts_12_17_df %>%
  bind_rows(vaccine_ts_12_plus_df) %>% 
  bind_rows(vaccine_ts_18_plus_df) %>% 
  filter(Date == max(Date, na.rm = TRUE)) %>% 
  select(Name, Age_group, Cumulative_dose_1, Cumulative_dose_2, Cumulative_dose_3_or_booster, Denominator) %>% 
  mutate(Proportion_first_dose = Cumulative_dose_1 / Denominator,
         Proportion_second_dose = Cumulative_dose_2 / Denominator,
         Proportion_third_dose = Cumulative_dose_3_or_booster / Denominator) %>% 
  mutate(Name = ifelse(Name == 'South East', 'South East region', Name))

vaccine_df_ltla %>%
  toJSON() %>%
  write_lines(paste0(output_directory_x, '/vaccine_at_a_glance.json'))

vac_info_df_wsx_12_plus <- vaccine_df_ltla %>%
  filter(Name == 'West Sussex') %>%
  filter(Age_group == '12 and over')

wsx_12_plus_first_doses <- as.numeric(vac_info_df_wsx_12_plus$Cumulative_dose_1)
wsx_12_plus_second_doses <- as.numeric(vac_info_df_wsx_12_plus$Cumulative_dose_2)
wsx_12_plus_denominator <- as.numeric(vac_info_df_wsx_12_plus$Denominator)
wsx_12_plus_first_doses_proportion <- as.numeric(vac_info_df_wsx_12_plus$Cumulative_dose_1 / vac_info_df_wsx_12_plus$Denominator)
wsx_12_plus_second_doses_proportion <- as.numeric(vac_info_df_wsx_12_plus$Cumulative_dose_2 / vac_info_df_wsx_12_plus$Denominator)

wsx_12_plus_first_dose_only <- wsx_12_plus_first_doses - wsx_12_plus_second_doses
wsx_12_plus_unvaccinated <- wsx_12_plus_denominator - wsx_12_plus_first_doses

vac_info_df_wsx_18_plus <- vaccine_df_ltla %>%
  filter(Name == 'West Sussex') %>%
  filter(Age_group == '18 and over')

wsx_18_plus_first_doses <- as.numeric(vac_info_df_wsx_18_plus$Cumulative_dose_1)
wsx_18_plus_second_doses <- as.numeric(vac_info_df_wsx_18_plus$Cumulative_dose_2) - as.numeric(vac_info_df_wsx_18_plus$Cumulative_dose_3_or_booster)
wsx_18_plus_third_doses <- as.numeric(vac_info_df_wsx_18_plus$Cumulative_dose_3_or_booster)
wsx_18_plus_denominator <- as.numeric(vac_info_df_wsx_18_plus$Denominator)
wsx_18_plus_first_doses_proportion <- as.numeric(vac_info_df_wsx_18_plus$Cumulative_dose_1 / vac_info_df_wsx_18_plus$Denominator)
wsx_18_plus_second_doses_proportion <- as.numeric(vac_info_df_wsx_18_plus$Cumulative_dose_2 / vac_info_df_wsx_18_plus$Denominator)
wsx_18_plus_third_doses_proportion <- as.numeric(vac_info_df_wsx_18_plus$Cumulative_dose_3_or_booster / vac_info_df_wsx_18_plus$Denominator)

wsx_18_plus_first_dose_only <- as.numeric(vac_info_df_wsx_18_plus$Cumulative_dose_1) - as.numeric(vac_info_df_wsx_18_plus$Cumulative_dose_2)
wsx_18_plus_unvaccinated <- wsx_18_plus_denominator - wsx_18_plus_first_doses

wsx_18_plus_yet_to_boost <- wsx_18_plus_second_doses - wsx_18_plus_third_doses

vac_info_df_12_plus <- vaccine_df_ltla %>%
  filter(Name == 'West Sussex') %>% 
  filter(Age_group == '12 and over') %>%
  mutate(`Not vaccinated` = Denominator - Cumulative_dose_1) %>%
  mutate(`Received first dose only` = Cumulative_dose_1 - Cumulative_dose_2) %>%
  rename('Received two doses' = Cumulative_dose_2) %>%
  select(Name, Age_group, 'Received two doses', 'Received first dose only', 'Not vaccinated') %>%
  pivot_longer(cols = c('Received two doses','Received first dose only', 'Not vaccinated'),
               names_to = 'Status',
               values_to = 'People') %>%
  mutate(Status = factor(Status, levels = rev(c('Received two doses','Received first dose only', 'Not vaccinated'))))

vac_info_df_12_plus <- vac_info_df_12_plus %>%
  group_by(Name, Age_group) %>%
  arrange(Status) %>%
  mutate(cumulative = cumsum(People),
         pos = lag(cumulative) + People/2) %>%
  mutate(pos = ifelse(is.na(pos), cumulative/2, pos)) %>%
  mutate(pos = sum(People) - pos) %>%
  mutate(Status_label = factor(paste0(Status, ' (', format(People, big.mark = ',', trim = TRUE), ')')))

vac_info_df_18_plus <- vaccine_df_ltla %>%
  filter(Name == 'West Sussex') %>% 
  filter(Age_group == '18 and over') %>%
  mutate(`Not vaccinated` = Denominator - Cumulative_dose_1) %>%
  mutate(`Received first dose only` = Cumulative_dose_1 - Cumulative_dose_2) %>%
  mutate(`Received two doses` = Cumulative_dose_2 - Cumulative_dose_3_or_booster) %>% 
  select(Name, Age_group, Cumulative_dose_3_or_booster, 'Received first dose only', 'Received two doses', 'Not vaccinated') %>%
  rename('Received third or booster dose' = Cumulative_dose_3_or_booster) %>%
  pivot_longer(cols = c('Received third or booster dose', 'Received two doses', 'Received first dose only', 'Not vaccinated'),
               names_to = 'Status',
               values_to = 'People') %>%
  mutate(Status = factor(Status, levels = rev(c('Received third or booster dose', 'Received two doses','Received first dose only', 'Not vaccinated'))))

vac_info_df_18_plus <- vac_info_df_18_plus %>%
  group_by(Name, Age_group) %>%
  mutate(cumulative = cumsum(People),
         pos = lag(cumulative) + People/2) %>%
  mutate(pos = ifelse(is.na(pos), cumulative/2, pos)) %>%
  mutate(pos = sum(People) - pos)

cumulative_vaccine_df_ltla <- vaccine_df_ltla %>%
  # mutate(first_label = paste0(format(Dose_1, big.mark = ',', trim = TRUE), ' (', round((Dose_1 / Denominator)*100, 1), '%)')) %>%
  # mutate(second_label = paste0(format(Dose_2, big.mark = ',', trim = TRUE), ' (', round((Dose_2 / Denominator)*100, 1), '%)')) %>%
  mutate(first_label = paste0(round((Cumulative_dose_1 / Denominator)*100, 1), '%')) %>%
  mutate(second_label = paste0(round((Cumulative_dose_2 / Denominator)*100, 1), '%')) %>%
  filter(Age_group == '12 and over') %>%
  mutate(Name = ifelse(Name == 'South East', 'South East region', Name)) %>%
  select(Name, first_label, second_label)

Last_week_vac_date <- paste0(ordinal(as.numeric(format(max(vaccine_age_df$Date) - 7, '%d'))), format(max(vaccine_age_df$Date) - 7, ' %B'))
This_week_vac_date <- paste0(ordinal(as.numeric(format(max(vaccine_age_df$Date), '%d'))), format(max(vaccine_age_df$Date), ' %B'))

# Export image file ####

#install.packages('jpeg')
# library(jpeg)
wscc_logo <- readPNG(paste0(github_repo_dir, '/images/WSCC-Blue-negative.png'))

down_img <- readPNG(paste0(github_repo_dir, '/images/double-down-arrows green.png'))
up_img <- readPNG(paste0(github_repo_dir, '/images/double-up-arrow red.png'))
same_img <- readPNG(paste0(github_repo_dir, '/images/equals-sign.png'))

down_img_w <- readPNG(paste0(github_repo_dir, '/images/double-down-arrows white.png'))
up_img_w <- readPNG(paste0(github_repo_dir, '/images/double-up-arrow white.png'))
same_img_w <- readPNG(paste0(github_repo_dir, '/images/equals-sign white.png'))

# This will be the coordinate system for placing our objects in grid later
vplayout <- function(x,y)
  viewport(layout.pos.row = x, layout.pos.col = y)

# might have to wrap jpeg call arround the newpage
#
#install.packages('Rttf2pt1')
library(extrafont)
loadfonts( device = 'win') # NB - any macs running this code do not like this option

jpeg(paste0(output_directory_x, '/Daily_infographic_socials.jpg'),
     width = 9,
     height = 4,
     units = "in",
     # pointsize = 14,
     res = 150,
     quality = 100)

grid.newpage()
pushViewport(viewport(layout = grid.layout(30, 20)))

grid.rect(gp = gpar(fill = "#0071B6", col = "#0071B6"))

grid.rect(x = unit(0.04, "npc"),
          y = unit(0.9, "npc"),
          width = unit(.17, "npc"),
          height = unit(0.06, "npc"),
          just = "left",
          default.units = "npc",
          gp=gpar(fill = "#F8E821", col = "#F8E821"),
          draw = TRUE,
          vp = NULL)

grid.text('CORONAVIRUS',
          just = "left",
          x = unit(0.05, "npc"),
          y = unit(.9, "npc"),
          gp = gpar(col = "#0071B6",
                    fontsize = "12",
                    # fontfamily = 'Verdana',
                    fontface = "bold"))

grid.text('COVID-19 CASE UPDATE',
          just = "left",
          x = unit(0.04, "npc"),
          y = unit(0.82, "npc"),
          gp = gpar(col = "#ffffff",
                    fontsize = "30",
                    fontfamily = 'Bahnschrift',
                    fontface = 'bold'))

grid.roundrect(x = unit(0.04, "npc"),
               y = unit(0.61, "npc"),
               width = unit(0.2, "npc"),
               height = unit(0.24, "npc"),
               just = "left",
               default.units = "npc",
               gp=gpar(fill = "#ffffff",
                       col = "#ffffff"),
               vp = NULL)

grid.text('New confirmed cases',
          just = "centre",
          x = unit(0.14, "npc"),
          y = unit(0.7, "npc"),
          gp = gpar(col = "#000000",
                    fontsize = "10",
                    # fontfamily = 'Verdana',
                    fontface = 'bold'))

grid.text('this week*',
          just = "centre",
          x = unit(0.14, "npc"),
          y = unit(0.66, "npc"),
          gp = gpar(col = "#000000",
                    fontsize = "10",
                    # fontfamily = 'Verdana',
                    fontface = 'bold'))

grid.text(format(cases_this_week$Rolling_7_day_new_cases, big.mark = ','),
          just = "centre",
          x = unit(0.14, "npc"),
          y = unit(0.58, "npc"),
          gp = gpar(col = "#000000",
                    fontsize = "35",
                    fontfamily = 'Bahnschrift',
                    fontface = 'bold'))

grid.roundrect(x = unit(0.28, "npc"),
               y = unit(0.61, "npc"),
               width = unit(.2, "npc"),
               height = unit(0.24, "npc"),
               just = "left",
               default.units = "npc",
               gp=gpar(fill = "#ffffff",
                       col = "#ffffff"),
               vp = NULL)

grid.text('Change since',
          just = "centre",
          x = unit(0.38, "npc"),
          y = unit(0.7, "npc"),
          gp = gpar(col = "#000000",
                    fontsize = "10",
                    # fontfamily = 'Verdana',
                    fontface = 'bold'))

grid.text('last week',
          just = "centre",
          x = unit(0.38, "npc"),
          y = unit(0.66, "npc"),
          gp = gpar(col = "#000000",
                    fontsize = "10",
                    # fontfamily = 'Verdana',
                    fontface = 'bold'))

if(change_direction_between_weeks == 'DOWN'){
  grid.raster(down_img,
              x = unit(0.29, "npc"),
              y = unit(0.58, "npc"),
              just = 'left',
              width = .04)

  grid.text(format(change_between_weeks, big.mark = ','),
            just = "left",
            x = unit(0.335, "npc"),
            y = unit(0.58, "npc"),
            gp = gpar(col = "#000000",
                      fontsize = "32",
                      fontfamily = 'Bahnschrift',
                      fontface = 'bold'))

  grid.text('Falling',
            just = "left",
            x = unit(0.29, "npc"),
            y = unit(0.52, "npc"),
            gp = gpar(col = "#000000",
                      fontsize = "8",
                      fontfamily = 'Bahnschrift',
                      fontface = 'bold'))
}

if(change_direction_between_weeks == 'UP'){
  grid.raster(up_img,
              x = unit(0.29, "npc"),
              y = unit(0.58, "npc"),
              just = 'left',
              width = .04)

  grid.text(paste0('+',format(change_between_weeks, big.mark = ',')),
            just = "left",
            x = unit(0.335, "npc"),
            y = unit(0.58, "npc"),
            gp = gpar(col = "#000000",
                      fontsize = "32",
                      fontfamily = 'Bahnschrift',
                      fontface = 'bold'))

  grid.text('Rising',
            just = "left",
            x = unit(0.29, "npc"),
            y = unit(0.52, "npc"),
            gp = gpar(col = "#000000",
                      fontsize = "8",
                      fontfamily = 'Bahnschrift',
                      fontface = 'bold'))
}

if(change_direction_between_weeks == 'SAME'){
  grid.raster(same_img,
              x = unit(0.29, "npc"),
              y = unit(0.58, "npc"),
              just = 'left',
              width = .04)

  grid.text('No change\nin cases',
            just = "left",
            x = unit(0.34, "npc"),
            y = unit(0.58, "npc"),
            gp = gpar(col = "#000000",
                      fontsize = "16",
                      fontfamily = 'Bahnschrift',
                      fontface = 'bold'))
}

grid.text(paste0('In the seven days to ', format(complete_date, '%A '), ordinal(as.numeric(format(complete_date, '%d'))), format(complete_date, ' %B'), ',  there were'),
          just = "left",
          x = unit(0.04, "npc"),
          y = unit(0.45, "npc"),
          gp = gpar(col = "#ffffff",
                    fontsize = "9",
                    # fontfamily = 'Verdana',
                    fontface = 'bold'))

grid.text(paste0(format(cases_this_week$Rolling_7_day_new_cases, big.mark = ','), ' new confirmed COVID-19 cases in West Sussex.'),
          just = "left",
          x = unit(0.04, "npc"),
          y = unit(0.41, "npc"),
          gp = gpar(col = "#ffffff",
                    fontsize = "9",
                    # fontfamily = 'Verdana',
                    fontface = 'bold'))

grid.text(ifelse(change_direction_between_weeks == 'SAME', 'Confirmed cases are the same as in the previous week', ifelse(change_direction_between_weeks == 'DOWN', 'Confirmed cases are falling compared to the previous week', ifelse(change_direction_between_weeks == 'UP', 'Confirmed cases are rising compared to the previous week', NA))),
          just = "left",
          x = unit(0.04, "npc"),
          y = unit(0.34, "npc"),
          gp = gpar(col = "#ffffff",
                    fontsize = "9",
                    # fontfamily = 'Verdana',
                    fontface = 'bold'))

grid.text(paste0('(', format(cases_last_week$Rolling_7_day_new_cases, big.mark = ','), ' cases in the seven days to the ', ordinal(as.numeric(format(complete_date - 7, '%d'))), format(complete_date - 7, ' %B %Y'), ').'),
          just = "left",
          x = unit(0.04, "npc"),
          y = unit(0.3, "npc"),
          gp = gpar(col = "#ffffff",
                    fontsize = "9",
                    # fontfamily = 'Verdana',
                    fontface = 'bold'))

grid.text(paste0('Since the start of the COVID-19 pandemic, there have been'),
          just = "left",
          x = unit(0.04, "npc"),
          y = unit(0.22, "npc"),
          gp = gpar(col = "#ffffff",
                    fontsize = "9",
                    # fontfamily = 'Verdana',
                    fontface = 'bold'))

grid.text(paste0(format(total_so_far$Cumulative_cases, big.mark = ','), ' confirmed cases in West Sussex (as of ', ordinal(as.numeric(format(last_date, '%d'))), format(last_date, ' %B'),').'),
          just = "left",
          x = unit(0.04, "npc"),
          y = unit(0.18, "npc"),
          gp = gpar(col = "#ffffff",
                    fontsize = "9",
                    # fontfamily = 'Verdana',
                    fontface = 'bold'))

# Table ####

text_colour <- '#ffffff'

text_colour <- '#000000'
grid.rect(x = unit(0.54, "npc"),
          y = unit(0.48, "npc"),
          width = unit(.44, "npc"),
          height = unit(0.5, "npc"),
          just = "left",
          default.units = "npc",
          gp=gpar(fill = "#ffffff", col = "#ffffff"),
          draw = TRUE,
          vp = NULL)

latest_table <- p12_test_summary %>%
  select(Name, Rolling_7_day_new_cases, Rolling_7_day_new_cases_per_100000, Change_direction) %>%
  mutate(Name = factor(Name, levels = c('Adur', 'Arun', 'Chichester', 'Crawley', 'Horsham', 'Mid Sussex', 'Worthing', 'West Sussex', 'South East region', 'England'))) %>%
  arrange(Name)

# seven_day_heading <- paste0('Cases in the 7 days to ', format(complete_date, '%d/%m/%y'))

grid.text('Area',
          just = "left",
          x = unit(0.55, "npc"),
          y = unit(0.71, "npc"),
          gp = gpar(col = text_colour,
                    fontsize = "9",
                    # fontfamily = 'Verdana',
                    fontface = 'bold'))


grid.text('Cases in the seven',
          just = "right",
          x = unit(0.79, "npc"),
          y = unit(0.71, "npc"),
          gp = gpar(col = text_colour,
                    fontsize = "9",
                    # fontfamily = 'Verdana',
                    fontface = 'bold'))

grid.text(paste0('days to ', format(complete_date, '%d/%m/%y')),
          just = "right",
          x = unit(0.79, "npc"),
          y = unit(0.67, "npc"),
          gp = gpar(col = text_colour,
                    fontsize = "9",
                    # fontfamily = 'Verdana',
                    fontface = 'bold'))

grid.text('Rate per 100,000',
          just = "right",
          x = unit(0.98, "npc"),
          y = unit(0.71, "npc"),
          gp = gpar(col = text_colour,
                    fontsize = "9",
                    # fontfamily = 'Verdana',
                    fontface = 'bold'))

grid.text('population (all ages)',
          just = "right",
          x = unit(0.98, "npc"),
          y = unit(0.67, "npc"),
          gp = gpar(col = text_colour,
                    fontsize = "9",
                    # fontfamily = 'Verdana',
                    fontface = 'bold'))

areas_to_loop <- c('Adur', 'Arun', 'Chichester', 'Crawley', 'Horsham', 'Mid Sussex', 'Worthing', 'West Sussex')

for(i in 1:length(areas_to_loop)){

  area_x <- areas_to_loop[i]

  seven_day_area_x <- latest_table %>%
    filter(Name == area_x)

  interval_pos <- .05

  grid.text(area_x,
            just = "left",
            x = unit(0.55, "npc"),
            y = unit(0.66 - interval_pos * i, "npc"),
            gp = gpar(col = text_colour,
                      # fontfamily = 'Verdana',
                      fontsize = "9"))

  grid.text(format(seven_day_area_x$Rolling_7_day_new_cases, big.mark = ',', trim = TRUE),
            just = "right",
            x = unit(0.79 - .03, "npc"),
            y = unit(0.66 - interval_pos * i, "npc"),
            gp = gpar(col = text_colour,
                      # fontfamily = 'Verdana',
                      fontsize = "9"))


  if(seven_day_area_x$Change_direction == 'Up'){
    grid.raster(up_img,
                x = unit(0.79, "npc"),
                y = unit(0.66 - interval_pos * i, "npc"),
                just = 'right',
                width = .018)
  }

  if(seven_day_area_x$Change_direction == 'Down'){
    grid.raster(down_img,
                x = unit(0.79, "npc"),
                y = unit(0.66 - interval_pos * i, "npc"),
                just = 'right',
                width = .018)
  }

  if(seven_day_area_x$Change_direction == 'Same'){
    grid.raster(same_img,
                x = unit(0.79, "npc"),
                y = unit(0.66 - interval_pos * i, "npc"),
                just = 'right',
                width = .018)
  }


  grid.text(round(seven_day_area_x$Rolling_7_day_new_cases_per_100000, 1),
            just = "right",
            x = unit(0.98, "npc"),
            y = unit(0.66 - interval_pos * i, "npc"),
            gp = gpar(col = text_colour,
                      # fontfamily = 'Verdana',
                      fontsize = "9"))

}

grid.lines(x = c(0.54,0.98),
           y = 0.645,
           default.units = "npc",
           gp = gpar(col = text_colour,
                     # lty = "dotted",
                     lwd = 1.2))

grid.lines(x = c(0.54,0.98),
           y = 0.73,
           default.units = "npc",
           gp = gpar(col = text_colour,
                     # lty = "dotted",
                     lwd = 1.2))

grid.lines(x = c(0.54,0.98),
           y = 0.285,
           default.units = "npc",
           gp = gpar(col = text_colour,
                     # lty = "dotted",
                     lwd = 1.2))

grid.lines(x = c(0.54,0.98),
           y = 0.225,
           default.units = "npc",
           gp = gpar(col = text_colour,
                     # lty = "dotted",
                     lwd = 1.2))
# Banner bottom ####

grid.rect(x = unit(0.5, "npc"),
          y = unit(0.05, "npc"),
          width = unit(1, "npc"),
          height = unit(0.15, "npc"),
          just = "centre",
          default.units = "npc",
          gp=gpar(fill = "#F8E821",
                  col = "#F8E821"),
          draw = TRUE,
          vp = NULL)

grid.text('PUBLICATION DATE:',
          just = "left",
          x = unit(0.04, "npc"),
          y = unit(0.07, "npc"),
          gp = gpar(col = "#000000",
                    fontsize = "14",
                    fontfamily = 'Bahnschrift',
                    fontface = 'bold'))

grid.text(paste0(ordinal(as.numeric(format(last_date + 1 , '%d'))), format(last_date +1, ' %B %Y')),
          just = "left",
          x = unit(0.27, "npc"),
          y = unit(0.07, "npc"),
          gp = gpar(col = "#0071B6",
                    fontsize = "16",
                    # fontfamily = 'Verdana',
                    fontface = 'bold'))

grid.text(paste0('*We measure change by looking at cases in this week'),
          just = "left",
          x = unit(0.65, "npc"),
          y = unit(0.098, "npc"),
          gp = gpar(col = "#000000",
                    # fontfamily = 'Verdana',
                    fontsize = "7"))

grid.text(paste0('(seven days to ', format(complete_date, '%A '), ordinal(as.numeric(format(complete_date, '%d'))), format(complete_date, ' %B %Y'), ') compared'),
          just = "left",
          x = unit(0.65, "npc"),
          y = unit(0.073, "npc"),
          gp = gpar(col = "#000000",
                    # fontfamily = 'Verdana',
                    fontsize = "7"))

grid.text(paste0('to the last week (seven days to ', ordinal(as.numeric(format(complete_date - 7, '%d'))), format(complete_date - 7, ' %B %Y'), '). This is '),
          just = "left",
          x = unit(0.65, "npc"),
          y = unit(0.048, "npc"),
          gp = gpar(col = "#000000",
                    # fontfamily = 'Verdana',
                    fontsize = "7"))

grid.text(paste0('because data for more recent days are considered incomplete.'),
          just = "left",
          x = unit(0.65, "npc"),
          y = unit(0.025, "npc"),
          gp = gpar(col = "#000000",
                    # fontfamily = 'Verdana',
                    fontsize = "7"))

grid.raster(wscc_logo,
            y = unit(0.96, "npc"),
            x = unit(0.85, "npc"),
            vjust = 1,
            hjust = 0,
            width = .12)

# grid.text("http://jsna.westsussex.gov.uk",just = "left", x = unit(0.05, "npc"), y = unit(.05, "npc"), gp = gpar(col = "#1c8ccd", fontsize = "11", fontface = "bold"))

# grid.text("Infographic images designed by Freepik and OCHA from Flaticon",just = "centre", x = unit(0.5, "npc"), y = unit(.05, "npc"), gp = gpar(col = "#333333", fontsize = "8"))

dev.off()

# Vaccine plus cases ####

jpeg(paste0(output_directory_x, '/Daily_infographic_socials_vaccine_cases.jpg'),
     width = 9,
     height = 9,
     units = "in",
     res = 150,
     quality = 100)

grid.newpage()
pushViewport(viewport(layout = grid.layout(30, 30)))

grid.rect(gp = gpar(fill = "#0071B6", col = "#0071B6"))

grid.rect(x = unit(0.04, "npc"),
          y = unit(0.94, "npc"),
          width = unit(.17, "npc"),
          height = unit(0.04, "npc"),
          just = "left",
          default.units = "npc",
          gp=gpar(fill = "#F8E821", col = "#F8E821"),
          draw = TRUE,
          vp = NULL)

grid.text('CORONAVIRUS',
          just = "left",
          x = unit(0.05, "npc"),
          y = unit(.94, "npc"),
          gp = gpar(col = "#0071B6",
                    fontsize = "12",
                    # fontfamily = 'Verdana',
                    fontface = "bold"))

grid.text('COVID-19 UPDATE',
          just = "left",
          x = unit(0.04, "npc"),
          y = unit(0.88, "npc"),
          gp = gpar(col = "#ffffff",
                    fontsize = "30",
                    fontfamily = 'Bahnschrift',
                    fontface = 'bold'))

grid.roundrect(x = unit(0.04, "npc"),
               y = unit(0.78, "npc"),
               width = unit(0.22, "npc"),
               height = unit(0.13, "npc"),
               just = "left",
               default.units = "npc",
               gp=gpar(fill = "#ffffff",
                       col = "#ffffff"),
               vp = NULL)

grid.text('New confirmed cases',
          just = "centre",
          x = unit(0.15, "npc"),
          y = unit(0.82, "npc"),
          gp = gpar(col = "#000000",
                    fontsize = "10",
                    # fontfamily = 'Verdana',
                    fontface = 'bold'))

grid.text('this week*',
          just = "centre",
          x = unit(0.15, "npc"),
          y = unit(0.8, "npc"),
          gp = gpar(col = "#000000",
                    fontsize = "10",
                    # fontfamily = 'Verdana',
                    fontface = 'bold'))

grid.text(format(cases_this_week$Rolling_7_day_new_cases, big.mark = ','),
          just = "centre",
          x = unit(0.15, "npc"),
          y = unit(0.76, "npc"),
          gp = gpar(col = "#000000",
                    fontsize = "35",
                    fontfamily = 'Bahnschrift',
                    fontface = 'bold'))

grid.roundrect(x = unit(0.27, "npc"),
               y = unit(0.78, "npc"),
               width = unit(.22, "npc"),
               height = unit(0.13, "npc"),
               just = "left",
               default.units = "npc",
               gp=gpar(fill = "#ffffff",
                       col = "#ffffff"),
               vp = NULL)

grid.text('Change since',
          just = "centre",
          x = unit(0.38, "npc"),
          y = unit(0.82, "npc"),
          gp = gpar(col = "#000000",
                    fontsize = "10",
                    # fontfamily = 'Verdana',
                    fontface = 'bold'))

grid.text('last week',
          just = "centre",
          x = unit(0.38, "npc"),
          y = unit(0.8, "npc"),
          gp = gpar(col = "#000000",
                    fontsize = "10",
                    # fontfamily = 'Verdana',
                    fontface = 'bold'))

if(change_direction_between_weeks == 'DOWN'){
  grid.raster(down_img,
              x = unit(0.285, "npc"),
              y = unit(0.76, "npc"),
              just = 'left',
              width = .04)
  
  grid.text(format(change_between_weeks, big.mark = ','),
            just = "left",
            x = unit(0.34, "npc"),
            y = unit(0.76, "npc"),
            gp = gpar(col = "#000000",
                      fontsize = "32",
                      fontfamily = 'Bahnschrift',
                      fontface = 'bold'))
  
  grid.text('Falling',
            just = "left",
            x = unit(0.285, "npc"),
            y = unit(0.73, "npc"),
            gp = gpar(col = "#000000",
                      fontsize = "8",
                      fontfamily = 'Bahnschrift',
                      fontface = 'bold'))
}

if(change_direction_between_weeks == 'UP'){
  grid.raster(up_img,
              x = unit(0.285, "npc"),
              y = unit(0.76, "npc"),
              just = 'left',
              width = .04)
  
  grid.text(paste0('+',format(change_between_weeks, big.mark = ',')),
            just = "left",
            x = unit(0.335, "npc"),
            y = unit(0.76, "npc"),
            gp = gpar(col = "#000000",
                      fontsize = "32",
                      fontfamily = 'Bahnschrift',
                      fontface = 'bold'))
  
  grid.text('Rising',
            just = "left",
            x = unit(0.285, "npc"),
            y = unit(0.73, "npc"),
            gp = gpar(col = "#000000",
                      fontsize = "8",
                      fontfamily = 'Bahnschrift',
                      fontface = 'bold'))
}

if(change_direction_between_weeks == 'SAME'){
  grid.raster(same_img,
              x = unit(0.285, "npc"),
              y = unit(0.76, "npc"),
              just = 'left',
              width = .04)
  
  grid.text('No change\nin cases',
            just = "left",
            x = unit(0.335, "npc"),
            y = unit(0.76, "npc"),
            gp = gpar(col = "#000000",
                      fontsize = "16",
                      fontfamily = 'Bahnschrift',
                      fontface = 'bold'))
}

grid.text(paste0('In the seven days to ', format(complete_date, '%A '), ordinal(as.numeric(format(complete_date, '%d'))), format(complete_date, ' %B'), ',  there were'),
          just = "left",
          x = unit(0.04, "npc"),
          y = unit(0.69, "npc"),
          gp = gpar(col = "#ffffff",
                    fontsize = "9",
                    # fontfamily = 'Verdana',
                    fontface = 'bold'))

grid.text(paste0(format(cases_this_week$Rolling_7_day_new_cases, big.mark = ','), ' new confirmed COVID-19 cases in West Sussex.'),
          just = "left",
          x = unit(0.04, "npc"),
          y = unit(0.67, "npc"),
          gp = gpar(col = "#ffffff",
                    fontsize = "9",
                    # fontfamily = 'Verdana',
                    fontface = 'bold'))

grid.text(ifelse(change_direction_between_weeks == 'SAME', 'Confirmed cases are the same as in the previous week', ifelse(change_direction_between_weeks == 'DOWN', 'Confirmed cases are falling compared to the previous week', ifelse(change_direction_between_weeks == 'UP', 'Confirmed cases are rising compared to the previous week', NA))),
          just = "left",
          x = unit(0.04, "npc"),
          y = unit(0.64, "npc"),
          gp = gpar(col = "#ffffff",
                    fontsize = "9",
                    # fontfamily = 'Verdana',
                    fontface = 'bold'))

grid.text(paste0('(', format(cases_last_week$Rolling_7_day_new_cases, big.mark = ','), ' cases in the seven days to the ', ordinal(as.numeric(format(complete_date - 7, '%d'))), format(complete_date - 7, ' %B %Y'), ').'),
          just = "left",
          x = unit(0.04, "npc"),
          y = unit(0.62, "npc"),
          gp = gpar(col = "#ffffff",
                    fontsize = "9",
                    # fontfamily = 'Verdana',
                    fontface = 'bold'))

grid.text(paste0('Since the start of the COVID-19 pandemic, there have been'),
          just = "left",
          x = unit(0.04, "npc"),
          y = unit(0.58, "npc"),
          gp = gpar(col = "#ffffff",
                    fontsize = "9",
                    # fontfamily = 'Verdana',
                    fontface = 'bold'))

grid.text(paste0(format(total_so_far$Cumulative_cases, big.mark = ','), ' confirmed cases in West Sussex (as of ', ordinal(as.numeric(format(last_date, '%d'))), format(last_date, ' %B'),').'),
          just = "left",
          x = unit(0.04, "npc"),
          y = unit(0.56, "npc"),
          gp = gpar(col = "#ffffff",
                    fontsize = "9",
                    # fontfamily = 'Verdana',
                    fontface = 'bold'))

# Table ####
text_colour <- '#ffffff'
text_colour <- '#000000'

grid.rect(x = unit(0.5, "npc"),
          y = unit(0.685, "npc"),
          width = unit(.47556, "npc"),
          height = unit(0.31, "npc"),
          just = "left",
          default.units = "npc",
          gp=gpar(fill = "#ffffff", col = "#ffffff"),
          draw = TRUE,
          vp = NULL)

latest_table <- p12_test_summary %>%
  mutate(case_label = paste0(format(Rolling_7_day_new_cases, big.mark = ',', trim = TRUE), ' (', format(round(Rolling_7_day_new_cases_per_100000, 1), big.mark = ',', trim = TRUE), ')')) %>%
  left_join(cumulative_vaccine_df_ltla, by = 'Name') %>%
  select(Name, case_label, Change_direction, first_label, second_label) %>%
  mutate(Name = factor(Name, levels = c('Adur', 'Arun', 'Chichester', 'Crawley', 'Horsham', 'Mid Sussex', 'Worthing', 'West Sussex', 'South East region', 'England'))) %>%
  arrange(Name)

seven_day_heading <- paste0('Cases in the 7 days to ', format(complete_date, '%d/%m/%y'))

grid.text('Area',
          just = "left",
          x = unit(0.52, "npc"),
          y = unit(0.825, "npc"),
          gp = gpar(col = text_colour,
                    fontsize = "9",
                    # fontfamily = 'Verdana',
                    fontface = 'bold'))

grid.text('Cases in the seven',
          just = "right",
          x = unit(0.75, "npc"),
          y = unit(0.825, "npc"),
          gp = gpar(col = text_colour,
                    fontsize = "9",
                    # fontfamily = 'Verdana',
                    fontface = 'bold'))

grid.text(paste0('days to ', format(complete_date, '%d/%m/%y'), '**'),
          just = "right",
          x = unit(0.75, "npc"),
          y = unit(0.805, "npc"),
          gp = gpar(col = text_colour,
                    fontsize = "9",
                    # fontfamily = 'Verdana',
                    fontface = 'bold'))

grid.text('Vaccinations so far',
          just = "centre",
          x = unit(0.87, "npc"),
          y = unit(0.83, "npc"),
          gp = gpar(col = text_colour,
                    fontsize = "9",
                    # fontfamily = 'Verdana',
                    fontface = 'bold'))

grid.text('(aged 12+)',
          just = "centre",
          x = unit(0.87, "npc"),
          y = unit(0.815, "npc"),
          gp = gpar(col = text_colour,
                    fontsize = "9",
                    # fontfamily = 'Verdana',
                    fontface = 'bold'))

grid.text(paste0('1st dose'),
          just = "right",
          x = unit(0.85, "npc"),
          y = unit(0.795, "npc"),
          gp = gpar(col = text_colour,
                    fontsize = "9",
                    # fontfamily = 'Verdana',
                    fontface = 'bold'))

grid.text(paste0('2nd dose'),
          just = "right",
          x = unit(0.95, "npc"),
          y = unit(0.795, "npc"),
          gp = gpar(col = text_colour,
                    fontsize = "9",
                    # fontfamily = 'Verdana',
                    fontface = 'bold'))

grid.text(paste0('** rates per 100,000 population are given in brackets.'),
          just = "left",
          x = unit(0.52, "npc"),
          y = unit(0.55, "npc"),
          gp = gpar(col = text_colour,
                    # fontfamily = 'Verdana',
                    fontsize = "9"))

areas_to_loop <- c('Adur', 'Arun', 'Chichester', 'Crawley', 'Horsham', 'Mid Sussex', 'Worthing', 'West Sussex', 'South East region', 'England')

for(i in 1:length(areas_to_loop)){
  
  area_x <- areas_to_loop[i]
  
  seven_day_area_x <- latest_table %>%
    filter(Name == area_x)
  
  interval_pos <- .022
  
  grid.text(gsub(' region', '', area_x),
            just = "left",
            x = unit(0.52, "npc"),
            y = unit(0.79 - interval_pos * i, "npc"),
            gp = gpar(col = text_colour,
                      # fontfamily = 'Verdana',
                      fontsize = "9"))
  
  grid.text(format(seven_day_area_x$case_label, big.mark = ',', trim = TRUE),
            just = "right",
            x = unit(0.75 - .03, "npc"),
            y = unit(0.79 - interval_pos * i, "npc"),
            gp = gpar(col = text_colour,
                      # fontfamily = 'Verdana',
                      fontsize = "9"))
  
  if(seven_day_area_x$Change_direction == 'Up'){
    grid.raster(up_img,
                x = unit(0.75, "npc"),
                y = unit(0.79 - interval_pos * i, "npc"),
                just = 'right',
                width = .018)
  }
  
  if(seven_day_area_x$Change_direction == 'Down'){
    grid.raster(down_img,
                x = unit(0.75, "npc"),
                y = unit(0.79 - interval_pos * i, "npc"),
                just = 'right',
                width = .018)
  }
  
  if(seven_day_area_x$Change_direction == 'Same'){
    grid.raster(same_img,
                x = unit(0.75, "npc"),
                y = unit(0.79 - interval_pos * i, "npc"),
                just = 'right',
                width = .018)
  }
  
  grid.text(seven_day_area_x$first_label,
            just = "right",
            x = unit(0.85, "npc"),
            y = unit(0.79 - interval_pos * i, "npc"),
            gp = gpar(col = text_colour,
                      # fontfamily = 'Verdana',
                      fontsize = "9"))
  
  grid.text(seven_day_area_x$second_label,
            just = "right",
            x = unit(0.95, "npc"),
            y = unit(0.79 - interval_pos * i, "npc"),
            gp = gpar(col = text_colour,
                      # fontfamily = 'Verdana',
                      fontsize = "9"))
  
}

grid.lines(x = c(0.5,0.98),
           y = 0.84,
           default.units = "npc",
           gp = gpar(col = text_colour,
                     # lty = "dotted",
                     lwd = 1.2))

grid.lines(x = c(0.5,0.98),
           y = 0.785,
           default.units = "npc",
           gp = gpar(col = text_colour,
                     # lty = "dotted",
                     lwd = 1.2))

grid.lines(x = c(0.5,0.98),
           y = 0.625,
           default.units = "npc",
           gp = gpar(col = text_colour,
                     # lty = "dotted",
                     lwd = 1.2))

grid.lines(x = c(0.5,0.98),
           y = 0.6,
           default.units = "npc",
           gp = gpar(col = text_colour,
                     # lty = "dotted",
                     lwd = 1.2))

grid.lines(x = c(0.5,0.98),
           y = 0.56,
           default.units = "npc",
           gp = gpar(col = text_colour,
                     # lty = "dotted",
                     lwd = 1.2))

# Vaccinations ##

grid.roundrect(x = unit(0.04, "npc"),
               y = unit(0.305, "npc"),
               width = unit(.45, "npc"),
               height = unit(0.43, "npc"),
               just = "left",
               default.units = "npc",
               gp=gpar(fill = "#ffffff", col = "#ffffff"),
               vp = NULL)


vac_uptake_12_plus_gg <- ggplot(vac_info_df_12_plus, aes(x = 1.9,
                                                         y = People,
                                                         fill = Status_label)) +
  geom_bar(stat="identity") +
  xlim(.1, 2.5) +
  coord_polar(theta = "y",
              start = 0,
              direction = 1) +
  labs(x = NULL,
       y = NULL) +
  scale_fill_manual(values = c("#00563f", "#fa8800", "#dbdbdb"),
                    breaks = rev(levels(vac_info_df_12_plus$Status_label)),
                    name = '') +
  scale_colour_manual(values= "#ffffff", guide = FALSE) +
  theme_bw()+
  theme(axis.ticks=element_blank(),
        axis.text=element_blank(),
        axis.title=element_blank(),
        panel.grid=element_blank(),
        panel.border=element_blank(),
        plot.margin = unit(c(0, 0, 0, 0), "cm"),
        panel.background = element_rect(fill = "transparent",colour = NA),
        plot.background = element_rect(fill = "transparent",colour = NA),
        legend.position = 'none')

print(vac_uptake_12_plus_gg,
      vp = vplayout(15:22, 1:10))

grid.text(paste0('In West Sussex, there have'),
          just = "left",
          x = unit(0.28, "npc"),
          y = unit(0.48, "npc"),
          gp = gpar(col = "#000000",
                    # fontfamily = 'Verdana',
                    fontsize = "10"))

grid.text(paste0('been a total of ', format(wsx_12_plus_first_doses, big.mark = ',')),
          just = "left",
          x = unit(0.28, "npc"),
          y = unit(0.46, "npc"),
          gp = gpar(col = "#000000",
                    # fontfamily = 'Verdana',
                    fontsize = "10"))

grid.text(paste0('first doses received among'),
          just = "left",
          x = unit(0.28, "npc"),
          y = unit(0.44, "npc"),
          gp = gpar(col = "#000000",
                    # fontfamily = 'Verdana',
                    fontsize = "10"))

grid.text(paste0('those aged 12 and over.'),
          just = "left",
          x = unit(0.28, "npc"),
          y = unit(0.42, "npc"),
          gp = gpar(col = "#000000",
                    # fontfamily = 'Verdana',
                    fontsize = "10"))

grid.text(paste0(round(wsx_12_plus_first_doses_proportion * 100, 0), '%'),
          just = "centre",
          x = unit(0.1725, "npc"),
          y = unit(0.43, "npc"),
          gp = gpar(col = "#000000",
                    fontsize = "18",
                    # fontfamily = 'Verdana',
                    fontface = 'bold'))

grid.text(paste0('of those aged'),
          just = "centre",
          x = unit(0.172, "npc"),
          y = unit(0.41, "npc"),
          gp = gpar(col = "#000000",
                    # fontfamily = 'Verdana',
                    fontsize = "9"))

grid.text(paste0('12+ have received'),
          just = "centre",
          x = unit(0.1675, "npc"),
          y = unit(0.395, "npc"),
          gp = gpar(col = "#000000",
                    # fontfamily = 'Verdana',
                    fontsize = "9"))

grid.text(paste0('at least one'),
          just = "centre",
          x = unit(0.172, "npc"),
          y = unit(0.38, "npc"),
          gp = gpar(col = "#000000",
                    # fontfamily = 'Verdana',
                    fontsize = "9"))

grid.text(paste0('dose'),
          just = "centre",
          x = unit(0.172, "npc"),
          y = unit(0.365, "npc"),
          gp = gpar(col = "#000000",
                    # fontfamily = 'Verdana',
                    fontsize = "9"))

grid.rect(x = unit(0.29, "npc"),
          y = unit(0.38, "npc"),
          width = unit(0.0175, "npc"),
          height = unit(0.0175, "npc"),
          just = "centre",
          default.units = "npc",
          gp=gpar(fill = "#00563f",
                  col = "#00563f"),
          draw = TRUE,
          vp = NULL)

grid.text(paste0(format(wsx_12_plus_second_doses, big.mark = ','),' received two doses'),
          just = "left",
          x = unit(0.305, "npc"),
          y = unit(0.38, "npc"),
          gp = gpar(col = "#000000",
                    # fontfamily = 'Verdana',
                    fontsize = "9"))

grid.rect(x = unit(0.29, "npc"),
          y = unit(0.355, "npc"),
          width = unit(0.0175, "npc"),
          height = unit(0.0175, "npc"),
          just = "centre",
          default.units = "npc",
          gp=gpar(fill = "#fa8800",
                  col = "#fa8800"),
          draw = TRUE,
          vp = NULL)

grid.text(paste0(format(wsx_12_plus_first_dose_only, big.mark = ','),' first dose only'),
          just = "left",
          x = unit(0.305, "npc"),
          y = unit(0.355, "npc"),
          gp = gpar(col = "#000000",
                    # fontfamily = 'Verdana',
                    fontsize = "9"))

grid.rect(x = unit(0.29, "npc"),
          y = unit(0.33, "npc"),
          width = unit(0.0175, "npc"),
          height = unit(0.0175, "npc"),
          just = "centre",
          default.units = "npc",
          gp=gpar(fill = "#dbdbdb",
                  col = "#dbdbdb"),
          draw = TRUE,
          vp = NULL)

grid.text(paste0(format(wsx_12_plus_unvaccinated, big.mark = ','),' not vaccinated'),
          just = "left",
          x = unit(0.305, "npc"),
          y = unit(0.33, "npc"),
          gp = gpar(col = "#000000",
                    # fontfamily = 'Verdana',
                    fontsize = "9"))

vac_uptake_18_plus_gg <- ggplot(vac_info_df_18_plus, aes(x = 1.9,
                                                         y = People,
                                                         fill = Status)) +
  geom_bar(stat="identity") +
  xlim(.1, 2.5) +
  coord_polar(theta = "y",
              start = 0,
              direction = 1) +
  labs(x = NULL,
       y = NULL) +
  scale_fill_manual(values = c('#01a1ec','#00563f', "#fa8800", "#dbdbdb"),
                    breaks = rev(levels(vac_info_df_18_plus$Status)),
                    name = '') +
  scale_colour_manual(values= "#ffffff", guide = FALSE) +
  theme_bw()+
  theme(axis.ticks=element_blank(),
        axis.text=element_blank(),
        axis.title=element_blank(),
        panel.grid=element_blank(),
        panel.border=element_blank(),
        plot.margin = unit(c(0, 0, 0, 0), "cm"),
        panel.background = element_rect(fill = "transparent",colour = NA),
        plot.background = element_rect(fill = "transparent",colour = NA),
        legend.position = 'none')

print(vac_uptake_18_plus_gg,
      vp = vplayout(21:28, 1:10))

grid.text(paste0(round(wsx_18_plus_first_doses_proportion * 100, 0), '%'),
          just = "centre",
          x = unit(0.1725, "npc"),
          y = unit(0.23, "npc"),
          gp = gpar(col = "#000000",
                    fontsize = "18",
                    # fontfamily = 'Verdana',
                    fontface = 'bold'))

grid.text(paste0('of those aged'),
          just = "centre",
          x = unit(0.172, "npc"),
          y = unit(0.21, "npc"),
          gp = gpar(col = "#000000",
                    # fontfamily = 'Verdana',
                    fontsize = "9"))

grid.text(paste0('18+ received'),
          just = "centre",
          x = unit(0.1675, "npc"),
          y = unit(0.195, "npc"),
          gp = gpar(col = "#000000",
                    # fontfamily = 'Verdana',
                    fontsize = "9"))

grid.text(paste0('at least one'),
          just = "centre",
          x = unit(0.172, "npc"),
          y = unit(0.18, "npc"),
          gp = gpar(col = "#000000",
                    # fontfamily = 'Verdana',
                    fontsize = "9"))

grid.text(paste0('dose'),
          just = "centre",
          x = unit(0.172, "npc"),
          y = unit(0.165, "npc"),
          gp = gpar(col = "#000000",
                    # fontfamily = 'Verdana',
                    fontsize = "9"))

grid.text(paste0('In West Sussex, there have'),
          just = "left",
          x = unit(0.28, "npc"),
          y = unit(0.29, "npc"),
          gp = gpar(col = "#000000",
                    # fontfamily = 'Verdana',
                    fontsize = "10"))

grid.text(paste0('been a total of ', format(wsx_18_plus_first_doses, big.mark = ',')),
          just = "left",
          x = unit(0.28, "npc"),
          y = unit(0.27, "npc"),
          gp = gpar(col = "#000000",
                    # fontfamily = 'Verdana',
                    fontsize = "10"))

grid.text(paste0('first doses received among'),
          just = "left",
          x = unit(0.28, "npc"),
          y = unit(0.25, "npc"),
          gp = gpar(col = "#000000",
                    # fontfamily = 'Verdana',
                    fontsize = "10"))

grid.text(paste0('those aged 18 and over.'),
          just = "left",
          x = unit(0.28, "npc"),
          y = unit(0.23, "npc"),
          gp = gpar(col = "#000000",
                    # fontfamily = 'Verdana',
                    fontsize = "10"))

grid.rect(x = unit(0.29, "npc"),
          y = unit(0.20, "npc"),
          width = unit(0.0175, "npc"),
          height = unit(0.0175, "npc"),
          just = "centre",
          default.units = "npc",
          gp=gpar(fill = "#01a1ec",
                  col = "#01a1ec"),
          draw = TRUE,
          vp = NULL)

grid.text(paste0(format(wsx_18_plus_third_doses, big.mark = ','),' received three'),
          just = "left",
          x = unit(0.305, "npc"),
          y = unit(0.20, "npc"),
          gp = gpar(col = "#000000",
                    # fontfamily = 'Verdana',
                    fontsize = "9"))

grid.text(paste0('(including booster) doses'),
          just = "left",
          x = unit(0.305, "npc"),
          y = unit(0.18, "npc"),
          gp = gpar(col = "#000000",
                    # fontfamily = 'Verdana',
                    fontsize = "9"))


grid.rect(x = unit(0.29, "npc"),
          y = unit(0.16, "npc"),
          width = unit(0.0175, "npc"),
          height = unit(0.0175, "npc"),
          just = "centre",
          default.units = "npc",
          gp=gpar(fill = "#00563f",
                  col = "#00563f"),
          draw = TRUE,
          vp = NULL)

grid.text(paste0(format(wsx_18_plus_second_doses, big.mark = ','),' received two doses'),
          just = "left",
          x = unit(0.305, "npc"),
          y = unit(0.16, "npc"),
          gp = gpar(col = "#000000",
                    # fontfamily = 'Verdana',
                    fontsize = "9"))

grid.rect(x = unit(0.29, "npc"),
          y = unit(0.135, "npc"),
          width = unit(0.0175, "npc"),
          height = unit(0.0175, "npc"),
          just = "centre",
          default.units = "npc",
          gp=gpar(fill = "#fa8800",
                  col = "#fa8800"),
          draw = TRUE,
          vp = NULL)

grid.text(paste0(format(wsx_18_plus_first_dose_only, big.mark = ','),' first dose only'),
          just = "left",
          x = unit(0.305, "npc"),
          y = unit(0.135, "npc"),
          gp = gpar(col = "#000000",
                    # fontfamily = 'Verdana',
                    fontsize = "9"))

grid.rect(x = unit(0.29, "npc"),
          y = unit(0.11, "npc"),
          width = unit(0.0175, "npc"),
          height = unit(0.0175, "npc"),
          just = "centre",
          default.units = "npc",
          gp=gpar(fill = "#dbdbdb",
                  col = "#dbdbdb"),
          draw = TRUE,
          vp = NULL)

grid.text(paste0(format(wsx_18_plus_unvaccinated, big.mark = ','),' not vaccinated'),
          just = "left",
          x = unit(0.305, "npc"),
          y = unit(0.11, "npc"),
          gp = gpar(col = "#000000",
                    # fontfamily = 'Verdana',
                    fontsize = "9"))

# Over time ####
grid.roundrect(x = unit(0.5, "npc"),
               y = unit(0.45, "npc"),
               width = unit(0.48, "npc"),
               height = unit(0.13, "npc"),
               just = "left",
               default.units = "npc",
               gp=gpar(fill = "#ffffff",
                       col = "#ffffff"),
               vp = NULL)

grid.text(paste0('The vaccination programme for West Sussex residents (12+),'),
          just = "left",
          x = unit(0.51, "npc"),
          y = unit(0.5, "npc"),
          gp = gpar(col = "#000000",
                    fontsize = "9",
                    # fontfamily = 'Verdana',
                    fontface = 'bold'))

grid.text(paste0('in the seven days to ', This_week_vac_date, ', has delivered:'),
          just = "left",
          x = unit(0.51, "npc"),
          y = unit(0.48, "npc"),
          gp = gpar(col = "#000000",
                    fontsize = "9",
                    # fontfamily = 'Verdana',
                    fontface = 'bold'))

grid.text(format(wsx_first_doses_seven_days, big.mark = ','),
          just = "centre",
          x = unit(0.57, "npc"),
          y = unit(0.44, "npc"),
          gp = gpar(col = "#000000",
                    fontsize = "30",
                    fontfamily = 'Bahnschrift',
                    fontface = 'bold'))

grid.text('first doses',
          just = "centre",
          x = unit(0.57, "npc"),
          y = unit(0.41, "npc"),
          gp = gpar(col = "#000000",
                    fontsize = "9",
                    fontface = 'bold'))

grid.text(format(wsx_second_doses_seven_days, big.mark = ','),
          just = "centre",
          x = unit(0.72, "npc"),
          y = unit(0.44, "npc"),
          gp = gpar(col = "#000000",
                    fontsize = "30",
                    fontfamily = 'Bahnschrift',
                    fontface = 'bold'))

grid.text('second doses',
          just = "centre",
          x = unit(0.72, "npc"),
          y = unit(0.41, "npc"),
          gp = gpar(col = "#000000",
                    fontsize = "9",
                    fontface = 'bold'))

grid.text(format(wsx_third_doses_seven_days, big.mark = ','),
          just = "centre",
          x = unit(0.89, "npc"),
          y = unit(0.44, "npc"),
          gp = gpar(col = "#000000",
                    fontsize = "30",
                    fontfamily = 'Bahnschrift',
                    fontface = 'bold'))

grid.text('third/booster doses',
          just = "centre",
          x = unit(0.89, "npc"),
          y = unit(0.41, "npc"),
          gp = gpar(col = "#000000",
                    fontsize = "9",
                    fontface = 'bold'))

grid.text('COVID-19 IN HOSPITALS',
          just = "left",
          x = unit(0.53, "npc"),
          y = unit(0.35, "npc"),
          gp = gpar(col = "#ffffff",
                    fontsize = "20",
                    fontfamily = 'Bahnschrift',
                    fontface = 'bold'))

grid.text('ACROSS THE REGION',
          just = "left",
          x = unit(0.53, "npc"),
          y = unit(0.32, "npc"),
          gp = gpar(col = "#ffffff",
                    fontsize = "20",
                    fontfamily = 'Bahnschrift',
                    fontface = 'bold'))

grid.text(paste0('In the South East region on ', format(se_hospital_df$Date, '%A '), ordinal(as.numeric(format(se_hospital_df$Date, '%d'))), format(se_hospital_df$Date, ' %B'), ', there'),
          just = "left",
          x = unit(0.53, "npc"),
          y = unit(0.28, "npc"),
          gp = gpar(col = "#ffffff",
                    fontsize = "10",
                    # fontfamily = 'Verdana',
                    fontface = 'bold'))

grid.text(paste0('were ', format(se_hospital_df$Patients_occupying_beds, big.mark = ','), ' COVID-19 positive patients in hospital beds.'),
          just = "left",
          x = unit(0.53, "npc"),
          y = unit(0.26, "npc"),
          gp = gpar(col = "#ffffff",
                    fontsize = "10",
                    # fontfamily = 'Verdana',
                    fontface = 'bold'))

grid.text(paste0('The number of people in hospital beds with COVID-19'),
          just = "left",
          x = unit(0.53, "npc"),
          y = unit(0.22, "npc"),
          gp = gpar(col = "#ffffff",
                    fontsize = "10",
                    # fontfamily = 'Verdana',
                    fontface = 'bold'))

grid.text(paste0('has ', se_hospital_df$Change_direction, ' compared to the 7 days before (', format(se_hospital_df$Previous_occupying_beds, big.mark = ','), ')'),
          just = "left",
          x = unit(0.53, "npc"),
          y = unit(0.2, "npc"),
          gp = gpar(col = "#ffffff",
                    fontsize = "10",
                    # fontfamily = 'Verdana',
                    fontface = 'bold'))

grid.text(paste0('Of those patients currently in hospital, ', format(se_hospital_df$Patients_occupying_mv_beds, big.mark = ','), ' were'),
          just = "left",
          x = unit(0.53, "npc"),
          y = unit(0.16, "npc"),
          gp = gpar(col = "#ffffff",
                    fontsize = "10",
                    # fontfamily = 'Verdana',
                    fontface = 'bold'))

grid.text(paste0('occupying beds capable of mechanical ventilation.'),
          just = "left",
          x = unit(0.53, "npc"),
          y = unit(0.14, "npc"),
          gp = gpar(col = "#ffffff",
                    fontsize = "10",
                    # fontfamily = 'Verdana',
                    fontface = 'bold'))

# Banner bottom #
grid.rect(x = unit(0.5, "npc"),
          y = unit(0.04, "npc"),
          width = unit(1, "npc"),
          height = unit(0.08, "npc"),
          just = "centre",
          default.units = "npc",
          gp=gpar(fill = "#F8E821",
                  col = "#F8E821"),
          draw = TRUE,
          vp = NULL)

grid.text('PUBLICATION DATE:',
          just = "left",
          x = unit(0.04, "npc"),
          y = unit(0.04, "npc"),
          gp = gpar(col = "#000000",
                    fontsize = "16",
                    fontfamily = 'Bahnschrift',
                    fontface = 'bold'))

grid.text(paste0(ordinal(as.numeric(format(last_date + 1 , '%d'))), format(last_date +1, ' %B %Y')),
          just = "left",
          x = unit(0.3, "npc"),
          y = unit(0.04, "npc"),
          gp = gpar(col = "#0071B6",
                    fontsize = "18",
                    # fontfamily = 'Verdana',
                    fontface = 'bold'))

grid.text(paste0('*We measure change by looking at cases in this week'),
          just = "left",
          x = unit(0.6, "npc"),
          y = unit(0.069, "npc"),  
          gp = gpar(col = "#000000",
                    # fontfamily = 'Verdana',
                    fontsize = "9"))

grid.text(paste0('(seven days to ', format(complete_date, '%A '), ordinal(as.numeric(format(complete_date, '%d'))), format(complete_date, ' %B %Y'), ') compared'),
          just = "left",
          x = unit(0.6, "npc"),
          y = unit(0.055, "npc"),
          gp = gpar(col = "#000000",
                    # fontfamily = 'Verdana',
                    fontsize = "9"))

grid.text(paste0('to the last week (seven days to ', ordinal(as.numeric(format(complete_date - 7, '%d'))), format(complete_date - 7, ' %B %Y'), '). This is '),
          just = "left",
          x = unit(0.6, "npc"),
          y = unit(0.04, "npc"),
          gp = gpar(col = "#000000",
                    # fontfamily = 'Verdana',
                    fontsize = "9"))

grid.text(paste0('because data for more recent days are considered incomplete.'),
          just = "left",
          x = unit(0.6, "npc"),
          y = unit(0.025, "npc"),
          gp = gpar(col = "#000000",
                    # fontfamily = 'Verdana',
                    fontsize = "9"))

grid.raster(wscc_logo,
            y = unit(0.96, "npc"),
            x = unit(0.85, "npc"),
            vjust = 1,
            hjust = 0,
            width = .12)

dev.off()