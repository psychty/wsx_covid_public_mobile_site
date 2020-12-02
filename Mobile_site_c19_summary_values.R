library(easypackages)

libraries(c("readxl", "readr", "plyr", "dplyr", "ggplot2", "png", "tidyverse", "reshape2", "scales", 'zoo', 'stats',"rgdal", 'rgeos', "tmaptools", 'sp', 'sf', 'maptools', 'leaflet', 'leaflet.extras', 'fingertipsR', 'spdplyr', 'geojsonio', 'rmapshaper', 'jsonlite', 'grid', 'aweek', 'xml2', 'rvest', 'officer', 'flextable', 'viridis', 'epitools', 'ukcovid19'))

options(scipen = 999)

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

github_repo_dir <- "~/GitHub/wsx_covid_public_mobile_site"
output_directory_x <- paste0(github_repo_dir, '/Outputs')

# 2019 MYE

mye_total <- read_csv('http://www.nomisweb.co.uk/api/v01/dataset/NM_2002_1.data.csv?geography=1816133633...1816133848,1820327937...1820328318,2092957697...2092957703,2013265921...2013265932&date=latest&gender=0&c_age=200&measures=20100&select=date_name,geography_name,geography_type,geography_code,obs_value') %>% 
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

mye_total %>% 
  write.csv(., paste0(github_repo_dir,'/Source_files/mye2019_ltla.csv'), row.names = FALSE)

if(exists('mye_total') == FALSE) {
  mye_total <- read_csv(paste0(github_repo_dir,'/Source_files/mye2019_ltla.csv')) %>%
    rename(Population = `All ages`,
           Type = Geography1)
}

area_code_names <- mye_total %>% 
  select(Code, Name)

mye_total <- mye_total %>%
  select(-Name)

# Cases ####

daily_cases <- read_csv('https://coronavirus.data.gov.uk/downloads/csv/coronavirus-cases_latest.csv') %>%   
  rename(Name = `Area name`) %>% 
  rename(Code = `Area code`) %>% 
  rename(Date = `Specimen date`) %>% 
  rename(New_cases = `Daily lab-confirmed cases`) %>% 
  rename(Cumulative_cases = `Cumulative lab-confirmed cases`) %>% 
  arrange(Name, Date) %>% 
  select(Name, Code, `Area type`, Date, New_cases, Cumulative_cases) %>% 
  group_by(Name, Code, Date) %>% 
  mutate(Count = n()) %>% 
  filter(!(`Area type` == 'ltla' & Count == 2)) %>% 
  select(-c(`Area type`, Count)) %>% 
  left_join(mye_total, by = 'Code') %>% 
  ungroup()

# If no specimens are taken on a day, there is no row for it, and it would be missing data. Indeed, the only zeros are on the latest day. We need to therefore backfill and say if no date exists where it should, then add it, with the cumulative total and zero for new cases.

# One way to do this is to create a new dataframe with a row for each area and date, and left join the daily_cases data to it.
first_date <- min(daily_cases$Date)
last_case_date <- max(daily_cases$Date)

query_filters <- c(
  # "areaType=utla"
  'areaName=West Sussex'
)

query_structure <- list(
  date = "date", 
  name = "areaName", 
  code = "areaCode", 
  daily = "newCasesBySpecimenDate",
  cumulative = "cumCasesBySpecimenDate"
)

last_date <- as.Date(last_update(filters = query_filters, structure = query_structure))

# PHE say the last four data points are incomplete (perhaps they should not publish them). Instead, we need to make sure we account for this so that it is not misinterpreted.
complete_date <- last_date - 5

data.frame(Item = 'latest_daily_case', Label = paste0(format(last_case_date, '%A '), ordinal(as.numeric(format(last_case_date, '%d'))), format(last_case_date, ' %B %Y'))) %>% 
  add_row(Item = 'daily_case_update_date',Label = paste0(format(last_date, '%A '), ordinal(as.numeric(format(last_date, '%d'))), format(last_date, ' %B %Y'))) %>% 
  add_row(Item = 'complete_date', Label = paste0(format(complete_date, '%A '), ordinal(as.numeric(format(complete_date, '%d'))), format(complete_date, ' %B %Y')))%>% 
  add_row(Item = 'first_case_period', Label =  format(first_date, '%d %B')) %>% 
  add_row(Item = 'last_case_period', Label =  format(last_case_date, '%d %B')) %>% 
  add_row(Item = 'previous_week_period', Label =  format(complete_date -7, '%A %d %B %Y')) %>% 
  toJSON() %>% 
  write_lines(paste0(output_directory_x, '/case_dates.json'))

Areas = daily_cases %>% 
  select(Name, Code, Type) %>% 
  unique()

Dates = seq.Date(first_date, last_case_date, by = '1 day')

daily_cases_reworked <- data.frame(Name = rep(Areas$Name, length(Dates)), Code = rep(Areas$Code, length(Dates)), Type = rep(Areas$Type, length(Dates)), check.names = FALSE) %>% 
  arrange(Name) %>% 
  group_by(Name) %>% 
  mutate(Date = seq.Date(first_date, last_case_date, by = '1 day')) %>% 
  left_join(daily_cases, by = c('Name', 'Code', 'Type', 'Date')) %>% 
  mutate(New_cases = ifelse(is.na(New_cases), 0, New_cases)) %>% 
  mutate(New_cumulative = cumsum(New_cases)) %>% 
  filter(!is.na(Cumulative_cases)) %>% 
  mutate(Calculated_same_as_original = ifelse(Cumulative_cases == New_cumulative, 'Yaas', 'Negative'))

# Case results are generally published in the afternoon and represent cases reported up to 9am of the reporting day. However, cases are assigned to the date of which the specimen was taken rather than when it was reported. This means it will be very unlikely that a specimen would be taken and results returned by 9am of the day of publication. As such, we consider the last five days (four days plus the day of reporting) as incomplete.

p12_test_df <- data.frame(Name = rep(Areas$Name, length(Dates)), Code = rep(Areas$Code, length(Dates)), Type = rep(Areas$Type, length(Dates)), check.names = FALSE) %>% 
  arrange(Name) %>% 
  group_by(Name) %>% 
  mutate(Date = seq.Date(first_date, last_case_date, by = '1 day')) %>% 
  mutate(Data_completeness = ifelse(Date > complete_date, 'Considered incomplete', 'Complete')) %>% 
  left_join(daily_cases, by = c('Name', 'Code', 'Type', 'Date')) %>% 
  mutate(New_cases = ifelse(is.na(New_cases), 0, New_cases)) %>% 
  rename(Original_cumulative = Cumulative_cases) %>% # We should keep the original cumulative cases for reference
  mutate(Cumulative_cases = cumsum(New_cases)) %>% # These are based on the new cases data being accurate
  group_by(Name) %>% 
  mutate(Period = format(Date, '%d %B')) %>%
  select(-Population) %>% 
  left_join(mye_total[c('Code', 'Population')], by = 'Code') %>% 
  mutate(Cumulative_per_100000 = (Cumulative_cases / Population) * 100000) %>% 
  mutate(New_cases_per_100000 = (New_cases / Population) * 100000) %>% 
  ungroup() %>% 
  mutate(Name = ifelse(Name == 'South East', 'South East region', Name))  %>% 
  group_by(Name) %>% 
  mutate(Rolling_7_day_new_cases = rollapply(New_cases, 7, sum, align = 'right', fill = NA)) %>% 
  mutate(Rolling_7_day_new_cases_per_100000 = ifelse(is.na(Rolling_7_day_new_cases), NA, (Rolling_7_day_new_cases / Population) * 100000)) %>% 
  mutate(Perc_change_on_rolling_7_days_actual = round((Rolling_7_day_new_cases - lag(Rolling_7_day_new_cases, 7))/ lag(Rolling_7_day_new_cases, 7), 2))  %>% 
  mutate(Perc_change_on_rolling_7_days_actual = ifelse(Perc_change_on_rolling_7_days_actual == Inf, 1, Perc_change_on_rolling_7_days_actual)) %>% 
  mutate(Perc_change_on_rolling_7_days_actual = replace_na(Perc_change_on_rolling_7_days_actual, 0)) %>%
  mutate(Rolling_7_day_average_new_cases = rollapply(New_cases, 7, mean, align = 'right', fill = NA)) %>%
  mutate(Previous_7_days_sum = lag(Rolling_7_day_new_cases, 7)) %>% 
  ungroup() 

rm(daily_cases, Areas, Dates, first_date, mye_total, area_code_names, daily_cases_reworked, query_structure)

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
  mutate(Change_direction = ifelse(Perc_change_on_rolling_7_days_actual <0, 'Down', ifelse(Perc_change_on_rolling_7_days_actual == 0, 'Same', ifelse(Perc_change_on_rolling_7_days_actual > 0, 'Up', NA)))) %>% 
  rename(Rate_date = Date)

p12_test_summary <- p12_test_summary_1 %>% 
  left_join(p12_test_summary_2, by = 'Name') 

# Cumulative to most recent case date, new cases and rolling 7 day incidence up to complete date only.

rm(p12_test_summary_1, p12_test_summary_2)

# Age specific cases ####

mye_ages <- read_csv('https://www.nomisweb.co.uk/api/v01/dataset/NM_2002_1.data.csv?geography=2092957699,2013265921...2013265932,1816133633...1816133848,1820327937...1820328318&date=latest&gender=0&c_age=1,3...18,210&measures=20100&select=geography_name,geography_code,c_age_name,obs_value,geography_type') %>% 
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

age_spec <- read_csv('https://coronavirus.data.gov.uk/downloads/demographic/cases/specimenDate_ageDemographic-unstacked.csv') %>% 
  filter(areaName %in% c('Adur', 'Arun', 'Chichester', 'Crawley', 'Horsham', 'Mid Sussex', 'Worthing', 'West Sussex', 'South East', 'England')) %>% 
  mutate(areaName = ifelse(areaName == 'South East', 'South East region', areaName)) %>% 
  select(areaCode, areaName, areaType, date, `newCasesBySpecimenDate-0_4`,`newCasesBySpecimenDate-5_9`,`newCasesBySpecimenDate-10_14`,`newCasesBySpecimenDate-15_19`,`newCasesBySpecimenDate-20_24`,`newCasesBySpecimenDate-25_29`,`newCasesBySpecimenDate-30_34`,`newCasesBySpecimenDate-35_39`,`newCasesBySpecimenDate-40_44`,`newCasesBySpecimenDate-45_49`,`newCasesBySpecimenDate-50_54`,`newCasesBySpecimenDate-55_59`,`newCasesBySpecimenDate-60_64`,`newCasesBySpecimenDate-65_69`,`newCasesBySpecimenDate-70_74`,`newCasesBySpecimenDate-75_79`,`newCasesBySpecimenDate-80_84`,`newCasesBySpecimenDate-85_89`,`newCasesBySpecimenDate-90+`,`newCasesBySpecimenDate-unassigned`) %>%
  pivot_longer(cols = c(`newCasesBySpecimenDate-0_4`,`newCasesBySpecimenDate-5_9`,`newCasesBySpecimenDate-10_14`,`newCasesBySpecimenDate-15_19`,`newCasesBySpecimenDate-20_24`,`newCasesBySpecimenDate-25_29`,`newCasesBySpecimenDate-30_34`,`newCasesBySpecimenDate-35_39`,`newCasesBySpecimenDate-40_44`,`newCasesBySpecimenDate-45_49`,`newCasesBySpecimenDate-50_54`,`newCasesBySpecimenDate-55_59`,`newCasesBySpecimenDate-60_64`,`newCasesBySpecimenDate-65_69`,`newCasesBySpecimenDate-70_74`,`newCasesBySpecimenDate-75_79`,`newCasesBySpecimenDate-80_84`,`newCasesBySpecimenDate-85_89`,`newCasesBySpecimenDate-90+`,`newCasesBySpecimenDate-unassigned`),
               names_to = 'Age') %>% 
  mutate(Age = gsub('newCasesBySpecimenDate-', '', Age)) %>% 
  mutate(Age = ifelse(Age == 'unassigned', 'Unknown', paste0(Age, ' years'))) %>% 
  mutate(Age = gsub('_', '-', Age)) %>% 
  mutate(Age = ifelse(Age %in% c('80-84 years', '85-89 years', '90+ years'), '80+ years', Age)) %>%
  filter(areaType != 'overview') %>% 
  mutate(areaType = ifelse(areaType == 'ltla', 'Lower Tier Local Authority', ifelse(areaType == 'utla', 'Upper Tier Local Authority', ifelse(areaType == 'region', 'Region', ifelse(areaType == 'nation' , 'Nation', NA))))) %>% 
  rename(Name = areaName,
         Code = areaCode,
         Type = areaType,
         Cases = value,
         Date = date) %>% 
  group_by(Name, Age, Date) %>% 
  summarise(Cases = sum(Cases, na.rm = TRUE)) 

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
  filter(Date == complete_date) %>% 
  mutate(New_cases_per_100000 = pois.exact(Cases, Population)[[3]]*100000) %>% 
  mutate(Cumulative_per_100000 = pois.exact(Cumulative_cases, Population)[[3]]*100000) %>% 
  rename(New_cases = Cases) %>% 
  select(Name, Date, Age, Cumulative_cases, Cumulative_per_100000, New_cases, New_cases_per_100000, Rolling_7_day_new_cases, ASR, Rolling_7_day_average_new_cases, Perc_change_on_rolling_7_days_actual) %>% 
  rename(Rolling_7_day_new_cases_per_100000 = ASR) %>% 
  mutate(Change_direction = ifelse(Perc_change_on_rolling_7_days_actual < 0, 'Down', ifelse(Perc_change_on_rolling_7_days_actual == 0, 'Same', ifelse(Perc_change_on_rolling_7_days_actual > 0, 'Up', NA)))) %>% 
  rename(Rate_date = Date) %>% 
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
  filter(Date == complete_date) %>% 
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
  toJSON() %>% 
  write_lines(paste0(output_directory_x, '/daily_cases.json'))

# Hospital admissions ####

# Hospital provider trusts do not have geographically defined boundaries for their population nor do they have complete lists of registered patients. However, modeled estimates of the catchment populations for hospital provider trusts in England are provided by Public Health England (PHE). These experimental statistics estimates the number of people who are using each hospital trust or have the potential to do so. Individual acute trusts sometimes use varying methods to define the population they serve, such as patient flow, CCG derived or travel time based estimates. PHE published modelled estimates use the patient flow method.

# Note: interpretation of the figures should take into account the fact that totals by date of death, particularly for recent prior days, are likely to be updated in future releases. For example as deaths are confirmed as testing positive for Covid-19, as more post-mortem tests are processed and data from them are validated. Any changes are made clear in the daily files.					
if(!file.exists(paste0(github_repo_dir, '/Source_files/etr.csv'))){
  download.file('https://files.digital.nhs.uk/assets/ods/current/etr.zip', paste0(github_repo_dir, '/etr.zip'), mode = 'wb')
  unzip(paste0(github_repo_dir, '/etr.zip'), exdir = github_repo_dir)
  file.remove(paste0(github_repo_dir, '/etr.zip'), paste0(github_repo_dir, '/etr.pdf'))
}

etr <- read_csv(paste0(github_repo_dir, '/Source_files/etr.csv'),col_names = c('Code', 'Name', 'National_grouping', 'Health_geography', 'Address_1', 'Address_2', 'Address_3', 'Address_4', 'Address_5', 'Postcode', 'Open_date', 'Close_date', 'Null_1', 'Null_2', 'Null_3', 'Null_4', 'Null_5', 'Contact', 'Null_6', 'Null_7', 'Null_8', 'Amended_record_indicator', 'Null_9', 'GOR', 'Null_10', 'Null_11', 'Null_12')) %>%
  select(Code, Name, National_grouping) %>%
  mutate(Name = capwords(Name, strict = TRUE)) %>%
  mutate(Name = gsub(' And ', ' and ', Name)) %>%
  mutate(Name = gsub(' Of ', ' of ', Name)) %>%
  mutate(Name = gsub(' Nhs ', ' NHS ', Name)) %>%
  add_row( Code = '-', Name = 'England', National_grouping = '-')

catchment_pop <- read_csv(paste0(github_repo_dir, '/Source_files/trust_catchment_population_estimates.csv')) %>% 
  left_join(etr, by = 'Code')

calls_hosp_webpage <- read_html('https://www.england.nhs.uk/statistics/statistical-work-areas/covid-19-hospital-activity/') %>%
  html_nodes("a") %>%
  html_attr("href")

download.file(grep('Weekly-covid', calls_hosp_webpage, value = T), paste0(github_repo_dir,'/Source_files/trust_admissions.xlsx'), mode = 'wb')

trust_admissions_1 <- read_excel( paste0(github_repo_dir,'/Source_files/trust_admissions.xlsx'),
                                sheet = 'Hosp ads & diag',
                                skip = 13) %>% 
  filter(!is.na(Name)) %>% 
  mutate(Name = capwords(Name, strict = TRUE)) %>% 
  mutate(Name = gsub('Nhs', 'NHS', Name)) %>% 
  mutate(Name = gsub(' And ', ' and ', Name)) %>% 
  mutate(Name = gsub('Cic', 'CIC', Name)) %>% 
  mutate(Name = gsub('C.i.c', 'C.I.C', Name)) %>% 
  pivot_longer(names_to = 'Date', values_to = 'Admissions_or_new_cases_in_last_24hrs', cols = 5:ncol(.)) %>% 
  mutate(Date = as.Date(as.numeric(Date), origin = "1899-12-30"))  %>% 
  filter(Name %in% c('England', 'South East', 'Western Sussex Hospitals NHS Foundation Trust', 'Surrey and Sussex Healthcare NHS Trust', 'Sussex Community NHS Foundation Trust', 'Brighton and Sussex University Hospitals NHS Trust')) %>% 
  select(!c('Type 1 Acute?', 'NHS England Region', 'Code'))

trust_admissions_2 <- read_excel( paste0(github_repo_dir,'/Source_files/trust_admissions.xlsx'),
                                sheet = 'New hosp cases',
                                skip = 13) %>% 
  filter(!is.na(Name)) %>% 
  mutate(Name = capwords(Name, strict = TRUE)) %>% 
  mutate(Name = gsub('Nhs', 'NHS', Name)) %>% 
  mutate(Name = gsub(' And ', ' and ', Name)) %>% 
  mutate(Name = gsub('Cic', 'CIC', Name)) %>% 
  mutate(Name = gsub('C.i.c', 'C.I.C', Name)) %>% 
  pivot_longer(names_to = 'Date', values_to = 'Patients_admitted_for_first_time_with_covid_or_new_cases_in_last_24hrs', cols = 5:ncol(.)) %>% 
  mutate(Date = as.Date(as.numeric(Date), origin = "1899-12-30"))  %>% 
  filter(Name %in% c('England', 'South East', 'Western Sussex Hospitals NHS Foundation Trust', 'Surrey and Sussex Healthcare NHS Trust', 'Sussex Community NHS Foundation Trust', 'Brighton and Sussex University Hospitals NHS Trust')) %>% 
  select(!c('Type 1 Acute?', 'NHS England Region', 'Code'))

# This is the same as the admissions definition except it only counts patients the first time they are admitted with or contract the virus in hospital. E.g. it excludes patients if they have more than one covid admission. 

trust_admissions_3 <- read_excel( paste0(github_repo_dir,'/Source_files/trust_admissions.xlsx'),
                                  sheet = 'Hosp ads from comm',
                                  skip = 13) %>% 
  filter(!is.na(Name)) %>% 
  mutate(Name = capwords(Name, strict = TRUE)) %>% 
  mutate(Name = gsub('Nhs', 'NHS', Name)) %>% 
  mutate(Name = gsub(' And ', ' and ', Name)) %>% 
  mutate(Name = gsub('Cic', 'CIC', Name)) %>% 
  mutate(Name = gsub('C.i.c', 'C.I.C', Name)) %>% 
  pivot_longer(names_to = 'Date', values_to = 'Patients_admitted_for_first_time_with_covid_or_new_cases_in_last_24hrs_for_patients_admitted_in_last_seven_days', cols = 5:ncol(.)) %>% 
  mutate(Date = as.Date(as.numeric(Date), origin = "1899-12-30"))  %>% 
  filter(Name %in% c('England', 'South East', 'Western Sussex Hospitals NHS Foundation Trust', 'Surrey and Sussex Healthcare NHS Trust', 'Sussex Community NHS Foundation Trust', 'Brighton and Sussex University Hospitals NHS Trust')) %>% 
  select(!c('Type 1 Acute?', 'NHS England Region', 'Code'))

# This is an approximation for cases where the virus was contracted prior to admission

trust_admissions_4 <- read_excel( paste0(github_repo_dir,'/Source_files/trust_admissions.xlsx'),
                                  sheet = 'All beds COVID',
                                  skip = 13) %>% 
  filter(!is.na(Name)) %>% 
  mutate(Name = capwords(Name, strict = TRUE)) %>% 
  mutate(Name = gsub('Nhs', 'NHS', Name)) %>% 
  mutate(Name = gsub(' And ', ' and ', Name)) %>% 
  mutate(Name = gsub('Cic', 'CIC', Name)) %>% 
  mutate(Name = gsub('C.i.c', 'C.I.C', Name)) %>% 
  pivot_longer(names_to = 'Date', values_to = 'Patients_occupying_beds', cols = 5:ncol(.)) %>% 
  mutate(Date = as.Date(as.numeric(Date), origin = "1899-12-30"))  %>% 
  filter(Name %in% c('England', 'South East', 'Western Sussex Hospitals NHS Foundation Trust', 'Surrey and Sussex Healthcare NHS Trust', 'Sussex Community NHS Foundation Trust', 'Brighton and Sussex University Hospitals NHS Trust')) %>% 
  select(!c('Type 1 Acute?', 'NHS England Region', 'Code'))

trust_admissions_5 <- read_excel( paste0(github_repo_dir,'/Source_files/trust_admissions.xlsx'),
                                  sheet = 'MV beds COVID',
                                  skip = 13) %>% 
  filter(!is.na(Name)) %>% 
  mutate(Name = capwords(Name, strict = TRUE)) %>% 
  mutate(Name = gsub('Nhs', 'NHS', Name)) %>% 
  mutate(Name = gsub(' And ', ' and ', Name)) %>% 
  mutate(Name = gsub('Cic', 'CIC', Name)) %>% 
  mutate(Name = gsub('C.i.c', 'C.I.C', Name)) %>% 
  pivot_longer(names_to = 'Date', values_to = 'Patients_occupying_beds', cols = 5:ncol(.)) %>% 
  mutate(Date = as.Date(as.numeric(Date), origin = "1899-12-30")) %>% 
  filter(Name %in% c('England', 'South East', 'Western Sussex Hospitals NHS Foundation Trust', 'Surrey and Sussex Healthcare NHS Trust', 'Sussex Community NHS Foundation Trust', 'Brighton and Sussex University Hospitals NHS Trust')) %>% 
  select(!c('Type 1 Acute?', 'NHS England Region', 'Code'))

trust_admissions_metadata <- read_excel( paste0(github_repo_dir,'/Source_files/trust_admissions.xlsx'),
                                  sheet = 'All beds COVID',
                                  skip = 2, 
                                  col_names = FALSE, 
                                  n_max = 5) %>% 
  rename(Item = ...1,
         Description = ...2) %>%
  mutate(Description = ifelse(Item == 'Published:', as.character(format(as.Date(as.numeric(Description), origin = "1899-12-30"), '%d-%b-%Y')), Description))

trust_admission_date <- read_excel( paste0(github_repo_dir,'/Source_files/trust_admissions.xlsx'),
                                    sheet = 'All beds COVID',
                                    skip = 2, 
                                    col_names = FALSE, 
                                    n_max = 5) %>% 
  rename(Item = ...1,
         Description = ...2) %>%
  filter(Item == 'Published:') %>% 
  mutate(Description  = as.Date(as.numeric(Description), origin = "1899-12-30"))

# rm(trust_admissions_1, trust_admissions_2, trust_admissions_3, trust_admissions_4, trust_admissions_5)

trust_summary_1_beds <- trust_admissions_4 %>% 
  group_by(Name) %>% 
  arrange(Name, Date) %>% 
  mutate(Previous_occupying_beds = lag(Patients_occupying_beds, 7)) %>% 
  mutate(Perc_change_on_beds_occupied = round((Patients_occupying_beds - lag(Patients_occupying_beds, 7))/ lag(Patients_occupying_beds, 7), 2)) %>% 
  mutate(Perc_change_on_beds_occupied = ifelse(Perc_change_on_beds_occupied == Inf, 1, Perc_change_on_beds_occupied)) %>% 
  mutate(Perc_change_on_beds_occupied = replace_na(Perc_change_on_beds_occupied, 0)) %>% 
  mutate(Date_pr = lag(Date, 7)) %>% 
  filter(Date %in% max(Date)) %>% 
  select(Name, Date, Patients_occupying_beds, Previous_occupying_beds, Perc_change_on_beds_occupied) %>% 
 mutate(Change_direction = ifelse(Perc_change_on_beds_occupied <0, 'decreased', ifelse(Perc_change_on_beds_occupied == 0, 'stayed the same', ifelse(Perc_change_on_beds_occupied > 0, 'increased', NA)))) %>% 
  rename(Beds_date = Date) %>% 
  mutate(Bed_type = 'All beds')

trust_summary_2_beds <- trust_admissions_5 %>% 
  group_by(Name) %>% 
  arrange(Name, Date) %>% 
  mutate(Previous_occupying_beds = lag(Patients_occupying_beds, 7)) %>% 
  mutate(Perc_change_on_beds_occupied = round((Patients_occupying_beds - lag(Patients_occupying_beds, 7))/ lag(Patients_occupying_beds, 7), 2)) %>% 
  mutate(Perc_change_on_beds_occupied = ifelse(Perc_change_on_beds_occupied == Inf, 1, Perc_change_on_beds_occupied)) %>% 
  mutate(Perc_change_on_beds_occupied = replace_na(Perc_change_on_beds_occupied, 0)) %>% 
  mutate(Date_pr = lag(Date, 7)) %>% 
  filter(Date %in% max(Date)) %>% 
  select(Name, Date, Patients_occupying_beds, Previous_occupying_beds, Perc_change_on_beds_occupied) %>% 
  mutate(Change_direction = ifelse(Perc_change_on_beds_occupied <0, 'Down', ifelse(Perc_change_on_beds_occupied == 0, 'Same', ifelse(Perc_change_on_beds_occupied > 0, 'Up', NA)))) %>% 
  rename(Beds_date = Date) %>% 
  mutate(Bed_type = 'Mechanical Ventilation')

trust_summary_2_beds %>% 
  rename(Patients_occupying_mv_beds = Patients_occupying_beds) %>% 
  select(Name, Patients_occupying_mv_beds) %>% 
  left_join(trust_summary_1_beds, by = 'Name') %>% 
  mutate(Beds_date = format(Beds_date, '%A %d %B')) %>%
  select(Name, Patients_occupying_beds, Beds_date, Previous_occupying_beds, Change_direction, Perc_change_on_beds_occupied , Patients_occupying_mv_beds) %>% 
  toJSON() %>% 
  write_lines(paste0(output_directory_x,'/trust_bed_summary.json'))

trust_admissions_summary <- trust_admissions_1 %>% 
  select(Name, Date, Admissions_or_new_cases_in_last_24hrs) %>% 
  group_by(Name) %>% 
  arrange(Name, Date) %>% 
  mutate(Previous_Admissions_or_new_cases_in_last_24hrs = lag(Admissions_or_new_cases_in_last_24hrs, 7)) %>% 
  mutate(Perc_change_on_new_cases_24hrs = round((Admissions_or_new_cases_in_last_24hrs - lag(Admissions_or_new_cases_in_last_24hrs, 7))/ lag(Admissions_or_new_cases_in_last_24hrs, 7), 2)) %>% 
  mutate(Perc_change_on_new_cases_24hrs = ifelse(Perc_change_on_new_cases_24hrs == Inf, 1,Perc_change_on_new_cases_24hrs)) %>% 
  mutate(Perc_change_on_new_cases_24hrs = replace_na(Perc_change_on_new_cases_24hrs, 0)) %>% 
  mutate(Date_pr = lag(Date, 7)) %>% 
  filter(Date %in% c(max(Date))) %>% 
  select(Name, Date, Admissions_or_new_cases_in_last_24hrs, Previous_Admissions_or_new_cases_in_last_24hrs, Perc_change_on_new_cases_24hrs) %>% 
  mutate(Change_direction = ifelse(Perc_change_on_new_cases_24hrs <0, 'Down', ifelse(Perc_change_on_new_cases_24hrs == 0, 'Same', ifelse(Perc_change_on_new_cases_24hrs > 0, 'Up', NA)))) %>% 
  rename(Admissions_date = Date)

trust_admissions_summary %>% 
  toJSON() %>% 
  write_lines(paste0(output_directory_x,'/trust_admission_summary.json'))

trust_summary_4 <- trust_admissions_2 %>% 
  select(Name, Date, Patients_admitted_for_first_time_with_covid_or_new_cases_in_last_24hrs) %>% 
  group_by(Name) %>% 
  arrange(Name, Date) %>% 
  mutate(Previous_Patients_admitted_for_first_time_with_covid_or_new_cases_in_last_24hrs = lag(Patients_admitted_for_first_time_with_covid_or_new_cases_in_last_24hrs, 7)) %>% 
  mutate(Perc_change_on_patients_admitted_for_first_time = round((Patients_admitted_for_first_time_with_covid_or_new_cases_in_last_24hrs - lag(Patients_admitted_for_first_time_with_covid_or_new_cases_in_last_24hrs, 7))/ lag(Patients_admitted_for_first_time_with_covid_or_new_cases_in_last_24hrs, 7), 2)) %>% 
  mutate(Perc_change_on_patients_admitted_for_first_time = ifelse(Perc_change_on_patients_admitted_for_first_time == Inf, 1,Perc_change_on_patients_admitted_for_first_time)) %>% 
  mutate(Perc_change_on_patients_admitted_for_first_time = replace_na(Perc_change_on_patients_admitted_for_first_time, 0)) %>% 
  mutate(Date_pr = lag(Date, 7)) %>% 
  filter(Date %in% c(max(Date))) %>% 
  select(Name, Date, Patients_admitted_for_first_time_with_covid_or_new_cases_in_last_24hrs, Previous_Patients_admitted_for_first_time_with_covid_or_new_cases_in_last_24hrs, Perc_change_on_patients_admitted_for_first_time) %>% 
  mutate(Change_direction = ifelse(Perc_change_on_patients_admitted_for_first_time <0, 'Down', ifelse(Perc_change_on_patients_admitted_for_first_time == 0, 'Same', ifelse(Perc_change_on_patients_admitted_for_first_time > 0, 'Up', NA)))) %>% 
  rename(First_time_admissions_date = Date)

trust_summary_5 <- trust_admissions_3 %>% 
  select(Name, Date, Patients_admitted_for_first_time_with_covid_or_new_cases_in_last_24hrs_for_patients_admitted_in_last_seven_days) %>% 
  group_by(Name) %>% 
  arrange(Name, Date) %>% 
  mutate(Previous_Patients_admitted_for_first_time_with_covid_or_new_cases_in_last_24hrs_for_patients_admitted_in_last_seven_days = lag(Patients_admitted_for_first_time_with_covid_or_new_cases_in_last_24hrs_for_patients_admitted_in_last_seven_days, 7)) %>% 
  mutate(Perc_change_on_patients_admitted_for_first_time_admissions_7_days = round((Patients_admitted_for_first_time_with_covid_or_new_cases_in_last_24hrs_for_patients_admitted_in_last_seven_days - lag(Patients_admitted_for_first_time_with_covid_or_new_cases_in_last_24hrs_for_patients_admitted_in_last_seven_days, 7))/ lag(Patients_admitted_for_first_time_with_covid_or_new_cases_in_last_24hrs_for_patients_admitted_in_last_seven_days, 7), 2)) %>% 
  mutate(Perc_change_on_patients_admitted_for_first_time_admissions_7_days = ifelse(Perc_change_on_patients_admitted_for_first_time_admissions_7_days == Inf, 1,Perc_change_on_patients_admitted_for_first_time_admissions_7_days)) %>% 
  mutate(Perc_change_on_patients_admitted_for_first_time_admissions_7_days = replace_na(Perc_change_on_patients_admitted_for_first_time_admissions_7_days, 0)) %>% 
  mutate(Date_pr = lag(Date, 7)) %>% 
  filter(Date %in% c(max(Date))) %>% 
  select(Name, Date, Patients_admitted_for_first_time_with_covid_or_new_cases_in_last_24hrs_for_patients_admitted_in_last_seven_days, Previous_Patients_admitted_for_first_time_with_covid_or_new_cases_in_last_24hrs_for_patients_admitted_in_last_seven_days, Perc_change_on_patients_admitted_for_first_time_admissions_7_days) %>% 
  mutate(Change_direction = ifelse(Perc_change_on_patients_admitted_for_first_time_admissions_7_days <0, 'Down', ifelse(Perc_change_on_patients_admitted_for_first_time_admissions_7_days == 0, 'Same', ifelse(Perc_change_on_patients_admitted_for_first_time_admissions_7_days > 0, 'Up', NA)))) %>% 
  rename(First_time_admissions_7_days_date = Date)

trust_admission_date %>% 
  mutate(Date_published = format(Description, '%A %d %B')) %>% 
  select(Date_published) %>% 
  toJSON() %>% 
  write_lines(paste0(output_directory_x,'/trust_meta.json'))

# Mortality ####

# Area lookup

mye_total_raw <- read_csv('http://www.nomisweb.co.uk/api/v01/dataset/NM_2002_1.data.csv?geography=1816133633...1816133848,1820327937...1820328318,2092957697...2092957703,2013265921...2013265932&date=latest&gender=0&c_age=200,209&measures=20100&select=date_name,geography_name,geography_code,geography_type,c_age_name,obs_value') %>%  
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

week_ending <- data.frame(Week_ending = get_date(week = 1:52, year = 2020)) %>% 
  mutate(Week_number = row_number())

download.file(paste0('https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fhealthandsocialcare%2fcausesofdeath%2fdatasets%2fdeathregistrationsandoccurrencesbylocalauthorityandhealthboard%2f2020/lahbtablesweek',substr(as.character(as.aweek(Sys.Date()-11)), 7,8), '.xlsx'),  paste0(github_repo_dir, '/Source_files/ons_mortality.xlsx'), mode = 'wb')

# # if the download does fail, it wipes out the old one, which we can use to our advantage
if(!file.exists(paste0(github_repo_dir, '/Source_files/ons_mortality.xlsx'))){
  download.file(paste0('https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fhealthandsocialcare%2fcausesofdeath%2fdatasets%2fdeathregistrationsandoccurrencesbylocalauthorityandhealthboard%2f2020/lahbtablesweek',substr(as.character(as.aweek(Sys.Date()-12)), 7,8), '.xlsx'),  paste0(github_repo_dir, '/Source_files/ons_mortality.xlsx'), mode = 'wb')
}

Occurrences_ltla <- read_excel(paste0(github_repo_dir, '/Source_files/ons_mortality.xlsx'), sheet = 'Occurrences - All data', skip = 2) %>% 
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

Occurrences_england <- read_excel(paste0(github_repo_dir, '/Source_files/ons_mortality.xlsx'), sheet = 'Occurrences - All data', skip = 2) %>% 
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

Occurrences_southeast <- read_excel(paste0(github_repo_dir, '/Source_files/ons_mortality.xlsx'), sheet = 'Occurrences - All data', skip = 2) %>% 
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

Occurrences_southeast_check <- read_excel(paste0(github_repo_dir, '/Source_files/ons_mortality.xlsx'), sheet = 'Occurrences - All data', skip = 2) %>% 
  rename(Name = `Area name`,
         Cause = `Cause of death`,
         Week_number = `Week number`,
         Place_of_death = `Place of death`,
         Deaths = `Number of deaths`) %>%  
  filter(`Area code` %in% lookup$LTLA19CD)

Occurrences_southeast_check %>% 
  select(Name) %>% 
  unique()

Occurrences<- Occurrences_ltla %>% 
  bind_rows(Occurrences_wsx) %>% 
  bind_rows(Occurrences_england) %>% 
  bind_rows(Occurrences_southeast)

rm(Occurrences_ltla, Occurrences_wsx, Occurrences_southeast, Occurrences_england)

deaths_labels <- Occurrences %>% 
  arrange(Week_number) %>% 
  select(Week_ending) %>% 
  unique() %>% 
  mutate(deaths_label = paste0('w/e ', ordinal(as.numeric(format(Week_ending, '%d'))), format(Week_ending, ' %b')))

# calculating release date 
Occurrences_meta <- week_ending %>% 
  filter(Week_number == max(Occurrences$Week_number)) %>% 
  mutate(registered_by = Week_ending + 8,
         published_on = Week_ending + 11) %>% 
  pivot_longer(cols = c(Week_ending, registered_by, published_on), names_to = 'Item') %>% 
  mutate(Label = format(value, '%A %d %B'))

Occurrences_meta %>% 
  toJSON() %>% 
  write_lines(paste0(output_directory_x, '/mortality_dates.json'))

weekly_all_place_all_deaths <- Occurrences %>% 
  filter(Cause == 'All causes') %>% 
  arrange(Week_number) %>% 
  group_by(Name, Week_ending) %>% 
  summarise(Deaths = sum(Deaths, na.rm = TRUE)) %>% 
  select(Name, Week_ending, Deaths) %>% 
  mutate(Week_ending = factor(paste0('w/e ', ordinal(as.numeric(format(Week_ending, '%d'))), format(Week_ending, ' %b')), levels = deaths_labels$deaths_label)) %>% 
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
  arrange(Week_number) %>% 
  select(Name, Cause, Week_ending, Deaths) %>% 
  mutate(Week_ending = factor(paste0('w/e ', ordinal(as.numeric(format(Week_ending, '%d'))), format(Week_ending, ' %b')), levels = deaths_labels$deaths_label)) %>% 
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
  filter(Week_number == max(Week_number)) %>% 
  select(Name, Week_number, Week_ending, Cause, Deaths) %>% 
  mutate(Cause = paste0(Cause, ' deaths this week')) %>% 
  pivot_wider(names_from = Cause, values_from = Deaths)
  
deaths_summary_2 <- All_settings_occurrences %>% 
  filter(Week_number == max(Week_number)) %>% 
  select(Name, Week_number, Cause, Cumulative_deaths) %>% 
  mutate(Cause = paste0(Cause, ' deaths so far')) %>% 
  pivot_wider(names_from = Cause, values_from = Cumulative_deaths)


# Care home deaths
carehome_deaths_summary <- Occurrences %>%
  filter(Place_of_death %in% 'Care home') %>% 
  arrange(Week_number) %>% 
  select(Name, Cause, Week_ending, Week_number, Deaths) %>% 
  # mutate(Week_ending = factor(paste0('w/e ', ordinal(as.numeric(format(Week_ending, '%d'))), format(Week_ending, ' %b')), levels = deaths_labels$deaths_label)) %>% 
  mutate(Cause = paste0(Cause, ' deaths this week')) %>% 
  pivot_wider(id_cols = c(Name, Week_ending, Week_number),
                names_from = Cause,
                values_from = Deaths) %>% 
  group_by(Name) %>% 
  arrange(Name, Week_number) %>% 
  mutate(`All causes deaths so far` = cumsum(`All causes deaths this week`),
         `COVID 19 deaths so far` = cumsum(`COVID 19 deaths this week`))  %>% 
  ungroup() %>% 
  filter(Week_number == max(Week_number)) %>% 
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

msoa_cases_1 <- read_csv('https://coronavirus.data.gov.uk/downloads/msoa_data/MSOAs_latest.csv') %>% 
  # filter(areaCode %in% msoa_lookup$MSOA11CD) %>% 
  filter(date %in% c(max(date))) %>% 
  select(areaCode, date, newCasesBySpecimenDateRollingRate) %>% 
  rename(Latest_rate = newCasesBySpecimenDateRollingRate) %>% 
  mutate(Latest_rate_key = factor(ifelse(is.na(Latest_rate), 'Less than 3 cases', ifelse(Latest_rate <= 50, 'Up to 50 per 100,000', ifelse(Latest_rate <= 100, '51-100 cases per 100,000', ifelse(Latest_rate <= 150, '101-150 cases per 100,000', ifelse(Latest_rate <= 200, '151-200 cases per 100,000', 'More than 200 cases per 100,000'))))), levels = c('Less than 3 cases', 'Up to 50 cases per 100,000', '51-100 cases per 100,000', '101-150 cases per 100,000', '151-200 cases per 100,000', 'More than 200 cases per 100,000')))

msoa_cases_raw <- as.data.frame(read_csv('https://coronavirus.data.gov.uk/downloads/msoa_data/MSOAs_latest.csv') %>% 
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

msoa_boundaries_json <- geojson_read("https://opendata.arcgis.com/datasets/23cdb60ee47e4fef8d72e4ee202accb0_0.geojson",  what = "sp") %>% 
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
  select(MSOA11NM, msoa11hclnm, Latest_rate, UTLA19NM, This_week, Last_week, Change_label, date, Change_actual) %>% 
  mutate(Change_label = paste0(ifelse(is.na(Last_week) & is.na(This_week), 'Cases have been below 3 in the last two 7 day periods.', ifelse(is.na(Last_week), paste0('Cases were below 3 in the previous 7 days (up to ', format(max(date)-7, '%A %d %B') ,') but have risen this week.'), ifelse(is.na(This_week), paste0('Cases are now below 3 in the latest 7 days but have fallen since the previous 7 day period (up to ', format(max(date)-7, '%A %d %B') ,').'), ifelse(Change_actual == 0, 'There is no change in case numbers over the last two weeks', ifelse(Change_actual < 0, paste0('<b class = "cases_go_down">', Change_actual, '</b> cases compared to the previous 7 days (', Last_week,  ' cases in the 7 days to ', format(max(date)-7, '%A %d %B') ,').'), ifelse(Change_actual >0, paste0('<b class = "cases_go_up">+', Change_actual, '</b> cases compared to the previous 7 days (', Last_week, ' cases in the 7 days to ', format(max(date)-7, '%A %d %B') ,').'), NA)))))))) %>% 
  mutate(This_week = ifelse(is.na(This_week), '0-2', format(This_week, big.mark = ',', trim = TRUE)),
         Last_week = ifelse(is.na(Last_week), '0-2', format(Last_week, big.mark = ',', trim = TRUE)),
         Latest_rate = ifelse(is.na(Latest_rate), 'No rate available', Latest_rate)) %>%
  select(!c('date', 'Change_actual')) %>% 
  toJSON() %>% 
  write_lines(paste0(output_directory_x,'/msoa_summary.json'))

utla_summary_1 <- p12_test_df %>% 
  filter(Type %in% c('Unitary Authority', 'Upper Tier Local Authority')) %>% 
  filter(Date == last_case_date) %>% 
  select(Name, Date, Cumulative_cases, Cumulative_per_100000) %>% 
  rename(Cumulative_date = Date)

utla_summary_2 <- p12_test_df %>% 
  filter(Date %in% c(complete_date)) %>% 
  select(Name, Date, New_cases, New_cases_per_100000, Rolling_7_day_new_cases, Rolling_7_day_new_cases_per_100000, Rolling_7_day_average_new_cases, Perc_change_on_rolling_7_days_actual, Previous_7_days_sum) %>% 
  mutate(Change_direction = ifelse(Perc_change_on_rolling_7_days_actual <0, 'Down', ifelse(Perc_change_on_rolling_7_days_actual == 0, 'Same', ifelse(Perc_change_on_rolling_7_days_actual > 0, 'Up', NA)))) %>% 
  rename(Rate_date = Date)

utla_summary <- utla_summary_1 %>% 
  left_join(utla_summary_2, by = 'Name') %>% 
  left_join(read_csv(paste0(github_repo_dir, '/Source_files/utla_tiers.csv')), by = 'Name') %>% 
  mutate(Change_label = ifelse(Change_direction == 'Down', paste0('decreased by ', format(abs(Previous_7_days_sum - Rolling_7_day_new_cases), big.mark = ',', trim = TRUE), ' cases (', round(abs(Perc_change_on_rolling_7_days_actual) * 100,1), '%)'),ifelse(Change_direction == 'Up', paste0('increased by ',   format(abs(Previous_7_days_sum - Rolling_7_day_new_cases), big.mark = ',', trim = TRUE), ' cases (', round(abs(Perc_change_on_rolling_7_days_actual) * 100,1), '%)'), 'stayed the same.')))

utla_summary %>% 
  toJSON() %>% 
  write_lines(paste0(output_directory_x,'/utla_summary.json'))


utla_restrictions_geojson <-geojson_read("https://opendata.arcgis.com/datasets/b216b4c8a4e74f6fb692a1785255d777_0.geojson",  what = "sp") %>% 
  filter(substr(ctyua19cd, 1,1 ) == 'E') %>% 
  mutate(ctyua19nm = ifelse(ctyua19nm %in% c('Cornwall', 'Isles of Scilly'), 'Cornwall and Isles of Scilly', ifelse(ctyua19nm %in% c('City of London', 'Hackney'), 'Hackney and City of London', ctyua19nm))) %>% 
  mutate(ctyua19cd = ifelse(ctyua19cd %in% c('E06000053', 'E06000052'), 'E06000052', ifelse(ctyua19cd %in% c('E09000001', 'E09000012'), 'E09000012', ctyua19cd))) %>% 
  group_by(ctyua19cd, ctyua19nm) %>% 
  summarise() %>% 
  arrange(ctyua19cd) %>% 
  left_join(utla_summary, by = c('ctyua19nm' = 'Name')) 

geojson_write(geojson_json(utla_restrictions_geojson), file = paste0(output_directory_x, '/utla_covid_latest.geojson'))

# daily_cases_df %>% 
#   filter(Name == 'West Sussex') %>% 
#   # filter(Date <= '2020-11-05') %>% 
#   arrange(-Rolling_7_day_new_cases_per_100000) %>% 
#   View()
