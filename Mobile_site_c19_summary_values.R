

library(easypackages)

libraries(c("readxl", "readr", "plyr", "dplyr", "ggplot2", "png", "tidyverse", "reshape2", "scales", 'zoo', 'stats',"rgdal", 'rgeos', "tmaptools", 'sp', 'sf', 'maptools', 'leaflet', 'leaflet.extras', 'fingertipsR', 'spdplyr', 'geojsonio', 'rmapshaper', 'jsonlite', 'grid', 'aweek', 'xml2', 'rvest', 'officer', 'flextable', 'viridis'))

capwords = function(s, strict = FALSE) {
  cap = function(s) paste(toupper(substring(s, 1, 1)),
                          {s = substring(s, 2); if(strict) tolower(s) else s},sep = "", collapse = " " )
  sapply(strsplit(s, split = " "), cap, USE.NAMES = !is.null(names(s)))}

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

github_repo_dir <- "~/Documents/Repositories/wsx_covid_public_mobile_site"
output_directory_x <- paste0(github_repo_dir, '/Outputs')
areas_to_loop <- c('West Sussex', 'Adur', 'Arun', 'Chichester', 'Crawley', 'Horsham', 'Mid Sussex', 'Worthing')


# Hospital admissions ####

# Hospital provider trusts do not have geographically defined boundaries for their population nor do they have complete lists of registered patients. However, modelled estimates of the catchment populations for hospital provider trusts in England are provided by Public Health England (PHE). These experimental statistics estimates the number of people who are using each hospital trust or have the potential to do so. Individual acute trusts sometimes use varying methods to define the population they serve, such as patient flow, CCG derived or travel time based estimates. PHE published modelled estimates use the patient flow method.

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
  pivot_longer(names_to = 'Date', values_to = 'COVID_confirmed_positive_patients_occupying_beds', cols = 5:ncol(.)) %>% 
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
  pivot_longer(names_to = 'Date', values_to = 'COVID_confirmed_positive_patients_occupying_mechanical_ventilation_beds', cols = 5:ncol(.)) %>% 
  mutate(Date = as.Date(as.numeric(Date), origin = "1899-12-30")) %>% 
  filter(Name %in% c('England', 'South East', 'Western Sussex Hospitals NHS Foundation Trust', 'Surrey and Sussex Healthcare NHS Trust', 'Sussex Community NHS Foundation Trust', 'Brighton and Sussex University Hospitals NHS Trust')) %>% 
  select(!c('Type 1 Acute?', 'NHS England Region', 'Code'))

trust_admissions <- trust_admissions_4 %>% 
  left_join(trust_admissions_5, by = c('Name', 'Date')) %>% 
  left_join(trust_admissions_1, by = c('Name', 'Date')) %>% 
  left_join(trust_admissions_2, by = c('Name', 'Date')) %>% 
  left_join(trust_admissions_3, by = c('Name', 'Date'))


