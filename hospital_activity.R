
# Hospital admissions ####

# Hospital provider trusts do not have geographically defined boundaries for their population nor do they have complete lists of registered patients. However, modeled estimates of the catchment populations for hospital provider trusts in England are provided by Public Health England (PHE). These experimental statistics estimates the number of people who are using each hospital trust or have the potential to do so. Individual acute trusts sometimes use varying methods to define the population they serve, such as patient flow, CCG derived or travel time based estimates. PHE published modelled estimates use the patient flow method.

# Note: interpretation of the figures should take into account the fact that totals by date of death, particularly for recent prior days, are likely to be updated in future releases. For example as deaths are confirmed as testing positive for Covid-19, as more post-mortem tests are processed and data from them are validated. Any changes are made clear in the daily files.	


library(easypackages)

libraries("readxl", "readr", "plyr", "dplyr", "ggplot2", "png", "tidyverse", "reshape2", "scales", 'zoo', 'stats',"rgdal", 'rgeos', "tmaptools", 'sp', 'sf', 'maptools', 'leaflet', 'leaflet.extras', 'spdplyr', 'geojsonio', 'rmapshaper', 'jsonlite', 'grid', 'aweek', 'xml2', 'rvest', 'officer', 'flextable', 'viridis', 'epitools')

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

#github_repo_dir <- "~/Documents/GitHub/wsx_covid_datapack_public"
github_repo_dir <- '~/GitHub/wsx_covid_datapack_public'
output_directory_x <- paste0(github_repo_dir, '/Outputs')


if(!file.exists(paste0(github_repo_dir, '/Source_files/etr.csv'))){
  download.file('https://files.digital.nhs.uk/assets/ods/current/etr.zip', paste0(github_repo_dir, '/etr.zip'), mode = 'wb')
  unzip(paste0(github_repo_dir, '/etr.zip'), exdir = github_repo_dir)
  file.remove(paste0(github_repo_dir, '/etr.zip'), paste0(github_repo_dir, '/etr.pdf'))
}


calls_hosp_webpage <- read_html('https://www.england.nhs.uk/statistics/statistical-work-areas/covid-19-hospital-activity/') %>%
  html_nodes("a") %>%
  html_attr("href")

download.file(grep('Weekly-covid', calls_hosp_webpage, value = T), paste0(github_repo_dir,'/Source files/trust_admissions.xlsx'), mode = 'wb')

trust_admission_date <- read_excel( paste0(github_repo_dir,'/Source files/trust_admissions.xlsx'),
                                    sheet = 'All beds COVID',
                                    skip = 2, 
                                    col_names = FALSE, 
                                    n_max = 5) %>% 
  rename(Item = ...1,
         Description = ...2) %>%
  filter(Item == 'Published:') %>% 
  mutate(Description  = as.Date(as.numeric(Description), origin = "1899-12-30"))


# We need to use data.gov to get cumulative admissions because the NHS digital data only starts on August 1st

uk_gov_admissions <- read_csv('https://api.coronavirus.data.gov.uk/v2/data?areaType=nhsTrust&metric=cumAdmissions&metric=cumAdmissionsByAge&metric=newAdmissions&format=csv')

# admissions_df <- trust_admissions_1 %>% 
#   group_by(Name) %>% 
#   arrange(Name, Date) %>% 
#   mutate(Rolling_7_day_admissions = rollapply(Admissions_or_new_cases_in_last_24hrs, 7, sum, align = 'right', fill = NA),
#          Total_admissions = cumsum(Admissions_or_new_cases_in_last_24hrs)) %>% 
#   mutate(Name = factor(Name, levels = c('Brighton and Sussex University Hospitals NHS Trust', 'Surrey and Sussex Healthcare NHS Trust', 'Sussex Community NHS Foundation Trust', 'Western Sussex Hospitals NHS Foundation Trust', 'South East', 'England'))) %>% 
#   arrange(Name) %>% 
#   ungroup()
# 
# admissions_table <- admissions_df %>% 
#   filter(Date == max(Date))

trust_admissions_1 <- read_excel( paste0(github_repo_dir,'/Source files/trust_admissions.xlsx'),
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
  select(!c('Type 1 Acute?', 'NHS England Region', 'Code')) %>% 
  mutate(Source_of_admission = 'All')

trust_admissions_1_ch <- read_excel(paste0(github_repo_dir,'/Source files/trust_admissions.xlsx'),
                                     sheet = 'Care home ads and diags',
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
  select(!c('Type 1 Acute?', 'NHS England Region', 'Code')) %>% 
  mutate(Source_of_admission = 'Care home')



admissions_source_df <- trust_admissions_1 %>% 
  bind_rows(trust_admissions_1_ch) %>% 
  pivot_wider(names_from = Source_of_admission, values_from = Admissions_or_new_cases_in_last_24hrs) %>% 
  mutate(All = replace_na(All, 0),
         `Care home` = replace_na(`Care home`, 0)) %>% 
  mutate(`Not care home` = All - `Care home`) %>% 
  select(!All) %>% 
  pivot_longer(cols = c(`Not care home`, `Care home`), names_to = 'Source_of_admission',  values_to = 'Admissions_or_new_cases_in_last_24hrs') %>% 
  mutate(Source_of_admission = factor(Source_of_admission, levels = c('Care home', 'Not care home'))) %>% 
  group_by(Name, Source_of_admission) %>% 
  arrange(Name, Source_of_admission, Date) %>% 
  mutate(Rolling_7_day_admissions = rollapply(Admissions_or_new_cases_in_last_24hrs, 7, sum, align = 'right', fill = NA),
         Total_admissions = cumsum(Admissions_or_new_cases_in_last_24hrs)) 

trust_x <- 'Western Sussex Hospitals NHS Foundation Trust'

admissions_source_x_df <- admissions_source_df %>% 
  filter(Name == trust_x)

ggplot(admissions_source_x_df,
       aes(x = Date,
           y = Admissions_or_new_cases_in_last_24hrs)) +
  geom_bar(stat = 'identity',
           position = position_stack(reverse = TRUE),
           width = .9,
           aes(fill = Source_of_admission)) +
  scale_x_date(date_labels = "%b %d",
               breaks = seq.Date(max(admissions_source_df$Date) -(52*7), max(admissions_source_df$Date), by = 7),
               expand = c(0.01,0.01)) +
  scale_fill_manual(values = c('#ED7D31', '#2F5597')) +
  labs(x = 'Date',
       y = 'Number of admissions',
       title = paste0('Number of new admissions with a positive COVID-19 test result; ',trust_x),
       subtitle = paste0('New admissions or inpatients with a new diagnosis; data up to ', format(max(admissions_source_df$Date), '%d %B %Y'), ' as at ', format(trust_admission_date$Description, '%d %B %Y'))) +
  ph_theme() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5))

rm(trust_admissions_1, trust_admissions_1_ch)

trust_c19_patients_occupying_beds <- read_excel( paste0(github_repo_dir,'/Source_files/trust_admissions.xlsx'),
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
  select(!c('Type 1 Acute?', 'NHS England Region', 'Code')) %>% 
  mutate(Bed_type = 'All')

trust_c19_patients_occupying_mv_beds <- read_excel( paste0(github_repo_dir,'/Source_files/trust_admissions.xlsx'),
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
  select(!c('Type 1 Acute?', 'NHS England Region', 'Code')) %>% 
  mutate(Bed_type = 'Mechanical ventilation')

# patients occupying beds as at 8am

numbers_in_beds <- trust_c19_patients_occupying_beds %>% 
  bind_rows(trust_c19_patients_occupying_mv_beds) %>% 
  pivot_wider(names_from = Bed_type, values_from = Patients_occupying_beds) %>% 
  mutate(All = replace_na(All, 0),
         `Mechanical ventilation` = replace_na(`Mechanical ventilation`, 0)) %>% 
  mutate(Not_mv = All - `Mechanical ventilation`)



trust_c19_patients_occupying_ga_beds <- read_excel( paste0(github_repo_dir,'/Source_files/trust_admissions.xlsx'),
                                                    sheet = 'Adult G&A Beds Occupied COVID',
                                                    skip = 13) %>% 
  filter(!is.na(Name)) %>% 
  mutate(Name = capwords(Name, strict = TRUE)) %>% 
  mutate(Name = gsub('Nhs', 'NHS', Name)) %>% 
  mutate(Name = gsub(' And ', ' and ', Name)) %>% 
  mutate(Name = gsub('Cic', 'CIC', Name)) %>% 
  mutate(Name = gsub('C.i.c', 'C.I.C', Name)) %>% 
  pivot_longer(names_to = 'Date', values_to = 'c19_patients_occupying_ga_beds', cols = 5:ncol(.)) %>% 
  mutate(Date = as.Date(as.numeric(Date), origin = "1899-12-30")) %>% 
  filter(Name %in% c('England', 'South East', 'Western Sussex Hospitals NHS Foundation Trust', 'Surrey and Sussex Healthcare NHS Trust', 'Sussex Community NHS Foundation Trust', 'Brighton and Sussex University Hospitals NHS Trust')) %>% 
  select(!c('Type 1 Acute?', 'NHS England Region', 'Code'))

trust_other_patients_occupying_ga_beds <- read_excel( paste0(github_repo_dir,'/Source_files/trust_admissions.xlsx'),
                                                    sheet = 'Adult G&A Bed Occupied NonCOVID',
                                                    skip = 13) %>% 
  filter(!is.na(Name)) %>% 
  mutate(Name = capwords(Name, strict = TRUE)) %>% 
  mutate(Name = gsub('Nhs', 'NHS', Name)) %>% 
  mutate(Name = gsub(' And ', ' and ', Name)) %>% 
  mutate(Name = gsub('Cic', 'CIC', Name)) %>% 
  mutate(Name = gsub('C.i.c', 'C.I.C', Name)) %>% 
  pivot_longer(names_to = 'Date', values_to = 'other_patients_occupying_ga_beds', cols = 5:ncol(.)) %>% 
  mutate(Date = as.Date(as.numeric(Date), origin = "1899-12-30")) %>% 
  filter(Name %in% c('England', 'South East', 'Western Sussex Hospitals NHS Foundation Trust', 'Surrey and Sussex Healthcare NHS Trust', 'Sussex Community NHS Foundation Trust', 'Brighton and Sussex University Hospitals NHS Trust')) %>% 
  select(!c('Type 1 Acute?', 'NHS England Region', 'Code'))

trust_vacant_ga_beds <- read_excel( paste0(github_repo_dir,'/Source_files/trust_admissions.xlsx'),
                                                      sheet = 'Adult G&A Beds Unoccupied',
                                                      skip = 13) %>% 
  filter(!is.na(Name)) %>% 
  mutate(Name = capwords(Name, strict = TRUE)) %>% 
  mutate(Name = gsub('Nhs', 'NHS', Name)) %>% 
  mutate(Name = gsub(' And ', ' and ', Name)) %>% 
  mutate(Name = gsub('Cic', 'CIC', Name)) %>% 
  mutate(Name = gsub('C.i.c', 'C.I.C', Name)) %>% 
  pivot_longer(names_to = 'Date', values_to = 'vacant_ga_beds', cols = 5:ncol(.)) %>% 
  mutate(Date = as.Date(as.numeric(Date), origin = "1899-12-30")) %>% 
  filter(Name %in% c('England', 'South East', 'Western Sussex Hospitals NHS Foundation Trust', 'Surrey and Sussex Healthcare NHS Trust', 'Sussex Community NHS Foundation Trust', 'Brighton and Sussex University Hospitals NHS Trust')) %>% 
  select(!c('Type 1 Acute?', 'NHS England Region', 'Code'))

bed_used_df <- trust_c19_patients_occupying_ga_beds %>% 
  left_join(trust_other_patients_occupying_ga_beds, by = c('Name', 'Date')) %>% 
  left_join(trust_vacant_ga_beds, by = c('Name', 'Date')) %>%
  pivot_longer(cols = c(c19_patients_occupying_ga_beds, other_patients_occupying_ga_beds, vacant_ga_beds), names_to = 'Bed_status', values_to = 'Beds') %>% 
  group_by(Name, Date) %>% 
  mutate(Total_open_beds = sum(Beds),
         Proportion = Beds/sum(Beds)) %>% 
  mutate(Bed_status = factor(ifelse(Bed_status == 'c19_patients_occupying_ga_beds', 'COVID-19 + patients', ifelse(Bed_status == 'other_patients_occupying_ga_beds', 'Other patients', ifelse(Bed_status == 'vacant_ga_beds', 'Vacant (open) beds', NA))), levels = rev(c('COVID-19 + patients', 'Other patients', 'Vacant (open) beds'))))

# trust_admissions_metadata <- read_excel( paste0(github_repo_dir,'/Source_files/trust_admissions.xlsx'),
#                                          sheet = 'All beds COVID',
#                                          skip = 2, 
#                                          col_names = FALSE, 
#                                          n_max = 5) %>% 
#   rename(Item = ...1,
#          Description = ...2) %>%
#   mutate(Description = ifelse(Item == 'Published:', as.character(format(as.Date(as.numeric(Description), origin = "1899-12-30"), '%d-%b-%Y')), Description))



eng_beds <- bed_used_df %>% 
  filter(Name == 'England')

ggplot(eng_beds,
       aes(x = Date,
           y = Proportion)) +
  geom_bar(stat = 'identity',
           position = position_fill(reverse = TRUE),
           colour = '#ffffff',
           aes(fill = Bed_status)) +
  scale_x_date(date_labels = "%b %d",
               breaks = seq.Date(max(bed_used_df$Date) -(52*7), max(bed_used_df$Date), by = 7),
               expand = c(0.01,0.01)) +
  scale_fill_manual(values = c('#12263a', '#c96480', '#abcdef')) +
  scale_y_continuous(limits = c(0,1),
                     breaks = seq(0,1, .1),
                     labels = percent_format(accuracy = 1)) +
  labs(x = 'Date',
       y = 'Proportion',
       title = paste0('Proportion of patients occupying adult beds in NHS hospital trusts'),
       subtitle = paste0('Share of all adult general and acute beds (%); data up to ', format(max(bed_used_df$Date), '%d %B %Y'), ' as at ', format(trust_admission_date$Description, '%d %B %Y')),
       caption = 'This measure only includes adult inpatient beds. It is estimated that adult beds comprised more than 99% of inpatient beds in NHS hospital trusts.') +
  ph_theme() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5))

ggplot(eng_beds,
       aes(x = Date,
           y = Beds)) +
  geom_bar(stat = 'identity',
           colour = '#ffffff',
           aes(fill = Bed_status)) +
  scale_x_date(date_labels = "%b %d",
               breaks = seq.Date(max(bed_used_df$Date) -(52*7), max(bed_used_df$Date), by = 7),
               expand = c(0.01,0.01)) +
  scale_fill_manual(values = c('#12263a', '#c96480', '#abcdef')) +
  labs(x = 'Date',
       y = 'Number of beds',
       title = paste0('Number of patients occupying adult beds in NHS hospital trusts'),
       subtitle = paste0('Share of all adult general and acute beds (%); data up to ', format(max(bed_used_df$Date), '%d %B %Y'), ' as at ', format(trust_admission_date$Description, '%d %B %Y')),
       caption = 'This measure only includes adult inpatient beds. It is estimated that adult beds comprised more than 99% of inpatient beds in NHS hospital trusts.') +
  ph_theme() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5))
  
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

trust_admissions_5 %>% 
  rename(Patients_occupying_MV_beds = Patients_occupying_beds) %>% 
  left_join(trust_admissions_4, by = c('Name', 'Date')) %>% 
  toJSON() %>% 
  write_lines(paste0(output_directory_x,'/trust_bed_occupancy_df.json'))

# capacity <- read_csv(paste0('https://api.coronavirus.data.gov.uk/v2/data?areaType=', area_level, '&metric=capacityPillarFour&metric=capacityPillarOne&metric=capacityPillarOneTwo&metric=capacityPillarThree&format=csv'))



# STP/ICS vaccine counts ####
# https://www.england.nhs.uk/statistics/statistical-work-areas/covid-19-vaccinations/
