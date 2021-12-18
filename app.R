
# setup -------------------------------------------------------------------

library(lubridate)
library(sf)
library(tmap)
library(shiny)
library(shinydashboard)
library(tidyverse)
library(dplyr)
library(plotly)

# data --------------------------------------------------------------------

# read in public school data, clean column names, filter & keep relevant cols

nces <-
  readxl::read_excel('data/raw/elsi_excel.xlsx') %>% 
  set_names(
    str_remove_all(
      names(.),
      '[^A-Za-z]|Latest available year|Public School')) %>% 
  mutate(StateName = tolower(StateName)) %>% 
  filter(StateAbbr %in% c('IL','CA','NY'),
         SchoolLevelSYonward == 'Elementary') %>% 
  select(-c(AgencyName,
            SchoolType,
            CharterSchool,
            MagnetSchool,
            SharedTimeSchool,
            UrbancentricLocale,
            FullTimeEquivalentFTETeachers,
            FreeLunchEligible,
            ReducedpriceLunchEligibleStudents))

# read in student expenditure data and select relevant columns

nerds <- 
  read_csv('data/raw/nerds.csv') %>% 
  mutate(SchoolIDNCESAssigned = as.numeric(ncesid)) %>% 
  select(SchoolIDNCESAssigned,
         pp_total_raw,
         state)

# store expenditure data separately for each state

nested_exp <- 
  nerds %>%
  filter(state %in% c('IL','CA','NY')) %>% 
  group_by(state) %>% 
  nest()

# specify path to read in school attendance boundary (SAB) shapefiles:

read_dir <- 'data/raw/spatial/SABS'

# read in SAB shapefiles, set names, and assign to global environment

list.files(
  read_dir,
  pattern = '.shp') %>% 
  purrr::map(
    ~ file.path(read_dir, .) %>% 
      st_read() %>% 
      st_make_valid()) %>% 
  set_names(
    'ny_sab',
    'chicago_sab',
    'la_sab') %>% 
  list2env(.GlobalEnv)

# specify path to read in redlining/HOLC map shapefiles

read_dir <- 'data/raw/spatial/HOLC'

# read in redlining shapefiles, set names, and assign to global environment

list.files(
  read_dir,
  pattern = '.shp') %>% 
  purrr::map(
    ~ file.path(read_dir, .) %>% 
      st_read() %>% 
      st_make_valid()) %>% 
  set_names(
    'la_holc_shp',
    'ch_holc_shp',
    'ny_bronx_holc_shp',
    'ny_brklyn_holc_shp',
    'ny_mhtn_holc_shp',
    'ny_queens_holc_shp',
    'ny_stisl_holc_shp') %>% 
  list2env(.GlobalEnv)

# store NY shapefiles as tibbles

list(ny_brklyn_holc_shp,
     ny_bronx_holc_shp,
     ny_mhtn_holc_shp,
     ny_queens_holc_shp,
     ny_stisl_holc_shp) %>% 
  map(~as_tibble(.)) %>% 
  set_names('ny_brklyn_holc_tibble',
            'ny_bronx_holc_tibble',
            'ny_mhtn_holc_tibble',
            'ny_queens_holc_tibble',
            'ny_stisl_holc_tibble') %>% 
  list2env(.GlobalEnv)

# combine New York shapefiles into one shapefile

ny_holc_shp <- 
  rbind(ny_brklyn_holc_tibble,
        ny_bronx_holc_tibble,
        ny_mhtn_holc_tibble,
        ny_queens_holc_tibble,
        ny_stisl_holc_tibble) %>% 
  filter(holc_grade %in% c('A',
                           'B',
                           'C',
                           'D')) %>% 
  st_as_sf()

# Get data ready

# transform projections of all shapefiles (redlining maps & SABs)

list(chicago_sab,
     la_sab,
     ny_sab %>% 
       filter(!st_is_empty(.)),
     ch_holc_shp,
     la_holc_shp,
     ny_holc_shp) %>% 
  map(~st_transform(., crs = 5070)) %>% 
  set_names('chicago_sab_prj',
            'la_sab_prj',
            'ny_sab_prj',
            'ch_holc_shp_prj',
            'la_holc_shp_prj',
            'ny_holc_shp_prj') %>% 
  list2env(.GlobalEnv)

# Chicago - find intersection of redlining maps and SABs

ch_sab_holc <- 
  st_intersection(
    ch_holc_shp_prj,
    chicago_sab_prj) %>% 
  st_transform(
    st_crs(ch_holc_shp))

# LA - find intersection of redlining maps and SABs 

la_sab_holc <- 
  st_intersection(
    la_holc_shp_prj,
    la_sab_prj) %>% 
  rename(unit_id = OBJECTID) %>% 
  st_transform(
    st_crs(la_holc_shp))

# NY - find intersection of redlining maps and SABs

ny_sab_holc <- 
  st_intersection(
    ny_holc_shp_prj,
    ny_sab_prj) %>% 
  rename(unit_id = esid_no) %>% 
  st_transform(
    st_crs(ny_holc_shp))

# add area percentage and create new column to store majority HOLC grade

# add in areas 

list(ch_sab_holc,
     la_sab_holc,
     ny_sab_holc) %>% 
  map(~mutate(.,
              area = st_area(.) %>%
                as.numeric()) %>% 
        group_by(unit_id,
                 holc_grade) %>%
        summarize(
          area = sum(area)) %>% 
        mutate(
          area_perc = area/sum(area),
          maj_grade = ifelse(
            area_perc > 0.5,
            holc_grade,
            NA),
          holc_maj_grade = last(na.omit(maj_grade))) %>%
        mutate_at('unit_id',
                  as.numeric) %>% 
        drop_na(holc_maj_grade) %>% 
        select(-maj_grade)) %>% 
  set_names('att_area_ch_perc',
            'att_area_la_perc',
            'att_area_ny_perc') %>% 
  list2env(.GlobalEnv)

# nest public school data 

nested_publicsch <- 
  nces %>% 
  group_by(StateAbbr) %>% 
  nest() 

# convert to spatial points files

list(nested_publicsch$data[[1]] %>% 
       add_column(
         StateAbbr = 'IL'),
     nested_publicsch$data[[2]] %>% 
       add_column(
         StateAbbr = 'NY'),
     nested_publicsch$data[[3]] %>% 
       add_column(
         StateAbbr = 'CA')) %>% 
  map(~st_as_sf(.,
                coords = c('Longitude',
                           'Latitude'),
                crs = 4269)) %>% 
  set_names('il_schools_sf',
            'ny_schools_sf',
            'ca_schools_sf') %>% 
  list2env(.GlobalEnv)

# transform CRS of both spatial points files and shapefiles to planar
# (required by st_intersection)

list(il_schools_sf,
     ca_schools_sf,
     ny_schools_sf,
     chicago_sab,
     la_sab,
     ny_sab %>% 
       filter(!st_is_empty(.))) %>% 
  map(~st_transform(.,
                    crs = 2163)) %>% 
  set_names('il_schools_prj',
            'ca_schools_prj',
            'ny_schools_prj',
            'chicago_sab_prj',
            'la_sab_prj',
            'ny_sab_prj') %>% 
  list2env(.GlobalEnv)

# nest SAB and redlining intersection data

nested_sab_holc <-
  rbind(
    ch_sab_holc %>% 
      select(unit_id) %>% 
      add_column(city = 'Chicago'),
    la_sab_holc %>%
      select(unit_id) %>% 
      add_column(city = 'Los Angeles'),
    ny_sab_holc %>%
      select(unit_id) %>% 
      add_column(city = 'New York')) %>% 
  group_by(city) %>% 
  nest()

# nest planar CRS spatial point files

nested_schools_prj <-
  rbind(
    il_schools_prj,
    ca_schools_prj,
    ny_schools_prj) %>% 
  group_by(StateAbbr) %>% 
  nest()

# intersect redlining shapefiles and points (schools)

schools_sab_holc <-
  map_dfr(
    1:length(unique(nested_sab_holc$data)),
    function(i) {
      st_intersection(
        st_transform(
          nested_sab_holc$data[[i]],
          crs = 2163),
        nested_schools_prj$data[[i]]) %>% 
        mutate_at('unit_id',
                  as.numeric)})

# separate data for the three cities and store in global env

list(
  schools_sab_holc %>% 
    filter(StateName == 'illinois'),
  schools_sab_holc %>% 
    filter(StateName == 'california'),
  schools_sab_holc %>% 
    filter(StateName == 'new york')) %>% 
  set_names('el_ch_schools_sab_holc',
            'el_la_schools_sab_holc',
            'el_ny_schools_sab_holc') %>% 
  list2env(.GlobalEnv)

# nest SAB, HOLC grade objects

att_area_perc <- 
  rbind(att_area_ch_perc %>% 
          add_column(city = 'Chicago',
                     state = 'illinois'),
        att_area_la_perc %>% 
          add_column(city = 'Los Angeles',
                     state = 'california'),
        att_area_ny_perc %>% 
          add_column(city = 'New York',
                     state = 'new york')) %>%
  group_by(city) %>% 
  nest()

# nest SAB and school objects

el_schools_sab_holc <- 
  rbind(el_ch_schools_sab_holc,
        el_la_schools_sab_holc,
        el_ny_schools_sab_holc) %>% 
  group_by(StateName) %>% 
  nest()

# store SAB, HOLC grade and school info in a dataframe

combined_df <-
  map_dfr(
    1:length(unique(att_area_perc$data)),
    function(i) {
      left_join(
        att_area_perc$data[[i]],
        el_schools_sab_holc$data[[i]],
        by = "unit_id") %>%
        as.data.frame()})

# store dataframes for each city separately

list(combined_df %>% 
       filter(state == 'illinois'),
     combined_df %>% 
       filter(state == 'california'),
     combined_df %>% 
       filter(state == 'new york')) %>%
  set_names('ch_df',
            'la_df',
            'ny_df') %>% 
  list2env(.GlobalEnv)

# determine schools in school attendance boundaries with HOLC grade A majority
# area for each city

list(ch_df,
     la_df,
     ny_df) %>% 
  map(~filter(.,
              holc_grade == 'A')) %>% 
  set_names('ch_schools_a',
            'la_schools_a',
            'ny_schools_a') %>% 
  list2env(.GlobalEnv)

# determine schools in school attendance boundaries with HOLC grade D majority
# area for each city

list(ch_df,
     la_df,
     ny_df) %>% 
  map(~filter(.,
              holc_grade == 'D')) %>% 
  set_names('ch_schools_d',
            'la_schools_d',
            'ny_schools_d') %>% 
  list2env(.GlobalEnv)

# determine schools in school attendance boundaries with HOLC grade A or B
# majority areas

list(ch_df,
     la_df,
     ny_df) %>% 
  map(~filter(.,
              holc_grade %in% c('A','B'))) %>% 
  set_names('ch_schools_a_b',
            'la_schools_a_b',
            'ny_schools_a_b') %>% 
  list2env(.GlobalEnv)

# determine schools in school attendance boundaries with HOLC grade C or D
# majority areas

list(ch_df,
     la_df,
     ny_df) %>% 
  map(~filter(.,
              holc_grade %in% c('C','D'))) %>% 
  set_names('ch_schools_c_d',
            'la_schools_c_d',
            'ny_schools_c_d') %>% 
  list2env(.GlobalEnv)

# store average race % of schools for HOLC majority A grade

list(ch_schools_a,
     la_schools_a,
     ny_schools_a) %>% 
  map(~select(.,
              AmericanIndianAlaskaNativeStudents:TotalRaceEthnicity) %>% 
        pivot_longer(
          AmericanIndianAlaskaNativeStudents:TwoorMoreRacesStudents,
          names_to = 'race_ethnicity') %>%
        transmute(
          race_ethnicity,
          value = as.numeric(value)/as.numeric(TotalRaceEthnicity) * 100) %>%
        group_by(race_ethnicity) %>%
        summarize(value = mean(value, na.rm = TRUE)) %>%
        add_column(holc_maj_grade = 'A') %>%
        mutate(
          race_ethnicity = str_replace_all(
            race_ethnicity,
            c('AmericanIndianAlaskaNativeStudents' = 'American Indian/Alaska Native',
              'AsianorAsianPacificIslanderStudents' = 'Asian/Asian Pacific Islander',
              'HispanicStudents' = 'Hispanic',
              'BlackorAfricanAmericanStudents' = 'Black/African American',
              'WhiteStudents' = 'White',
              'NatHawaiianorOtherPacificIslStudents' = 'Native Hawaiian/Pacific Islander',
              'TwoorMoreRacesStudents' = 'Two or more Races')))) %>% 
  set_names('ch_schools_a_race',
            'la_schools_a_race',
            'ny_schools_a_race') %>% 
  list2env(.GlobalEnv)

# store average race % of schools for HOLC majority D grade

list(ch_schools_d,
     la_schools_d,
     ny_schools_d) %>% 
  map(~select(.,
              AmericanIndianAlaskaNativeStudents:TotalRaceEthnicity) %>% 
        pivot_longer(
          AmericanIndianAlaskaNativeStudents:TwoorMoreRacesStudents,
          names_to = 'race_ethnicity') %>%
        transmute(
          race_ethnicity,
          value = as.numeric(value)/as.numeric(TotalRaceEthnicity) * 100) %>%
        group_by(race_ethnicity) %>%
        summarize(value = mean(value, na.rm = TRUE)) %>%
        add_column(holc_maj_grade = 'D') %>%
        mutate(
          race_ethnicity = str_replace_all(
            race_ethnicity,
            c('AmericanIndianAlaskaNativeStudents' = 'American Indian/Alaska Native',
              'AsianorAsianPacificIslanderStudents' = 'Asian/Asian Pacific Islander',
              'HispanicStudents' = 'Hispanic',
              'BlackorAfricanAmericanStudents' = 'Black/African American',
              'WhiteStudents' = 'White',
              'NatHawaiianorOtherPacificIslStudents' = 'Native Hawaiian/Pacific Islander',
              'TwoorMoreRacesStudents' = 'Two or more Races')))) %>% 
  set_names('ch_schools_d_race',
            'la_schools_d_race',
            'ny_schools_d_race') %>% 
  list2env(.GlobalEnv)

# store average race % of schools for HOLC majority A/B grade

list(ch_schools_a_b,
     la_schools_a_b,
     ny_schools_a_b) %>% 
  map(~select(.,
              AmericanIndianAlaskaNativeStudents:TotalRaceEthnicity) %>% 
        pivot_longer(
          AmericanIndianAlaskaNativeStudents:TwoorMoreRacesStudents,
          names_to = 'race_ethnicity') %>%
        transmute(
          race_ethnicity,
          value = as.numeric(value)/as.numeric(TotalRaceEthnicity) * 100) %>%
        group_by(race_ethnicity) %>%
        summarize(value = mean(value, na.rm = TRUE)) %>%
        add_column(holc_maj_grade = 'A/B') %>%
        mutate(
          race_ethnicity = str_replace_all(
            race_ethnicity,
            c('AmericanIndianAlaskaNativeStudents' = 'American Indian/Alaska Native',
              'AsianorAsianPacificIslanderStudents' = 'Asian/Asian Pacific Islander',
              'HispanicStudents' = 'Hispanic',
              'BlackorAfricanAmericanStudents' = 'Black/African American',
              'WhiteStudents' = 'White',
              'NatHawaiianorOtherPacificIslStudents' = 'Native Hawaiian/Pacific Islander',
              'TwoorMoreRacesStudents' = 'Two or more Races')))) %>% 
  set_names('ch_schools_a_b_race',
            'la_schools_a_b_race',
            'ny_schools_a_b_race') %>% 
  list2env(.GlobalEnv)

# store average race % of schools for HOLC majority C/D grade

list(ch_schools_c_d,
     la_schools_c_d,
     ny_schools_c_d) %>% 
  map(~select(.,
              AmericanIndianAlaskaNativeStudents:TotalRaceEthnicity) %>% 
        pivot_longer(
          AmericanIndianAlaskaNativeStudents:TwoorMoreRacesStudents,
          names_to = 'race_ethnicity') %>%
        transmute(
          race_ethnicity,
          value = as.numeric(value)/as.numeric(TotalRaceEthnicity) * 100) %>%
        group_by(race_ethnicity) %>%
        summarize(value = mean(value, na.rm = TRUE)) %>%
        add_column(holc_maj_grade = 'C/D') %>%
        mutate(
          race_ethnicity = str_replace_all(
            race_ethnicity,
            c('AmericanIndianAlaskaNativeStudents' = 'American Indian/Alaska Native',
              'AsianorAsianPacificIslanderStudents' = 'Asian/Asian Pacific Islander',
              'HispanicStudents' = 'Hispanic',
              'BlackorAfricanAmericanStudents' = 'Black/African American',
              'WhiteStudents' = 'White',
              'NatHawaiianorOtherPacificIslStudents' = 'Native Hawaiian/Pacific Islander',
              'TwoorMoreRacesStudents' = 'Two or more Races')))) %>% 
  set_names('ch_schools_c_d_race',
            'la_schools_c_d_race',
            'ny_schools_c_d_race') %>% 
  list2env(.GlobalEnv)

# store average free and reduced price lunch students % of schools in majority
# HOLC grade A SABs

list(ch_schools_a,
     la_schools_a,
     ny_schools_a) %>% 
  map(~transmute(.,
                 free_lunch_avg = as.numeric(FreeandReducedLunchStudents)/as.numeric(TotalStudentsAllGradesExcludesAE) * 100) %>%
        summarise(across(everything(), ~ mean(.x, na.rm = TRUE))) %>%
        add_column(holc_maj_grade = 'A')) %>%
  set_names('ch_schools_a_lunch',
            'la_schools_a_lunch',
            'ny_schools_a_lunch') %>% 
  list2env(.GlobalEnv)

# store average free and reduced price lunch students % of schools in majority
# HOLC grade D SABs

list(ch_schools_d,
     la_schools_d,
     ny_schools_d) %>% 
  map(~transmute(.,
                 free_lunch_avg = as.numeric(FreeandReducedLunchStudents)/as.numeric(TotalStudentsAllGradesExcludesAE) * 100) %>%
        summarise(across(everything(), ~ mean(.x, na.rm = TRUE))) %>%
        add_column(holc_maj_grade = 'D')) %>%
  set_names('ch_schools_d_lunch',
            'la_schools_d_lunch',
            'ny_schools_d_lunch') %>% 
  list2env(.GlobalEnv)

# store average free and reduced price lunch students % of schools in majority
# HOLC grade A/B SABs

list(ch_schools_a_b,
     la_schools_a_b,
     ny_schools_a_b) %>% 
  map(~transmute(.,
                 free_lunch_avg = as.numeric(FreeandReducedLunchStudents)/as.numeric(TotalStudentsAllGradesExcludesAE) * 100) %>%
        summarise(across(everything(), ~ mean(.x, na.rm = TRUE))) %>%
        add_column(holc_maj_grade = 'A/B')) %>%
  set_names('ch_schools_a_b_lunch',
            'la_schools_a_b_lunch',
            'ny_schools_a_b_lunch') %>% 
  list2env(.GlobalEnv)

# store average free and reduced price lunch students % of schools in majority
# HOLC grade C/D SABs

list(ch_schools_c_d,
     la_schools_c_d,
     ny_schools_c_d) %>% 
  map(~transmute(.,
                 free_lunch_avg = as.numeric(FreeandReducedLunchStudents)/as.numeric(TotalStudentsAllGradesExcludesAE) * 100) %>%
        summarise(across(everything(), ~ mean(.x, na.rm = TRUE))) %>%
        add_column(holc_maj_grade = 'C/D')) %>%
  set_names('ch_schools_c_d_lunch',
            'la_schools_c_d_lunch',
            'ny_schools_c_d_lunch') %>% 
  list2env(.GlobalEnv)

# nest school data for majority A SABs

nested_schools_a <-
  rbind(la_schools_a,
        ch_schools_a,
        ny_schools_a) %>%
  group_by(state) %>% 
  nest()

# store average student expenditure of schools for HOLC majority A grade

combined_schools_a_exp <-
  map_dfr(
    1:length(unique(nested_schools_a$data)),
    function(i) {
      left_join(
        nested_schools_a$data[[i]] %>% 
          mutate(SchoolIDNCESAssigned = as.numeric(SchoolIDNCESAssigned)),
        nested_exp$data[[i]],
        by = "SchoolIDNCESAssigned") %>%
        select(pp_total_raw) %>% 
        summarise(across(everything(), ~ mean(.x, na.rm = TRUE))) %>%
        add_column(holc_maj_grade = 'A')})

# add state column

combined_schools_a_exp$state <- c('CA', 'IL', 'NY')

# nest school data for majority D SABs

nested_schools_d <-
  rbind(la_schools_d,
        ch_schools_d,
        ny_schools_d) %>%
  group_by(state) %>% 
  nest()

# store average student expenditure of schools for HOLC majority D grade

combined_schools_d_exp <-
  map_dfr(
    1:length(unique(nested_schools_d$data)),
    function(i) {
      left_join(
        nested_schools_d$data[[i]] %>% 
          mutate(SchoolIDNCESAssigned = as.numeric(SchoolIDNCESAssigned)),
        nested_exp$data[[i]],
        by = "SchoolIDNCESAssigned") %>%
        select(pp_total_raw) %>% 
        summarise(across(everything(), ~ mean(.x, na.rm = TRUE))) %>%
        add_column(holc_maj_grade = 'D')})

# add state column

combined_schools_d_exp$state <- c('CA', 'IL', 'NY')

# nest school data for majority A/B SABs

nested_schools_a_b <-
  rbind(la_schools_a_b,
        ch_schools_a_b,
        ny_schools_a_b) %>%
  group_by(state) %>% 
  nest()

# store average student expenditure of schools for HOLC majority A/B grade

combined_schools_a_b_exp <-
  map_dfr(
    1:length(unique(nested_schools_a_b$data)),
    function(i) {
      left_join(
        nested_schools_a_b$data[[i]] %>% 
          mutate(SchoolIDNCESAssigned = as.numeric(SchoolIDNCESAssigned)),
        nested_exp$data[[i]],
        by = "SchoolIDNCESAssigned") %>%
        select(pp_total_raw) %>% 
        summarise(across(everything(), ~ mean(.x, na.rm = TRUE))) %>%
        add_column(holc_maj_grade = 'A/B')})

# add state column

combined_schools_a_b_exp$state <- c('CA', 'IL', 'NY')

# nest school data for majority C/D SABs

nested_schools_c_d <-
  rbind(la_schools_c_d,
        ch_schools_c_d,
        ny_schools_c_d) %>%
  group_by(state) %>% 
  nest()

# store average student expenditure of schools for HOLC majority C/D grade

combined_schools_c_d_exp <-
  map_dfr(
    1:length(unique(nested_schools_c_d$data)),
    function(i) {
      left_join(
        nested_schools_c_d$data[[i]] %>% 
          mutate(SchoolIDNCESAssigned = as.numeric(SchoolIDNCESAssigned)),
        nested_exp$data[[i]],
        by = "SchoolIDNCESAssigned") %>%
        select(pp_total_raw) %>% 
        summarise(across(everything(), ~ mean(.x, na.rm = TRUE))) %>%
        add_column(holc_maj_grade = 'C/D')})

# add state column

combined_schools_c_d_exp$state <- c('CA', 'IL', 'NY')


# summarizing data for app server -----------------------------------------

# bind Chicago, LA and NY data - for interactive maps

ch_la_ny <- 
  rbind(att_area_ch_perc %>%
          add_column(city = 'Chicago') %>% 
          st_as_sf(),
        att_area_la_perc %>% 
          add_column(city = 'Los Angeles') %>% 
          st_as_sf(),
        att_area_ny_perc %>% 
          add_column(city = 'New York') %>% 
          st_as_sf()) %>% 
  mutate(
    holc_maj_grade = factor(holc_maj_grade,
                            levels = c('A',
                                       'B',
                                       'C',
                                       'D'))) %>% 
  rename('Majority Grade' = holc_maj_grade)

# Summarize data for table

list(att_area_ch_perc,
     att_area_la_perc,
     att_area_ny_perc) %>% 
  map(~rename(.,sab_id = unit_id) %>%
        select(-geometry)) %>%
  set_names('ch_table',
            'la_table',
            'ny_table') %>% 
  list2env(.GlobalEnv)

# Summarize data for plot_race - A vs D - Chicago

ch_plot_a_d_race <-
    rbind(ch_schools_a_race,
          ch_schools_d_race) %>%
      rename('Majority Grade' = holc_maj_grade) %>% 
      mutate(paired = rep(1:7,2))

# Summarize data for plot_race - A/B vs C/D - Chicago

ch_plot_ab_cd_race <-
    rbind(ch_schools_a_b_race,
          ch_schools_c_d_race) %>% 
      rename('Majority Grade' = holc_maj_grade) %>% 
      mutate(paired = rep(1:7,2))

# Summarize data for plot_race - A vs D - LA

la_plot_a_d_race <-
    rbind(la_schools_a_race,
          la_schools_d_race) %>% 
      rename('Majority Grade' = holc_maj_grade) %>% 
      mutate(paired = rep(1:7,2))

# Summarize data for plot_race - A/B vs C/D - LA

la_plot_ab_cd_race <-
    rbind(la_schools_a_b_race,
          la_schools_c_d_race) %>% 
      rename('Majority Grade' = holc_maj_grade) %>% 
      mutate(paired = rep(1:7,2))

# Summarize data for plot_race - A vs D - NY

ny_plot_a_d_race <-
    rbind(ny_schools_a_race,
          ny_schools_d_race) %>% 
      rename('Majority Grade' = holc_maj_grade) %>% 
      mutate(paired = rep(1:7,2))

# Summarize data for plot_race - A/B vs C/D - NY

ny_plot_ab_cd_race <-
    rbind(ny_schools_a_b_race,
          ny_schools_c_d_race) %>% 
      rename('Majority Grade' = holc_maj_grade) %>% 
      mutate(paired = rep(1:7,2))

# Summarize data for plot_lunch - A vs D - Chicago

ch_plot_a_d_lunch <-
    rbind(ch_schools_a_lunch,
          ch_schools_d_lunch)

# Summarize data for plot_lunch - A/B vs C/D - Chicago

ch_plot_ab_cd_lunch <-
    rbind(ch_schools_a_b_lunch,
          ch_schools_c_d_lunch)

# Summarize data for plot_lunch - A vs D - LA

la_plot_a_d_lunch <-
    rbind(la_schools_a_lunch,
          la_schools_d_lunch)

# Summarize data for plot_lunch - A/B vs C/D - LA

la_plot_ab_cd_lunch <-
    rbind(la_schools_a_b_lunch,
          la_schools_c_d_lunch)

# Summarize data for plot_lunch - A vs D - NY

ny_plot_a_d_lunch <-
    rbind(ny_schools_a_lunch,
          ny_schools_d_lunch)

# Summarize data for plot_lunch - A/B vs C/D - NY

ny_plot_ab_cd_lunch <-
    rbind(ny_schools_a_b_lunch,
          ny_schools_c_d_lunch)

# Summarize data for plot_exp - A vs D - Chicago

ch_plot_a_d_exp <-
    rbind(combined_schools_a_exp,
          combined_schools_d_exp) %>% 
      filter(state == 'IL')

# Summarize data for plot_exp - A/B vs C/D - Chicago

ch_plot_ab_cd_exp <-
    rbind(combined_schools_a_b_exp,
          combined_schools_c_d_exp) %>% 
      filter(state == 'IL')

# Summarize data for plot_exp - A vs D - LA

la_plot_a_d_exp <-
    rbind(combined_schools_a_exp,
          combined_schools_d_exp) %>% 
      filter(state == 'CA')

# Summarize data for plot_exp - A/B vs C/D - LA

la_plot_ab_cd_exp <-
    rbind(combined_schools_a_b_exp,
          combined_schools_c_d_exp) %>% 
      filter(state == 'CA')

# Summarize data for plot_exp - A vs D - NY

ny_plot_a_d_exp <-
    rbind(combined_schools_a_exp,
          combined_schools_d_exp) %>% 
      filter(state == 'NY')

# Summarize data for plot_exp - A/B vs C/D - NY

ny_plot_ab_cd_exp <-
    rbind(combined_schools_a_b_exp,
          combined_schools_c_d_exp) %>% 
      filter(state == 'NY')

# load HOLC map area descriptions

holc_ad_data <- 
  read_csv('data/raw/holc_ad_data.csv') %>% 
  drop_na(data) %>% 
  select(data)

# filter to area descriptions with specific words/terms

patterns <- c('infiltration','encroachment',
              'colored', 'low grade',
              'low class', 'subversive',
              'undesirable', 'negro',
              'inharmonious', 'lower grade')

descs <- 
  holc_ad_data %>% 
  filter(grepl(paste(patterns,
                     collapse="|"),
               data))

# user interface ----------------------------------------------------------

ui <- 
  
  dashboardPage(
    skin = "red",
    dashboardHeader(title = 'Racist Legacy'),
    
    dashboardSidebar(
      
      sidebarMenu(
        
        menuItem('Home Page',
                 icon = icon('home'),
                 tabName = 'home')),
      
      radioButtons(
        inputId = 'city',
        label = 'Select city:',
        choiceNames = c('Chicago', 'Los Angeles', 'New York'),
        choiceValues = c('Chicago', 'Los Angeles', 'New York')),
      
      sidebarMenu(
        menuItem('Map',
                 icon = icon('map'),
                 tabName = 'maps')),
      
      checkboxInput(
        inputId = 'view_schools',
        label = 'View Schools on Map'),
      
      sidebarMenu(
        
        menuItem('Racial Composition',
                 icon = icon('users'),
                 tabName = 'plot_race'),
        
        menuItem('Free & Reduced Price Lunch',
                 icon = icon('utensils'),
                 tabName = 'plot_lunch'),
        
        menuItem('Per Pupil Expenditure',
                 icon = icon('dollar-sign'),
                 tabName = 'plot_exp'),
        
        menuItem('Area Descriptions',
                 icon = icon('file-alt'),
                 tabName = 'area_desc'),
        
        menuItem('Statistical Analysis',
                 icon = icon('subscript'),
                 tabName = 'stats'),
        
        menuItem('Table',
                 icon = icon('table'),
                 tabName = 'tables'),
        
        menuItem('Data Sources',
                 icon = icon('link'),
                 tabName = 'data_sources')
        
      )
    ),
    
    dashboardBody(
      tags$head(
        tags$link(
          rel = 'stylesheet',
          type = 'text/css',
          href = 'dashboard_styles.css'
        ),
        tags$style(
          HTML('
               /* body */
               .content-wrapper, .right-side {
               background-color: #ffffff;
               }')
        )),
      
      tabItems(
        
        tabItem(
          tabName = 'home',
          h2('Racist Legacy of Redlining:
          Racial Composition, Student Expenditure and Economically
          Disadvantaged Students in Schools',
             align = 'center'),
          h3('Background'),
          h4("In the 1930s, the Home Owners’ Loan Corporation (HOLC)
             drew",
             tags$a(href = "https://dsl.richmond.edu/panorama/redlining/#loc=5/39.1/-94.58",
                    "maps"),
             "for most metropolitan cities in the United
             States. As shown below, these maps were color-coded and
             assigned grades to indicate where it was safe to insure
             mortgages, with ‘A’ being the best grade a neighborhood
             could receive, and ‘D’ (colored red on the original
             maps) being the worst. An overview of the accompanying",
             tags$a(href = "https://dsl.richmond.edu/panorama/redlining/#loc=5/39.1/-94.58&text=downloads",
                    "area descriptions"),
             "that HOLC wrote for many
             neighborhoods makes it quite apparent that the
             motivation for these maps was not mortgage security,
             but rather to ensure that supposedly incompatible
             racial groups should not live in the same neighborhoods.
             Even though this practice (known as redlining) was banned
             in 1968 with the passing of the Fair Housing Act, its
             effects still persist."),
          br(),
          strong('Select a city from the sidebar on the left to view its
            redlining map below.'),
          plotOutput(outputId = 'redlining_map',
                     height = "600px"),
          h3('Introduction'),
          h4("The education domain has not been explored as much as
             other domains (e.g. housing) in order to
             understand the effects of redlining. Some researchers
             have analyzed",
             tags$a(href = "https://www.urban.org/research/publication/dividing-lines-racially-unequal-school-boundaries-us-public-school-systems",
                    "whether unequal school attendance boundaries are linked to redlining maps"),
             ", while others
             have explored the",
             tags$a(href = "https://www.edworkingpapers.com/ai21-363",
                    "impact of redlining on educational outcomes"),
             "by analyzing school districts and schools
             currently located in formerly redlined neighborhoods."),
          br(),
          img(src = "sab_img_bigger.png",
              style = "display: block; margin-left: auto; margin-right: auto;",
              height = "60%",
              width = "60%",
              align = "center"),
          br(),
          h4("However, there has been no analysis yet focusing on
             school attendance boundaries i.e. exploring the impact
             of redlining on educational outcomes by analyzing
             school attendance boundaries with formerly redlined
             neighborhoods. This analysis would be useful because
             students studying in public schools usually go to that
             school because their family lives in that school’s
             attendance zone. This means that even though a school
             may be located in a former ‘C’ grade area,
             most students coming to that school may be from a former
             ‘A’ grade neighborhood, as shown in the map below of a
             school attendance boundary in Los Angeles. Therefore,
             analyzing a school by looking at the former majority
             grade of its attendance boundary would be more
             representative of its student body."),
          br(),
          plotOutput(outputId = 'static_sab'),
          h3("Overall Question"),
          h4("Is there a link between former redlining maps and
               current racial composition, student expenditure, and
               the number of economically disadvantaged students
               in schools?"),
          h3("Analysis"),
          h4("Using shapefiles for redlining maps and elementary
             school attendance boundaries, attendance boundaries
             that intersected with redlining maps were identified,
             and the area of each (former) HOLC grade within each
             school attendance boundary was calculated. The school
             attendance boundaries were then classified according
             to the majority HOLC grade (defined as greater than
             50% of the area),
             as shown in the map below
             of the same school attendance boundary above.
             Comparisons were then made between public elementary schools
             in majority A and D attendance boundaries,
             and between public elementary schools in either A
             or B, and either C or D majority attendance boundaries."),
          br(),
          plotOutput(outputId = 'static_sab_maj'),
          br(),
          br(),
          h4(em("All maps and graphs have been plotted using an
            accessible color palette, inspired by a color palette
            by",
            tags$a(href = "https://www.nature.com/articles/nmeth.1618",
                   "Bang Wong."),
            "The palette was also tested in",
            tags$a(href = "https://www.color-blindness.com/coblis-color-blindness-simulator/",
                   "Coblis"), 
            ", a color blindness simulator."))),
        
        tabItem(
          tabName = 'maps',
          h2('Map'),
          h4("School attendance boundaries colored by the majority HOLC grade.
          Moving your cursor over the map will display unique IDs of each
          school attendance boundary, and if you have checked the",
          strong('View Schools on Map'),
          "option in the sidebar, you can move your cursor over
          a school to view its ID
          (assigned by the",
          tags$a(href = "https://nces.ed.gov/",
                    "National Center for Education Statistics"),
          ")."),
          br(),
          tmapOutput(outputId = 'sab_map')),
        
        tabItem(
          tabName = 'tables',
          h2('Summary Table'),
          h4("The following table displays the unique school attendance
              boundary (SAB) ID (sab_id),
              the area (area) and area percentage (area_perc)
              of each HOLC grade (holc_grade) in an SAB,
              and the HOLC grade (holc_maj_grade)
              that makes up the majority of the SAB's area
              (in meters)."),
          br(),
          dataTableOutput(outputId = 'summary_table')),
        
        tabItem(
          tabName = 'plot_race',
          h2('Racial Composition'),
          h4("Compare the average racial composition of schools'
            student bodies. For instance, if
            you choose to compare A vs D, the plot will compare the average racial
            composition of schools in school attendance boundaries (SABs) with
            majority HOLC A grade versus majority D grade."),
          br(),
          selectInput(
            inputId = 'grades_select1',
            label = 'Compare SABs',
            choices = c('A vs D',
                        'A/B vs C/D')),
          plotlyOutput(outputId = 'race_output',
                       width = "90%")),
        
        tabItem(
          tabName = 'plot_lunch',
          h2('Free & Reduced Price Lunch'),
          h4("Compare the average percentage of students receiving free and
            reduced price lunch. For instance, if you choose to compare A vs D, 
            the plot will compare the average percentage of students receiving
            free and reduced price lunch of schools in school attendance boundaries
            (SABs) with majority HOLC A grade versus majority D grade."),
          br(),
          selectInput(
            inputId = 'grades_select2',
            label = 'Compare SABs',
            choices = c('A vs D',
                        'A/B vs C/D')),
          plotOutput(outputId = 'lunch_output',
                     width = "90%"),
          br(),
          h4("Note: The number of students receiving free or reduced price lunch
             is being used as a proxy for the number of economically disadvantaged
             students.")),
        
        tabItem(
          tabName = 'plot_exp',
          h2('Per Pupil Expenditure'),
          h4("Compare the average per pupil expenditure of schools.
            For instance, if you choose to compare A vs D, 
            the plot will compare the average per pupil expenditure of schools
            in school attendance boundaries (SABs) with majority HOLC A grade
            versus majority D grade."),
          br(),
          selectInput(
            inputId = 'grades_select3',
            label = 'Compare SABs',
            choices = c('A vs D',
                        'A/B vs C/D')),
          plotOutput(outputId = 'exp_output',
                     width = "90%")),
        
        tabItem(
          tabName = 'area_desc',
          h2('HOLC Area Descriptions'),
          h4("The Home Owners’ Loan Corporation wrote area
             descriptions for most neighborhoods that it graded.
             While some of these include notes on infrastructure
             and the types of houses in an area, many contain
             racially explicit language. Although they are
             difficult to read, it is important to be aware of them
             in order to try to understand the depth of 
             an extremely discriminatory and inhumane practice."),
          br(),
          actionButton("genbutton",
                       "Click to View an Area Description"),
          h4(''),
          br(),
          tags$head(tags$style(HTML("pre { white-space: pre-wrap; word-break: keep-all; }"))),
          verbatimTextOutput("text")),
        
        tabItem(
          tabName = 'stats',
          h2('Statistical Analysis'),
          br(),
          h4(em(HTML(paste0("MajGrade",tags$sub("i")),
                     paste0(" ","= "),
                     paste0("B",tags$sub("0")),
                     paste0(" ","+ "),
                     paste0("B",tags$sub("1")),
                     paste0("WhiteMaj",tags$sub("i")),
                     paste0(" ","+ "),
                     paste0("B",tags$sub("2")),
                     paste0("BlackMaj",tags$sub("i")),
                     paste0(" ","+ "),
                     paste0("B",tags$sub("3")),
                     paste0("LunchMaj",tags$sub("i")),
                     paste0(" ","+ "),
                     paste0("E",tags$sub("i"))))),
          h4("An ordered logistic model was run with the variable storing the
             majority grade of school attendance boundaries (MajGrade) as
             the dependent variable. All independent variables were binary,
             including variables indicating whether a school has a majority
             White student body (WhiteMaj), whether it has a majority Black
             student body (BlackMaj), and whether a majority of the students
             are receiving free or reduced price lunch (LunchMaj)."),
          br(),
          img(src = "pstat.png",
              style = "display: block; margin-left: auto; margin-right: auto;",
              height = "100%",
              width = "100%",
              align = "center"),
          br(),
          h4("Using the ordered logistic model results, it was found that
             an elementary public school is approximately 14% less likely to be in a
             majority D grade school attendance boundary if it has a
             majority White student population. This discrete difference
             was found to be statistically significant (depicted by
             the non-overlapping confidence intervals above).")),
        
        tabItem(
          tabName = 'data_sources',
          h2('Data Sources'),
          br(),
          h4(tags$a(href = "https://dsl.richmond.edu/panorama/redlining/#loc=5/39.1/-94.58&text=downloads",
                 "Shapefiles of HOLC Redlining Maps")),
          h4(tags$a(href = "https://data.cityofchicago.org/",
                 "Shapefiles of Elementary School Attendance Boundaries in Chicago")),
          h4(tags$a(href = "https://geohub.lacity.org",
                 "Shapefiles of Elementary School Attendance Boundaries in Los Angeles")),
          h4(tags$a(href = "https://data.cityofnewyork.us/",
                 "Shapefiles of Elementary School Attendance Boundaries in New York")),
          h4(tags$a(href = "https://dsl.richmond.edu/panorama/redlining/#loc=5/39.1/-94.58&text=downloads",
                 "HOLC Map Area Descriptions")),
          h4(tags$a(href = "https://nces.ed.gov/ccd/elsi/",
                 "Racial/Ethnic Enrollment in Public Elementary Schools")),
          h4(tags$a(href = "https://nces.ed.gov/ccd/elsi/",
                 "Students Receiving Free or Reduced Price Lunch in
                 Public Elementary Schools")),
          h4(tags$a(href = "https://edunomicslab.org/nerds/",
                 "Per Pupil Expenditure in Public Elementary Schools")))
        
      )
    )
  )

# server ------------------------------------------------------------------

server <- 
  function(input, output) { 
    
    ### map
    
    ch_la_ny_schools_map <-
      reactive({
        if (input$city == 'Chicago' && input$view_schools) {
          ch_la_ny %>% 
            filter(city == 'Chicago') %>% 
            tm_shape() +
            tm_polygons(alpha = 0.6,
                        id = "unit_id",
                        col = "Majority Grade",
                        palette = c('#01E0A3',
                                    '#AFAFFF',
                                    '#F0E442',
                                    '#D55E00')) +
            tmap_options(check.and.fix = TRUE) +
            tm_shape(el_ch_schools_sab_holc %>% 
                       filter(unit_id %in% att_area_ch_perc$unit_id)) +
            tm_dots(size = 0.005,
                    id = "SchoolIDNCESAssigned",
                    col = "black",
                    alpha = 0.7)
        } else if (input$city == 'Chicago') {
          ch_la_ny %>% 
            filter(city == 'Chicago') %>% 
            tm_shape() +
            tm_polygons(alpha = 0.6,
                        id = "unit_id",
                        col = "Majority Grade",
                        palette = c('#01E0A3',
                                    '#AFAFFF',
                                    '#F0E442',
                                    '#D55E00')) +
            tmap_options(check.and.fix = TRUE)
        } else if (input$city == 'Los Angeles' && input$view_schools){
          ch_la_ny %>% 
            filter(city == 'Los Angeles') %>% 
            tm_shape() +
            tm_polygons(alpha = 0.6,
                        id = "unit_id",
                        col = "Majority Grade",
                        palette = c('#01E0A3',
                                    '#AFAFFF',
                                    '#F0E442',
                                    '#D55E00')) +
            tmap_options(check.and.fix = TRUE) +
            tm_shape(el_la_schools_sab_holc %>% 
                       filter(unit_id %in% att_area_la_perc$unit_id)) +
            tm_dots(size = 0.005,
                    id = "SchoolIDNCESAssigned",
                    col = "black",
                    alpha = 0.7)
        } else if (input$city == 'Los Angeles') {
          ch_la_ny %>% 
            filter(city == 'Los Angeles') %>% 
            tm_shape() +
            tm_polygons(alpha = 0.6,
                        id = "unit_id",
                        col = "Majority Grade",
                        palette = c('#01E0A3',
                                    '#AFAFFF',
                                    '#F0E442',
                                    '#D55E00')) +
            tmap_options(check.and.fix = TRUE)
        } else if (input$city == 'New York' && input$view_schools) {
          ch_la_ny %>% 
            filter(city == 'New York') %>% 
            tm_shape() +
            tm_polygons(alpha = 0.6,
                        id = "unit_id",
                        col = "Majority Grade",
                        palette = c('#01E0A3',
                                    '#AFAFFF',
                                    '#F0E442',
                                    '#D55E00')) +
            tmap_options(check.and.fix = TRUE) +
            tm_shape(el_ny_schools_sab_holc %>% 
                       filter(unit_id %in% att_area_ny_perc$unit_id)) +
            tm_dots(size = 0.005,
                    id = "SchoolIDNCESAssigned",
                    col = "black",
                    alpha = 0.7)
        } else {
          ch_la_ny %>% 
            filter(city == 'New York') %>% 
            tm_shape() +
            tm_polygons(alpha = 0.6,
                        id = "unit_id",
                        col = "Majority Grade",
                        palette = c('#01E0A3',
                                    '#AFAFFF',
                                    '#F0E442',
                                    '#D55E00')) +
            tmap_options(check.and.fix = TRUE)}})
    
    # Table data
    
    ch_la_ny_table <-
      reactive({
        if (input$city == 'Chicago') {
          ch_table
        } else if (input$city == 'Los Angeles'){
          la_table 
        } else {
          ny_table
        }})
    
    ### Plots
    
    # race data:
    
    ch_plot_race <-
      reactive({
        if (input$grades_select1 == 'A vs D') {
          ch_plot_a_d_race
        } else {
          ch_plot_ab_cd_race}})
    
    la_plot_race <-
      reactive({
        if (input$grades_select1 == 'A vs D') {
          la_plot_a_d_race
        } else {
          la_plot_ab_cd_race}})
    
    ny_plot_race <-
      reactive({
        if (input$grades_select1 == 'A vs D') {
          ny_plot_a_d_race
        } else {
          ny_plot_ab_cd_race}})
    
    # plot race data
    
    ch_la_ny_plot_race <-
      reactive({
        if (input$city == 'Chicago') {
          ch_plot_race()
        } else if (input$city == 'Los Angeles') {
          la_plot_race() 
        } else {
          ny_plot_race()}})
    
    # lunch data:
    
    ch_plot_lunch <-
      reactive({
        if (input$grades_select2 == 'A vs D') {
          ch_plot_a_d_lunch
        } else {
          ch_plot_ab_cd_lunch}})
    
    la_plot_lunch <-
      reactive({
        if (input$grades_select2 == 'A vs D') {
          la_plot_a_d_lunch
        } else {
          la_plot_ab_cd_lunch}})
    
    ny_plot_lunch <-
      reactive({
        if (input$grades_select2 == 'A vs D') {
          ny_plot_a_d_lunch
        } else {
          ny_plot_ab_cd_lunch}})
    
    # plot lunch data
    
    ch_la_ny_plot_lunch <-
      reactive({
        if (input$city == 'Chicago') {
          ch_plot_lunch()
        } else if (input$city == 'Los Angeles') {
          la_plot_lunch() 
        } else {
          ny_plot_lunch()}})
    
    # expenditure data:
    
    ch_plot_exp <-
      reactive({
        if (input$grades_select3 == 'A vs D') {
          ch_plot_a_d_exp
        } else {
          ch_plot_ab_cd_exp}})
    
    la_plot_exp <-
      reactive({
        if (input$grades_select3 == 'A vs D') {
          la_plot_a_d_exp
        } else {
          la_plot_ab_cd_exp}})
    
    ny_plot_exp <-
      reactive({
        if (input$grades_select3 == 'A vs D') {
          ny_plot_a_d_exp
        } else {
          ny_plot_ab_cd_exp}})
    
    # plot expenditure data
    
    ch_la_ny_plot_exp <-
      reactive({
        if (input$city == 'Chicago') {
          ch_plot_exp()
        } else if (input$city == 'Los Angeles') {
          la_plot_exp() 
        } else {
          ny_plot_exp()}})
    
    # build a reactive expression that only invalidates 
    # when the value of input$genbutton becomes out of date 
    # (i.e., when the button is pressed)
    
    ntext <- 
      eventReactive(input$genbutton, {
        descs$data %>%
          sample(size = 1)})
    
    # static map
    
    ch_la_ny_static_map <-
      reactive({
        if (input$city == 'Chicago') {
          ch_holc_shp %>% 
            ggplot(aes(fill = holc_grade)) +
            geom_sf() +
            labs(fill = 'HOLC Grade') +
            scale_fill_manual(values = c('#01E0A3',
                                         '#AFAFFF',
                                         '#F0E442',
                                         '#D55E00')) + 
            theme_void() +
            theme(legend.title = element_text(size = 14),
                  legend.text = element_text(size = 14))
        } else if (input$city == 'Los Angeles') {
          la_holc_shp %>% 
            ggplot(aes(fill = holc_grade)) +
            geom_sf() +
            labs(fill = 'HOLC Grade') +
            scale_fill_manual(values = c('#01E0A3',
                                         '#AFAFFF',
                                         '#F0E442',
                                         '#D55E00')) + 
            theme_void() +
            theme(legend.title = element_text(size = 14),
                  legend.text = element_text(size = 14))
        } else {
          ny_holc_shp %>% 
            ggplot(aes(fill = holc_grade)) +
            geom_sf() +
            labs(fill = 'HOLC Grade') +
            scale_fill_manual(values = c('#01E0A3',
                                         '#AFAFFF',
                                         '#F0E442',
                                         '#D55E00')) + 
            theme_void() +
            theme(legend.title = element_text(size = 14),
                  legend.text = element_text(size = 14))}})
    
    # sab static map
    
    static_sab_map <-
      reactive({la_sab_holc %>%
          filter(unit_id == 297) %>%
          ggplot() +
          geom_sf(aes(fill = holc_grade)) +
          geom_sf(data = schools_sab_holc %>%
                    filter(unit_id == 297),
                  size = 3) +
          theme_void() +
          labs(fill = 'HOLC Grade') +
          scale_fill_manual(values = c('#01E0A3',
                                       '#AFAFFF',
                                       '#F0E442')) +
          theme(legend.title = element_text(size = 14),
                legend.text = element_text(size = 14))})
    
    static_sab_maj_map <-
      reactive({att_area_la_perc %>% 
          filter(unit_id == 297) %>%
          st_as_sf() %>%
          ggplot() +
          geom_sf(aes(fill = holc_maj_grade)) +
          geom_sf(data = schools_sab_holc %>%
                    filter(unit_id == 297),
                  size = 3) +
          theme_void() +
          labs(fill = 'Majority HOLC Grade') +
          scale_fill_manual(values = c('#01E0A3')) +
          theme(legend.title = element_text(size = 14),
                legend.text = element_text(size = 14))})
      
    
    # Outputs -------------------------------------------------------------
    
    # Static maps
    
    output$redlining_map <-
      renderPlot(
        ch_la_ny_static_map())
    
    # Map:
    
    output$sab_map <-
      renderTmap(
        ch_la_ny_schools_map())
    
    output$static_sab <-
      renderPlot(
        static_sab_map())
    
    output$static_sab_maj <-
      renderPlot(
        static_sab_maj_map())
    
    # Summary table:
    
    output$summary_table <-
      renderDataTable(
        ch_la_ny_table())
    
    # Plot:
    
    output$race_output <-
      renderPlotly(
        ch_la_ny_plot_race() %>% 
          ggplot(aes(x = value,
                     y = reorder(
                       race_ethnicity,
                       value))) +
          geom_line(aes(group = paired),
                    color = 'slategray') +
          geom_point(aes(color = `Majority Grade`),
                     size = 3,
                     alpha = 0.6) +
          labs(title = 'Average Racial Composition in Schools',
               subtitle = 'Schools in School Attendance Boundaries of Majority A and D HOLC grades\n',
               y = 'Race/Ethnicity\n\n\n\n\n\n\n',
               x = 'Percentage (%)\n') +
          scale_color_manual(values = c("#01E0A3",
                                        "#D55E00")) +
          scale_y_discrete(limits = c('White',
                                      'Native Hawaiian/Pacific Islander',
                                      'Two or more Races',
                                      'Hispanic',
                                      'Black/African American',
                                      'Asian/Asian Pacific Islander',
                                      'American Indian/Alaska Native')) +
          theme_classic() + 
          theme(text = element_text(size = 15),
                plot.title = element_text(hjust = 0.5)))
    
    output$lunch_output <-
      renderPlot(
        ch_la_ny_plot_lunch() %>%
          ggplot(
            aes(x = holc_maj_grade,
                y = free_lunch_avg)) +
          geom_bar(
            fill = "royalblue",
            alpha = 0.7,
            position = 'dodge',
            stat = 'identity') + 
          theme_minimal() +
          theme(text = element_text(size = 17),
                plot.title = element_text(hjust = 0.5)) +
          labs(title = 'Average Number of Students Receiving Free and Reduced Price Lunches in Schools',
               x = 'Majority Grade\n',
               y = '\nStudents (%)') +
          coord_flip())
    
    output$exp_output <-
      renderPlot(
        ch_la_ny_plot_exp() %>%
          ggplot(
            aes(x = holc_maj_grade,
                y = pp_total_raw)) +
          geom_bar(
            fill = "royalblue",
            alpha = 0.7,
            position = 'dodge',
            stat = 'identity') +
          theme_minimal() +
          theme(text = element_text(size = 17),
                plot.title = element_text(hjust = 0.5)) + 
          labs(title = 'Average Per Pupil Expenditure in Schools',
               x = 'Majority Grade\n',
               y = '\nPer Pupil Expenditure ($)') + 
          scale_y_continuous(labels = scales::comma) +
          coord_flip())
    
    output$text <- 
      renderText({
        ntext()})
    
  }

# knit and run app --------------------------------------------------------

shinyApp(ui, server)