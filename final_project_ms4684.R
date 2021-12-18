
# setup -------------------------------------------------------------------

# load libraries

library(lubridate)
library(sf)
library(tmap)
library(shiny)
library(shinydashboard)
library(tidyverse)
library(obsval)

# use the following code to install the obsval package 
# devtools::install_github("chrismeserole/obsval")

# Data Acquisition --------------------------------------------------------

# read in public school data, clean col names, filter & keep relevant columns

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

# specify path to read in school attendance boundary (SAB) shapefiles

read_dir <- 'data/raw/spatial/SABS'

# read in shapefiles, set names, and assign to global environment

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

# specify path to read in HOLC/redlining map shapefiles

read_dir <- 'data/raw/spatial/HOLC'

# read in shapefiles, set names, and assign to global environment

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

# Data Analysis -----------------------------------------------------------

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
  
  # convert back to target CRS
  
  st_transform(
    st_crs(ch_holc_shp))

# LA - find intersection of redlining maps and SABs 

la_sab_holc <- 
  st_intersection(
    la_holc_shp_prj,
    la_sab_prj) %>% 
  
  # rename column
  
  rename(unit_id = OBJECTID) %>% 
  
  # convert back to target CRS
  
  st_transform(
    st_crs(la_holc_shp))

# NY - find intersection of redlining maps and SABs

ny_sab_holc <- 
  st_intersection(
    ny_holc_shp_prj,
    ny_sab_prj) %>% 
  
  # rename column
  
  rename(unit_id = esid_no) %>% 
  
  # convert back to target CRS
  
  st_transform(
    st_crs(ny_holc_shp))

# add in areas 

list(ch_sab_holc,
     la_sab_holc,
     ny_sab_holc) %>% 
  map(~mutate(.,
              area = st_area(.) %>%
                as.numeric()) %>% 
        
        # add area percentage & create col to store majority HOLC grade
        
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
        
        # convert unit_id to numeric variable
        
        mutate_at('unit_id',
                  as.numeric) %>% 
        
        # remove SABs without a redlining/HOLC majority grade
        # (i.e. where holc_maj_grade is NA),
        # and remove the majority HOLC grade column (maj_grade)
        
        drop_na(holc_maj_grade) %>% 
        select(-maj_grade)) %>% 
  
  # set names and assign to global environment
  
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
      
      # Output per iteration:
      
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
        as.data.frame()
    })

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

# store average free and reduced lunch students % of schools in majority HOLC
# grade A SABs

list(ch_schools_a,
     la_schools_a,
     ny_schools_a) %>% 
  map(~transmute(.,
                 free_lunch_avg = as.numeric(
                   FreeandReducedLunchStudents)/as.numeric(
                     TotalStudentsAllGradesExcludesAE) * 100) %>%
        summarise(across(everything(), ~ mean(.x, na.rm = TRUE))) %>%
        add_column(holc_maj_grade = 'A')) %>%
  set_names('ch_schools_a_lunch',
            'la_schools_a_lunch',
            'ny_schools_a_lunch') %>% 
  list2env(.GlobalEnv)

# store average free and reduced lunch students % of schools in majority HOLC
# grade D SABs

list(ch_schools_d,
     la_schools_d,
     ny_schools_d) %>% 
  map(~transmute(.,
                 free_lunch_avg = as.numeric(
                   FreeandReducedLunchStudents)/as.numeric(
                     TotalStudentsAllGradesExcludesAE) * 100) %>%
        summarise(across(everything(), ~ mean(.x, na.rm = TRUE))) %>%
        add_column(holc_maj_grade = 'D')) %>%
  set_names('ch_schools_d_lunch',
            'la_schools_d_lunch',
            'ny_schools_d_lunch') %>% 
  list2env(.GlobalEnv)

# store average free and reduced lunch students % of schools in majority HOLC
# grade A/B SABs

list(ch_schools_a_b,
     la_schools_a_b,
     ny_schools_a_b) %>% 
  map(~transmute(.,
                 free_lunch_avg = as.numeric(
                   FreeandReducedLunchStudents)/as.numeric(
                     TotalStudentsAllGradesExcludesAE) * 100) %>%
        summarise(across(everything(), ~ mean(.x, na.rm = TRUE))) %>%
        add_column(holc_maj_grade = 'A/B')) %>%
  set_names('ch_schools_a_b_lunch',
            'la_schools_a_b_lunch',
            'ny_schools_a_b_lunch') %>% 
  list2env(.GlobalEnv)

# store average free and reduced lunch students % of schools in majority HOLC
# grade C/D SABs

list(ch_schools_c_d,
     la_schools_c_d,
     ny_schools_c_d) %>% 
  map(~transmute(.,
                 free_lunch_avg = as.numeric(
                   FreeandReducedLunchStudents)/as.numeric(
                     TotalStudentsAllGradesExcludesAE) * 100) %>%
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
        add_column(holc_maj_grade = 'A')
    })

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
        add_column(holc_maj_grade = 'D')
    })

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
        add_column(holc_maj_grade = 'A/B')
    })

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
        add_column(holc_maj_grade = 'C/D')
    })

# add state column

combined_schools_c_d_exp$state <- c('CA', 'IL', 'NY')
