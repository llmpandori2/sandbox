#########################################################################
# Title: Veg Management Data Wrangling
# Purpose: Get LB data in format requested
# Author: Lauren Pandori
# Created: 8/27/21
# Edited: 8/27/21
#########################################################################

# packages
library(readxl)
library(xlsx)
library(janitor)
library(data.table)
library(splitstackshape)
library(tidyverse)

# load data to be converted 
tm21 <- read_excel('VegMgmt/2021Treatment_Master.xlsx')
tm20 <- read_excel('VegMgmt/2020Treatment_Master.xlsx')
tm19 <- read_excel('VegMgmt/2019Treatment_Master.xlsx')
tm18 <- read_excel('VegMgmt/2018Treatment_Master.xlsx')

# load template
template <- read_excel("VegMgmt/ListofWeedsinWeedMgmtArea.xlsx")

# make list of acceptable values for template (zones + polygon IDs)
template <- tibble(template) %>%
  clean_names() %>%
  select(zone, polygon_id) %>%
  mutate(zone_abr = if_else(zone == 'Topside', 'TS',
                    if_else(zone == 'Cabrillo Road', 'CR',
                    if_else(zone == 'Bayside', 'BS',
                    if_else(zone == 'Tidepools', 'TP', 'NS'))))) %>%
  mutate(zone_abr = paste(zone_abr, polygon_id))

# tidy function
tidy_fn <- function(dataset, yearvalue) {
  
dataset2 <- tibble(dataset) %>%
  # names in  snake_case
  clean_names() %>%
  # pick only site and target species columns
  select(site, target_species) %>%
  mutate(year = yearvalue) %>%
  # remove columns with NAs for site and target species
  filter(!is.na(site) | !is.na(target_species)) %>%
  # expand delimited species column
  cSplit('target_species', direction = 'long', sep = ',') %>%
  # get site type 
  mutate(sitetype = substr(site,1,2)) %>% 
  # filter for recognized site types (TP, TS, CR, BS)
  filter(sitetype %in% c('TP', 'TS', 'CR', 'BS')) %>%
  # make list of numbers for site types 
  mutate(numberlist = substr(site, 3, nchar(site))) %>%
  # separate number column by ",", "&" and "/"
  separate(col = numberlist, 
           into = c('number1', 'number2', 'number3'), 
           sep = c(',')) %>%
  separate(col = number1, 
           into = c('number4', 'number5', 'number6'), 
           sep = c('&')) %>%
  separate(col = number4, 
           into = c('number7', 'number8', 'number9'), 
           sep = c('/')) %>%
  # remove spaces from number strings + parse numeric (to get rid of spaces and IPMT headers, etc.)
  mutate(number2 = as.numeric(gsub(' ', '', number2, fixed = TRUE)),
         number3 = as.numeric(gsub(' ', '', number3, fixed = TRUE)),
         number5 = as.numeric(gsub(' ', '', number5, fixed = TRUE)),
         number6 = as.numeric(gsub(' ', '', number6, fixed = TRUE)),
         number7 = as.numeric(gsub(' ', '', number7, fixed = TRUE)),
         number8 = as.numeric(gsub(' ', '', number8, fixed = TRUE)),
      number9 = as.numeric(gsub(' ', '', number9, fixed = TRUE))) %>%
  # pivot longer 
  pivot_longer(cols = number7:number3,
               names_to = 'numbercol', values_to = 'number') %>%
  # remove na values
  filter(!is.na(number)) %>%
  # put together numbers and site types
  mutate(zone_abr = paste(sitetype, number)) %>%
  # remove values where no spp is written
  filter(!is.na(target_species)) %>%
  # select 3 relevant columns (site, target_species and year)
  select(zone_abr, target_species, year) %>%
  # get distinct values
  distinct() %>%
  # add "present" column with an "x" for presence
  mutate(present = 'x') %>%
  # pivot to wide format (species codes as columns)
  pivot_wider(names_from = target_species, values_from = present) %>%
  # make parallel to the template
  full_join(template, ., by = 'zone_abr') %>%
  # add year column
  mutate(year = yearvalue) %>%
  # remove zone_abr column
  select(-zone_abr) %>%
  # restore original column names
  rename(Zone = zone, `Polygon ID` = polygon_id, Year = year)

write_csv(dataset2, paste('VegMgmt/tm', yearvalue, '.csv', sep = ''), na = '')

}

tidy_fn(tm21, 2021)
tidy_fn(tm20, 2020)
tidy_fn(tm19, 2019)
tidy_fn(tm18, 2018)


# write loop to write excel file with 1 year per tab
# this part doesn't work on desktop - something wrong w java
# for now, write separate csvs and copy + pasta
# rough_tidy_fn(tm21, 2021)
# 
# datasets <- c(rough_tidy_fn(tm21, 2021), rough_tidy_fn(tm20, 2020), 
#               rough_tidy_fn(tm19, 2019), rough_tidy_fn(tm18, 2018))
# 
# titles <- c('2021', '2020', '2019', '2018')
# 
# 
# for (i in seq_along(datasets)) {
#   write.xlsx(x = datasets[i], 
#              file = "ListofWeedsinWeedMgmgtArea_2018_2021.xlsx", 
#              sheetName = titles[i],
#              append = TRUE)
# }






