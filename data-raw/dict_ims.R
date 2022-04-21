# trying to recreate dict_ims... gave up

library(tidyverse)
library(countrycode)
library(readxl)

d <- read_csv("I:/ADRI/project/data-unpd/tims/data/total/tims2020.csv")
a <- read_excel("I:/ADRI/project/data-unpd/tims/data-raw/tims2020/aggregates_correspondence_table_2020_1.xlsx", skip = 10)

# d0 <- d %>%
#   select(pob, por) %>%
#   pivot_longer(cols = 1:2, values_to = "iso3c") %>%
#   distinct(iso3c) %>%
#   drop_na()
# 
# d0 <- d %>%
#   filter(country_code < 900) %>%
#   distinct(alpha3, name, country_code) %>%
#   mutate(name = str_remove(string = name, pattern = "[*]$")) %>%
#   rename(iso3c = alpha3, 
#          iso3n = country_code)
# 
# migest::dict_ims %>%
#   filter(!(iso3c %in% d0$iso3c))
# 
# a0 <- a %>%
#   rename(name = 1, 
#          iso3n = 3, 
#          iso3c = 4,
#          region_sub = 10, 
#          region = 16,
#          region_sdg = 14)
#          # more = 17,
#          # less = 18,
#          # least = 19,
#          # lldc = 22, 
#          # sids = 23, 
#          # high = 24, 
#          # middle = 25, 
#          # middle_upper = 26,
#          # middle_lower = 27,
#          # low = 28) 
# 
# a1 <- a0 %>%
#   filter(iso3n < 900) %>%
#   rename(
#     least = "Least developed countries\r\n941",
#     less = "Less developed regions\r\n902",
#     more = "More developed regions\r\n901",
#     lldc = "Land-Locked Developing Countries (LLDC)\r\n1636",
#     sids = "Small Island Developing States (SIDS)\r\n1637",
#     high = "High-income Countries\r\n1503",                                
#     middle = "Middle-income Countries\r\n1517",                       
#     upper_middle = "Upper-middle-income Countries\r\n1502",
#     lower_middle = "Lower-middle-income Countries\r\n1501",
#     low = "Low-income Countries\r\n1500"    
#   ) %>%
#   mutate(
#     un_develop = case_when(
#       least == 941 ~ "least",
#       less == 902 ~ "less",
#       more == 901 ~ "more"
#     ), 
#     lldc = lldc == 1636,
#     sids = sids == 1637,
#     wb_develop = case_when(
#       high == 1503 ~ "high",
#       middle == 1517 ~ "middle",
#       low ==1500 ~ "low"
#     ),
#     wb_develop_detail = case_when(
#       upper_middle == 1502 ~ "upper_middle",
#       lower_middle == 1502 ~ "lower_middle",
#       TRUE ~ wb_develop
#     )
#   ) %>%
#   select(1,3,4,10,16,14,contains("_develop"), lldc, sids)

dict_ims <- migest::dict_ims %>%
  rename(region_ac2022 = region_ac2021) %>%
  mutate(region_wb = countrycode(sourcevar = iso3c, origin = "iso3c", destination = "region"),
         region_wb = case_when(
           iso3c == "CHI" ~ "Europe & Central Asia",
           iso3c == "ESH" ~ "Sub-Saharan Africa", 
           iso3c == "MYT" ~ "Sub-Saharan Africa", 
           iso3c == "REU" ~ "Sub-Saharan Africa", 
           iso3c == "SCG" ~ "Europe & Central Asia",
           iso3c == "SHN" ~ "Sub-Saharan Africa", 
           iso3c == "SUD" ~ "Sub-Saharan Africa", 
           iso3c == "WLF" ~ "East Asia & Pacific",
           TRUE ~ region_wb
         ))

usethis::use_data(dict_ims, overwrite = TRUE)

