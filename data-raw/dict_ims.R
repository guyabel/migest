library(tidyverse)
library(countrycode)
library(readxl)

# download.file(url = "https://www.un.org/development/desa/pd/sites/www.un.org.development.desa.pd/files/aggregates_correspondence_table_2020_1.xlsx", 
#               destfile = "./data-raw/aggregates_correspondence_table_2020_1.xlsx",  mode = "wb")

d <- read_excel(path = "./data-raw/aggregates_correspondence_table_2020_1.xlsx", skip = 10) 

d0 <- d %>%
  rename(name = 1,
         iso3n = 3,
         iso3c = 4,
         region_sub = 10,
         region = 16,
         region_sdg = 14,
  )

d0 <- d0 %>%
  filter(iso3n < 900) %>%
  rename(
    least = "Least developed countries\r\n941",
    less = "Less developed regions\r\n902",
    more = "More developed regions\r\n901",
    lldc = "Land-Locked Developing Countries (LLDC)\r\n1636",
    sids = "Small Island Developing States (SIDS)\r\n1637",
    high = "High-income Countries\r\n1503",
    middle = "Middle-income Countries\r\n1517",
    upper_middle = "Upper-middle-income Countries\r\n1502",
    lower_middle = "Lower-middle-income Countries\r\n1501",
    low = "Low-income Countries\r\n1500"
  ) %>%
  mutate(
    un_develop = case_when(
      least == 941 ~ "least",
      less == 902 ~ "less",
      more == 901 ~ "more"
    ),
    lldc = lldc == 1636,
    sids = sids == 1637,
    wb_income = case_when(
      high == 1503 ~ "high",
      middle == 1517 ~ "middle",
      low ==1500 ~ "low"
    ),
    wb_income_detail = case_when(
      upper_middle == 1502 ~ "upper_middle",
      lower_middle == 1502 ~ "lower_middle",
      TRUE ~ wb_income
    )
  ) %>%
  select(1,3,4,10,16,14,contains("_develop"), contains("_income"),lldc, sids)

d0 <- d0 %>%
  mutate(name = str_remove_all(string = name, pattern = "[*]")) %>%
  replace_na(list(lldc = FALSE, sids = FALSE))

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

d0 <- d0 %>%
  # rename(region_ac2022 = region_ac2021) %>%
  mutate(region_wb = countrycode(sourcevar = iso3c, origin = "iso3c", destination = "region"),
         region_wb = case_when(
           iso3c == "CHI" ~ "Europe & Central Asia",
           iso3c == "ESH" ~ "Sub-Saharan Africa", 
           iso3c == "MYT" ~ "Sub-Saharan Africa", 
           iso3c == "REU" ~ "Sub-Saharan Africa", 
           iso3c == "SHN" ~ "Sub-Saharan Africa", 
           iso3c == "WLF" ~ "East Asia & Pacific",
           TRUE ~ region_wb
         ))

d0 <- d0 %>%
  mutate(name_short = countrycode(sourcevar = iso3c, origin = "iso3c", destination = "country.name.en"),
         name_short = case_when(
           iso3c == "CHI" ~ "Channel Islands",
           iso3c == "MMR" ~ name,
           iso3c == "FSM" ~ "FS Micronesia",
           iso3c == "MAF" ~ "Saint Martin",
           iso3c == "COG" ~ name,
           iso3c == "COD" ~ "DR Congo",
           iso3c == "HKG" ~ "Hong Kong SAR",
           iso3c == "MAC" ~ "Macao SAR",
           iso3c == "PSE" ~ "Palestine",
           TRUE ~ name_short
         )) %>%
  relocate(name, iso3c, iso3n, name_short)


# add in countries for changes in political geography that have used in 
# estimation
d1 <- d0 %>%
  filter(iso3c %in% c("SDN", "SRB", "ETH", "ZAF", "MNE", "CZE", "RUS", "IDN")) %>%
  rename(iso3c0 = iso3c,
         name0 = name) %>%
  mutate(iso3c = case_when(
           iso3c0 == "SDN" ~ "SUD", 
           iso3c0 == "SRB" ~ "SCG",
           iso3c0 == "ETH" ~ "ETI",
           iso3c0 == "ZAF" ~ "RSA",
           iso3c0 == "MNE" ~ "YUG",
           iso3c0 == "CZE" ~ "CSK",
           iso3c0 == "RUS" ~ "SUN",
           iso3c0 == "IDN" ~ "IDA",
           TRUE ~ iso3c0
         ),
         name = case_when(
           iso3c == "SUD" ~ "Sudan (incl. South Sudan)", 
           iso3c == "SCG" ~ "Serbia and Montenegro", 
           iso3c == "ETI" ~ "Ethiopia (incl. Eriteria)", 
           iso3c == "RSA" ~ "South Africa (incl. Namibia)",
           iso3c == "YUG" ~ "Yugoslavia", 
           iso3c == "CSK" ~ "Czechoslovakia", 
           iso3c == "SUN" ~ "Soviet Union",
           iso3c == "IDA" ~ "Indonesia (incl. East Timor)"
         ),
         name_short = name,
         ims = FALSE) %>%
  select(-iso3c0, -name0) %>%
  relocate(name, iso3c) 

# combine
d2 <- d0 %>%
  mutate(ims = TRUE) %>%
  bind_rows(d1) %>%
  relocate(1:4, ims, region, contains("region_"))

# chord diagram regions
d2 <- d2 %>%
  # fill in north america sub region
  mutate(region_sub = ifelse(test = is.na(region_sub), yes = region, no = region_sub)) %>%
  mutate(region_as2014 = case_when(
    region == "Oceania" ~ region,
    region_sub == "Caribbean" ~ "Central America",
    region_sdg == "Sub-Saharan Africa" ~ region_sdg,
    region %in% c("Africa", "Europe") ~ region_sub,
    TRUE ~ str_remove(string = region_sub, pattern = "ern")
  )) %>%
  mutate(region_sab2014 = case_when(
    iso3c %in% c("ARM", "AZE", "BLR", "EST", "GEO", "KAZ", "KGZ", 
                 "LVA", "LTU", "MDA", "RUS", "TJK", "TKM", "UZB", 
                 "UKR") ~ "Fmr Soviet Union",
    region == "Asia" ~ str_remove(string = region_sub, pattern = "ern"),
    TRUE ~ region
  )) %>%
  mutate(region_a2018 = case_when(
    region_sub %in% c("Eastern Europe", "Central Asia") ~ "East Europe & Central Asia",
    region_sub %in% c("South-Eastern Asia", "Eastern Asia") ~ "Eastern Asia",
    region == "Asia" ~ region_sub,
    TRUE ~ region
  )) %>%
  mutate(region_ac2022 = case_when(
    region_sub %in% c("Eastern Europe", "Central Asia") ~ "East Europe & Central Asia",
    region_sdg == "Sub-Saharan Africa" ~ region_sdg,
    region %in% c("Asia", "Africa") ~ str_remove(string = region_sub, pattern = "ern"),
    TRUE ~ region
  ))

d3 <- d2 %>%
  arrange(region, region_sub, name)
  
#   
# migest::dict_ims$region_ac2022 %>%
#   sort() %>% 
#   table()
# d2$region_ac2022 %>%
#   sort() %>% 
#   table()

# migest::dict_ims %>%
# # d2 %>%
#   filter(region_a2018 == "Eastern Asia")
#   filter(region_a2018 == "East Europe & Central Asia")
dict_ims <- d3
usethis::use_data(dict_ims, overwrite = TRUE)

