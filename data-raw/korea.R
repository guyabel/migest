library(tidyverse)
library(readxl)

# there is also monthly and quarterly data !
d <- read_excel("./data-raw/Number_of_internal_migrants_by_origin_and_destination_for_province_20210719124216.xlsx", na = "-")

d0 <- d %>%
  slice(-1) %>%
  rename(dest = 1, 
         orig = 2) %>%
  pivot_longer(cols = -(1:2), names_to = "year", values_to = "flow") %>%
  mutate(year = str_sub(string = year, start = 2, end = 5),
         year = as.integer(year)) %>%
  drop_na() %>%
  relocate(orig) %>%
  separate(col = dest, into = c("dest_code", "dest"), sep = " ", convert = TRUE) %>%
  separate(col = orig, into = c("orig_code", "orig"), sep = " ", convert = TRUE) %>%
  select(-contains("code")) %>%
  mutate(flow = as.integer(flow),
         # orig = str_remove(string = orig, pattern = "-do"),
         # dest = str_remove(string = dest, pattern = "-do"),
         orig = fct_inorder(orig), 
         dest = fct_inorder(dest)) %>%
  filter(year >= 2012) %>%
  arrange(year, orig, dest)


korea_reg <- d0

unique(korea_reg$orig)
unique(korea_reg$dest)
usethis::use_data(korea_reg, overwrite = TRUE)

d <- read_csv(file = "./data-raw/Resident_Population_by_City__County__and_District_1992-2020.csv", na = "-")
d1 <- d %>%
  rename(region = 1) %>%
  pivot_longer(cols = -1, names_to = "year", values_to = "population") %>%
  filter(year >= 2012) %>%
  mutate(year = as.integer(year)) %>%
  drop_na() %>%
  mutate(region = ifelse(region == "Sejong_si", "Sejong", region), 
         region = ifelse(region == "Jeju-do", "Jeju", region))
         # region = str_remove(string = region, pattern = "-do"))
unique(d1$region) %in% levels(korea_reg$orig)

korea_pop <- d1
usethis::use_data(korea_pop, overwrite = TRUE)


library(sf)
k <- raster::getData(country = "KOR", level  = 1) %>%
  st_as_sf() %>%
  mutate(mid = st_centroid(geometry))
detach("package:raster", unload=TRUE)

library(geosphere)
c0 <- do.call(rbind, st_geometry(k$mid))
korea_dist <- round(distm(c0)/1000)
# k$NAME_1 <- str_remove(string =k$NAME_1, pattern = "-do")
dimnames(korea_dist) <- list(orig = k$NAME_1, dest = k$NAME_1)
k$NAME_1 %in% unique(korea_pop$region)
  
usethis::use_data(korea_dist, overwrite = TRUE)
