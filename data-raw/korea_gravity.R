library(tidyverse)
library(readxl)

##  
## migration data 
##
# there is also monthly and quarterly data !
m <- read_excel("./data-raw/Number_of_internal_migrants_by_origin_and_destination_for_province_20210719124216.xlsx", na = "-")

m0 <- m %>%
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

unique(m0$orig)
unique(m0$dest)

##
## population data
## 
p <- read_csv(file = "./data-raw/Resident_Population_by_City__County__and_District_1992-2020.csv", na = "-")
p0 <- p %>%
  rename(region = 1) %>%
  pivot_longer(cols = -1, names_to = "year", values_to = "pop") %>%
  filter(year >= 2012) %>%
  mutate(year = as.integer(year)) %>%
  drop_na() %>%
  mutate(region = ifelse(region == "Sejong_si", "Sejong", region), 
         region = ifelse(region == "Jeju-do", "Jeju", region),
         pop = pop/1e6)
         # region = str_remove(string = region, pattern = "-do"))
unique(p0$region) %in% levels(m0$orig)
levels(m0$orig) %in% unique(p0$region)

##
## distance
##
library(sf)
k <- raster::getData(country = "KOR", level  = 1, path = "./data-raw") %>%
  st_as_sf() %>%
  mutate(mid = st_centroid(geometry))
k$NAME_1 %in% unique(p0$region)

c0 <- do.call(rbind, st_geometry(k$mid))
d0 <- c0 %>%
  geosphere::distm() %>%
  round(x = ./1000) %>%
  as_tibble() %>%
  set_names(nm = k$NAME_1) %>%
  mutate(orig = k$NAME_1) %>%
  pivot_longer(cols = -orig, names_to = "dest", values_to = "dist_cent")
d0

d1 <- st_distance(k$geometry) %>%
  round(x = ./1000) %>%
  as_tibble() %>%
  set_names(nm = k$NAME_1) %>%
  mutate(orig = k$NAME_1) %>%
  pivot_longer(cols = -orig, names_to = "dest", values_to = "dist_min") %>%
  mutate(dist_min = as.numeric(dist_min),
         contig = dist_min == 0)
d1

# area  
a0 <- st_area(k$geometry) %>%
  round(x = ./1000) %>%
  enframe(value = "area") %>%
  select(-name) %>%
  mutate(region = k$NAME_1)

##
## population weighted distance
##
# this is from world pop
# https://www.worldpop.org/doi/10.5258/SOTON/WP00703
c1 <- read_csv("./data-raw/PWD_2020_sub_national_100m.csv") %>%
  filter(ISO == "KOR")
c2 <- c1 %>%
  select(PWC_Lon, PWC_Lat)

d2 <- c2 %>%
  geosphere::distm() %>%
  round(x = ./1000) %>%
  as_tibble() %>%
  set_names(nm = k$NAME_1) %>%
  mutate(orig = k$NAME_1) %>%
  pivot_longer(cols = -orig, names_to = "dest", values_to = "dist_pw") 
d2
c1$Adm_N %in% unique(p0$region)
unique(p0$region) %in% c1$Adm_N

##
## contiguity and combine all distances
##
##
## economic
##
# https://kosis.kr/statHtml/statHtml.do?orgId=101&tblId=DT_1C86&conn_path=I2
library(readxl)
e <- read_excel("./data-raw/Regional_GDP__Gross_regional_income_and_Individual_income_20231118124541.xlsx",
                na = "-")
e0 <- e %>%
  rename(year = 1, 
         region = 2, 
         gdp_pc = 3, 
         ginc_pc = 4,
         iinc_pc = 5,
         pconsum_pc = 6) %>%
  fill(year) %>%
  filter(region == "Whole country") %>%
  mutate(year = as.integer(year))

e1 <- e0 %>%
  set_names(nm = c(names(.)[1:2], paste0("orig_", names(.)[3:6])))
e2 <- e1 %>%
  set_names(str_replace_all(string = names(.), pattern = "orig", replacement = "dest"))

##
## combine
##
korea_gravity <- m0 %>%
  left_join(d0) %>%
  left_join(d1) %>%
  left_join(d2) %>%
  left_join(p0, by = c("orig" = "region", "year" = "year")) %>%
  rename(orig_pop = pop) %>%
  left_join(p0, by = c("dest" = "region", "year" = "year")) %>%
  rename(dest_pop = pop) %>%
  left_join(a0, by = c("orig" = "region")) %>%
  rename(orig_area = area) %>%
  left_join(a0, by = c("dest" = "region")) %>%
  rename(dest_area = area) %>%
  left_join(e1, by = c("orig" = "region", "year" = "year")) %>%
  left_join(e2, by = c("dest" = "region", "year" = "year")) %>%
  relocate(1:4, contains("dist"))

usethis::use_data(korea_gravity, overwrite = TRUE)