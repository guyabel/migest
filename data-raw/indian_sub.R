# csv copy from 
# https://archive.org/details/in.ernet.dli.2015.130424/page/n59/mode/2up
# https://archive.org/stream/in.ernet.dli.2015.130424/2015.130424.A-Historical-Study-Of-Internal-Migration-In-The-Indian-Sub-continent-1901-1931_djvu.txt

d0 <-
  read_csv("./data-raw/zachariah1964-tab36-india.csv", col_names = "text") %>%
  drop_na() %>%
  mutate(t1 = ifelse(str_detect(string = text, pattern = "MALE|FEMALE"), text, NA), 
         t2 = ifelse(str_detect(string = text, pattern = "United Provinces|Kashmir"), text, t1)) %>%
  fill(t1) %>%
  fill(t2) %>%
  group_by(t1, t2) %>%
  mutate(r1 = 1:n(), 
         k1 = ifelse(r1 == 1, 1, 0), 
         k2 = ifelse(!str_detect(string = t2, pattern = "MALE|FEMALE"), 1, 0),
         k2 = ifelse(str_detect(string = t2, pattern = "Kashmir") & r1 > 4 , 0, k2)) %>%
  ungroup() %>%
  filter(k1 == 1 | k2 == 1) %>%
  mutate(sex_year = t1, 
         state = ifelse(str_detect(string = text, pattern = "[0-9+]"), NA, text)) %>%
  select(-(t1:k2)) %>%
  fill(state) %>%
  filter(text != sex_year, 
         text != state) %>%
  group_by(sex_year, state) %>%
  mutate(type = c("in_migrants", "out_migrants", "net_migrants")) %>% 
  ungroup() %>%
  mutate(number = str_remove_all(string = text, pattern = "[^[:digit:]]+"),
         # number = str_remove_all(string = number, pattern = "[[:blank:]]+")),
         number = as.numeric(number)) %>%
  pivot_wider(names_from = type, values_from = number, id_cols = c(sex_year, state)) %>%
  separate(col = sex_year, into = c("sex", "year")) %>%
  mutate(state = recode(state, 
                        "Balucliistan" = "Baluchistan",
                        "iammu & Kashmir" = "Jammu & Kashmir",
                        "Jammu <& Kashmir" = "Jammu & Kashmir",
                        "lammu & Kashmir" = "Jammu & Kashmir",
                        "Travancore-Cocltin" = "Travancore-Cochin",
                        "Travancorc-Coehin" = "Travancore-Cochin",
                        "Travancore-Coehin" = "Travancore-Cochin",
                        "Nortb-West Zone" = "North-West Zone",
                        "N.W. Frontier Province" = "N. W. Frontier Province")) 

d0 <- d0 %>%
  mutate(year = as.integer(year), 
         net = in_migrants - out_migrants, 
         net_abs = abs(net), 
         not_same = net_abs != net_migrants)

# check
d0 %>%
  filter(not_same)

# correct
d0 <- d0 %>%
  mutate(in_migrants = ifelse(not_same & in_migrants == 192425, 792425, in_migrants),
         in_migrants = ifelse(not_same & in_migrants == 12261, 122687, in_migrants),
         in_migrants = ifelse(not_same & in_migrants == 128921, 728927, in_migrants),
         net_migrants = ifelse(not_same & net_migrants == 16, 76617, net_migrants),
         net = in_migrants - out_migrants, 
         net_abs = abs(net), 
         not_same = net_abs != net_migrants)

# check         
d0 %>%
  filter(not_same)

z <- unique(d0$state) %>%
  str_subset(pattern = "Zone|Bombay|United|Burma")

d0 <- d0 %>%
  select(-net_migrants, -net_abs, -not_same) %>%
  rename(net_migrants = net) %>%
  mutate(zone = ifelse(state %in% z, state, NA)) %>%
  fill(zone) %>%
  relocate(zone, state)


# d9 <- d0 %>%
#   filter(state == "Assam", between(year, 1921, 1931), sex == "MALES")
# 
# 
# d0 %>%
#   filter(between(year, 1921, 1931)) %>%
#   group_by(state, sex) %>%
#   net_from_lifetime(in_survival = 0.81, out_survival = 0.81)

indian_sub <- d0 %>%
  mutate(sex = str_to_lower(sex),
         sex = str_remove(string = sex, pattern = "s"))

usethis::use_data(indian_sub, overwrite = TRUE)
