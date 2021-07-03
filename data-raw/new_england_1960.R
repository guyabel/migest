# copy and pasted from pdf to excel

d <- read_csv("./data-raw/un1986-tab8-10-new-england.csv")
d0 <- d %>%
  pivot_longer(cols = -(1:2), names_to = "place_of_birth", values_to = "pop") %>%
  filter(place_of_birth != "Total", 
         age != "All", 
         age != "10+") %>%
  mutate(age = str_replace(string = age, pattern = "--", replacement = "-"), 
         place_of_birth = fct_inorder(place_of_birth),
         age = fct_inorder(age)) %>%
  separate(age, into = c("age_min", "age_max"), remove = FALSE, convert = TRUE) %>%
  relocate(place_of_birth, year) %>%
  select(-age_max) %>%
  rename(birthplace = place_of_birth) %>%
  mutate(age = fct_inorder(age), 
         age = fct_reorder(age, age_min)) %>%
  group_by(year) %>%
  mutate(age_grp = droplevels(age) %>% as.numeric) %>%
  ungroup()

d2 <- d0 %>%
  filter(year == 1960) %>%
  select(age_grp, age) %>%
  distinct() %>%
  rename(age_1960 = age)

d3 <- d0 %>%
  left_join(d2) %>%
  select(-age, -age_min, -age_grp) %>%
  pivot_wider(names_from = year, names_prefix = "pop_", values_from = pop)
d3

new_england_1960 <- d3
usethis::use_data(new_england_1960, overwrite = TRUE)