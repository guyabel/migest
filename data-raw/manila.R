d <- read_csv("./data-raw/manila.csv")

d0 <- d %>%
  filter(age_1970 != "All ages") %>%
  select(-contains("pop_exp"),-contains("phl_pop"), -net) %>%
  mutate(age_1970 = str_replace(string = age_1970, pattern = "--", replacement = "-"),
         age_1970 = fct_inorder(age_1970)) %>%
  relocate(-phl_census_sr)
  

manila_1970 <- d0
usethis::use_data(manila_1970, overwrite = TRUE)