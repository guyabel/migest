d <- read_csv("./data-raw/italy.csv")

d0 <- d %>%
  mutate(age = age*5, 
         age = paste0(age-5, "-", age-1),
         age = ifelse(age == "95-99", "95+", age),
         age = fct_inorder(age)) %>%
  rename(age_grp = age)

italy_macro <- d0
usethis::use_data(italy_macro, overwrite = TRUE)