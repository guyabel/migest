library(tidyverse)
d <- read_csv("./data-raw/bombay.csv")

d0 <- d %>%
  filter(age_1941 != "All ages") %>%
  mutate(age_1951 = str_replace(string = age_1951, pattern = "--", replacement = "-"),
         age_1951 = fct_inorder(age_1951),
         age_1941 = str_replace(string = age_1941, pattern = "--", replacement = "-"),
         age_1941 = fct_inorder(age_1941))

d1 <- d0 %>%
  relocate(contains("age")) %>%
  relocate(-sr) %>%
  select(-net, -pop_exp_1951)
# levels(alabama$age_1970)
bombay_1951 <- d1
usethis::use_data(bombay_1951, overwrite = TRUE)
