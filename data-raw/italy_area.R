library(tidyverse)
library(readxl)

r <- c("Northwest", "Northeast", "Center", "South", "Islands", "Total")
n <- c("orig", "age", paste(r, rep(seq(1970, 2000, by = 5), each = 6)))
d <- read_excel("./data-raw/italy_data.xls", col_names = n, skip = 4, n_max = 132)

d0 <- d %>%
  fill(orig) %>%
  pivot_longer(cols = -(1:2), names_to = "dest year", values_to = "flow") %>%
  separate(col = "dest year", into = c("dest", "year"), convert = TRUE) %>%
  mutate_if(is.character, fct_inorder) %>%
  arrange(year, orig, dest, age) %>%
  filter(orig != "Total", dest != "Total", age != "Total") %>%
  rename(age_grp = age) %>%
  droplevels %>%
  relocate(orig, dest, year, age_grp) %>%
  arrange(year, age_grp, dest, orig)
# write_csv(d0, "./data/italy_tidy.csv")

# d <- read_csv("./data-raw/italy.csv")
# d0 <- d %>%
#   mutate(age = age*5, 
#          age = paste0(age-5, "-", age-1),
#          age = ifelse(age == "95-99", "95+", age),
#          age = fct_inorder(age)) %>%
#   rename(age_grp = age)

italy_area <- d0
usethis::use_data(italy_area, overwrite = TRUE)