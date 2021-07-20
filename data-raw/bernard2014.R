# library(tidyverse)
# d <- read_csv("./data-raw/bernard2014-taba1.csv")
# 
# nmi_ipumsi <- d
# usethis::use_data(nmi_ipumsi, overwrite = TRUE)

# library(devtools)
library(ADRItools)
# install_github(repo = "guyabel/teaching/ADRItools")

library(haven)
library(tidyverse)
b <- read_dta(file = "D:\\ADRI\\project\\ipumsi-data\\data\\BRA2000_ipumsi.dta",
              col_select = c("age", "migrate5", "perwt", "geolev1"))

f <- read_dta(file = "D:\\ADRI\\project\\ipumsi-data\\data\\FRA2006_ipumsi.dta",
              col_select = c("age", "migrate5", "perwt", "geolev1"))

print_labels(b$migrate5)
d1 <- b %>%
  group_by(age) %>%
  summarise(#M = sum(perwt[migrate5 == 12]),
            M = sum(perwt[migrate5 %in% c(12, 20, 30)]),
            P = sum(perwt),
            m = M/P) %>%
  ungroup() %>%
  ipums_clean_dta(numeric_convert = "age")

ggplot(data = d1, mapping = aes(x = age, y = m)) +
  geom_line()

index_age(age = d1$age, m = d1$m)

d2 <- f %>%
  group_by(age) %>%
  summarise(#M = sum(perwt[migrate5 == 12]),
    M = sum(perwt[migrate5 %in% c(12, 20, 30)]),
    P = sum(perwt),
    m = M/P) %>%
  ungroup() %>%
  ipums_clean_dta(numeric_convert = "age")

d2 <- mutate(d2, nmi = m/sum(m) * 100)

d <- d1 %>%
  mutate(ipumsi_sample = "BRA2000") %>%
  bind_rows(d2) %>%
  replace_na(list(ipumsi_sample = "FRA2006")) %>%
  relocate(ipumsi_sample) %>%
  rename(migrants = M, 
         population = P) %>%
  select(-m) %>%
  rename(sample = ipumsi_sample)
d$nmi <- NULL
ipumsi_age <- d

usethis::use_data(ipumsi_age, overwrite = TRUE)