library(tidyverse)

d <- read_csv("./data-raw/model_schedules.csv")

d0 <- d %>%
  pivot_longer(cols = -(1:2), names_to = "param") %>%
  mutate(schedule_abb = abbreviate(schedule, minlength = 2),
         schedule_abb = str_to_lower(schedule_abb)) %>%
  relocate(contains("sche"))

d0 %>%
  filter(schedule_abb  == "ws",
         sex == "male") %>%
  select(param, value) %>%
  deframe()
rc_model_un <- d0
usethis::use_data(rc_model_un, overwrite = TRUE)

rc9.fund<-list(a1=0.02, alpha1=0.1, a2=0.06, alpha2=0.1, mu2=20, lambda2=0.4, c=0.003)
rc_model_fund <- enframe(rc9.fund) %>%
  mutate(value = unlist(value)) %>%
  rename(param = 1)
usethis::use_data(rc_model_fund, overwrite = TRUE)


