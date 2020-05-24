library(tidyverse)

m0 <- read_csv("./data-raw/un1986-tab1-united-arab-republic-1960.csv", skip = 2, n_max = 11) %>%
  select(-13) %>%
  column_to_rownames(var = names(.)[1]) %>%
  set_names(nm = rownames(.))  %>%
  as.matrix() %>%
dimnames(m0) <- list(orig = rownames(m0), dest = colnames(m0))


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

d0 %>%
  filter(not_same)

d0 <- d0 %>%
  mutate(in_migrants = ifelse(not_same & in_migrants == 192425, 792425, in_migrants),
         in_migrants = ifelse(not_same & in_migrants == 12261, 122687, in_migrants),
         in_migrants = ifelse(not_same & in_migrants == 128921, 728927, in_migrants),
         net_migrants = ifelse(not_same & net_migrants == 16, 76617, net_migrants),
         net = in_migrants - out_migrants, 
         net_abs = abs(net), 
         not_same = net_abs != net_migrants)
         
d0 %>%
  filter(not_same)

d0 <- d0 %>%
  select(-net_migrants, -net_abs, -not_same) %>%
  rename(net_migrants = net)


# 
# d9 <- d0 %>%
#   filter(state == "Assam", between(year, 1921, 1931), sex == "MALES")
# 
# 
# d0 %>%
#   filter(between(year, 1921, 1931)) %>%
#   group_by(state, sex) %>%
#   net_from_lifetime(in_survival = 0.81, out_survival = 0.81)
# 

# install.packages("tesseract")
library(tesseract)
eng <- tesseract("eng")
png_file <- pdftools::pdf_convert("./data-raw/eldridge1965.pdf", pages = 191:199,  dpi = 600)
numbers2 <- tesseract(options = list(tessedit_char_whitelist = "-+0123456789"))
text <- tesseract::ocr(png_file, engine = numbers2)
cat(text)
# copy and paste to excel and then use text to columns button to clean

d0 <- 
  read_csv("./data-raw/eldridge1965-tabD.csv") %>%
  fill(place_of_birth) %>%
  fill(year) %>%
  pivot_longer(cols = 4:7, names_to = "race_sex", values_to = "pop") %>%
  separate(race_sex, into = c("race", "sex"), sep = "_")

# check
d0 %>%
  group_by(place_of_birth, year, race, sex) %>%
  summarise(total_text = last(pop),
            total_calc = sum(pop) - total_text, 
            d = total_text - total_calc) %>%
  arrange(d)
  arrange(desc(d))
  

d0 <- d0 %>%
  filter(!str_detect(string = age, pattern = "Total"))
table(d0$age)

d0 <- d0 %>%
  mutate(place_of_birth = fct_inorder(place_of_birth), 
         age = fct_inorder(age),
         race = fct_inorder(race),
         sex = fct_rev(sex)) %>%
  arrange(place_of_birth, year, race, sex, age) %>%
  mutate(age = str_replace(string = age, pattern = "--", replacement = "-"))


d1 <- d0 %>%
  group_by(place_of_birth, year, race, sex) %>%
  summarise(pop = sum(pop)) %>%
  group_by(place_of_birth, race, sex) %>%

d1 %>%
  # filter(race == "white", sex == "male") %>%
  pivot_wider(names_from = year, values_from = pop) %>%
  mutate(surv_ratio = `1960`/`1950`)


numbers2 <- tesseract(options = list(tessedit_char_whitelist = "-0123456789"))
text <- tesseract::ocr("./data-raw/un1986-tab8.png", engine = numbers2)
cat(text)


p1 <- tesseract(options = list(tessedit_char_whitelist = "-0123456789 "))
text <- tesseract::ocr("./data-raw/plane1981-tab1.png")
text <- tesseract::ocr("./data-raw/plane1981-tab2.png")
cat(text)
