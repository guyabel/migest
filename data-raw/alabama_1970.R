# library(tesseract)
# eng1 <- tesseract(options = list(tessedit_char_whitelist = "-0123456789 "))
# eng2 <- tesseract(options = list(tessedit_char_whitelist = ".0123456789 "))
# text <- tesseract::ocr("./data-raw/bogue1982_p58.png", engine = eng1)
# text <- tesseract::ocr("./data-raw/bogue1982_p59.png", engine = eng1)
# text <- tesseract::ocr("./data-raw/bogue1982_p22.png", engine = eng2)
# # text <- tesseract::ocr("./data-raw/bogue1982_p61.png")
# # text <- tesseract::ocr("./data-raw/bogue1982_p62.png")
# cat(text)
# 
# d <- str_split(string = text, pattern = "\n")
# dput(d, file = "./data-raw/temp.txt")
# file.show("./data-raw/temp.txt")
# 
d <- read_csv("./data-raw/alabama.csv")

d0 <- d %>%
  select(-male_age_1970, -contains("pop_exp")) %>%
  rename(age_1970 = female_age_1970) %>%
  pivot_longer(cols = -(1:2),
               names_to = c("sex", "type", "year"),
               names_sep = "_") %>%
  replace_na(list(year = "")) %>%
  unite(col = "type", c("type", "year")) %>%
  pivot_wider(names_from = type) %>%
  filter(age_1970 != "TOTAL") %>%
  mutate(age_1970 = str_replace(string = age_1970, pattern = "--", replacement = "-"),
         age_1970 = fct_inorder(age_1970)) %>%
  arrange(sex) %>%
  rename(us_census_sr = sr_)

d1 <- d0 %>%
  select(-contains("net")) %>%
  relocate(age_1970, sex, race)

# d2 <- d1 %>%
#   mutate(age_1960 = as.character(age_1970)) %>%
#   relocate(age_1960, age_1970) %>%
#   group_by(race, sex) %>%
#   mutate(age_1960 = lead(age_1960, 2)) %>%
#   relocate(race, age_1960)
#   
# levels(alabama$age_1970)
alabama_1970 <- d1
usethis::use_data(alabama_1970, overwrite = TRUE)
