# # install.packages("tesseract")
# library(tesseract)
# eng <- tesseract("eng")
# png_file <- pdftools::pdf_convert("./data-raw/eldridge1965.pdf", pages = 191:199,  dpi = 600)
# numbers2 <- tesseract(options = list(tessedit_char_whitelist = "-+0123456789"))
# text <- tesseract::ocr(png_file, engine = numbers2)
# cat(text)
# # copy and paste to excel and then use text to columns button to clean

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
  # arrange(d)
  arrange(desc(d))


d1 <- d0 %>%
  filter(!str_detect(string = age, pattern = "Total")) %>%
  mutate(age = str_replace(string = age, pattern = "--", replacement = "-"))
# no under 10 in 1960s
table(d1$age)

d2 <- d1 %>%
  select(year, age) %>%
  distinct() %>%
  pivot_wider(names_from = year, values_from = age, 
              names_prefix = "age_", values_fn = list) %>%
  unnest()

d3 <- d1 %>%
  left_join(d2, by = c("age" = "age_1960")) %>%
  left_join(d2, by = c("age" = "age_1950")) %>%
  mutate(age_1950 = ifelse(year == 1950, age, age_1950),
         age_1960 = ifelse(year == 1960, age, age_1960)) %>%
  select(-age) %>%
  pivot_wider(names_from = year, names_prefix = "pop_", values_from = pop) %>%
  mutate(place_of_birth = fct_inorder(place_of_birth), 
         race = fct_inorder(race),
         age_1950 = fct_inorder(age_1950),
         age_1960 = fct_inorder(age_1960),
         sex = fct_rev(sex)) %>%
  rename(birthplace = place_of_birth) 

usa_1960 <- d3
usethis::use_data(usa_1960, overwrite = TRUE)