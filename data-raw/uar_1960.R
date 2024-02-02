uar_1960 <- read_csv("./data-raw/un1986-tab1-united-arab-republic-1960.csv", skip = 2, n_max = 11) %>%
  select(-13) %>%
  column_to_rownames(var = names(.)[1]) %>%
  set_names(nm = rownames(.))  %>%
  as.matrix()

dimnames(uar_1960) <- list(orig = rownames(uar_1960), dest = colnames(uar_1960))

usethis::use_data(uar_1960, overwrite = TRUE)
