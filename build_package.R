library(usethis)
create_tidy_package(path = "Github/migest")
# usethis::use_badge()
usethis::use_pipe()
usethis::use_tibble()
usethis::use_build_ignore(c("build_package.R", "data-raw"))

roxygen2::roxygenise()

usethis::use_spell_check()

