# usethis::use_cran_badge()
# usethis::use_lifecycle_badge(stage = "stable")
usethis::use
# usethis::use_pipe()
# usethis::use_tibble()
# usethis::use_news_md()

usethis::use_build_ignore(
  c("build_package.R", "data-raw", "hex", "old-scripts",
    "tests", "LICENSE", "README")
)

roxygen2::roxygenise()

devtools::check()
devtools::build()

file.show("NEWS.md")

# usethis::use_pkgdown()
pkgdown::build_site(run_dont_run = TRUE)
pkgdown::build_reference()

usethis::use_spell_check()

