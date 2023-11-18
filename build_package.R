# usethis::use_cran_badge()
# usethis::use_lifecycle_badge(stage = "stable")
# cranlogs::cranlogs_badge(package_name = "migest", summary = "grand-total")
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

##
## these are key if devtools::check() is working by github actions are not
##
usethis::use_github_action()
usethis::use_github_action_check_standard()
usethis::use_github_action("pkgdown")
usethis::use_github_actions_badge()
file.show("NEWS.md")

# usethis::use_pkgdown()
roxygen2::roxygenise()
# devtools::build()
# devtools::install()
pkgdown::build_site(run_dont_run = TRUE)
pkgdown::build_site_github_pages()
pkgdown::build_reference() 

usethis::use_spell_check()

