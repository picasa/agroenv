usethis::use_build_ignore("dev_history.r")
usethis::use_mit_license("Pierre Casadebaig")
attachment::att_to_description()

usethis::use_pipe()
usethis::use_package("readr", "Imports")
usethis::use_package("dplyr", "Imports")
usethis::use_package("lubridate", "Imports")
usethis::use_package("stringr", "Imports")
usethis::use_package("zoo", "Imports")
usethis::use_package("nasapower", "Imports")


usethis::use_vignette("aa-usecase")
usethis::use_r("get_station")
usethis::use_r("distance_haversine")
usethis::use_data(station_index, version = 3)
usethis::use_r("station_index")
usethis::use_r("get_climate")
usethis::use_r("et_penman_monteith")


devtools::check()

usethis::use_git()
