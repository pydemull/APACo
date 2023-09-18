## code to prepare `INCLUSION` dataset goes here
INCLUSION <- readr::read_csv2("data-raw/INCLUSION.csv")
usethis::use_data(INCLUSION, overwrite = TRUE)

## code to prepare `VISIT_6M` dataset goes here
VISIT_6M <- readr::read_csv2("data-raw/VISIT_6M.csv")
usethis::use_data(VISIT_6M, overwrite = TRUE)

## code to prepare `VISIT_12M` dataset goes here
VISIT_12M <- readr::read_csv2("data-raw/VISIT_12M.csv")
usethis::use_data(VISIT_12M, overwrite = TRUE)

## code to prepare `IPAQ` dataset goes here
IPAQ <- readr::read_csv2("data-raw/IPAQ.csv") |> dplyr::rename("bloc1_q2" = "bloc_q2")
usethis::use_data(IPAQ, overwrite = TRUE)

## code to prepare `EMAPS` dataset goes here
EMAPS <- readr::read_csv2("data-raw/EMAPS.csv")
usethis::use_data(EMAPS, overwrite = TRUE)

## code to prepare `BARRIERS` dataset goes here
BARRIERS <- readr::read_csv2("data-raw/BARRIERS.csv")
usethis::use_data(BARRIERS, overwrite = TRUE)

