clicc_key <- openxlsx::read.xlsx("data-raw/CLICC item to name.xlsx")

usethis::use_data(clicc_key, overwrite = T)

case_closure_key <- openxlsx::read.xlsx("data-raw/Case closure item to name.xlsx")

usethis::use_data(case_closure_key, overwrite = T)
