# Prepare alerts data set containing the threshold above which a score triggers a feedback alert
# In the data, session 1 indicates session 1 after baseline. Baseline is coded as session 0 and alerts are not offered at session 0.

depression <- openxlsx::read.xlsx("data-raw/Equations and alerts-Titanium_4.19.15.xlsx", startRow = 29) %>%
  tidyr::pivot_longer(names_to = "session", values_to = "score", cols = -c(`Bin#`)) %>%
  dplyr::mutate(subscale = "Depression34")

anxiety <- openxlsx::read.xlsx("data-raw/Equations and alerts-Titanium_4.19.15.xlsx", startRow = 31, sheet = "Anxiety34") %>%
  dplyr::select(-X21) %>%
  tidyr::pivot_longer(names_to = "session", values_to = "score", cols = -c(`Bin#`)) %>%
  dplyr::mutate(subscale = "Anxiety34")

social_anxiety <- openxlsx::read.xlsx("data-raw/Equations and alerts-Titanium_4.19.15.xlsx", startRow = 27, sheet = "SocialAnxiety34") %>%
  dplyr::select(-X21) %>%
  tidyr::pivot_longer(names_to = "session", values_to = "score", cols = -c(`Bin#`)) %>%
  dplyr::mutate(subscale = "Social_Anxiety34")

academics <- openxlsx::read.xlsx("data-raw/Equations and alerts-Titanium_4.19.15.xlsx", startRow = 23, sheet = "Academics34") %>%
  dplyr::select(-X21) %>%
  tidyr::pivot_longer(names_to = "session", values_to = "score", cols = -c(`Bin#`)) %>%
  dplyr::mutate(subscale = "Academics34")

eating <- openxlsx::read.xlsx("data-raw/Equations and alerts-Titanium_4.19.15.xlsx", startRow = 19, sheet = "Eating34") %>%
  dplyr::select(-X21) %>%
  tidyr::pivot_longer(names_to = "session", values_to = "score", cols = -c(`Bin#`)) %>%
  dplyr::mutate(subscale = "Eating34")

hostility <- openxlsx::read.xlsx("data-raw/Equations and alerts-Titanium_4.19.15.xlsx", startRow = 24, sheet = "Hostility34") %>%
  tidyr::pivot_longer(names_to = "session", values_to = "score", cols = -c(`Bin#`)) %>%
  dplyr::mutate(subscale = "Hostility34")

alcohol <- openxlsx::read.xlsx("data-raw/Equations and alerts-Titanium_4.19.15.xlsx", startRow = 19, sheet = "Alcohol34") %>%
  tidyr::pivot_longer(names_to = "session", values_to = "score", cols = -c(`Bin#`)) %>%
  dplyr::mutate(subscale = "Alcohol34")

DI <- openxlsx::read.xlsx("data-raw/Equations and alerts-Titanium_4.19.15.xlsx", startRow = 55, sheet = "DistressIndex") %>%
  tidyr::pivot_longer(names_to = "session", values_to = "score", cols = -c(`Bin#`)) %>%
  dplyr::mutate(subscale = "DI")

alerts <- rbind(depression, anxiety, social_anxiety, academics, eating, hostility, alcohol, DI) %>%
  dplyr::rename("bin" = "Bin#") %>%
  dplyr::select(subscale, dplyr::everything())

usethis::use_data(alerts, overwrite = T)
