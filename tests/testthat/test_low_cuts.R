context("low_cuts")

library(CCMHr)
library(testthat)

test_data <- data.frame(Depression34 = c(NA, seq(from = 0, to = 4, by = .5)), Anxiety34 = c(NA, seq(from = 0, to = 4, by = .5)), Social_Anxiety34 = c(NA, seq(from = 0, to = 4, by = .5)), Academics34 = c(NA, seq(from = 0, to = 4, by = .5)), Eating34 = c(NA, seq(from = 0, to = 4, by = .5)), Hostility34 = c(NA, seq(from = 0, to = 4, by = .5)), Alcohol34 = c(NA, seq(from = 0, to = 4, by = .5)), DI = c(NA, seq(from = 0, to = 4, by = .5)))

solution_data_2018 <- data.frame(Depression34 = c(NA, seq(from = 0, to = 4, by = .5)),
                            Anxiety34 = c(NA, seq(from = 0, to = 4, by = .5)),
                            Social_Anxiety34 = c(NA, seq(from = 0, to = 4, by = .5)),
                            Academics34 = c(NA, seq(from = 0, to = 4, by = .5)),
                            Eating34 = c(NA, seq(from = 0, to = 4, by = .5)),
                            Hostility34 = c(NA, seq(from = 0, to = 4, by = .5)),
                            Alcohol34 = c(NA, seq(from = 0, to = 4, by = .5)),
                            DI = c(NA, seq(from = 0, to = 4, by = .5)),
                            depression_low_cut34 = c(NA, 0, 0, 1, 1, 1, 1, 1, 1, 1),
                            depression_hi_cut34 = c(NA, 0, 0, 0, 0, 1, 1, 1, 1, 1),
                            anxiety_low_cut34 = c(NA, 0, 0, 0, 1, 1, 1, 1, 1, 1),
                            anxiety_hi_cut34 = c(NA, 0, 0, 0, 0, 0, 1, 1, 1, 1),
                            social_anxiety_low_cut34 = c(NA, 0, 0, 0, 0, 1, 1, 1, 1, 1),
                            social_anxiety_hi_cut34 = c(NA, 0, 0, 0, 0, 0, 1, 1, 1, 1),
                            academics_low_cut34 = c(NA, 0, 0, 0, 1, 1, 1, 1, 1, 1),
                            academics_hi_cut34 = c(NA, 0, 0, 0, 0, 0, 1, 1, 1, 1),
                            eating_low_cut34 = c(NA, 0, 0, 0, 1, 1, 1, 1, 1, 1),
                            eating_hi_cut34 = c(NA, 0, 0, 0, 1, 1, 1, 1, 1, 1),
                            hostility_low_cut34 = c(NA, 0, 0, 1, 1, 1, 1, 1, 1, 1),
                            hostility_hi_cut34 = c(NA, 0, 0, 0, 1, 1, 1, 1, 1, 1),
                            alcohol_low_cut34 = c(NA, 0, 0, 1, 1, 1, 1, 1, 1, 1),
                            alcohol_hi_cut34 = c(NA, 0, 0, 0, 1, 1, 1, 1, 1, 1),
                            DI_low_cut = c(NA, 0, 0, 0, 1, 1, 1, 1, 1, 1),
                            DI_hi_cut = c(NA, 0, 0, 0, 0, 0, 1, 1, 1, 1))

solution_data_2019 <- data.frame(Depression34 = c(NA, seq(from = 0, to = 4, by = .5)),
                                 Anxiety34 = c(NA, seq(from = 0, to = 4, by = .5)),
                                 Social_Anxiety34 = c(NA, seq(from = 0, to = 4, by = .5)),
                                 Academics34 = c(NA, seq(from = 0, to = 4, by = .5)),
                                 Eating34 = c(NA, seq(from = 0, to = 4, by = .5)),
                                 Hostility34 = c(NA, seq(from = 0, to = 4, by = .5)),
                                 Alcohol34 = c(NA, seq(from = 0, to = 4, by = .5)),
                                 DI = c(NA, seq(from = 0, to = 4, by = .5)),
                                 depression_low_cut34 = c(NA, 0, 0, 1, 1, 1, 1, 1, 1, 1),
                                 depression_hi_cut34 = c(NA, 0, 0, 0, 0, 1, 1, 1, 1, 1),
                                 anxiety_low_cut34 = c(NA, 0, 0, 0, 1, 1, 1, 1, 1, 1),
                                 anxiety_hi_cut34 = c(NA, 0, 0, 0, 0, 0, 1, 1, 1, 1),
                                 social_anxiety_low_cut34 = c(NA, 0, 0, 0, 1, 1, 1, 1, 1, 1),
                                 social_anxiety_hi_cut34 = c(NA, 0, 0, 0, 0, 0, 1, 1, 1, 1),
                                 academics_low_cut34 = c(NA, 0, 0, 0, 1, 1, 1, 1, 1, 1),
                                 academics_hi_cut34 = c(NA, 0, 0, 0, 0, 0, 1, 1, 1, 1),
                                 eating_low_cut34 = c(NA, 0, 0, 1, 1, 1, 1, 1, 1, 1),
                                 eating_hi_cut34 = c(NA, 0, 0, 0, 1, 1, 1, 1, 1, 1),
                                 hostility_low_cut34 = c(NA, 0, 0, 1, 1, 1, 1, 1, 1, 1),
                                 hostility_hi_cut34 = c(NA, 0, 0, 0, 1, 1, 1, 1, 1, 1),
                                 alcohol_low_cut34 = c(NA, 0, 0, 1, 1, 1, 1, 1, 1, 1),
                                 alcohol_hi_cut34 = c(NA, 0, 0, 0, 1, 1, 1, 1, 1, 1),
                                 DI_low_cut = c(NA, 0, 0, 0, 1, 1, 1, 1, 1, 1),
                                 DI_hi_cut = c(NA, 0, 0, 0, 0, 0, 1, 1, 1, 1))

test_data_first <- data.frame(Depression34_first = c(NA, seq(from = 0, to = 4, by = .5)), Anxiety34_first = c(NA, seq(from = 0, to = 4, by = .5)), Social_Anxiety34_first = c(NA, seq(from = 0, to = 4, by = .5)), Academics34_first = c(NA, seq(from = 0, to = 4, by = .5)), Eating34_first = c(NA, seq(from = 0, to = 4, by = .5)), Hostility34_first = c(NA, seq(from = 0, to = 4, by = .5)), Alcohol34_first = c(NA, seq(from = 0, to = 4, by = .5)), DI_first = c(NA, seq(from = 0, to = 4, by = .5)))

solution_data_first_2018 <- data.frame(Depression34_first = c(NA, seq(from = 0, to = 4, by = .5)),
                            Anxiety34_first = c(NA, seq(from = 0, to = 4, by = .5)),
                            Social_Anxiety34_first = c(NA, seq(from = 0, to = 4, by = .5)),
                            Academics34_first = c(NA, seq(from = 0, to = 4, by = .5)),
                            Eating34_first = c(NA, seq(from = 0, to = 4, by = .5)),
                            Hostility34_first = c(NA, seq(from = 0, to = 4, by = .5)),
                            Alcohol34_first = c(NA, seq(from = 0, to = 4, by = .5)),
                            DI_first = c(NA, seq(from = 0, to = 4, by = .5)),
                            depression_low_cut34 = c(NA, 0, 0, 1, 1, 1, 1, 1, 1, 1),
                            depression_hi_cut34 = c(NA, 0, 0, 0, 0, 1, 1, 1, 1, 1),
                            anxiety_low_cut34 = c(NA, 0, 0, 0, 1, 1, 1, 1, 1, 1),
                            anxiety_hi_cut34 = c(NA, 0, 0, 0, 0, 0, 1, 1, 1, 1),
                            social_anxiety_low_cut34 = c(NA, 0, 0, 0, 0, 1, 1, 1, 1, 1),
                            social_anxiety_hi_cut34 = c(NA, 0, 0, 0, 0, 0, 1, 1, 1, 1),
                            academics_low_cut34 = c(NA, 0, 0, 0, 1, 1, 1, 1, 1, 1),
                            academics_hi_cut34 = c(NA, 0, 0, 0, 0, 0, 1, 1, 1, 1),
                            eating_low_cut34 = c(NA, 0, 0, 0, 1, 1, 1, 1, 1, 1),
                            eating_hi_cut34 = c(NA, 0, 0, 0, 1, 1, 1, 1, 1, 1),
                            hostility_low_cut34 = c(NA, 0, 0, 1, 1, 1, 1, 1, 1, 1),
                            hostility_hi_cut34 = c(NA, 0, 0, 0, 1, 1, 1, 1, 1, 1),
                            alcohol_low_cut34 = c(NA, 0, 0, 1, 1, 1, 1, 1, 1, 1),
                            alcohol_hi_cut34 = c(NA, 0, 0, 0, 1, 1, 1, 1, 1, 1),
                            DI_low_cut = c(NA, 0, 0, 0, 1, 1, 1, 1, 1, 1),
                            DI_hi_cut = c(NA, 0, 0, 0, 0, 0, 1, 1, 1, 1))

solution_data_first_2019 <- data.frame(Depression34_first = c(NA, seq(from = 0, to = 4, by = .5)),
                                       Anxiety34_first = c(NA, seq(from = 0, to = 4, by = .5)),
                                       Social_Anxiety34_first = c(NA, seq(from = 0, to = 4, by = .5)),
                                       Academics34_first = c(NA, seq(from = 0, to = 4, by = .5)),
                                       Eating34_first = c(NA, seq(from = 0, to = 4, by = .5)),
                                       Hostility34_first = c(NA, seq(from = 0, to = 4, by = .5)),
                                       Alcohol34_first = c(NA, seq(from = 0, to = 4, by = .5)),
                                       DI_first = c(NA, seq(from = 0, to = 4, by = .5)),
                                       depression_low_cut34 = c(NA, 0, 0, 1, 1, 1, 1, 1, 1, 1),
                                       depression_hi_cut34 = c(NA, 0, 0, 0, 0, 1, 1, 1, 1, 1),
                                       anxiety_low_cut34 = c(NA, 0, 0, 0, 1, 1, 1, 1, 1, 1),
                                       anxiety_hi_cut34 = c(NA, 0, 0, 0, 0, 0, 1, 1, 1, 1),
                                       social_anxiety_low_cut34 = c(NA, 0, 0, 0, 1, 1, 1, 1, 1, 1),
                                       social_anxiety_hi_cut34 = c(NA, 0, 0, 0, 0, 0, 1, 1, 1, 1),
                                       academics_low_cut34 = c(NA, 0, 0, 0, 1, 1, 1, 1, 1, 1),
                                       academics_hi_cut34 = c(NA, 0, 0, 0, 0, 0, 1, 1, 1, 1),
                                       eating_low_cut34 = c(NA, 0, 0, 1, 1, 1, 1, 1, 1, 1),
                                       eating_hi_cut34 = c(NA, 0, 0, 0, 1, 1, 1, 1, 1, 1),
                                       hostility_low_cut34 = c(NA, 0, 0, 1, 1, 1, 1, 1, 1, 1),
                                       hostility_hi_cut34 = c(NA, 0, 0, 0, 1, 1, 1, 1, 1, 1),
                                       alcohol_low_cut34 = c(NA, 0, 0, 1, 1, 1, 1, 1, 1, 1),
                                       alcohol_hi_cut34 = c(NA, 0, 0, 0, 1, 1, 1, 1, 1, 1),
                                       DI_low_cut = c(NA, 0, 0, 0, 1, 1, 1, 1, 1, 1),
                                       DI_hi_cut = c(NA, 0, 0, 0, 0, 0, 1, 1, 1, 1))

# Tests
test_that("CCAPS34 cuts", {
  expect_equal(ccaps34_cuts(test_data, first = F, version = "2018"), solution_data_2018)
})

test_that("CCAPS34_first cuts", {
  expect_equal(ccaps34_cuts(test_data_first, first = T, version = "2018"), solution_data_first_2018)
})

test_that("CCAPS34 cuts", {
  expect_equal(ccaps34_cuts(test_data, first = F, version = "2019"), solution_data_2019)
})

test_that("CCAPS34_first cuts", {
  expect_equal(ccaps34_cuts(test_data_first, first = T, version = "2019"), solution_data_first_2019)
})

test_that("CCAPS34 cuts error", {
  expect_error(ccaps34_cuts(data.frame(X = c(1, 2))))
})
