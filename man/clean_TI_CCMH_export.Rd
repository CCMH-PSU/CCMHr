% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/TI_CCMH_export_support_functions.r
\name{clean_TI_CCMH_export}
\alias{clean_TI_CCMH_export}
\title{Clean TI appointment export file}
\usage{
clean_TI_CCMH_export(
  data,
  id_var = NULL,
  key = CCMHr::TI_CCMH_export_forms_key,
  included_forms = c("CCAPS", "SDS", "CLICC", "Closure", "AUDIT"),
  save_individual_form = FALSE,
  path_individual_form = NULL,
  save = FALSE,
  path = "TI_CCMH_export_clean.csv"
)
}
\value{
A data frame with cleaned TI exported data forms.
}
\description{
@param data A data frame.
@param id_var A quoted string to indicate the name of the ID variable if it exist. By default, \code{NULL}.
@param key A key to rename the columns in the form data file. Default is \code{CCMHr::TI_CCMH_export_forms_key}
@param included_forms A string or list of strings to indicate the forms that should be included in the data. Options include: "CCAPS", "SDS", "CLICC", "Closure", and "AUDIT". By default, \code{c("CCAPS", "SDS", "CLICC", "Closure", "AUDIT")}
@param save_individual_form A logical statement indicates whether the individual forms should be saved seperate under a local folder. By default, \code{FALSE}
@param path_individual_form A quoted string to indicate the path where the individual forms should be saved. By default, "NULL" or saved under the current working directory
@param save A logical statement indicates whether the overall data frame should be saved as a file under a local folder. If false, the data will be returned as an object. By default, \code{FALSE}
@param path A quoted string to indicate the path where the appointment data should be saved. By default, \code{TI_appointment_clean.csv}
}
