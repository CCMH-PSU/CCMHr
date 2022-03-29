
<!-- README.md is generated from README.Rmd. Please edit that file -->

# CCMHr

The goal of CCMHr is to collect functions commonly used when working
with CCMH data.

# Installation

This package isn’t on CRAN, so you’ll need to use the devtools package
to install it.

``` r
devtools::install_github("CCMH-PSU/CCMHr")
```

then load it

``` r
library(CCMHr)
```

# Functions

## Basic data cleaning

  - `check_CCAPS` is used in initially cleaning the CCAPS data from TI
    or the webservice to check that all of the CCAPS item values are in
    bounds. It alerts you if any of the items are out of bounds. Karl at
    TI also runs a check on this, so it should never return an error,
    but better safe than sorry. This is run before the CCAPS subscales
    are scored each time we get a new year of data.

  - `score_CCAPS` runs all the scoring syntax to produce CCAPS subscale
    scores for the CCAPS-34 and CCAPS-62, running checks for validity
    and not scoring any administrations that don’t pass validity checks
    (e.g. variance of 0 or too much missing data).

  - `CCAPS34_cuts` and `CCAPS62_cuts` create dichotomous variables for
    the CCAPS-34 and CCAPS-62 indicating whether each subsclae was above
    the low and high cut scores. The function takes a data frame with
    CCAPS subscales, either in the form `Depression34` or
    `Depression34_first`, specified by the `first` argument, as well as
    an arguemnt specifying which year of cut scores to use, with options
    currently for 2018 & 2019.

<!-- `check_column_classes` -->

## Advanced data cleaning

  - `create_courses` separates the data into courses of therapy based on
    a 90 day criteria. This provides the option of keeping all courses
    or only each client’s first course.

  - `sds_to_factor` converts SDS variables from numeric into factors of
    their actual response options.

  - `delete_duplicate_appointments` removes a client’s duplicate
    appointments that have the same AppointID

## Plotting

  - `ccmh_theme` adds some CCMH specific theming to ggplots. This
    includes changing the font to Avenir, adding spacing between axes
    and axes titles, and increasing the font size. Any of this can be
    overwritten by an additional ggplot theme argument if parts of it
    are not desired.

  - `add_caption` adds a caption to a plot attributing it to CCMH.

## Other CCMH functions

  - `setup_data_request` creates the folders necessary for data request
    cleaning and creates a skeleton data request cleaning syntax file
    with basic syntax that is run on most data requests.

  - `remove_empty` removes columns that are entirely empty. This is
    usually due to items being deactivated.

  - `remove_free_response` removes any free response columns. This is
    done prior to sharing data in case those columns have identifying
    information.

  - The `rename_subscales` functions renames CCAPS subscles them for use
    in a graph or a table (e.g. Anxiety34 -\> Generalized Anxiety)

  - CLI cleaning functions: `bin_enrollment`, `bin_utilization`,
    `bin_utilization`, and `bin_inst_utilization` discretize the CLI
    variables into predetermined bins.

## Other general functions

  - `loadRDa` loads .rda and .rdata files with the ability to assign
    them to a new object name instead of the one they were originally
    saved with. Using `load` on .rda files loads them into the
    environment with the object name they had when they were saved.

  - `get_mode` gets the mode of a numeric vector.

  - `first_present` and `last_present` provide the first/last non-NA
    value in a vector.

## Data files

CCMHr also contains several data objects that can be loaded into R from
the package.

  - `alerts` provides a lookup table for each subscale, starting CCAPS
    bin, and session indicating the score above which a CCAPS alert is
    produced on the CCAPS profile report.

  - `clicc_key` contains a key for the CLICC items and numbers.

  - `case_closure_key` contains a key for the Case Closure items and
    numbers.
