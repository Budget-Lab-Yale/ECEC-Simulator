#------------------------------------------------------------------------------
# shared_nsece_ages.R -- one definition each of NSECE 2019 centre enrolment by
# age 0-4, single year of age -> reviewed 2019 age-year row.
#
# Sourced by resources/supply/state_supply_params.R (centre staffing index
# weights). Constants only -- no packages, no I/O.
#------------------------------------------------------------------------------

# NSECE 2019 center enrollment by age 0-4, national, weighted -- ACF, Measuring
# Supply Capacity at Center-based CCEE (2023), Exhibit 1 (counts rounded to 3
# significant figures by ACF). Staffing index weights: direct survey count of
# centre-enrolled children by single year of age -- preferred over model-
# predicted ACS children by care type (raw/US/US_ACS__p0_children_by_care_type_
# hours_age__2019.csv, forest prediction).
NSECE_CENTRE_ENROLLED <- c(age_0 = 426000, age_1 = 718000, age_2 = 1010000,
                           age_3 = 2170000, age_4 = 2340000)

# Single year of age 0-4 -> reviewed 2019 centre row (month-weighted ordinary
# bands, cut-offs inside a year split).
# No added care sectors or choices -- ages aggregate to one centre index.
NSECE_AGE_YEAR <- c(age_0 = 'age_0', age_1 = 'age_1', age_2 = 'age_2',
                    age_3 = 'age_3', age_4 = 'age_4')

# Ages 0-4, build's age range
NSECE_AGES_0_4 <- names(NSECE_AGE_YEAR)
