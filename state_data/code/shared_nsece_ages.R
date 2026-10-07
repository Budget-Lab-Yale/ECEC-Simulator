#------------------------------------------------------------------------------
# shared_nsece_ages.R -- one definition each of NSECE 2019 centre enrolment by
# age 0-4 and single year of age -> licensing age band.
#
# Sourced by resources/supply/state_supply_params.R (centre staffing index
# weights). Constants only -- no packages, no I/O.
#------------------------------------------------------------------------------

# NSECE 2019 center enrollment by age 0-4, national, weighted -- ACF, Measuring
# Supply Capacity at Center-based CCEE (2023), Exhibit 1 (counts rounded to 3
# significant figures by ACF).
NSECE_CENTRE_ENROLLED <- c(age_0 = 426000, age_1 = 718000, age_2 = 1010000,
                           age_3 = 2170000, age_4 = 2340000)

# Single year of age 0-4 -> licensing age band of ACF child-staff ratios
# (CAP 2018 Table 1: infant, toddler = 35 months, preschool = 3 years -- no
# separate 4-year rule, age 4 takes preschool)
NSECE_AGE_BAND <- c(age_0 = 'infant', age_1 = 'toddler', age_2 = 'toddler',
                    age_3 = 'preschool', age_4 = 'preschool')

# Ages 0-4, build's age range
NSECE_AGES_0_4 <- names(NSECE_AGE_BAND)
