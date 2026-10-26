#------------------------------------------------------------------------------
# shared_nsece_ages.R -- one definition each of NSECE 2019 centre enrolment by
# age 0-4, single year of age -> licensing age point (2017 study), reviewed
# 2019 age-year row, licensing age band (2014 input; ID, NYC centre rows).
#
# Sourced by resources/supply/state_supply_params.R (centre staffing index
# weights), resources/supply/licensing_ratios_2017.R (age points). Constants
# only -- no packages, no I/O.
#------------------------------------------------------------------------------

# NSECE 2019 center enrollment by age 0-4, national, weighted -- ACF, Measuring
# Supply Capacity at Center-based CCEE (2023), Exhibit 1 (counts rounded to 3
# significant figures by ACF). Staffing index weights: direct survey count of
# centre-enrolled children by single year of age -- preferred over model-
# predicted ACS children by care type (raw/US/US_ACS__p0_children_by_care_type_
# hours_age__2019.csv, forest prediction).
NSECE_CENTRE_ENROLLED <- c(age_0 = 426000, age_1 = 718000, age_2 = 1010000,
                           age_3 = 2170000, age_4 = 2340000)

# Single year of age 0-4 -> representative licensing age point of 2017 Child
# Care Licensing Study (ICPSR 37700, PS_RAT_*): five of study's ratio ages, one
# per year of age. Ratio at one age stands for whole year -- 2017 comparison
# index, 2019 audit points only. 2019 build month-weights bands instead
# (NSECE_AGE_YEAR).
NSECE_AGE_POINT <- c(age_0 = '11_months', age_1 = '18_months', age_2 = '35_months',
                     age_3 = '3_years', age_4 = '4_years')

# Single year of age 0-4 -> reviewed 2019 centre row (licensing_ratios_2019.R
# centre_age_years(): month-weighted ordinary bands, cut-offs inside a year split).
# No added care sectors or choices -- ages aggregate to one centre index.
NSECE_AGE_YEAR <- c(age_0 = 'age_0', age_1 = 'age_1', age_2 = 'age_2',
                    age_3 = 'age_3', age_4 = 'age_4')

# Single year of age 0-4 -> licensing age band of ACF child-staff ratios
# (CAP 2018 Table 1: infant, toddler = 35 months, preschool = 3 years -- no
# separate 4-year rule, age 4 takes preschool). Reads 2014 input, maps ID / NYC
# centre rows (3 bands) onto 5 age points, gives 3-point comparison index.
NSECE_AGE_BAND <- c(age_0 = 'infant', age_1 = 'toddler', age_2 = 'toddler',
                    age_3 = 'preschool', age_4 = 'preschool')

# Ages 0-4, build's age range
NSECE_AGES_0_4 <- names(NSECE_AGE_POINT)
