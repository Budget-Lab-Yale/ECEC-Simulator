# Submit an IPUMS USA extract for the state childcare wage work.
#
# KEPT FOR THE COUNTY CROSSWALK. The wage work this extract (usa:33) fed was
# removed 1 Oct 2026; its us2019a records now give every ACS household its
# county (resources/acs/acs_county_crosswalk_2019.py).
#
# IPUMS_API_KEY is read by ipumsr from ~/.Renviron (mode 600). The value is
# never printed or passed on the command line.
#
# WHY EACH VARIABLE -- every one fixes something the 2018 extract could not do:
#
#   INCEARN, INCBUS00   The 2018 derivation filtered on INCWAGE > 0 and so
#                       dropped the unincorporated self-employed, whose
#                       earnings are business income: 13.6% of weighted
#                       childcare workers, most of them home-based providers.
#   CLASSWKR            Self-employed (incorporated or not) vs wage/salary vs
#                       government, so a wage can be measured per SECTOR -- the
#                       supply side prices home-based care and centers
#                       separately, and pooling them hides the difference.
#   WKSWORK1            Continuous weeks worked, which the ACS asks again from
#                       2019. Replaces the interval midpoints the 2018 file
#                       forced, and their noise in the wage denominator.
#   OCCSOC, INDNAICS    Detailed codes, so childcare workers (39-9011) separate
#                       from preschool teachers (25-2011), who are paid more
#                       and more often degree-holding and lifted both means.
#   REPWTP              80 replicate weights: a standard error on every state
#                       wage ratio, as the NSCH rate already carries.
#   PUMA, COUNTYFIP     Sub-state geography. NYC is county-defined; this is the
#                       first extract that could give NYC its own wage.
#   GQ                  Excludes group quarters from a household-based measure.
#
# SAMPLES
#   us2019a   2019 ACS 1-year: the base year exactly.
#   us2019c   2015-2019 ACS 5-year: roughly five times the records, for the
#             thin cells. Vermont had 13 no-degree childcare workers in 2018.

suppressPackageStartupMessages(library(ipumsr))

if (!nzchar(Sys.getenv('IPUMS_API_KEY'))) {
  stop('IPUMS_API_KEY is not set in this R session')
}

ext <- define_extract_micro(
  collection  = 'usa',
  description = 'ECEC-Simulator state childcare wages and labour, 2019',
  samples     = c('us2019a', 'us2019c'),
  variables   = list(
    'STATEFIP', 'PUMA', 'COUNTYFIP', 'GQ',
    'AGE', 'SEX', 'EMPSTAT', 'LABFORCE',
    'CLASSWKR', 'OCC', 'OCCSOC', 'IND', 'INDNAICS',
    'UHRSWORK', 'WKSWORK1', 'WKSWORK2',
    'INCWAGE', 'INCBUS00', 'INCEARN',
    'EDUC',
    'PERWT', 'REPWTP'),
  data_format = 'fixed_width')

sub <- submit_extract(ext)
cat('SUBMITTED extract number:', sub$number, '\n')
cat('status:', get_extract_info(sub)$status, '\n')
