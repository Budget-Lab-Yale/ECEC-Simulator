#------------------------------------------------------------------------------
# 14_program_hours_sensitivity.R -- Unpaid Center-Based annual hours built
# program by program (pre-K, Head Start, Early Head Start) from each program's
# weekly hours, operating weeks, against step 6's grid hours.
#
# SENSITIVITY ONLY -- writes nothing simulator reads. Main build keeps step 6
# (p0 part-time share on 10/40 grid x 50 weeks) for every care type. Why not
# in build:
#   - simulator holds every child at 10 or 40 hrs/week for WEEKS_PER_YEAR
#     (50); demand file's annual hours = children-equivalents on that grid. A
#     36-week school-year target reads there as ~28% fewer children, against
#     step 4's own child count. Other five types, NSECE calibration all
#     measure usual weekly hours x 50 -- no type carries a summer gap, so
#     cutting weeks for programs alone breaks like-for-like.
#   - data thin: Head Start schedule mix national only (PIR snapshot, no
#     state series); no enrolment-weighted pre-K schedule (NIEER prints
#     program minimums, 13 of 62 programs 'determined locally'); NHES EHS
#     cell 74 records.
#
# PER PROGRAM p (children = build's step 4 net counts: centers, ages 0-4, HS
# children pre-K count already holds removed -- each child once):
#   hours_p = children_p x weekly_p x weeks_p
# Three versions:
#   grid_50    build's step 6: children x grid_annual_hours(p0 UCB pt share)
#   usual_50   weekly = NHES usual hrs/week at the program x 50 weeks --
#              simulator's frame, program's own weekly hours
#   usual_ow   NHES weekly x program's operating weeks
#   sched_ow   scheduled weekly (PIR option mix; pre-K: NHES, no schedule
#              source) x operating weeks
#
# WEEKLY HOURS.
#   usual    NHES-ECPP 2019 (raw/US/), CPHRS = hrs/week child usually attends
#            center program, weight FEWT; center program now (CPNNOWX = 1).
#            Pre-K CPPLACEX = 2, ages 3-4; HS CPHEADST = 1, ages 3-4; EHS
#            CPHEADST = 1, ages 0-2. Same program flags as AGE5_NHES_NATIONAL
#            (00_). Parent report of usual hours -- closer to attendance than
#            schedule, NOT an attendance rate.
#   sched    Head Start, EHS: PIR 2018-19 center-based funded slots by option
#            (constants below), hours/day x days/week per option. Pre-K: no
#            enrolment-weighted public schedule -> NHES usual hours.
# No public attendance rate (days present / days enrolled) for any program
# at 2018-19 vintage -- scheduled hours stay scheduled.
#
# OPERATING WEEKS.
#   pre-K    36 = 180-day school year / 5. NIEER 2019 yearbook p. 14 (PDF):
#            62 programs -- 44 school/academic year, 1 full calendar year, 17
#            determined locally; Appendix A p. 232 (PDF): typical programs 175
#            to 180 days, ~1,000-1,170 funded hrs/year for school-day.
#   HS, EHS  by PIR option: full calendar year slots 50 (WEEKS_PER_YEAR);
#            part-day 32 (federal Head Start minimum, 3.5 hrs/day 4 days/week
#            32 weeks -- NIEER 2019 yearbook p. 337 (PDF), Minnesota note);
#            other full-day 36. PIR reports full-calendar-year slots only
#            inside full-working-day line -- weeks a lower bound.
#
# OVERLAP. Step 4 already nets HS children pre-K count holds (one share,
# pooled_overlap_share()); their hours enter once, at pre-K's weekly hours.
# EHS (0-2), HS (3-4) do not share children. Wraparound care -- extra hours
# in a paid setting around a program day -- not in CPHRS (one arrangement),
# not in PIR slots: it sits w/ Center-Based or home types, never here.
# Summer weeks of school-year programs not reassigned to another type.
#
# Usage: Rscript state_data/code/14_program_hours_sensitivity.R
#   reads raw/US/US_NHES__ecpp_public_use_file__2019.csv
#   writes out/program_hours_sensitivity.csv
#------------------------------------------------------------------------------

# build_of(), raw_path(), GEOGRAPHIES, AIAN_EHS_FRAC, grid_annual_hours();
# main() guarded
source('state_data/code/00_build_demand_targets.R')

OUT_FILE  <- file.path(STATE_DATA_ROOT, 'out', 'program_hours_sensitivity.csv')
NHES_FILE <- 'US_NHES__ecpp_public_use_file__2019.csv'
PROGRAMS  <- c('public pre-K', 'Head Start', 'Early Head Start')

# PIR 2018-19 CENTER-BASED FUNDED SLOTS BY OPTION -- OHS Services Snapshots,
# p. 1 'Detail - Center-based Funded Enrollment' (raw/US/
# US_OHS__services_snapshot_{hs,ehs,aian}__2018-19.pdf). Columns: part-day 4
# days, full-day 4 days (>6 hrs), part-day 5 days, full-day 5 days (>6 hrs);
# fwd = of full-day 5 days, full working day (10+ hrs); fcy = of fwd, full
# calendar year. AIAN pools HS, EHS -- split by AIAN_EHS_FRAC (00_), as step 4.
PIR_SLOTS <- list(
  hs   = c(pd4 = 102484, fd4 = 46458, pd5 = 117263, fd5 = 385147,
           fwd = 101704, fcy = 55943),
  ehs  = c(pd4 = 36, fd4 = 2239, pd5 = 3074, fd5 = 89275,
           fwd = 42178, fcy = 35225),
  aian = c(pd4 = 2650, fd4 = 4117, pd5 = 3944, fd5 = 10253,
           fwd = 3311, fcy = 2290))

# Hours/day per option. Part-day 3.5 = federal minimum (NIEER 2019 p. 337);
# full-day '>6 hours' (PIR) taken at 6.5 (NIEER extended-day threshold, p.
# 14); full working day 10 (PIR '>= 10 hours').
HRS_PART_DAY <- 3.5
HRS_FULL_DAY <- 6.5
HRS_FULL_WORKING_DAY <- 10

# Operating weeks (header, OPERATING WEEKS)
WEEKS_SCHOOL_YEAR <- 36
WEEKS_HS_PART_DAY <- 32



pir_schedule <- function(s) {

  #----------------------------------------------------------------------------
  # Slot-weighted scheduled weekly hours, operating weeks, one PIR program.
  # Options: pd4, pd5 part-day at 32 weeks; fd4, fd5 less fwd full-day at 36;
  # fwd less fcy full working day at 36; fcy full working day at 50.
  #
  # Params:
  #   - s (named num): slots by option, as PIR_SLOTS
  #
  # Returns: (named num) weekly (hrs/week), weeks, annual (weekly x weeks per
  #   slot, averaged -- not weekly x weeks of averages), fcy_share
  #----------------------------------------------------------------------------

  n <- c(s[['pd4']], s[['pd5']], s[['fd4']], s[['fd5']] - s[['fwd']],
         s[['fwd']] - s[['fcy']], s[['fcy']])
  weekly <- c(HRS_PART_DAY * 4, HRS_PART_DAY * 5, HRS_FULL_DAY * 4,
              HRS_FULL_DAY * 5, HRS_FULL_WORKING_DAY * 5, HRS_FULL_WORKING_DAY * 5)
  weeks  <- c(WEEKS_HS_PART_DAY, WEEKS_HS_PART_DAY, WEEKS_SCHOOL_YEAR,
              WEEKS_SCHOOL_YEAR, WEEKS_SCHOOL_YEAR, WEEKS_PER_YEAR)
  w <- n / sum(n)
  c(weekly = sum(w * weekly), weeks = sum(w * weeks),
    annual = sum(w * weekly * weeks), fcy_share = s[['fcy']] / sum(n))
}



program_schedules <- function() {

  #----------------------------------------------------------------------------
  # PIR schedules for HS, EHS w/ AIAN slots split by age as step 4.
  #
  # Returns: (list) 'Head Start', 'Early Head Start' -> pir_schedule() result
  #----------------------------------------------------------------------------

  a <- PIR_SLOTS$aian
  list('Head Start'       = pir_schedule(PIR_SLOTS$hs + a * (1 - AIAN_EHS_FRAC)),
       'Early Head Start' = pir_schedule(PIR_SLOTS$ehs + a * AIAN_EHS_FRAC))
}



nhes_weekly_hours <- function() {

  #----------------------------------------------------------------------------
  # NHES-ECPP 2019 usual hrs/week by program (header, WEEKLY HOURS), w/
  # replicate-weight SE (FEWT1-80, jackknife factor 79/80).
  #
  # Returns: (tibble) program, n (records), weekly, se
  #----------------------------------------------------------------------------

  nh <- read_chr(raw_path(NHES_FILE))
  v  <- function(x) num(nh[[x]])
  base <- v('CPNNOWX') == 1 & v('CPHRS') > 0
  sel <- list('public pre-K'     = base & v('CPPLACEX') == 2 & v('AGE2018') %in% 3:4,
              'Head Start'       = base & v('CPHEADST') == 1 & v('AGE2018') %in% 3:4,
              'Early Head Start' = base & v('CPHEADST') == 1 & v('AGE2018') <= 2)
  reps <- grep('^FEWT[0-9]+$', names(nh), value = TRUE)
  if (length(reps) != 80) stop(NHES_FILE, ': expected 80 replicate weights, found ', length(reps))
  hrs <- v('CPHRS')

  dplyr::bind_rows(lapply(names(sel), function(p) {
    k   <- which(sel[[p]])
    est <- weighted.mean(hrs[k], v('FEWT')[k])
    r   <- vapply(reps, function(w) weighted.mean(hrs[k], v(w)[k]), numeric(1))
    tibble::tibble(program = p, n = length(k), weekly = est,
                   se = sqrt(sum((r - est)^2) * 79 / 80))
  }))
}



geo_rows <- function(g, usual, sched) {

  #----------------------------------------------------------------------------
  # One geography: build's step 4 net children, four hour versions, by program.
  #
  # Params:
  #   - g (chr); usual (named num): NHES weekly by program
  #   - sched (list): program_schedules()
  #
  # Returns: (tibble) geo, program, children, hours by version; plus row
  #   'Unpaid Center-Based' w/ build's annual hours, totals
  #----------------------------------------------------------------------------

  res <- build_of(g)
  kids <- res$programs$net[PROGRAMS]
  pt <- res$shares$part_time[['Unpaid Center-Based']]
  ucb <- res$care$annual_hours[res$care$ecec_type == 'Unpaid Center-Based']

  wk <- c('public pre-K' = WEEKS_SCHOOL_YEAR,
          'Head Start' = sched[['Head Start']][['weeks']],
          'Early Head Start' = sched[['Early Head Start']][['weeks']])
  # pre-K: no schedule source -> NHES usual weekly x school year
  sched_annual <- c('public pre-K' = usual[['public pre-K']] * WEEKS_SCHOOL_YEAR,
                    'Head Start' = sched[['Head Start']][['annual']],
                    'Early Head Start' = sched[['Early Head Start']][['annual']])

  d <- tibble::tibble(geo = g, program = PROGRAMS, children = unname(kids),
                      grid_50  = unname(kids) * grid_annual_hours(pt),
                      usual_50 = unname(kids * usual[PROGRAMS] * WEEKS_PER_YEAR),
                      usual_ow = unname(kids * usual[PROGRAMS] * wk[PROGRAMS]),
                      sched_ow = unname(kids * sched_annual[PROGRAMS]))
  tot <- d %>%
    dplyr::summarise(dplyr::across(children:sched_ow, sum)) %>%
    dplyr::mutate(geo = g, program = 'Unpaid Center-Based')
  if (abs(tot$grid_50 - ucb) > 1e-6 * ucb) {
    stop(g, ': grid_50 ', tot$grid_50, ' != build Unpaid Center-Based hours ', ucb)
  }
  dplyr::bind_rows(d, tot)
}



main <- function() {

  #----------------------------------------------------------------------------
  # Every geography: per-program hours, four versions; writes OUT_FILE,
  # prints inputs, national, range of change.
  #
  # Returns: (tibble) OUT_FILE rows, invisibly
  #----------------------------------------------------------------------------

  nh <- nhes_weekly_hours()
  sched <- program_schedules()
  cat('NHES-ECPP 2019 usual hrs/week at program (FEWT; replicate SE):\n')
  for (i in seq_len(nrow(nh))) {
    cat(sprintf('  %-17s %5.2f (SE %.2f, %d records)\n', nh$program[i],
                nh$weekly[i], nh$se[i], nh$n[i]))
  }
  cat('PIR 2018-19 scheduled (center-based option mix):\n')
  for (p in names(sched)) {
    s <- sched[[p]]
    cat(sprintf('  %-17s %5.2f hrs/week, %4.1f weeks, %5.0f hrs/year, full calendar year %.1f%%\n',
                p, s[['weekly']], s[['weeks']], s[['annual']], 100 * s[['fcy_share']]))
  }

  usual <- setNames(nh$weekly, nh$program)
  d <- dplyr::bind_rows(lapply(GEOGRAPHIES, geo_rows, usual = usual, sched = sched))
  readr::write_csv(d, OUT_FILE)
  cat('wrote', OUT_FILE, '\n')

  # National = 50 states + DC; NYC inside NY
  ucb <- dplyr::filter(d, program == 'Unpaid Center-Based')
  nat <- ucb %>% dplyr::filter(geo %in% names(STATE_POSTAL_TO_FIPS)) %>%
    dplyr::summarise(dplyr::across(children:sched_ow, sum))
  cat(sprintf('\nUnpaid Center-Based hours, national (50 states + DC): build %.0f M, hrs/child %.0f\n',
              nat$grid_50 / 1e6, nat$grid_50 / nat$children))
  for (v in c('usual_50', 'usual_ow', 'sched_ow')) {
    ch <- ucb[[v]] / ucb$grid_50 - 1
    cat(sprintf('  %-9s %8.0f M (%+.1f%%), hrs/child %4.0f; geographies %+.1f%% to %+.1f%% (%s, %s)\n',
                v, nat[[v]] / 1e6, 100 * (nat[[v]] / nat$grid_50 - 1), nat[[v]] / nat$children,
                100 * min(ch), 100 * max(ch), ucb$geo[which.min(ch)], ucb$geo[which.max(ch)]))
  }
  invisible(d)
}


if (sys.nframe() == 0) main()
