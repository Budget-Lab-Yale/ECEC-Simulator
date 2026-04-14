library(Haver)
library(fredr)
library(tibble)
library(dplyr)
library(lubridate)

setwd("C:/Users/hre2/OneDrive - Yale University/Desktop")
haver.direct("on")

map_df <- read.csv("haver_emp_codes.csv", stringsAsFactors = FALSE) %>%
  select(code, alias)

# Pull Haver data
hist <- haver.data(codes = map_df$code, start='1994-01-01') %>% as.data.frame

# Convert replace row names with codes
idx <- match(names(hist),  tolower(gsub("@EMPL$", "", map_df$code)))
names(hist) <- ifelse(is.na(idx), names(hist), map_df$alias[idx])

# Add year (date column)
hist$year<- 1994:2025
hist <- hist %>% relocate(year, .before = 1)

# Get CBO projections for lfpr
cbo_proj <- read.csv("lfpr_proj_cbo_ltbo2026.csv", stringsAsFactors = FALSE)

# Append projections to historical
proj <- bind_rows(hist,cbo_proj)

#Next, for each age/sex category find the ratio of EPOP to LFPR for 2015-2019 (this is 1-UPOP), and apply it to
#projected LFPR to project EPOP forward.
epop_lfpr_ratios <- hist %>% filter(year>=2015 & year<=2019) %>%
                              mutate(across(starts_with("epop_"), 
                                ~ . / get(sub("epop_", "lfpr_", cur_column())),
                                    .names = "{sub('epop_', 'ratio_', .col)}")) %>%
                                      select(starts_with("ratio")) %>%
                                        summarise(across(everything(), mean, na.rm = TRUE))
proj <- cross_join(proj, epop_lfpr_ratios)
epop_cols <- grep("^epop_", names(proj), value = TRUE)
for (col in epop_cols) {
  suffix <- sub("^epop_", "", col)
  lfpr_col <- paste0("lfpr_", suffix)
  ratio_col <- paste0("ratio_", suffix)
  if (lfpr_col %in% names(proj) & ratio_col %in% names(proj)) {
    proj[[col]] <- ifelse(is.na(proj[[col]]), 
                          proj[[lfpr_col]] * proj[[ratio_col]], 
                          proj[[col]])
  }
}

#Write EPOPs to CSV
epop_out<- proj%>% select(year, starts_with('epop'))
write.csv(epop_out, file = 'epop_projections.csv', row.names = FALSE, na='')