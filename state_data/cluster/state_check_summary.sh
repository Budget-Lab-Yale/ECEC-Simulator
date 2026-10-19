#!/bin/bash
#------------------------------------------------------------------------------
# state_check_summary.sh -- start-of-simulation state checks, per geography,
# from a state run's log (3a prints them; a failed check stops the run).
# Columns: geo, check, value (max over parent unit types where >1 row).
#   ni_check_error  net income rebuild, max relative gap (state-price alpha only)
#   identity_error  rebuilt alpha vs calibration probabilities, max abs gap
#   frozen_gap      base-year sector hours vs re-anchored, max relative gap
#   delta_iters     iterations of the care-type delta solve
#
# Usage: bash state_data/cluster/state_check_summary.sh <run log> > out.csv
#------------------------------------------------------------------------------
awk '
  /^Building state context:/ { geo = $4 }
  /^ +(c1|c2plus) +[0-9]+ / && NF == 6 {
    if (!((geo, "ni") in ni) || $5 > ni[geo, "ni"]) ni[geo, "ni"] = $5
    if (!((geo, "id") in ni) || $6 > ni[geo, "id"]) ni[geo, "id"] = $6
  }
  /Frozen base year/ { fz[geo] = $NF }
  /State demand adjustment .*converged in/ { for (i = 1; i <= NF; i++) if ($i == "in") it[geo] = $(i + 1) }
  /^Building state context:/ { order[++n] = geo }
  END {
    print "geo,check,value"
    for (k = 1; k <= n; k++) { g = order[k]
      if ((g, "ni") in ni) print g ",ni_check_error," ni[g, "ni"]
      if ((g, "id") in ni) print g ",identity_error," ni[g, "id"]
      if (g in fz) print g ",frozen_gap," fz[g]
      if (g in it) print g ",delta_iters," it[g]
    }
  }' "$1"
