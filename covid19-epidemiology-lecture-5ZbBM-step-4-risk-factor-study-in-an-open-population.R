# NOTES ------------------------------------------------------------------------

# https://www.coursera.org/learn/covid19-epidemiology/lecture/5ZbBM/step-4-risk-factor-study-in-an-open-population
# 48 students visited animals on a zoo field trip
# 10 students got sick with the plague
# Some students visited one or more animals for an "up close" visit that involved touching (see table)
# Calculate the relative risk for being a case for each animal visited compared to those visiting no animals
# Based on this calculation, what is the most likely exposure?

# DEPENDENCIES -----------------------------------------------------------------

library(readr)

# DATA -------------------------------------------------------------------------

data_str <- 'Exhibit visited	Cases	Non-cases	Total
Lion	2	19	21
Lemur	1	18	19
Prairie dog	8	25	33
Capybara	7	4	11
None	2	10	12'

# LOAD DATA --------------------------------------------------------------------

df <- read_delim(data_str, delim = '\t')

# PERFORM CALCULATIONS ---------------------------------------------------------

# determine the attack rate in cases that visited animal exhibits
attack_rate_exhibit <- df$Cases / df$Total
attack_rate_exhibit <- round(attack_rate_exhibit, 2)

# determine the reference attack rate in cases that did not visit any animal exhibits
idx <- which(df$`Exhibit visited` == 'None')
attack_rate_none <- df[ idx, 'Cases' ] / df[ idx, 'Total' ]
attack_rate_none <- as.numeric(attack_rate_none)
attack_rate_none <- round(attack_rate_none, 2)

# calculate the relative risk
relative_risk <- attack_rate_exhibit / attack_rate_none
relative_risk <- round(relative_risk, 2)
relative_risk[ idx ] <- NA # relative risk not meaningful for our reference attack rate

# SOLUTION ---------------------------------------------------------------------

# which exhibit has the highest relative risk?
idx <- which.max(relative_risk)
(as.character(df[ idx, 'Exhibit visited' ]))
# Visiting Capybara is associated with the highest relative risk

# which exhibits have relative risk > 1?
idx <- which(relative_risk > 1)
((df[ idx, 'Exhibit visited' ]))
# Visiting Capybara and Prairie Dog exhibits are both associated with relative risk > 1
