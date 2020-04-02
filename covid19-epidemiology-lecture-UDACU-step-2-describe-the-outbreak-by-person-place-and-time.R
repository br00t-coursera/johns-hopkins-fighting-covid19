# NOTES ------------------------------------------------------------------------

# https://www.coursera.org/learn/covid19-epidemiology/lecture/UDACU/step-2-describe-the-outbreak-by-person-place-and-time
# Look at this line list of cases and draw an epidemic curve for it. If you want,
# you can use the case definition you previously developed to further characterize
# the course of cases over time.

# DEPENDENCIES -----------------------------------------------------------------

library(ggplot2)

# LOAD DATA --------------------------------------------------------------------

# we're lazy, so let's reuse our data from step 1! the "line_list" variable will
# contain everything we need to make a nice colorful epidemic curve
source('covid19-epidemiology-lecture-W9Wf7-step-1-identify-cases.R')

# PLOT EPIDEMIC CURVE ----------------------------------------------------------

g <- ggplot(line_list, aes(onset_day))
g <- g + geom_bar(aes(fill = case_type))
g <- g + ggtitle('epidemic curve')
g <- g + xlab('onset date')
print(g)
