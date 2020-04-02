# notes ------------------------------------------------------------------------

# a quick attempt to use R to analyze the data presented in the video on this page:
# https://www.coursera.org/learn/covid19-epidemiology/lecture/W9Wf7/step-1-identify-cases

# dependencies -----------------------------------------------------------------

library(readr)
library(stringr)

# data -------------------------------------------------------------------------

data_str <- 'ID	Onset	Symptoms	Lab Result
1	March 15	Fever, muscle aches, discolored/swollen lymph node	Positive
2	March 15	Fatigue	Negative
3	March 16	Fever, fatigue	Missing
4	March 16	Headache	Missing
5	March 16	Fever, muscle aches, discolored/swollen lymph node	Missing
6	March 16	Fever, headache	Positive
7	March 17	Fever, muscle aches, discolored/swollen lymph . node	Positive
8	March 17	Muscle aches	Missing
9	March 17	Fever	Missing
10	March 18	Fever	Missing'

# load data --------------------------------------------------------------------
df <- read_delim(data_str, delim = '\t')

# clean data -------------------------------------------------------------------

# clean onset data
onset_month <- str_split(df$Onset, ' ') %>% sapply(., '[', 1)
onset_day <- str_split(df$Onset, ' ') %>% sapply(., '[', 2)

# symptoms data is a bit messy let's do a bit of tidying
symptoms <- str_split(df$Symptoms, ',') %>% # split comma-separated string into individual tokens
    lapply(., str_replace_all, '\\.', '') %>%  # remove extraneous characters
    lapply(., str_trim) %>% # remove excess whitespace
    lapply(., str_replace_all, '\\s+', ' ') %>%  # remove excess whitespace
    lapply(., tolower) %>% lapply(., sort) # sort tokens alphabetically

# analyze data -----------------------------------------------------------------

# suspected case: a person for whom there is a suspicion of the disease but no strong evidence
# or laboratory confirmation
# probable case: a suspected case for whom there is strong circumstantial evidence of infection
# for example, exposure to a known case
# confirmed case: a person for whom there is definitive clinical or laboratory confirmation
# that he or she is a case
lab_result <- df$`Lab Result`
confirmed_case <- str_detect(lab_result, 'Positive')
idx <- str_detect(lab_result, 'Missing')
confirmed_case[ idx ] <- NA

# what are all possible symptoms one can experience?
(unlist(symptoms) %>% unique %>% sort)

# let's put our data into a tidier format
has_discolored_swollen_lymph_node <- str_detect(symptoms, 'discolored/swollen lymph node')
has_fatigue <- str_detect(symptoms, 'fatigue')
has_fever <- str_detect(symptoms, 'fever')
has_headache <- str_detect(symptoms, 'headache')
has_muscle_aches <- str_detect(symptoms, 'muscle aches')

# extract case IDs
id <- df$ID

# now combine our cleaned data into a data.frame
df2 <- data.frame(has_discolored_swollen_lymph_node,
                  has_fatigue,
                  has_fever,
                  has_headache,
                  has_muscle_aches,
                  onset_month,
                  onset_day,
                  id,
                  confirmed_case)
print(df2)

# analyze symptoms of confirmed cases
idx <- which(df2$confirmed_case) # these are our confirmed cases

(apply(df2[ idx, 1:5 ], 2, all)) # which symptom is present in all confirmed cases? fever

(apply(df2[ idx, 1:5 ], 2, any)) # which symptoms are present in any or all confirmed cases: discolored / swollen lymph nodes, fever, headache, muscle aches

(apply(df2[ idx, 1:5 ], 2, which) %>%
    lapply(., length) %>%
    lapply(., '/', sum(confirmed_case, na.rm = TRUE))
)
# we find fever in 100% of confirmed cases
# we find discolored / swollen lymph nodes in ~ 67% of confirmed cases
# we find muscle ache in ~ 67% of confirmed cases
# we find headache in ~ 33% of confirmed cases

# fatigue and onset date of the symptoms don't appear to have any predictive value (???)
print(df2[ idx, c('has_fatigue', 'onset_month', 'onset_day', 'confirmed_case') ])

# conclusions ------------------------------------------------------------------

# so we create the following definitions
# suspected cases: anyone with any of the following symptoms: discolored/swollen lymph node, fever, headache, muscle aches
# probable cases: anyone with a fever and any one of the following symptoms:  discolored / swollen lymph nodes, muscle aches, headache
# confirmed cases: anyone with positive lab results

# suspected cases
idx <- which(df2$has_discolored_swollen_lymph_node |
                 df2$has_fever |
                 df2$has_headache |
                 df2$has_muscle_aches)
print(df2[ idx, ]) # here are all of our suspected cases

# probable cases
idx <- which(df2$has_fever &
                 (df2$has_discolored_swollen_lymph_node |
                                  df2$has_headache |
                                  df2$has_muscle_aches))
print(df2[ idx, ]) # here are all of our probably cases

# confirmed cases: anyone with positive lab results
idx <- which(df2$confirmed_case)
print(df2[ idx, ]) # here are all of our confirmed cases

# on this data set our criteria appear to work, we progressively narrow down our
# range of cases from suspected to probable to confirmed and (more importantly)
# we don't miss any confirmed cases!
