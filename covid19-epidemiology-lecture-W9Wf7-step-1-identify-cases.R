# NOTES ------------------------------------------------------------------------

# a quick attempt to use R to analyze the data presented in the video on this page:
# https://www.coursera.org/learn/covid19-epidemiology/lecture/W9Wf7/step-1-identify-cases
# Make a definition for suspected, probable, and confirmed cases for the following outbreak

# DEPENDENCIES -----------------------------------------------------------------

library(readr)
library(stringr)

# DATA -------------------------------------------------------------------------

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

# LOAD DATA --------------------------------------------------------------------
df <- read_delim(data_str, delim = '\t')

# CLEAN DATA -------------------------------------------------------------------

# clean onset data
onset_month <- str_split(df$Onset, ' ') %>% sapply(., '[', 1)
onset_day <- str_split(df$Onset, ' ') %>% sapply(., '[', 2)

# symptoms data is a bit messy let's do a bit of tidying
symptoms <- str_split(df$Symptoms, ',') %>% # split comma-separated string into individual tokens
    lapply(., str_replace_all, '\\.', '') %>%  # remove extraneous characters
    lapply(., str_trim) %>% # remove excess whitespace
    lapply(., str_replace_all, '\\s+', ' ') %>%  # remove excess whitespace
    lapply(., tolower) %>% lapply(., sort) # sort tokens alphabetically

# what are all possible symptoms experienced by subjects in our data set?
(unlist(symptoms) %>% unique %>% sort)

# let's put our symptom data into a tidier format, more amenable to analysis
has_discolored_swollen_lymph_node <- str_detect(symptoms, 'discolored/swollen lymph node')
has_fatigue <- str_detect(symptoms, 'fatigue')
has_fever <- str_detect(symptoms, 'fever')
has_headache <- str_detect(symptoms, 'headache')
has_muscle_aches <- str_detect(symptoms, 'muscle aches')

# probably productive to start out by looking at symptoms associated with confirmed
# cases. let's review what we learned the lecture with respect to classification
# of cases:
#     1. suspected case: a person for whom there is a suspicion of the disease but no strong evidence
#     or laboratory confirmation
#     2. probable case: a suspected case for whom there is strong circumstantial evidence of infection
#     for example, exposure to a known case
#     3. confirmed case: a person for whom there is definitive clinical or laboratory confirmation
#     that he or she is a case
# let's identify who in our data set is a confirmed case
lab_result <- df$`Lab Result`
confirmed_case <- str_detect(lab_result, 'Positive')
idx <- str_detect(lab_result, 'Missing')
confirmed_case[ idx ] <- NA

# extract case IDs
id <- df$ID

# now combine our cleaned data into a data.frame (our line list)
line_list <- data.frame(has_discolored_swollen_lymph_node,
                        has_fatigue,
                        has_fever,
                        has_headache,
                        has_muscle_aches,
                        onset_month,
                        onset_day,
                        id,
                        confirmed_case,
                        case_type = NA,
                        stringsAsFactors = FALSE)
print(line_list)

# ANALYZE DATA -----------------------------------------------------------------

# analyze symptoms of confirmed cases
idx <- which(line_list$confirmed_case) # these are our confirmed cases

# which symptom is present in all confirmed cases?
(apply(line_list[ idx, 1:5 ], 2, all))
# fever is present in all confirmed cases

# which symptoms are present in some or all confirmed cases?
(apply(line_list[ idx, 1:5 ], 2, any))
# discolored / swollen lymph nodes, fever, headache, muscle aches are present some or all confirmed cases
# fatigue is not present in any of the confirmed cases

# what is the percentage of cases where we find these symptoms?
(apply(line_list[ idx, 1:5 ], 2, which) %>%
        lapply(., length) %>%
        lapply(., '/', sum(confirmed_case, na.rm = TRUE))
)
# we find fever in 100% of confirmed cases
# we find discolored / swollen lymph nodes in ~ 67% of confirmed cases
# we find muscle ache in ~ 67% of confirmed cases
# we find headache in ~ 33% of confirmed cases

# how many people per day experienced onset of symptoms?
hist(as.integer(line_list$onset_day), breaks = c(15:19), main = 'onset of symptoms', xlab = 'day')

# fatigue and onset date of the symptoms don't really appear to have any predictive value (???)
print(line_list[ idx, c('has_fatigue', 'onset_month', 'onset_day', 'confirmed_case') ])
# all confirmed cases experienced onset between Mar 15 - 17, but they are only classified
# as confirmed since they have positive laboratory tests, which would make sense
# if they were amongst the first to fall ill?

# CONCLUSIONS ------------------------------------------------------------------

# based on the results of our simple analysis we create the following criteria
# to define what type of case a person may have:
#     1. suspected cases: anyone with any of the following symptoms:
#     discolored/swollen lymph node, fever, headache, muscle aches
#     2. probable cases: anyone with a fever and any one of the following symptoms:
#     discolored / swollen lymph nodes, muscle aches, headache
#     3. confirmed cases: anyone with positive lab results
# there is some debate about the difference between a suspected and probable case
# the instructors weigh in on this issue in this thread:
# https://www.coursera.org/learn/covid19-epidemiology/discussions/weeks/1/threads/1AFymgQWRLuBcpoEFhS7Pw

# extract suspected cases
idx <- which(line_list$has_discolored_swollen_lymph_node |
                 line_list$has_fever |
                 line_list$has_headache |
                 line_list$has_muscle_aches)
print(line_list[ idx, ]) # here are all of our suspected cases
line_list[ idx, 'case_type' ] <- 'SUSPECTED'

# extract probable cases
idx <- which(line_list$has_fever &
                 (line_list$has_discolored_swollen_lymph_node |
                      line_list$has_headache |
                      line_list$has_muscle_aches))
print(line_list[ idx, ]) # here are all of our probable cases
line_list[ idx, 'case_type' ] <- 'PROBABLE'

# extract confirmed cases
idx <- which(line_list$confirmed_case)
print(line_list[ idx, ]) # here are all of our confirmed cases
line_list[ idx, 'case_type' ] <- 'CONFIRMED'

print(line_list)
# on this data set our criteria appear to work, we progressively narrow down our
# range of cases from suspected to probable to confirmed and (more importantly)
# we don't miss any confirmed cases!
