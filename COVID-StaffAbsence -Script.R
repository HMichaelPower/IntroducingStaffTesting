## %######################################################%##
#                                                          #
####           Modeling the introduction of             ####
####       testing staff for SARS-CoV-2 infection       ####
#                                                          #
## %######################################################%##

###### COMMENTS  #####
######
###### naming is hard - but consistent style is possible
###### I find camelCase easiest to type and easy to read
###### see http://adv-r.had.co.nz/Style.html (who prefers snakes)
######
###### use styler to make indentations etc consistent

styler:::style_active_file() # tidy the layout

source("utilityFunctions.R")
source("modelFunctions.R")
loadPackages(
  list(
    "readxl",
    "cowplot",
    "patchwork",
    "ggbeeswarm", # alternative plot to violin
    "wesanderson", # palettes for plots
    "paletteer",
    "gt"
  )
)

## %######################################################%##
#                                                          #
####                    Parameters                      ####
#                                                          #
## %######################################################%##

######
###### This section must map to Table 1 in the Manuscript, and vice versa
######

# setting and epidemiology
workWeek <- 53.6 # hours
timeToResult <- tibble(ll = 5.4, v = 10.7, ul = 16.1)

incidence <- tibble(ll = 0.00001, v = 0.000471, ul = 0.001) ##### why do we need this?
prev <- tibble(ll = 0.001, v = 0.013502, ul = 0.06)
prevSymptomsCov <- tibble(ll = 0.05, v = 0.14, ul = 0.2) # proportion of infected people who have symptoms
prevSymptomsNonCov <- tibble(ll = 0.01, v = 0.05, ul = 0.1) # proportion of the population who have symptoms not due to covid
cohortAll <- tibble(ll = 11346, v = 11346, ul = 11346) # staff complement
contactAverage <- tibble(ll = 0.6, v = 4, ul = 8) # number of other staff a staff member is in close contact with
transmission <- tibble(ll = 0.005, v = 0.05, ul = 0.2) # probability of an infected staff member infecting a colleague

# test accuracies

SnNoTest <- tibble(test = "NoTest", measure = "sensitivity", ll = 0.0, v = 0.0, ul = 0.0)
SpNoTest <- tibble(test = "NoTest", measure = "specificity", ll = 1.0, v = 1.0, ul = 1.0)

SnPerfectTest <- tibble(test = "PerfectTest", measure = "sensitivity", ll = 1.0, v = 1.0, ul = 1.0)
SpPerfectTest <- tibble(test = "PerfectTest", measure = "specificity", ll = 1.0, v = 1.0, ul = 1.0)

SnLFD <- tibble(test = "LFD", measure = "sensitivity", ll = 0.523, v = 0.575, ul = 0.626) # lateral flow device
SpLFD <- tibble(test = "LFD", measure = "specificity", ll = 0.994, v = 0.9961, ul = 0.9974)

SnLAMP <- tibble(test = "LAMP", measure = "sensitivity", ll = 0.73, v = 0.79, ul = 0.84) # loop-mediated isothermal amplification
SpLAMP <- tibble(test = "LAMP", measure = "specificity", ll = 0.99, v = 1.0, ul = 1.0)

SnPCR <- tibble(test = "PCR", measure = "sensitivity", ll = 0.8, v = 0.8, ul = 0.9)
SpPCR <- tibble(test = "PCR", measure = "specificity", ll = 0.99, v = 1.0, ul = 1.0)

testAccuracies <- bind_rows(
  SnNoTest, SpNoTest,
  SnPerfectTest, SpPerfectTest,
  SnLFD, SpLFD,
  SnLAMP, SpLAMP,
  SnPCR, SpPCR
)

correlation <- tibble(ll = 0.0, v = 0.0, ul = 0.5) # Conditional dependence between test results

strategies <- tibble(
  strategy = c("NoTest", "PerfectTest", "PCR", "LFD", "LAMP", "LFD-PCR", "LAMP-PCR")
)


## %######################################################%##
#                                                          #
####                 derived variables                  ####
#                                                          #
## %######################################################%##

cohort <- cohortAll -
  cohortAll * prev * prevSymptomsCov - # remove people with symptomatic COVID-19
  cohortAll * prevSymptomsNonCov # remove people with symptoms due to other diseases

prevCohort <- (cohortAll * prev * (1 - prevSymptomsCov)) / cohort



# accuracy measures for sequential testing
###### i don't understand these calculations???
###### LFD and LAMP are not used in the same scenario, so why is the correlation important?


TPSeq <- cohort$v * prev$v * SnLFD$v * ((correlation$v * (sqrt(SnLFD$v * (1 - SnLFD$v) * SnLAMP$v * (1 - SnLAMP$v))) + SnLFD$v * SnLAMP$v) / SnLFD$v)
FPTest1Seq <- (1 - prev$v) * (1 - SpLFD$v) * cohort$v
TNTest2Seq <- cohort$v * (1 - prev$v) * (1 - SpLFD$v) * (SpLAMP$v - (correlation$v * (sqrt(SpLFD$v * (1 - SpLFD$v) * SpLAMP$v * (1 - SpLAMP$v))) + SpLAMP$v * SpLFD$v)) / (1 - SpLFD$v)
FPTest2Seq <- cohort$v * (1 - prev$v) * (1 - SpLFD$v) - TNTest2Seq
FNTest1Seq <- cohort$v * (1 - prev$v) * (1 - SpLFD$v) - TNTest2Seq
FNTest2Seq <- (cohort$v * prev$v * SnLFD$v) - TPSeq
TNTest1Seq <- (1 - prev$v) * SpLFD$v * cohort$v


## %######################################################%##
#                                                          #
####   Model outcomes and 1-way sensitivity analyses    ####
#                                                          #
## %######################################################%##

###### vector calculations give the SA as a dataframe for each outcome

modelOutcomes <- NA
for (testName in unique(testAccuracies$test)) {
  modelOutcome <- testOutcomes(
    sensitivity = select(
      filter(testAccuracies, test == testName, measure == "sensitivity"),
      c("ll", "v", "ul")
    ),
    specificity = select(
      filter(testAccuracies, test == testName, measure == "specificity"),
      c("ll", "v", "ul")
    ),
    population = cohort,
    prevalence = prevCohort
  ) %>%
    mutate(test = testName) %>%
    relocate(test)
  modelOutcomes <- if (anyNA(modelOutcomes)) modelOutcome else bind_rows(modelOutcomes, modelOutcome)
}
print(modelOutcomes)


###### next step will be to build a table of all outcomes
