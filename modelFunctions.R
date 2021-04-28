##%######################################################%##
#                                                          #
####        Functions written for modelling the         ####
####     introduction of testing asymptomatic staff     ####
#                                                          #
##%######################################################%##

styler:::style_active_file() # tidy the layout

# calculate TP, FP, FN, TN, PPV, NPV given sensitivity, specificity, population, and prevalence
testOutcomes <- function(sensitivity, specificity, population, prevalence) {
  DP <- population * prevalence # number with the condition
  DN <- population - DP # number without the condition
  
  TP <- sensitivity * DP  
  FN <- DP - TP   
  
  TN <- specificity * DN
  FP <- DN - TN  
  
  PPV <- TP / (TP + FP)
  NPV <- TN / (TN + FN)
  
  return(
    tibble(
      TP = TP,
      FP = FP,
      FN = FN,
      TN = TN,
      PPV = PPV,
      NPV = NPV
      )
    )
  
  
}