## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ---- eval=FALSE---------------------------------------------------------
#  install.packages("finalfit")

## ------------------------------------------------------------------------
library(finalfit)
explanatory = c("age.factor", "extent.factor", "perfor.factor")
dependent = 'mort_5yr'

colon_s %>%
  finalfit_newdata(explanatory = explanatory, newdata = list(
    c("<40 years",  "Submucosa", "No"),
    c("<40 years", "Submucosa", "Yes"),
    c("<40 years", "Adjacent structures", "No"),
    c("<40 years", "Adjacent structures", "Yes") )) -> newdata
newdata

## ------------------------------------------------------------------------
colon_s %>% 
  glmmulti(dependent, explanatory) %>% 
  boot_predict(newdata, 
    estimate_name = "Predicted probability of death",
    R=100, boot_compare = FALSE,
    digits = c(2,3))

## ---- eval=FALSE---------------------------------------------------------
#  knitr::kable(table, row.names = FALSE, align = c("l", "l", "l", "r"))

## ------------------------------------------------------------------------
colon_s %>% 
  glmmulti(dependent, explanatory) %>% 
  boot_predict(newdata, 
    estimate_name = "Predicted probability of death",
    compare_name = "Absolute risk difference",
    R=100, digits = c(2,3))

## ---- eval=FALSE---------------------------------------------------------
#  library(finalfit)
#  library(ggplot2)
#  theme_set(theme_bw())
#  
#  explanatory = c("nodes", "extent.factor", "perfor.factor")
#  dependent = 'mort_5yr'
#  
#  colon_s %>%
#    finalfit_newdata(explanatory = explanatory, rowwise = FALSE,
#      newdata = list(
#        rep(seq(0, 30), 4),
#        c(rep("Muscle", 62), rep("Adjacent structures", 62)),
#        c(rep("No", 31), rep("Yes", 31), rep("No", 31), rep("Yes", 31))
#      )
#    ) -> newdata
#  
#  colon_s %>%
#    glmmulti(dependent, explanatory) %>%
#    boot_predict(newdata, boot_compare = FALSE,
#    R=100, condense=FALSE) %>%
#    ggplot(aes(x = nodes, y = estimate, ymin = estimate_conf.low,
#        ymax = estimate_conf.high, fill=extent.factor))+
#      geom_line(aes(colour = extent.factor))+
#      geom_ribbon(alpha=0.1)+
#      facet_grid(.~perfor.factor)+
#      xlab("Number of postive lymph nodes")+
#      ylab("Probability of death")+
#      labs(fill = "Extent of tumour", colour = "Extent of tumour")+
#      ggtitle("Probability of death by lymph node count")

