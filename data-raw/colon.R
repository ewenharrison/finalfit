# Alterations to survival::colon dataset for summarizer vignettes
# Load packages
library(Hmisc)
library(tidyverse)
library(forcats)
library(magrittr)
library(summarizer)

# Load dataset
rm(list=ls())
mydata = survival::colon

# Filter dataset
mydata %>%
	dplyr::filter(etype=="2") %>% # etype:	event type: 1=recurrence,2=death # only death
	dplyr::filter(rx!="Obs")  ->  # rx:	treatment: "Obs"= observation
	mydata

mydata$rx %<>%
	fct_drop() # This is because we did filter(rx!="Obs"): removes the "Obs" level (even though there are now rows with this value left)
mydata %<>%
	select(-study, -etype)

# Transform variables
mydata$sex %>%
	factor() %>%
	fct_recode('Female' = '0',
						 'Male'   = '1') ->
	mydata$sex.factor

mydata$age %>%# in years
	cut(breaks = c(0,40,60,95), # define the cutoffs between categories
			#labels = c("<40 years", "40-60 years", ">60 years")
			#using the labels option is slightly dangerous  as break and labels can get out of sync
			include.lowest = TRUE) %>%
	fct_recode("<40 years"   = "[0,40]", #this is safer as if e.g. change 40 to 30 (so becomes "[0,30]"), so it won't blindly over-write
						 "40-60 years" = "(40,60]") ->
	mydata$age.factor

mydata$obstruct %>% # obstruction of colon by tumour
	factor() %>%
	fct_recode('No'  = '0',
						 'Yes' = '1') ->
	mydata$obstruct.factor

mydata$perfor %>% # perforation of colon
	factor() %>%
	fct_recode('No'  = '0',
						 'Yes' = '1') ->
	mydata$perfor.factor

mydata$adhere %>% # adherence to nearby organs by tumour
	factor() %>%
	fct_recode('No'  = '0',
						 'Yes' = '1') ->
	mydata$adhere.factor

mydata$differ %>% # differentiation of tumour
	factor() %>%
	fct_recode('Well'     = '1',
						 'Moderate' = '2',
						 'Poor'     = '3') ->
	mydata$differ.factor

mydata$extent %>% # Extent of local spread
	factor() %>%
	fct_recode('Submucosa'           = '1',
						 'Muscle'              = '2',
						 'Serosa'              = '3',
						 'Adjacent structures' = '4') ->
	mydata$extent.factor

mydata$surg %>% #	time from surgery to registration
	factor() %>%
	fct_recode('Short' = '0',
						 'Long'  = '1') ->
	mydata$surg.factor

mydata$node4 %>% # more than 4 positive lymph nodes
	factor() %>%
	fct_recode('No'  = '0',
						 'Yes' = '1') ->
	mydata$node4.factor

# new variable
mydata$time %>% # was the patient dead at 3 years follow-up
	cut(time,
			breaks = c(0,365*3,100000), right = F,
			labels=c("Died","Alive"),
			include.lowest=TRUE) ->
	mydata$mort_3y.factor

mydata %>%
	count(mort_3y.factor, status)

mydata %>% # was there any local complications experienced
	mutate(loccomp = ifelse(perfor==1|obstruct==1|adhere==1, 1, 0)) -> mydata

mydata$loccomp %>%
	factor() %>%
	fct_recode('No'  = '0',
						 'Yes' = '1') ->
	mydata$loccomp.factor

mydata
str(mydata)
survival::colon

# Label up using Hmisc::label()
label(mydata$rx) = "Treatment"
label(mydata$sex) = "Sex"
label(mydata$age) = "Age (years)"
label(mydata$obstruct) = "Obstruction of colon"
label(mydata$perfor) = "Perforation of colon"
label(mydata$adhere) = "Adherence to organs"
label(mydata$status) = "Status"
label(mydata$differ) = "Differentiation"
label(mydata$extent) = "Local spread"
label(mydata$surg) = "Time of surgery"
label(mydata$node4) = "More than 4 positive nodes"
label(mydata$time) = "Time to event"


label(mydata$sex.factor) = "Sex"
label(mydata$age.factor) = "Age"
label(mydata$obstruct.factor) = "Obstruction of colon"
label(mydata$perfor.factor) = "Perforation of colon"
label(mydata$adhere.factor) = "Adherence to organs"
label(mydata$differ.factor) = "Differentiation"
label(mydata$extent.factor) = "Local spread"
label(mydata$surg.factor) = "Time of surgery"
label(mydata$mort_3y.factor) = "3-year mortality"
label(mydata$loccomp) = "Local complication"
label(mydata$loccomp.factor) = "Local complication"

