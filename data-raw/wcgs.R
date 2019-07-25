library(epitools)
library(dplyr)
library(forcats)
library(finalfit)

data(wcgs)
save(wcgs, file = "data-raw/wcgs.rda")

wcgs = wcgs %>% 
	rename(
		age = age0,
		height = height0,
		weight = weight0, 
		sbp = sbp0,
		dbp = dbp0,
		chol = chol0,
		behpat = behpat0,
		dibpat = dibpat0,
		ncigs = ncigs0,
		chd = chd69,
		timechd = time169,
		arcus = arcus0
	) %>% 
	mutate(
		smoking = if_else(ncigs == 0, "Non-smoker", "Smoker") %>% 
			factor(),
		behpat = factor(behpat) %>% 
			fct_recode(
				"A1" = "1",
				"A2" = "2", 
				"B3" = "3", 
				"B4" = "4"),
		dibpat = factor(dibpat) %>% 
			fct_recode(
				"A" = "0",
				"B" = "1"
			),
		chd = factor(chd) %>% 
			fct_recode(
				"No" = "0",
				"Yes" = "1"),
		typechd = factor(typechd) %>% 
			fct_recode(
				"No" = "0",
				"MI_SD" = "1",
				"Silent_MI" = "2",
				"Angina" = "3"
			),
		arcus = factor(arcus) %>% 
			fct_recode(
				"No" = "0",
				"Yes" = "1"), 
	) %>%
	# Labels
	mutate(
		id = ff_label(id, "Subject ID"),
		age = ff_label(age, "Age (years)"),
		height = ff_label(height, "Height (inches)"),
		weight = ff_label(weight, "Weight (pounds)"),
		sbp = ff_label(sbp, "Systolic BP (mmHg)"),
		dbp = ff_label(dbp, "Diastolic BP (mmHg)"),
		chol = ff_label(chol, "Cholesterol (mg/100 ml)"),
		behpat = ff_label(behpat, "Behaviour pattern"),
		dibpat = ff_label(dibpat, "Behaviour pattern"),
		ncigs = ff_label(ncigs, "Cigarettes/day"),
		smoking = ff_label(smoking, "Smoking"),
		arcus = ff_label(arcus, "Corneal arcus"),
		chd = ff_label(chd, "CHD event"),
		typechd = ff_label(typechd, "Type CHD"),
		timechd = ff_label(timechd, "Time to CHD event")
	) %>% 
	select(
		id, age, 
		height, weight, 
		sbp, dbp, 
		chol, 
		behpat, dibpat, 
		ncigs, smoking,
		arcus,
		chd, typechd, timechd)

save(wcgs, file = "data/wcgs.rda", compress = TRUE)
