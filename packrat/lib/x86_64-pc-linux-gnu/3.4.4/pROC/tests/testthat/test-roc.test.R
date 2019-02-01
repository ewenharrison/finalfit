library(pROC)
data(aSAH)

context("roc.test")


test_that("roc.test works", {
	t1 <<- roc.test(r.wfns, r.s100b)
	t2 <<- roc.test(r.wfns, r.ndka)
	t3 <<- roc.test(r.ndka, r.s100b)
})

test_that("roc.test statistic and p are as expected with defaults", {
	expect_equal(t1$statistic, c(Z=2.20898359144091))
	expect_equal(t1$p.value, 0.0271757822291882)
	expect_match(t1$method, "DeLong")
	expect_match(t1$method, "correlated")
	expect_identical(t1$alternative, "two.sided")
	
	expect_equal(t2$statistic, c(Z=2.79777591868904))
	expect_equal(t2$p.value, 0.00514557970691098)
	expect_match(t2$method, "DeLong")
	expect_match(t2$method, "correlated")
	expect_identical(t2$alternative, "two.sided")
	
	expect_equal(t3$statistic, c(Z=-1.39077002573558))
	expect_equal(t3$p.value, 0.164295175223054)
	expect_match(t3$method, "DeLong")
	expect_match(t3$method, "correlated")
	expect_identical(t3$alternative, "two.sided")
})

test_that("two.sided roc.test produces identical p values when roc curves are reversed", {
	t1b <- roc.test(r.s100b, r.wfns)
	expect_equal(t1b$p.value, t1$p.value)
	expect_equal(t1b$statistic, -t1$statistic)
	
	t2b <- roc.test(r.ndka, r.wfns)
	expect_equal(t2b$p.value, t2$p.value)
	expect_equal(t2b$statistic, -t2$statistic)
	
	t3b <- roc.test(r.s100b, r.ndka)
	expect_equal(t3b$p.value, t3$p.value)
	expect_equal(t3b$statistic, -t3$statistic)
})

test_that("unpaired roc.test works", {
	# Warns about pairing
	expect_warning(t1up <<- roc.test(r.wfns, r.s100b, paired = FALSE))
	expect_warning(t2up <<- roc.test(r.wfns, r.ndka, paired = FALSE))
	expect_warning(t3up <<- roc.test(r.ndka, r.s100b, paired = FALSE))
})

test_that("unpaired roc.test statistic and p are as expected", {
	expect_equal(t1up$statistic, c(D=1.43490640926908))
	expect_equal(t1up$p.value, 0.152825378808796)
	expect_match(t1up$method, "DeLong")
	expect_identical(t1up$alternative, "two.sided")
	
	expect_equal(t2up$statistic, c(D=3.10125096778969))
	expect_equal(t2up$p.value, 0.00220950791756457)
	expect_match(t2up$method, "DeLong")
	expect_identical(t2up$alternative, "two.sided")
	
	expect_equal(t3up$statistic, c(D=-1.55995743389685))
	expect_equal(t3up$p.value, 0.120192832430845)
	expect_match(t3up$method, "DeLong")
	expect_identical(t3up$alternative, "two.sided")
})

test_that("unpaired two.sided roc.test produces identical p values when roc curves are reversed", {
	expect_warning(t1upb <- roc.test(r.s100b, r.wfns, paired = FALSE))
	expect_equal(t1upb$p.value, t1up$p.value)
	expect_equal(t1upb$statistic, -t1up$statistic)
	
	expect_warning(t2upb <- roc.test(r.ndka, r.wfns, paired = FALSE))
	expect_equal(t2upb$p.value, t2up$p.value)
	expect_equal(t2upb$statistic, -t2up$statistic)
	
	expect_warning(t3upb <- roc.test(r.s100b, r.ndka, paired = FALSE))
	expect_equal(t3upb$p.value, t3up$p.value)
	expect_equal(t3upb$statistic, -t3up$statistic)
})


test_that("one-sided roc.test work and produce expected results", {
	t1gt <- roc.test(r.wfns, r.s100b, alternative = "greater")
	t1lt <- roc.test(r.wfns, r.s100b, alternative = "less")
	
	expect_equal(t1gt$statistic, t1$statistic)
	expect_equal(t1lt$statistic, t1$statistic)
	
	expect_equal(t1gt$p.value, 0.0135878911145941)
	expect_equal(t1lt$p.value, 0.986412108885406)
	
	expect_match(t1gt$method, "DeLong")
	expect_match(t1gt$method, "correlated")
	expect_identical(t1gt$alternative, "greater")
	expect_match(t1lt$method, "DeLong")
	expect_match(t1lt$method, "correlated")
	expect_identical(t1lt$alternative, "less")
})

test_that("unpaired one-sided roc.test work and produce expected results", {
	expect_warning(t1upgt <- roc.test(r.wfns, r.s100b, alternative = "greater", paired = FALSE))
	expect_warning(t1uplt <- roc.test(r.wfns, r.s100b, alternative = "less", paired = FALSE))
	
	expect_equal(t1upgt$statistic, t1up$statistic)
	expect_equal(t1uplt$statistic, t1up$statistic)
	
	expect_equal(t1upgt$p.value, 0.076412689404398)
	expect_equal(t1uplt$p.value, 0.923587310595602)
	
	expect_match(t1upgt$method, "DeLong")
	expect_identical(t1upgt$alternative, "greater")
	expect_match(t1uplt$method, "DeLong")
	expect_identical(t1uplt$alternative, "less")
})

test_that("roc.formula works", {
	expect_silent(t1c <- roc.test(aSAH$outcome ~ aSAH$wfns + aSAH$s100b, quiet = TRUE)) # make sure silent is passed
	expect_equal(t1c$statistic, t1$statistic)
	expect_equal(t1c$p.value, t1$p.value)
	expect_match(t1$method, "DeLong")
	expect_match(t1$method, "correlated")
	expect_identical(t1$alternative, "two.sided")
				 
	expect_warning(t1upc <- roc.test(aSAH$outcome ~ aSAH$wfns + aSAH$s100b, quiet = TRUE, paired = FALSE))
	expect_equal(t1upc$statistic, t1up$statistic)
	expect_equal(t1upc$p.value, t1up$p.value)
	expect_match(t1upc$method, "DeLong")
	expect_identical(t1upc$alternative, "two.sided")
})


test_that("roc.formula supports subset and na.omit", {
	check.only.items <- c("p.value", "statistic")
	
	expect_identical(
		roc.test(outcome ~ wfns + ndka, data = aSAH, subset = (gender == "Female"), quiet = TRUE)[check.only.items],
		roc.test(aSAH$outcome[aSAH$gender == "Female"], aSAH$wfns[aSAH$gender == "Female"], aSAH$ndka[aSAH$gender == "Female"], quiet = TRUE)[check.only.items]
	)
	
	# Generate missing values
	aSAH.missing <- aSAH
	aSAH.missing$wfns[1:20] <- NA
	aSAH.missing$ndka[1:20] <- NA
	expect_identical(
		roc.test(outcome ~ wfns + ndka, data = aSAH.missing, na.action = na.omit, quiet = TRUE)[check.only.items],
		roc.test(aSAH$outcome[21:113], aSAH$wfns[21:113], aSAH$ndka[21:113], quiet = TRUE)[check.only.items]
	)
	#na.fail should fail
	expect_error(roc.test(outcome ~ wfns + ndka, data = aSAH.missing, na.action = na.fail, quiet = TRUE))
	#weights should fail too
	expect_error(roc.test(outcome ~ wfns + ndka, data = aSAH, weights = seq_len(nrow(aSAH))), regexp = "weights are not supported")
	
	# Both na.action and subset
	expect_identical(
		roc.test(outcome ~ wfns + ndka, data = aSAH.missing, na.action = na.omit, subset = (gender == "Female"), quiet = TRUE)[check.only.items],
		roc.test(aSAH$outcome[21:113][aSAH[21:113,]$gender == "Female"], aSAH$wfns[21:113][aSAH[21:113,]$gender == "Female"], aSAH$ndka[21:113][aSAH[21:113,]$gender == "Female"], quiet = TRUE)[check.only.items]
	)
})

test_that("paired tests don't work on unpaired curves", {
	# Make an unpaired ROC curve
	up.r.ndka <- roc(controls = aSAH$ndka[aSAH$outcome == "Good"], cases = aSAH$ndka[aSAH$outcome == "Poor"], quiet = TRUE)
	# unpaired by default
	t4 <- roc.test(r.wfns, up.r.ndka)
	expect_false(grepl("correlated", t4$method))
	# Shoud be an error:
	expect_error(roc.test(r.wfns, up.r.ndka, paired = TRUE))

})

