library(pROC)
data(aSAH)

context("auc")

test_that("full auc works", {
	expect_equal(as.numeric(auc(r.wfns)), 0.823678861788618)
	expect_equal(as.numeric(auc(r.wfns.percent)), 82.3678861788618)

	expect_equal(as.numeric(auc(r.ndka)), 0.611957994579946)
	expect_equal(as.numeric(auc(r.ndka.percent)), 61.1957994579946)
})


test_that("partial auc works on arbitrary intervals", {
	expect_equal(as.numeric(auc(r.wfns, partial.auc = c(1, .9))), 0.0334417344173442)
	expect_equal(as.numeric(auc(r.wfns.percent, partial.auc = c(100, 90))), 3.34417344173442)
	# direction is unspecified
	expect_equal(as.numeric(auc(r.wfns, partial.auc = c(.9, 1))), 0.0334417344173442)
	expect_equal(as.numeric(auc(r.wfns.percent, partial.auc = c(90, 100))), 3.34417344173442)
	
	# Arbitrary intervals
	expect_equal(as.numeric(auc(r.wfns, partial.auc = c(.9, .8))), 0.0598373983739837)
	expect_equal(as.numeric(auc(r.wfns.percent, partial.auc = c(90, 80))), 5.98373983739837)
	expect_equal(as.numeric(auc(r.wfns, partial.auc = c(.5, 0))), 0.488134475939354)
	expect_equal(as.numeric(auc(r.wfns.percent, partial.auc = c(50, 0))), 48.8134475939354)
	
	# NDKA
	expect_equal(as.numeric(auc(r.ndka, partial.auc = c(1, .9))), 0.0107046070460705)
	expect_equal(as.numeric(auc(r.ndka.percent, partial.auc = c(100, 90))), 1.07046070460705)
	# direction is unspecified
	expect_equal(as.numeric(auc(r.ndka, partial.auc = c(.9, 1))), 0.0107046070460705)
	expect_equal(as.numeric(auc(r.ndka.percent, partial.auc = c(90, 100))), 1.07046070460705)
	
	# Arbitrary intervals
	expect_equal(as.numeric(auc(r.ndka, partial.auc = c(.9, .8))), 0.0277777777777778)
	expect_equal(as.numeric(auc(r.ndka.percent, partial.auc = c(90, 80))), 2.77777777777778)
	expect_equal(as.numeric(auc(r.ndka, partial.auc = c(.5, 0))), 0.416836043360434)
	expect_equal(as.numeric(auc(r.ndka.percent, partial.auc = c(50, 0))), 41.6836043360434)
	
	# Full interval == full auc
	expect_equal(as.numeric(auc(r.wfns, partial.auc = c(1, 0))), 0.823678861788618)
	expect_equal(as.numeric(auc(r.wfns.percent, partial.auc = c(100, 0))), 82.3678861788618)
	# direction is unspecified
	expect_equal(as.numeric(auc(r.ndka, partial.auc = c(1, 0))), 0.611957994579946)
	expect_equal(as.numeric(auc(r.ndka.percent, partial.auc = c(100, 0))), 61.1957994579946)
})


test_that("partial auc works with focus on SE", {
	expect_equal(as.numeric(auc(r.wfns, partial.auc = c(1, .9), partial.auc.focus = "se")), 0.0400999322493225)
	expect_equal(as.numeric(auc(r.wfns.percent, partial.auc = c(100, 90), partial.auc.focus = "se")), 4.00999322493225)
	# direction is unspecified
	expect_equal(as.numeric(auc(r.wfns, partial.auc = c(.9, 1), partial.auc.focus = "se")), 0.0400999322493225)
	expect_equal(as.numeric(auc(r.wfns.percent, partial.auc = c(90, 100), partial.auc.focus = "se")), 4.00999322493225)
	
	# Arbitrary intervals
	expect_equal(as.numeric(auc(r.wfns, partial.auc = c(.9, .8), partial.auc.focus = "se")), 0.0609953703703703)
	expect_equal(as.numeric(auc(r.wfns.percent, partial.auc = c(90, 80), partial.auc.focus = "se")), 6.09953703703703)
	expect_equal(as.numeric(auc(r.wfns, partial.auc = c(.5, 0), partial.auc.focus = "se")), 0.483358739837398)
	expect_equal(as.numeric(auc(r.wfns.percent, partial.auc = c(50, 0), partial.auc.focus = "se")), 48.3358739837398)
	
	# NDKA
	expect_equal(as.numeric(auc(r.ndka, partial.auc = c(1, .9), partial.auc.focus = "se")), 0.0037940379403794)
	expect_equal(as.numeric(auc(r.ndka.percent, partial.auc = c(100, 90), partial.auc.focus = "se")), 0.37940379403794)
	# direction is unspecified
	expect_equal(as.numeric(auc(r.ndka, partial.auc = c(.9, 1), partial.auc.focus = "se")), 0.0037940379403794)
	expect_equal(as.numeric(auc(r.ndka.percent, partial.auc = c(90, 100), partial.auc.focus = "se")), 0.37940379403794)
	
	# Arbitrary intervals
	expect_equal(as.numeric(auc(r.ndka, partial.auc = c(.9, .8), partial.auc.focus = "se")), 0.0242547425474255)
	expect_equal(as.numeric(auc(r.ndka.percent, partial.auc = c(90, 80), partial.auc.focus = "se")), 2.42547425474255)
	expect_equal(as.numeric(auc(r.ndka, partial.auc = c(.5, 0), partial.auc.focus = "se")), 0.428523035230352)
	expect_equal(as.numeric(auc(r.ndka.percent, partial.auc = c(50, 0), partial.auc.focus = "se")), 42.8523035230352)
	
	# Full interval == full auc
	expect_equal(as.numeric(auc(r.wfns, partial.auc = c(1, 0), partial.auc.focus = "se")), 0.823678861788618)
	expect_equal(as.numeric(auc(r.wfns.percent, partial.auc = c(100, 0), partial.auc.focus = "se")), 82.3678861788618)
	# direction is unspecified
	expect_equal(as.numeric(auc(r.ndka, partial.auc = c(1, 0), partial.auc.focus = "se")), 0.611957994579946)
	expect_equal(as.numeric(auc(r.ndka.percent, partial.auc = c(100, 0), partial.auc.focus = "se")), 61.1957994579946)
})


test_that("partial auc works with correction enabled", {
	expect_equal(as.numeric(auc(r.wfns, partial.auc = c(1, .9), partial.auc.correct = TRUE)), 0.649693339038653)
	expect_equal(as.numeric(auc(r.wfns.percent, partial.auc = c(100, 90), partial.auc.correct = TRUE)), 64.9693339038653)
	# direction is unspecified
	expect_equal(as.numeric(auc(r.wfns, partial.auc = c(.9, 1), partial.auc.correct = TRUE)), 0.649693339038653)
	expect_equal(as.numeric(auc(r.wfns.percent, partial.auc = c(90, 100), partial.auc.correct = TRUE)), 64.9693339038653)
	
	# Arbitrary intervals
	expect_equal(as.numeric(auc(r.wfns, partial.auc = c(.9, .8), partial.auc.correct = TRUE)), 0.763749402199904)
	expect_equal(as.numeric(auc(r.wfns.percent, partial.auc = c(90, 80), partial.auc.correct = TRUE)), 76.3749402199904)
	expect_equal(as.numeric(auc(r.wfns, partial.auc = c(.5, 0), partial.auc.correct = TRUE)), 0.952537903757416)
	expect_equal(as.numeric(auc(r.wfns.percent, partial.auc = c(50, 0), partial.auc.correct = TRUE)), 95.2537903757416)
	
	# NDKA
	expect_equal(as.numeric(auc(r.ndka, partial.auc = c(1, .9), partial.auc.correct = TRUE)), 0.530024247610897)
	expect_equal(as.numeric(auc(r.ndka.percent, partial.auc = c(100, 90), partial.auc.correct = TRUE)), 53.0024247610897)
	# direction is unspecified
	expect_equal(as.numeric(auc(r.ndka, partial.auc = c(.9, 1), partial.auc.correct = TRUE)), 0.530024247610897)
	expect_equal(as.numeric(auc(r.ndka.percent, partial.auc = c(90, 100), partial.auc.correct = TRUE)), 53.0024247610897)
	
	# Arbitrary intervals
	expect_equal(as.numeric(auc(r.ndka, partial.auc = c(.9, .8), partial.auc.correct = TRUE)), 0.575163398692811)
	expect_equal(as.numeric(auc(r.ndka.percent, partial.auc = c(90, 80), partial.auc.correct = TRUE)), 57.5163398692811)
	expect_equal(as.numeric(auc(r.ndka, partial.auc = c(.5, 0), partial.auc.correct = TRUE)), 0.667344173441734)
	expect_equal(as.numeric(auc(r.ndka.percent, partial.auc = c(50, 0), partial.auc.correct = TRUE)), 66.7344173441734)
	
	# Full interval == full auc
	expect_equal(as.numeric(auc(r.wfns, partial.auc = c(1, 0), partial.auc.correct = TRUE)), 0.823678861788618)
	expect_equal(as.numeric(auc(r.wfns.percent, partial.auc = c(100, 0), partial.auc.correct = TRUE)), 82.3678861788618)
	# direction is unspecified
	expect_equal(as.numeric(auc(r.ndka, partial.auc = c(1, 0), partial.auc.correct = TRUE)), 0.611957994579946)
	expect_equal(as.numeric(auc(r.ndka.percent, partial.auc = c(100, 0), partial.auc.correct = TRUE)), 61.1957994579946)
})


test_that("partial auc works with focus on SE and correction enabled", {
	expect_equal(as.numeric(auc(r.wfns, partial.auc = c(1, .9), partial.auc.focus = "se", partial.auc.correct = TRUE)), 0.68473648552275)
	expect_equal(as.numeric(auc(r.wfns.percent, partial.auc = c(100, 90), partial.auc.focus = "se", partial.auc.correct = TRUE)), 68.473648552275)
	# direction is unspecified
	expect_equal(as.numeric(auc(r.wfns, partial.auc = c(.9, 1), partial.auc.focus = "se", partial.auc.correct = TRUE)), 0.68473648552275)
	expect_equal(as.numeric(auc(r.wfns.percent, partial.auc = c(90, 100), partial.auc.focus = "se", partial.auc.correct = TRUE)), 68.473648552275)
	
	# Arbitrary intervals
	expect_equal(as.numeric(auc(r.wfns, partial.auc = c(.9, .8), partial.auc.focus = "se", partial.auc.correct = TRUE)), 0.770561002178649)
	expect_equal(as.numeric(auc(r.wfns.percent, partial.auc = c(90, 80), partial.auc.focus = "se", partial.auc.correct = TRUE)), 77.0561002178649)
	expect_equal(as.numeric(auc(r.wfns, partial.auc = c(.5, 0), partial.auc.focus = "se", partial.auc.correct = TRUE)), 0.933434959349593)
	expect_equal(as.numeric(auc(r.wfns.percent, partial.auc = c(50, 0), partial.auc.focus = "se", partial.auc.correct = TRUE)), 93.3434959349593)
	
	# NDKA
	expect_warning(auc(r.ndka, partial.auc = c(1, .9), partial.auc.focus = "se", partial.auc.correct = TRUE))
	expect_warning(auc(r.ndka.percent, partial.auc = c(100, 90), partial.auc.focus = "se", partial.auc.correct = TRUE))
	expect_identical(as.numeric(auc(r.ndka, partial.auc = c(1, .9), partial.auc.focus = "se", partial.auc.correct = TRUE)), NA_real_)
	expect_identical(as.numeric(auc(r.ndka.percent, partial.auc = c(100, 90), partial.auc.focus = "se", partial.auc.correct = TRUE)), NA_real_)
	# direction is unspecified
	expect_warning(auc(r.ndka, partial.auc = c(.9, 1), partial.auc.focus = "se", partial.auc.correct = TRUE))
	expect_warning(auc(r.ndka.percent, partial.auc = c(90, 100), partial.auc.focus = "se", partial.auc.correct = TRUE))
	expect_identical(as.numeric(auc(r.ndka, partial.auc = c(.9, 1), partial.auc.focus = "se", partial.auc.correct = TRUE)), NA_real_)
	expect_identical(as.numeric(auc(r.ndka.percent, partial.auc = c(90, 100), partial.auc.focus = "se", partial.auc.correct = TRUE)), NA_real_)
	
	# Arbitrary intervals
	expect_equal(as.numeric(auc(r.ndka, partial.auc = c(.9, .8), partial.auc.focus = "se", partial.auc.correct = TRUE)), 0.554439662043679)
	expect_equal(as.numeric(auc(r.ndka.percent, partial.auc = c(90, 80), partial.auc.focus = "se", partial.auc.correct = TRUE)), 55.4439662043679)
	expect_equal(as.numeric(auc(r.ndka, partial.auc = c(.5, 0), partial.auc.focus = "se", partial.auc.correct = TRUE)), 0.714092140921409)
	expect_equal(as.numeric(auc(r.ndka.percent, partial.auc = c(50, 0), partial.auc.focus = "se", partial.auc.correct = TRUE)), 71.4092140921409)
	
	# Full interval == full auc
	expect_equal(as.numeric(auc(r.wfns, partial.auc = c(1, 0), partial.auc.focus = "se", partial.auc.correct = TRUE)), 0.823678861788618)
	expect_equal(as.numeric(auc(r.wfns.percent, partial.auc = c(100, 0), partial.auc.focus = "se", partial.auc.correct = TRUE)), 82.3678861788618)
	# direction is unspecified
	expect_equal(as.numeric(auc(r.ndka, partial.auc = c(1, 0), partial.auc.focus = "se", partial.auc.correct = TRUE)), 0.611957994579946)
	expect_equal(as.numeric(auc(r.ndka.percent, partial.auc = c(100, 0), partial.auc.focus = "se", partial.auc.correct = TRUE)), 61.1957994579946)
})


test_that("auc can create a roc curve", {
	expect_equal(as.numeric(auc(aSAH$outcome, aSAH$wfns)), as.numeric(auc(r.wfns)))
	expect_equal(as.numeric(auc(aSAH$outcome, aSAH$ndka)), as.numeric(auc(r.ndka)))
	# With formula
	expect_equal(as.numeric(auc(outcome ~ wfns, aSAH)), as.numeric(auc(r.wfns)))
	expect_equal(as.numeric(auc(outcome ~ ndka, aSAH)), as.numeric(auc(r.ndka)))
})

test_that("auc can create a roc curve with percent", {
	expect_equal(as.numeric(auc(aSAH$outcome, aSAH$wfns, percent = TRUE)), as.numeric(auc(r.wfns.percent)))
	expect_equal(as.numeric(auc(aSAH$outcome, aSAH$ndka, percent = TRUE)), as.numeric(auc(r.ndka.percent)))
	# With formula
	expect_equal(as.numeric(auc(outcome ~ wfns, aSAH, percent = TRUE)), as.numeric(auc(r.wfns.percent)))
	expect_equal(as.numeric(auc(outcome ~ ndka, aSAH, percent = TRUE)), as.numeric(auc(r.ndka.percent)))
})


test_that("auc.formula behaves", {
	expect_equal(
		as.numeric(auc(outcome ~ wfns, data = aSAH)),
		as.numeric(auc(aSAH$outcome, aSAH$wfns))
	)
	
	expect_equal(
		as.numeric(auc(outcome ~ wfns, data = aSAH, subset = (gender == "Female"))),
		as.numeric(auc(aSAH$outcome[aSAH$gender == "Female"], aSAH$wfns[aSAH$gender == "Female"]))
	)
	
	# Generate missing values
	aSAH.missing <- aSAH
	aSAH.missing$ndka[1:20] <- NA
	expect_equal(
		as.numeric(auc(outcome ~ ndka, data = aSAH.missing, na.action = na.omit)),
		as.numeric(auc(aSAH[21:113,]$outcome, aSAH[21:113,]$ndka))
	)
	#na.fail should fail
	expect_error(auc(outcome ~ ndka, data = aSAH.missing, na.action = na.fail))
	#weights should fail too
	expect_error(auc(outcome ~ ndka, data = aSAH, weights = seq_len(nrow(aSAH))), regexp = "weights are not supported")
	
	# Both na.action and subset
	expect_equal(
		as.numeric(auc(outcome ~ ndka, data = aSAH.missing, na.action = na.omit, subset = (gender == "Female"))),
		as.numeric(auc(aSAH[21:113,]$outcome[aSAH[21:113,]$gender == "Female"], aSAH[21:113,]$ndka[aSAH[21:113,]$gender == "Female"]))
	)
})

