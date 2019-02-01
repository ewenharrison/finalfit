library(pROC)
data(aSAH)

level.values <- list(
	forward = c("Good", "Poor"),
	reversed = c("Poor", "Good")
)

expected.algorithm <- list(
	pROC:::roc.utils.perfs.all.safe,
	pROC:::roc.utils.perfs.all.fast,
	pROC:::rocUtilsPerfsAllC,
	pROC:::roc.utils.perfs.all.test,
	pROC:::rocUtilsPerfsAllC
)

for (marker in c("ndka", "wfns", "s100b")) {
	for (levels.direction in names(level.values)) {
		for (percent in c(FALSE, TRUE)) {
			for (direction in c("auto", "<", ">")) {
				for (algorithm in 1:5) {
					context(sprintf("'roc' function works with percent = %s, marker = %s, levels.direction = %s, direction = %s and algorithm = %s", percent, marker, levels.direction, direction, algorithm))
					r <- roc(aSAH$outcome, aSAH[[marker]], levels = level.values[[levels.direction]], direction = direction, percent = percent, algorithm = algorithm, quiet = TRUE)
					
					test_that("roc.formula produces the same results as roc.default", {
						rf <- roc(as.formula(sprintf("outcome ~ %s", marker)), data = aSAH, levels = level.values[[levels.direction]], direction = direction, percent = percent, algorithm = algorithm, quiet = TRUE)
						expect_is(rf, "roc")
						expect_equal(as.numeric(rf$auc), as.numeric(r$auc))
						for (item in c("percent", "sensitivities", "specificities", "thresholds", "direction", "cases", "controls", "fun.sesp")) {
							expect_identical(rf[[item]], r[[item]], label = sprintf("roc(outcome ~ %s, %s, %s, %s, %s)[[\"%s\"]]", marker, levels.direction, percent, direction, algorithm, item))
						}
						for (item in c("original.predictor", "original.response", "predictor", "response", "levels")) {
							expect_identical(unname(rf[[item]]), unname(r[[item]]), label = sprintf("roc(outcome ~ %s, %s, %s, %s, %s)[[\"%s\"]]", marker, levels.direction, percent, direction, algorithm, item))
						}
						expect_identical(rf$fun.sesp, expected.algorithm[[algorithm]])
					})
					
					test_that("roc.default works with control/cases as well", {
						rcs <- roc(controls = r$controls, cases = r$cases, levels = level.values[[levels.direction]], direction = direction, percent = percent, algorithm = algorithm, quiet = TRUE)
						expect_is(rcs, "roc")
						expect_equal(as.numeric(rcs$auc), as.numeric(r$auc))
						for (item in c("percent", "sensitivities", "specificities", "thresholds", "direction", "cases", "controls", "fun.sesp")) {
							expect_identical(rcs[[item]], r[[item]])
						}
						expect_identical(rcs$fun.sesp, expected.algorithm[[algorithm]])
					})
					
					test_that("roc.default produces the expected results", {
						expected.direction <- ifelse(direction == "auto", ifelse(levels.direction == "forward", "<", ">"), direction)
					
						expect_is(r, "roc")
						expect_identical(r$percent, percent)
						expect_identical(r$fun.sesp, expected.algorithm[[algorithm]])
						expect_identical(r$direction, expected.direction)
						expect_identical(r$levels, level.values[[levels.direction]])
						
						expect_equal(r$thresholds, expected.roc[[marker]][[levels.direction]][[expected.direction]][["thresholds"]])
						expect_equal(r$sensitivities, expected.roc[[marker]][[levels.direction]][[expected.direction]][["sensitivities"]] * ifelse(percent, 100, 1))
						expect_equal(r$specificities, expected.roc[[marker]][[levels.direction]][[expected.direction]][["specificities"]] * ifelse(percent, 100, 1))
					})
				}
			}
		}
	}
}


test_that("roc.default handles NAs", {
	# Generate missing values
	aSAH.missing <- aSAH
	aSAH.missing$ndka[1:20] <- NA
	aSAH.missing$wfns[1:20] <- NA
	
	# na.rm=FALSE works
	# With NDKA
	expect_true(is.na(roc(aSAH.missing$outcome, aSAH.missing$ndka, na.rm = FALSE)))
	expect_false(is.na(auc(roc(aSAH.missing$outcome, aSAH.missing$ndka, na.rm = TRUE))))
	# With WFNS
	expect_true(is.na(roc(aSAH.missing$outcome, aSAH.missing$wfns, na.rm = FALSE)))
	expect_false(is.na(auc(roc(aSAH.missing$outcome, aSAH.missing$wfns, na.rm = TRUE))))
	
	# Same as subset
	expect_identical(
		as.numeric(auc(roc(aSAH.missing$outcome, aSAH.missing$ndka, na.rm = TRUE))),
		as.numeric(auc(roc(aSAH[21:113,]$outcome, aSAH.missing[21:113,]$ndka))))
	# With ordered
	expect_identical(
		as.numeric(auc(roc(aSAH.missing$outcome, aSAH.missing$wfns, na.rm = TRUE))),
		as.numeric(auc(roc(aSAH[21:113,]$outcome, aSAH.missing[21:113,]$wfns))))
	
	# Also with case/controls
	expect_identical(
		as.numeric(auc(roc(controls = aSAH.missing$ndka[aSAH.missing$outcome == "Good"], cases = aSAH.missing$ndka[aSAH.missing$outcome == "Poor"]))),
		as.numeric(auc(roc(aSAH[21:113,]$outcome, aSAH.missing[21:113,]$ndka))))
	# With ordered
	expect_identical(
		as.numeric(auc(roc(controls = aSAH.missing$wfns[aSAH.missing$outcome == "Good"], cases = aSAH.missing$wfns[aSAH.missing$outcome == "Poor"]))),
		as.numeric(auc(roc(aSAH[21:113,]$outcome, aSAH.missing[21:113,]$wfns))))
})

test_that("roc.formula behaves", {
	# By this point we've tested the main stuff, so just check a few basic elements
	roc.check.only.items <- c("sensitivities", "specificities", "thresholds", "cases", "controls")
	
	expect_identical(
		roc(outcome ~ wfns, data = aSAH)[roc.check.only.items],
		roc(aSAH$outcome, aSAH$wfns)[roc.check.only.items]
	)
	
	expect_identical(
		roc(outcome ~ wfns, data = aSAH, subset = (gender == "Female"))[roc.check.only.items],
		roc(aSAH$outcome[aSAH$gender == "Female"], aSAH$wfns[aSAH$gender == "Female"])[roc.check.only.items]
	)
	
	# Generate missing values
	aSAH.missing <- aSAH
	aSAH.missing$ndka[1:20] <- NA
	expect_identical(
		roc(outcome ~ ndka, data = aSAH.missing, na.action = na.omit)[roc.check.only.items],
		roc(aSAH[21:113,]$outcome, aSAH[21:113,]$ndka)[roc.check.only.items]
	)
	#na.fail should fail
	expect_error(roc(outcome ~ ndka, data = aSAH.missing, na.action = na.fail))
	#weights should fail too
	expect_error(roc(outcome ~ ndka, data = aSAH, weights = seq_len(nrow(aSAH)), quiet = TRUE), regexp = "weights are not supported")
	
	# Both na.action and subset
	expect_identical(
		roc(outcome ~ ndka, data = aSAH.missing, na.action = na.omit, subset = (gender == "Female"))[roc.check.only.items],
		roc(aSAH[21:113,]$outcome[aSAH[21:113,]$gender == "Female"], aSAH[21:113,]$ndka[aSAH[21:113,]$gender == "Female"])[roc.check.only.items]
	)
})

test_that("roc can't take both response/predictor and case/control", {
	skip("This doesn't throw an error currently.")
	expect_error(roc(aSAH$outcome, aSAH$ndka, controls = aSAH$ndka[aSAH$outcome == "Good"], cases = aSAH$ndka[aSAH$outcome == "Poor"]))
})


test_that("microbenchmark works", {
	skip_if_not_installed("microbenchmark")
	# Algorithm 3 (C) should be selected with small low number of thresholds like aSAH$wfns
	expect_output(r <- roc(aSAH$outcome, aSAH$wfns, algorithm = 0), "Selecting algorithm 3")
	
	# Algorithm 2 (R cumsum) should be selected with large datasets with many thresholds
	# This is going to be slow, so skip unless we're running slow tests
	skip_if_not(exists("run_slow_tests") && run_slow_tests, message = "Slow test skipped")
	expect_output(r <- roc(round(runif(10000)), rnorm(10000), algorithm = 0), "Selecting algorithm 2")
})

# The code below can be used to refresh the "expected.roc" data, just in case...
# expected.roc <- list()
# for (marker in c("ndka", "wfns", "s100b")) {
# 	expected.roc[[marker]] <- list()
# 	for (levels.direction in names(level.values)) {
# 		expected.roc[[marker]][[levels.direction]] <- list()
# 		for (direction in c("<", ">")) {
# 			r <- roc(aSAH$outcome, aSAH[[marker]], levels = level.values[[levels.direction]], direction = direction, percent = FALSE, quiet = TRUE)
# 			if (!isTRUE(percent) && direction != "auto") {
# 				expected.roc[[marker]][[levels.direction]][[direction]] <- r[c("sensitivities", "specificities", "thresholds")]
# 				expected.roc[[marker]][[levels.direction]][[direction]][["auc"]] <- as.numeric(r$auc)
# 			}
# 		}
# 	}
# }
# save("expected.roc", system.file("extdata", "test-roc-expected.R", package="pROC"), file = "dump_roc_expected.R")

