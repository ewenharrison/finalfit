context("plot")

# Tests powered by vdiffr. 
# To update the reference with vdiffr:
# > library(vdiffr)
# > source("tests/testthat.R")
# > manage_cases()

test_that("plot draws correctly", {
	test_basic_plot <- function() plot(r)
	# S100b
	r <- r.s100b
	vdiffr::expect_doppelganger("basic-s100b", test_basic_plot)
	
	r <- r.ndka
	vdiffr::expect_doppelganger("basic-ndka", test_basic_plot)
	
	r <- r.wfns
	vdiffr::expect_doppelganger("basic-wfns", test_basic_plot)
})

test_that("legacy.axis works correctly", {
	r <- r.s100b
	test_legacy.axis_plot <- function() plot(r, legacy.axes=TRUE)
	vdiffr::expect_doppelganger("legacy.axes", test_legacy.axis_plot)
})

test_that("Advanced screenshot 1 works correctly", {
	test_advanced_screenshot_1 <- function() {
		plot(r.s100b.percent,
			 reuse.auc = FALSE, partial.auc=c(100, 90), partial.auc.correct=TRUE, # define a partial AUC (pAUC)
			 print.auc=TRUE, #display pAUC value on the plot with following options:
			 print.auc.pattern="Corrected pAUC (100-90%% SP):\n%.1f%%", print.auc.col="#1c61b6",
			 auc.polygon=TRUE, auc.polygon.col="#1c61b6", # show pAUC as a polygon
			 max.auc.polygon=TRUE, max.auc.polygon.col="#1c61b622", # also show the 100% polygon
			 main="Partial AUC (pAUC)")
		
		plot(r.s100b.percent,
			 reuse.auc = FALSE, partial.auc=c(100, 90), partial.auc.correct=TRUE,
			 partial.auc.focus="se", # focus pAUC on the sensitivity
			 add=TRUE, type="n", # add to plot, but don't re-add the ROC itself (useless)
			 print.auc=TRUE, print.auc.pattern="Corrected pAUC (100-90%% SE):\n%.1f%%", print.auc.col="#008600",
			 print.auc.y=40, # do not print auc over the previous one
			 auc.polygon=TRUE, auc.polygon.col="#008600",
			 max.auc.polygon=TRUE, max.auc.polygon.col="#00860022")
	}
	vdiffr::expect_doppelganger("advanced.screenshot.1", test_advanced_screenshot_1)
})


test_that("Advanced screenshot 2 works correctly", {
	skip_if_not(exists("run_slow_tests") && run_slow_tests, message = "Slow test skipped")
	set.seed(42) # For reproducible CI
	test_advanced_screenshot_2 <- function() {
		rocobj <- plot.roc(aSAH$outcome, aSAH$s100b,
						   main="Confidence intervals", percent=TRUE,
						   ci=TRUE, # compute AUC (of AUC by default)
						   print.auc=TRUE) # print the AUC (will contain the CI)
		
		ciobj <- ci.se(rocobj, progress = "none", # CI of sensitivity
					   specificities=seq(0, 100, 5)) # over a select set of specificities
		
		plot(ciobj, type="shape", col="#1c61b6AA") # plot as a blue shape
		plot(ci(rocobj, of="thresholds", thresholds="best", progress="none")) # add one threshold
	}
	vdiffr::expect_doppelganger("advanced.screenshot.2", test_advanced_screenshot_2)
})


test_that("Advanced screenshot 3 works correctly", {
	test_advanced_screenshot_3 <- function() {
		plot(r.s100b.percent, main="Smoothing")
		
		lines(smooth(r.s100b.percent), # smoothing (default: binormal)
			  col = "#1c61b6")
		
		lines(smooth(r.s100b.percent, method = "density"), # density smoothing
			  col = "#008600")
		
		lines(smooth(r.s100b.percent, method = "fitdistr", # fit a distribution
					 density = "lognormal"), # let the distribution be log-normal
			  col = "#840000")
		
		legend("bottomright", legend = c("Empirical", "Binormal", "Density", "Fitdistr\n(Log-normal)"), col = c("black", "#1c61b6", "#008600", "#840000"),lwd = 2)
	}
	vdiffr::expect_doppelganger("advanced.screenshot.3", test_advanced_screenshot_3)
})



test_that("Advanced screenshot 4 works correctly", {
	skip_if_not(exists("run_slow_tests") && run_slow_tests, message = "Slow test skipped")
	set.seed(42) # For reproducible CI
	test_advanced_screenshot_4 <- function() {
		rocobj <- plot.roc(aSAH$outcome, aSAH$s100b,
						   main="Confidence intervals of specificity/sensitivity", percent=TRUE,
						   ci=TRUE, of="se", # ci of sensitivity
						   specificities=seq(0, 100, 5), # on a select set of specificities
						   ci.type="shape", ci.col="#1c61b6AA", # plot the CI as a blue shape
						   progress = "none") # hide progress bar
		
		plot(ci.sp(rocobj, sensitivities=seq(0, 100, 5), progress = "none"), # ci of specificity
			 type="bars") # print this one as bars
	}
	vdiffr::expect_doppelganger("advanced.screenshot.4", test_advanced_screenshot_4)
})



test_that("Advanced screenshot 5 works correctly", {
	skip_if_not(exists("run_slow_tests") && run_slow_tests, message = "Slow test skipped")
	set.seed(42) # For reproducible CI
	test_advanced_screenshot_5 <- function() {
		plot.roc(aSAH$outcome, aSAH$s100b,
				 main="Confidence interval of a threshold", percent=TRUE,
				 ci=TRUE, of="thresholds", # compute AUC (of threshold)
				 thresholds="best", # select the (best) threshold
				 print.thres="best", # also highlight this threshold on the plot
				 progress = "none") # hide progress bar
	}
	vdiffr::expect_doppelganger("advanced.screenshot.5", test_advanced_screenshot_5)
})


test_that("Advanced screenshot 6 works correctly", {
	test_advanced_screenshot_6 <- function() {
		plot(r.s100b.percent, main="Statistical comparison", col="#1c61b6")
		lines(r.ndka.percent, col="#008600")
		testobj <- roc.test(r.s100b.percent, r.ndka.percent)
		text(50, 50, labels=paste("p-value =", format.pval(testobj$p.value)), adj=c(0, .5))
		legend("bottomright", legend=c("S100B", "NDKA"), col=c("#1c61b6", "#008600"), lwd=2)
	}
	vdiffr::expect_doppelganger("advanced.screenshot.6", test_advanced_screenshot_6)
})