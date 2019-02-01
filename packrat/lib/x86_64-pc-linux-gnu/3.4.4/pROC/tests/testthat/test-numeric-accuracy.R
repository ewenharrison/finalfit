library(pROC)
data(aSAH)

numacc.response <- c(2, 1, 1, 2, 2, 1, 2, 2, 1, 1, 1, 2, 1, 2, 2, 2, 2, 2)
numacc.predictor <- c(0.960602681556147, 0.0794407386056549, 0.144842404246611, 
			   0.931816485855784, 0.931816485855784, 0.97764041048215, 0.653549466997938699464, 
			   0.796401132206396, 0.427720540184519, 0.811278021288732, 0.0188323116581187, 
			   0.653549466997938588442, 0.653549466997938477419, 0.959111701445925, 0.931816485855784, 
			   0.663663279418747, 0.800100838413179, 0.780456095511079)
# Predictor has near-ties that can break numerical comparisons

test_that("AUC is consistent across algorithms with numerical near-ties", {
	r1 <- roc(numacc.response, numacc.predictor, algorithm=1)
	r2 <- roc(numacc.response, numacc.predictor, algorithm=2)
	r3 <- roc(numacc.response, numacc.predictor, algorithm=3)
	expect_equal(as.numeric(auc(r1)), as.numeric(auc(r2)))
	expect_equal(as.numeric(auc(r1)), as.numeric(auc(r3)))
})

test_that("AUC is consistent across algorithms with numerical near-ties and direction = >", {
	r1 <- roc(2-numacc.response, numacc.predictor, algorithm=1)
	r2 <- roc(2-numacc.response, numacc.predictor, algorithm=2)
	r3 <- roc(2-numacc.response, numacc.predictor, algorithm=3)
	expect_equal(as.numeric(auc(r1)), as.numeric(auc(r2)))
	expect_equal(as.numeric(auc(r1)), as.numeric(auc(r3)))
})

test_that("delong theta is consistent with auc", {
	r1 <- roc(numacc.response, numacc.predictor, algorithm=1)
	r2 <- roc(numacc.response, numacc.predictor, algorithm=2)
	r3 <- roc(numacc.response, numacc.predictor, algorithm=3)
	expect_equal(pROC:::delongPlacements(r1)$theta, as.numeric(auc(r1)))
	expect_equal(pROC:::delongPlacements(r2)$theta, as.numeric(auc(r2)))
	expect_equal(pROC:::delongPlacements(r3)$theta, as.numeric(auc(r3)))
})

test_that("delong theta is consistent with auc and direction = >", {
	r1 <- roc(2-numacc.response, numacc.predictor, algorithm=1)
	r2 <- roc(2-numacc.response, numacc.predictor, algorithm=2)
	r3 <- roc(2-numacc.response, numacc.predictor, algorithm=3)
	expect_equal(pROC:::delongPlacements(r1)$theta, 1-as.numeric(auc(r1)))
	expect_equal(pROC:::delongPlacements(r2)$theta, 1-as.numeric(auc(r2)))
	expect_equal(pROC:::delongPlacements(r3)$theta, 1-as.numeric(auc(r3)))
})

# Test some crazy values
# Multiple sequencial near-tie that will break the thresholding algorithm at the limits close to +-Inf or 0
# Compare that with an "easy" curve with values with well defined intermediate averages
test_that("Hard predictor has same results as easy one", {
	numacc.predictor.hard <- c(-0x1.fffffffffffffp+1023, -0x1.ffffffffffffep+1023, -0x1.ffffffffffffdp+1023, # Close to -Inf
							   -0x1.249ad2594c37fp+332, -0x1.249ad2594c37ep+332, -0x1.249ad2594c37dp+332, -0x1.249ad2594c37cp+332, -0x1.249ad2594c37bp+332, -0x1.249ad2594c37ap+332, # Close to -1e100
							   -0x0.0000000000003p-1022, -0x0.0000000000002p-1022, -0x0.0000000000001p-1022, -0x0p+0, # Close to -0
							   0x0p+0, 0x0.0000000000001p-1022, 0x0.0000000000002p-1022, 0x0.0000000000003p-1022, # Close to +0
							   0x1.249ad2594c37ap+332, 0x1.249ad2594c37bp+332, 0x1.249ad2594c37cp+332, 0x1.249ad2594c37dp+332, 0x1.249ad2594c37ep+332, 0x1.249ad2594c37fp+332, # Close to +1e100
							   0x1.ffffffffffffdp+1023, 0x1.ffffffffffffep+1023, 0x1.fffffffffffffp+1023) # Close to +Inf
	numacc.predictor.easy <- c(-103, -102, -101,
							   -10, -9, -8, -7, -6, -5,
							   -0.1, -0.01, -0.001, 0,
							   0, 0.001, 0.01, 0.1,
							   5, 6, 7, 8, 9, 10,
							   101, 102, 103)
	response <- rbinom(length(numacc.predictor.easy), 1, 0.5)
	roc.hard <- roc(response, numacc.predictor.hard, direction="<")
	roc.easy <- roc(response, numacc.predictor.easy, direction="<")
	expect_equal(roc.hard$sensitivities, roc.easy$sensitivities, info = paste("Random response: ", paste(response, collapse=",")))
	expect_equal(roc.hard$specificities, roc.easy$specificities, info = paste("Random response: ", paste(response, collapse=",")))
	expect_equal(roc.hard$direction, roc.easy$direction, info = paste("Random response: ", paste(response, collapse=",")))
})

test_that("Hard predictor has same results as easy one, random sampling", {
	skip_if_not(exists("run_slow_tests") && run_slow_tests, message = "Slow test skipped")
	numacc.predictor.hard <- c(-0x1.fffffffffffffp+1023, -0x1.ffffffffffffep+1023, -0x1.ffffffffffffdp+1023, # Close to -Inf
							   -0x1.249ad2594c37fp+332, -0x1.249ad2594c37ep+332, -0x1.249ad2594c37dp+332, -0x1.249ad2594c37cp+332, -0x1.249ad2594c37bp+332, -0x1.249ad2594c37ap+332, # Close to -1e100
							   -0x0.0000000000003p-1022, -0x0.0000000000002p-1022, -0x0.0000000000001p-1022, -0x0p+0, # Close to -0
							   0x0p+0, 0x0.0000000000001p-1022, 0x0.0000000000002p-1022, 0x0.0000000000003p-1022, # Close to +0
							   0x1.249ad2594c37ap+332, 0x1.249ad2594c37bp+332, 0x1.249ad2594c37cp+332, 0x1.249ad2594c37dp+332, 0x1.249ad2594c37ep+332, 0x1.249ad2594c37fp+332, # Close to +1e100
							   0x1.ffffffffffffdp+1023, 0x1.ffffffffffffep+1023, 0x1.fffffffffffffp+1023) # Close to +Inf
	numacc.predictor.easy <- c(-103, -102, -101,
							   -10, -9, -8, -7, -6, -5,
							   -0.1, -0.01, -0.001, 0,
							   0, 0.001, 0.01, 0.1,
							   5, 6, 7, 8, 9, 10,
							   101, 102, 103)
	a <- replicate(1000, function(n) {
		response <- rbinom(length(numacc.predictor.easy), 1, 0.5)
		sample.vector <- sample(length(numacc.predictor.easy), replace = as.logical(rbinom(1, 1, 0.5)))
		roc.hard <- roc(response, numacc.predictor.hard[sample.vector], direction="<")
		roc.easy <- roc(response, numacc.predictor.easy[sample.vector], direction="<")
		expect_equal(roc.hard$sensitivities, roc.easy$sensitivities, info = 
					 	c(paste("Random response: ", paste(response,      collapse=",")),
					 	  paste("Random sample:",    paste(sample.vector, collapse=","))))
		expect_equal(roc.hard$specificities, roc.easy$specificities, info = 
					 	c(paste("Random response: ", paste(response,      collapse=",")),
					 	  paste("Random sample:",    paste(sample.vector, collapse=","))))
		expect_equal(roc.hard$direction, roc.easy$direction, info = 
					 	c(paste("Random response: ", paste(response,      collapse=",")),
					 	  paste("Random sample:",    paste(sample.vector, collapse=","))))
		
	})
})
