# Copyright (C) 2010 Jelmer Ypma. All Rights Reserved.
# This code is published under the L-GPL.
#
# File:   test-banana.R
# Author: Jelmer Ypma
# Date:   10 June 2010
#
# Example showing how to solve the Rosenbrock Banana function.
#
# Changelog:
#   27/10/2013: Changed example to use unit testing framework testthat.

context("Banana")

test_that("Test Rosenbrock Banana optimization with objective and gradient in separate functions.", {
    # initial values
    x0 <- c( -1.2, 1 )
    
    opts <- list( "algorithm"   = "NLOPT_LD_LBFGS",
                  "xtol_rel"    = 1.0e-8,
                  "print_level" = 0 )
    
    ## Rosenbrock Banana function and gradient in separate functions
    eval_f <- function(x) {
        return( 100 * (x[2] - x[1] * x[1])^2 + (1 - x[1])^2 )
    }
    
    eval_grad_f <- function(x) {
        return( c( -400 * x[1] * (x[2] - x[1] * x[1]) - 2 * (1 - x[1]),
                    200 * (x[2] - x[1] * x[1])) )
    }

    # Solve Rosenbrock Banana function.
    res <- nloptr( 
        x0          = x0, 
        eval_f      = eval_f, 
        eval_grad_f = eval_grad_f,
        opts        = opts )
    
    # Check results.
    expect_that( res$objective, equals( 0.0 ) )
    expect_that( res$solution, equals( c( 1.0, 1.0 ) ) )
} )

test_that("Test Rosenbrock Banana optimization with objective and gradient in the same function.", {
    # initial values
    x0 <- c( -1.2, 1 )
    
    opts <- list( "algorithm"   = "NLOPT_LD_LBFGS",
                  "xtol_rel"    = 1.0e-8,
                  "print_level" = 0 )
    
    ## Rosenbrock Banana function and gradient in one function
    # this can be used to economize on calculations
    eval_f_list <- function(x) {
        return( list( "objective" = 100 * (x[2] - x[1] * x[1])^2 + (1 - x[1])^2,
                      "gradient"  = c( -400 * x[1] * (x[2] - x[1] * x[1]) - 2 * (1 - x[1]),
                                        200 * (x[2] - x[1] * x[1])) ) )
    }
    
    # Solve Rosenbrock Banana function. using an objective function that
    # returns a list with the objective value and its gradient               
    res <- nloptr( 
        x0     = x0, 
        eval_f = eval_f_list,
        opts   = opts )

    # Check results.
    expect_that( res$objective, equals( 0.0 ) )
    expect_that( res$solution, equals( c( 1.0, 1.0 ) ) )
} )
