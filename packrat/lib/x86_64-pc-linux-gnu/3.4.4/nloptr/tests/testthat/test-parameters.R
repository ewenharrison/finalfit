# Copyright (C) 2010 Jelmer Ypma. All Rights Reserved.
# This code is published under the L-GPL.
#
# File:   parameters.R
# Author: Jelmer Ypma
# Date:   17 August 2010
#
# Example shows how we can have an objective function 
# depend on parameters or data. The objective function 
# is a simple polynomial.
#
# CHANGELOG:
#   05/05/2014: Changed example to use unit testing framework testthat.

context("Simple polynomial with additional data.")

test_that( "Test simple polyonmial where parameters are supplied as additional data.", {
    # Objective function and gradient in terms of parameters.
    eval_f <- function(x, params) { 
        return( params[1]*x^2 + params[2]*x + params[3] ) 
    }
    
    eval_grad_f <- function(x, params) { 
        return( 2*params[1]*x + params[2] ) 
    }
    
    # Define parameters that we want to use.
    params <- c(1,2,3)
    
    # Define initial value of the optimzation problem.
    x0 <- 0
    
    # solve using nloptr adding params as an additional parameter
    res <- nloptr( 
            x0          = x0, 
            eval_f      = eval_f, 
            eval_grad_f = eval_grad_f,
    		opts        = list("algorithm"="NLOPT_LD_MMA", "xtol_rel" = 1e-6),
            params      = params )

    # Solve using algebra
    # Minimize f(x) = ax^2 + bx + c.
    # Optimal value for control is -b/(2a).
    expect_that( res$solution, equals( -params[2]/(2*params[1]), tolerance = 1e-7 ) )

    # With value of the objective function f(-b/(2a)).
    expect_that( res$objective, equals( eval_f( -params[2]/(2*params[1]), params ) ) )
} )
