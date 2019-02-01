# Copyright (C) 2010 Jelmer Ypma. All Rights Reserved.
# This code is published under the L-GPL.
#
# File:   simple.R
# Author: Jelmer Ypma
# Date:   20 June 2010
#
# Example showing how to solve a simple constrained problem.
#
# min x^2
# s.t. x >= 5
#
# re-formulate constraint to be of form g(x) <= 0
#      5 - x <= 0
# we could use a bound constraint as well here
#
# CHANGELOG:
#   05/05/2014: Changed example to use unit testing framework testthat.

context("Simple contstrained problem.")

test_that( "Test simple constrained optimization problem with gradient information." , {
    # Objective function.
    eval_f <- function(x) { 
        return( x^2 )
    }
    
    # Gradient of objective function.
    eval_grad_f <- function(x) { 
        return( 2*x )
    }
    
    # Inequality constraint function.
    eval_g_ineq <- function( x ) {
        return( 5 - x )
    }
    
    # Jacobian of constraint.
    eval_jac_g_ineq <- function( x ) {
        return( -1 )
    }
    
    # Optimal solution.
    solution.opt <- 5
    
    # Solve using NLOPT_LD_MMA with gradient information supplied in separate function
    res <- nloptr( x0              = 1, 
                   eval_f          = eval_f, 
                   eval_grad_f     = eval_grad_f,
                   eval_g_ineq     = eval_g_ineq,
                   eval_jac_g_ineq = eval_jac_g_ineq,                
                   opts            = list("algorithm"="NLOPT_LD_MMA", "xtol_rel" = 1e-4) )

    # Run some checks on the optimal solution.
    expect_that( res$solution, equals( solution.opt ) )
    
    # Check whether constraints are violated (up to specified tolerance).
    expect_that( eval_g_ineq( res$solution ) <= res$options$tol_constraints_ineq, is_true() )
} )

test_that( "Test simple constrained optimization problem without gradient information." , {
    # Objective function.
    eval_f <- function(x) { 
        return( x^2 )
    }
    
    # Inequality constraint function.
    eval_g_ineq <- function( x ) {
        return( 5 - x )
    }
    
    # Optimal solution.
    solution.opt <- 5
    
    # Solve using NLOPT_LN_COBYLA without gradient information
    res <- nloptr( x0              = 1, 
                   eval_f          = eval_f,    			
                   eval_g_ineq     = eval_g_ineq, 
                   opts            = list(
                        "algorithm"            = "NLOPT_LN_COBYLA", 
                        "xtol_rel"             = 1e-6,
                        "tol_constraints_ineq" = 1e-6 ) )
    
    # Run some checks on the optimal solution.
    expect_that( res$solution, equals( solution.opt, tolerance = 1e-6 ) )
    
    # Check whether constraints are violated (up to specified tolerance).
    expect_that( eval_g_ineq( res$solution ) <= res$options$tol_constraints_ineq, is_true() )
} )
