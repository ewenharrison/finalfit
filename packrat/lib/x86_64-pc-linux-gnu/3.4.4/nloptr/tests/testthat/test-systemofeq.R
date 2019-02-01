# Copyright (C) 2010 Jelmer Ypma. All Rights Reserved.
# This code is published under the L-GPL.
#
# File:   systemofeq.R
# Author: Jelmer Ypma
# Date:   20 June 2010
#
# Example showing how to solve a system of equations.
#
# min 1
# s.t. x^2 + x - 1 = 0
#
# Optimal solution for x: -1.61803398875
#
# CHANGELOG:
#   16/06/2011: added NLOPT_LD_SLSQP
#   05/05/2014: Changed example to use unit testing framework testthat.

context("System of equations.")

test_that( "Solve system of equations using NLOPT_LD_MMA with local optimizer NLOPT_LD_MMA.", {
    # Objective function.
    eval_f0 <- function( x, params ) { 
        return( 1 )
    }
    
    # Gradient of objective function.
    eval_grad_f0 <- function( x, params ) { 
        return( 0 )
    }
    
    # Equality constraint function.
    eval_g0_eq <- function( x, params ) {
        return( params[1]*x^2 + params[2]*x + params[3] )
    }
    
    # Jacobian of constraint.
    eval_jac_g0_eq <- function( x, params ) {
        return( 2*params[1]*x + params[2] )
    }
    
    # Define vector with addiitonal data.
    params <- c(1, 1, -1)
    
    # Define optimal solution.
    solution.opt <- -1.61803398875
    
    #
    # Solve using NLOPT_LD_MMA with local optimizer NLOPT_LD_MMA.
    #
    local_opts <- list( "algorithm" = "NLOPT_LD_MMA",
                        "xtol_rel"  = 1.0e-6 )
                        
    opts <- list( "algorithm"  = "NLOPT_LD_AUGLAG",
                  "xtol_rel"   = 1.0e-6,
                  "local_opts" = local_opts )
    
    res <- nloptr( x0            = -5, 
                   eval_f        = eval_f0, 
                   eval_grad_f   = eval_grad_f0,
                   eval_g_eq     = eval_g0_eq,
                   eval_jac_g_eq = eval_jac_g0_eq,                
                   opts          = opts,
        		   params        = params )
    
    # Run some checks on the optimal solution.
    expect_that( res$solution, equals( solution.opt ) )
    
    # Check whether constraints are violated (up to specified tolerance).
    expect_that( eval_g0_eq( res$solution, params = params ), equals( 0, tolerance = res$options$tol_constraints_eq ) )
} )

test_that( "Solve system of equations using NLOPT_LD_SLSQP.", {
    # Objective function.
    eval_f0 <- function( x, params ) { 
        return( 1 )
    }
    
    # Gradient of objective function.
    eval_grad_f0 <- function( x, params ) { 
        return( 0 )
    }
    
    # Equality constraint function.
    eval_g0_eq <- function( x, params ) {
        return( params[1]*x^2 + params[2]*x + params[3] )
    }
    
    # Jacobian of constraint.
    eval_jac_g0_eq <- function( x, params ) {
        return( 2*params[1]*x + params[2] )
    }
    
    # Define vector with addiitonal data.
    params <- c(1, 1, -1)

    # Define optimal solution.
    solution.opt <- -1.61803398875

    #       
    # Solve using NLOPT_LD_SLSQP.
    #
    opts <- list( "algorithm" = "NLOPT_LD_SLSQP",
                  "xtol_rel"  = 1.0e-6 )
    
    res <- nloptr( x0            = -5, 
                   eval_f        = eval_f0, 
                   eval_grad_f   = eval_grad_f0,
                   eval_g_eq     = eval_g0_eq,
                   eval_jac_g_eq = eval_jac_g0_eq,                
                   opts          = opts,
                   params        = params )
    
    # Run some checks on the optimal solution.
    expect_that( res$solution, equals( solution.opt ) )
    
    # Check whether constraints are violated (up to specified tolerance).
    expect_that( eval_g0_eq( res$solution, params = params ), equals( 0, tolerance = res$options$tol_constraints_eq ) )
} )
