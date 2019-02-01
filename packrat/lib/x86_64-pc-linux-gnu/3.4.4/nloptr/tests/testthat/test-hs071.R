# Copyright (C) 2010 Jelmer Ypma. All Rights Reserved.
# This code is published under the L-GPL.
#
# File:   hs071.R
# Author: Jelmer Ypma
# Date:   10 June 2010
#
# Example problem, number 71 from the Hock-Schittkowsky test suite.
#
# \min_{x} x1*x4*(x1 + x2 + x3) + x3
# s.t.
#    x1*x2*x3*x4 >= 25
#    x1^2 + x2^2 + x3^2 + x4^2 = 40
#    1 <= x1,x2,x3,x4 <= 5
# 
# we re-write the inequality as
#   25 - x1*x2*x3*x4 <= 0
#
# and the equality as
#   x1^2 + x2^2 + x3^2 + x4^2 - 40 = 0
#
# x0 = (1,5,5,1)
#
# Optimal solution = (1.00000000, 4.74299963, 3.82114998, 1.37940829)
#
# CHANGELOG:
#   05/05/2014: Changed example to use unit testing framework testthat.


context("HS071")

test_that( "Test HS071.", {
    
    #
    # f(x) = x1*x4*(x1 + x2 + x3) + x3
    #
    eval_f <- function( x ) { 
        return( list( "objective" = x[1]*x[4]*(x[1] + x[2] + x[3]) + x[3],
                      "gradient" = c( x[1] * x[4] + x[4] * (x[1] + x[2] + x[3]),
                                      x[1] * x[4],
                                      x[1] * x[4] + 1.0,
                                      x[1] * (x[1] + x[2] + x[3]) ) ) ) 
    }
    
    # Inequality constraints.
    eval_g_ineq <- function( x ) {
        constr <- c( 25 - x[1] * x[2] * x[3] * x[4] )
                     
        grad   <- c( -x[2]*x[3]*x[4],                       
                     -x[1]*x[3]*x[4],
                     -x[1]*x[2]*x[4],
                     -x[1]*x[2]*x[3] )
        return( list( "constraints"=constr, "jacobian"=grad ) )
    }
    
    # Equality constraints.
    eval_g_eq <- function( x ) {
        constr <- c( x[1]^2 + x[2]^2 + x[3]^2 + x[4]^2 - 40 )
                     
        grad   <- c(  2.0*x[1],
                      2.0*x[2],
                      2.0*x[3],
                      2.0*x[4] )
        return( list( "constraints"=constr, "jacobian"=grad ) )
    }
    
    # Initial values.
    x0 <- c( 1, 5, 5, 1 )
    
    # Lower and upper bounds of control.
    lb <- c( 1, 1, 1, 1 )
    ub <- c( 5, 5, 5, 5 )
    
    # Optimal solution.
    solution.opt <- c(1.00000000, 4.74299963, 3.82114998, 1.37940829)
    
    # Set optimization options.
    local_opts <- list( "algorithm" = "NLOPT_LD_MMA",
                        "xtol_rel"  = 1.0e-7 )
    opts <- list( "algorithm"   = "NLOPT_LD_AUGLAG",
                  "xtol_rel"    = 1.0e-7,
                  "maxeval"     = 1000,
                  "local_opts"  = local_opts,
                  "print_level" = 0 )
    
    # Do optimization.
    res <- nloptr( x0          = x0, 
                   eval_f      = eval_f, 
                   lb          = lb, 
                   ub          = ub, 
                   eval_g_ineq = eval_g_ineq, 
                   eval_g_eq   = eval_g_eq, 
                   opts        = opts )
    
    # Run some checks on the optimal solution.
    expect_that( res$solution, equals( solution.opt, tolerance = 1e-6 ) )
    expect_that( all( res$solution >= lb ), is_true() )
    expect_that( all( res$solution <= ub ), is_true() )
    
    # Check whether constraints are violated (up to specified tolerance).
    expect_that( eval_g_ineq( res$solution )$constr <= res$options$tol_constraints_ineq, is_true() )
    expect_that( eval_g_eq( res$solution )$constr, equals( 0, tolerance = res$options$tol_constraints_eq ) )
} )
