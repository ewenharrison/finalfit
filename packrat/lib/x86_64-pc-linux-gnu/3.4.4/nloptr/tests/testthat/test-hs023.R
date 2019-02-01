# Copyright (C) 2010 Jelmer Ypma. All Rights Reserved.
# This code is published under the L-GPL.
#
# File:   hs023.R
# Author: Jelmer Ypma
# Date:   16 August 2010
#
# Example problem, number 23 from the Hock-Schittkowsky test suite..
#
# \min_{x} x1^2 + x2^2
# s.t.
#   x1 + x2 >= 1
#   x1^2 + x2^2 >= 1
#   9*x1^2 + x2^2 >= 9
#   x1^2 - x2 >= 0
#   x2^2 - x1 >= 0
#
# with bounds on the variables
#   -50 <= x1, x2 <= 50
#
# we re-write the inequalities as
#   1 - x1 - x2 <= 0
#   1 - x1^2 - x2^2 <= 0
#   9 - 9*x1^2 - x2^2 <= 0
#   x2 - x1^2 <= 0
#   x1 - x2^2 <= 0 
#
# the initial value is
# x0 = (3, 1)
#
# Optimal solution = (1, 1)
#
# CHANGELOG:
#   05/05/2014: Changed example to use unit testing framework testthat.

context("HS023")

test_that( "Test HS023.", {
    #
    # f(x) = x1^2 + x2^2
    #
    eval_f <- function( x ) { 
        return( list( "objective" = x[1]^2 + x[2]^2,
                      "gradient" = c( 2*x[1],
                                      2*x[2] ) ) ) 
    }
    
    # Inequality constraints.
    eval_g_ineq <- function( x ) {
        constr <- c( 1 - x[1] - x[2],
                     1 - x[1]^2 - x[2]^2,
                     9 - 9*x[1]^2 - x[2]^2,
                     x[2] - x[1]^2,
                     x[1] - x[2]^2 )
                     
        grad   <- rbind( c( -1, -1 ),
                         c( -2*x[1], -2*x[2] ),
                         c( -18*x[1], -2*x[2] ),
                         c( -2*x[1], 1 ),
                         c( 1, -2*x[2] ) )
                         
        return( list( "constraints"=constr, "jacobian"=grad ) )
    }
    
    # Initial values.
    x0 <- c( 3, 1 )
    
    # Lower and upper bounds of control.
    lb <- c( -50, -50 )
    ub <- c(  50,  50 )
    
    # Optimal solution.
    solution.opt <- c( 1, 1 )
    
    # Solve with MMA.
    opts <- list( "algorithm"            = "NLOPT_LD_MMA",
                  "xtol_rel"             = 1.0e-7, 
                  "tol_constraints_ineq" = rep( 1.0e-6, 5 ),
                  "print_level"          = 0 )

    res <- nloptr(
                x0          = x0, 
                eval_f      = eval_f, 
                lb          = lb, 
                ub          = ub, 
                eval_g_ineq = eval_g_ineq, 
                opts        = opts )
    
    # Run some checks on the optimal solution.
    expect_that( res$solution, equals( solution.opt, tolerance = 1e-7 ) )
    expect_that( all( res$solution >= lb ), is_true() )
    expect_that( all( res$solution <= ub ), is_true() )
    
    # Check whether constraints are violated (up to specified tolerance).
    expect_that( all( eval_g_ineq( res$solution )$constr <= res$options$tol_constraints_ineq ), is_true() )
} )
