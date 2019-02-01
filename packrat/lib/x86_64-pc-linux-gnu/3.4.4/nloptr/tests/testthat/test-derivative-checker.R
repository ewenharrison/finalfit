# Copyright (C) 201 Jelmer Ypma. All Rights Reserved.
# This code is published under the L-GPL.
#
# File:   test-derivative-checker.R
# Author: Jelmer Ypma
# Date:   24 July 2010
#
# Example showing results of the derivative checker.
#
# Changelog:
#   27/10/2013: Changed example to use unit testing framework testthat.

context("Derivative checker")

test_that("Test derivative checker.", {
    # Define objective function.
    f <- function( x, a ) {
        return( sum( ( x - a )^2 ) )
    }
    
    # Define gradient function without errors.
    f_grad <- function( x, a ) {
        return( 2*( x - a ) )
    }
    
    res <- check.derivatives( .x=1:10, func=f, func_grad=f_grad, check_derivatives_print='none', a=runif(10) )
    expect_that( sum( res$flag_derivative_warning ), equals( 0 ) )
    
    # res <- check.derivatives( .x=1:10, func=f, func_grad=f_grad, check_derivatives_print='errors', a=runif(10) )
    # expect_that( sum( res$flag_derivative_warning ), equals( 0 ) )
    # 
    # res <- check.derivatives( .x=1:10, func=f, func_grad=f_grad, check_derivatives_print='all', a=runif(10) )
    # expect_that( sum( res$flag_derivative_warning ), equals( 0 ) )
    
    
    # Define gradient function with 1 error.
    f_grad <- function( x, a ) {
        return( 2*( x - a ) + c(0,.1,rep(0,8)) )
    }
    
    res <- check.derivatives( .x=1:10, func=f, func_grad=f_grad, check_derivatives_print='none', a=runif(10) )
    expect_that( sum( res$flag_derivative_warning ), equals( 1 ) )
    
     #res <- check.derivatives( .x=1:10, func=f, func_grad=f_grad, check_derivatives_print='errors', a=runif(10) )
     #expect_that( sum( res$flag_derivative_warning ), equals( 1 ) )
     #
     #res <- check.derivatives( .x=1:10, func=f, func_grad=f_grad, check_derivatives_print='all', a=runif(10) )
     #expect_that( sum( res$flag_derivative_warning ), equals( 1 ) )
    
    
    # Define objective function.
    g <- function( x, a ) {
        return( c( sum(x-a), sum( (x-a)^2 ) ) )
    }
    
    # Define gradient function with 2 errors.
    g_grad <- function( x, a ) {
        return( rbind( rep(1,length(x)) + c(0,.01,rep(0,8)), 2*(x-a) + c(0,.1,rep(0,8)) ) )
    }
    
    res <- check.derivatives( .x=1:10, func=g, func_grad=g_grad, check_derivatives_print='none', a=runif(10) )
    expect_that( sum( res$flag_derivative_warning ), equals( 2 ) )
    
    # res <- check.derivatives( .x=1:10, func=g, func_grad=g_grad, check_derivatives_print='errors', a=runif(10) )
    # expect_that( sum( res$flag_derivative_warning ), equals( 2 ) )
    # 
    # res <- check.derivatives( .x=1:10, func=g, func_grad=g_grad, check_derivatives_print='all', a=runif(10) )
    # expect_that( sum( res$flag_derivative_warning ), equals( 2 ) )
} )

