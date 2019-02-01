# Copyright (C) 2011 Jelmer Ypma. All Rights Reserved.
# This code is published under the L-GPL.
#
# File:   test_options.R
# Author: Jelmer Ypma
# Date:   7 August 2011
#
# Test printing of options.

library('nloptr')

opts <- list( "algorithm"   = "NLOPT_LD_LBFGS",
              "xtol_rel"    = 1.0e-8,
              "print_level" = 1 )


nloptr.print.options( opts )
