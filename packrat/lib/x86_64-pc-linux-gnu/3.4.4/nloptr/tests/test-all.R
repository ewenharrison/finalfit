if(require("testthat", quietly = TRUE)) {
    test_check("nloptr")
} else {
    print( "package 'testthat' not available, cannot run unit tests" )
}
