

if (utils::packageVersion("testthat") <= "1.0.2") {
    capture_output <- function(code, print = FALSE, width = 80) {
        oldwidth <- getOption("width")
        if (width != oldwidth) {
            skip_if_not(with(R.Version(), paste(major, minor, sep = "."))
                        >= "3.4.0", "Setting width fails on R < 3.4.0")
            
            options(width = width)
            on.exit(options(width = oldwidth), add = TRUE)
        }
        testthat::capture_output(code, print)
    }
}
