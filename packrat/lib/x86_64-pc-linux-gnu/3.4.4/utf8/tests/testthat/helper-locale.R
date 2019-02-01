switch_ctype <- function(mode = c("C", "UTF-8"))
{
    mode <- match.arg(mode)

    if (mode == "UTF-8") {
        sysname <- Sys.info()[["sysname"]]
        if (sysname == "Windows") {
            ctype <- "English_United States.1252"
        } else if (sysname == "Darwin") {
            ctype <- "UTF-8"
        } else {
            ctype <- "en_US.utf8"
        }
    } else {
        ctype <- "C"
    }

    ctype0 <- Sys.getlocale("LC_CTYPE")
    suppressWarnings({
        Sys.setlocale("LC_CTYPE", ctype)
    })
    if (Sys.getlocale("LC_CTYPE") != ctype) {
        skip(paste0("Cannot change locale to '", ctype, "'"))
    }
    if (mode == "UTF-8" && !output_utf8()) {
        skip("Cannot change to UTF-8 output")
    }

    ctype0
}
