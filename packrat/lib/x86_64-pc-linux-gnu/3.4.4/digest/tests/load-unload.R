## ## Test that package can be loaded and unloaded cleanly

## dlls0 <- getLoadedDLLs()
## stopifnot(!is.element("digest", loadedNamespaces()),
##           !is.element("digest", names(dlls0)))

## loadNamespace("digest")
## dlls1 <- getLoadedDLLs()
## stopifnot(length(dlls1) == length(dlls0) + 1,
##           is.element("digest", names(dlls1)))

## unloadNamespace("digest")
## dlls2 <- getLoadedDLLs()
## stopifnot(length(dlls2) == length(dlls0),
##           !is.element("digest", names(dlls2)))

## library("digest")
## dlls3 <- getLoadedDLLs()
## stopifnot(length(dlls3) == length(dlls0) + 1,
##           is.element("digest", names(dlls3)))

## unloadNamespace("digest")
## dlls4 <- getLoadedDLLs()
## stopifnot(length(dlls4) == length(dlls0),
##           !is.element("digest", names(dlls4)))
