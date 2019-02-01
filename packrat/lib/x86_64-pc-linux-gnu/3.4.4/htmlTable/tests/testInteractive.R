library(htmlTable)

interactiveTable(matrix(c("asdsadadadas",
                          "DSASDS  asd as dasd ad ads dasd dsa ADSADASDASD"), ncol = 2),
                 minimized.columns = 2)

interactiveTable(matrix(c("asdsadadadas",
                          "DSASDS  asd as dasd ad ads dasd dsa ADSADASDASD"),
                        ncol = 2,
                        nrow = 10),
                 minimized.columns = 2,
                 button = TRUE)


knitr::knit_print(interactiveTable(matrix(c("asdsadadadas",
                                            "DSASDS  asd as dasd ad ads dasd dsa ADSADASDASD"),
                                          ncol = 2,
                                          nrow = 10),
                                   minimized.columns = 2))

htmlTable:::print.interactiveTable(
  interactiveTable(matrix(c("asdsadadadas",
                            "DSASDS  asd as dasd ad ads dasd dsa ADSADASDASD"),
                          ncol = 2,
                          nrow = 10),
                   minimized.columns = 2,
                   button = TRUE))
