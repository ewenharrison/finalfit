library(lme4)
## utility for timing individual tests
testfiles <- setdiff(list.files(pattern="*\\.[Rr]$"),"test_times.R")
tmpf <- function(x) {
    cat("*** ",x,"\n")
    system.time(local(source(x,echo=FALSE)))["elapsed"]
}
fn <- "test_timings.RData"
if (!file.exists(fn)) {
    times <- sapply(testfiles,tmpf)
    save("times",file=fn)
}
if (FALSE) {
    load(fn)
    pdf("timings.pdf",width=5,height=8)
    dotchart(sort(times))
    abline(v=c(2,10))
    dev.off()
}
