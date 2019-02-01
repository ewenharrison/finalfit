cat("1\n")
print("2")
warning("3")
print("4")
message("5")
stop("6")
stop("7", call. = FALSE)

f <- function(x) {
  print("8")
  message("9")
  warning("10")
  stop("11")
}
f()

