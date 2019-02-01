require(xtable)
V <- matrix(c(1.140380e-03,  3.010497e-05,  7.334879e-05,
              3.010497e-05,  3.320683e-04, -5.284854e-05,
              7.334879e-05, -5.284854e-05,  3.520928e-04), nrow = 3)
### Simple test of print.xtableMatharray
print.xtableMatharray(xtable(V, display = rep("E", 4)))

class(V) <- c("xtableMatharray")
class(V)

### Test without any additional arguments
mth <- xtableMatharray(V)
str(mth)
print(mth)

### Test with arguments to xtable
mth <- xtableMatharray(V, display = rep("E", 4))
str(mth)
print(mth)

mth <- xtableMatharray(V, digits = 6)
str(mth)
print(mth)

### Test with additional print.xtableMatharray arguments
mth <- xtableMatharray(V, digits = 6)
str(mth)
print(mth, format.args = list(decimal.mark = ","))
print(mth, scalebox = 0.5)
print(mth, comment = TRUE)
print(mth, timestamp = "2000-01-01")
print(mth, comment = TRUE, timestamp = "2000-01-01")





