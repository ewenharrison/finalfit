# These test cases check that interweave
# works for a variety of situations

a <- 1  # Comment after an expression
b <- 2

{
  a
  b
}

# Here is a comment which should be followed
# by two new lines

{
  print(a)  # comment in a block
  print(b)
}

a; b

a; b # Comment
