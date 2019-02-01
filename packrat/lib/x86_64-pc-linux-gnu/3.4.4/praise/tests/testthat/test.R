
context("parts")

test_that("all parts are lowercase (except some)", {
  for (p in names(praise_parts)) {
    pp <- setdiff(praise_parts[[p]], "R package")
    expect_true( all(is_all_lowercase(pp)), info = p )
  }
})

context("templating")

test_that("template without praise word", {

  str <- "This is just a random string"
  expect_equal(praise(str), str)

  expect_equal(praise(""), "")

})

test_that("corner cases are OK", {
  praise_check("", "")
  praise_check("x", "x")
  praise_check("${adjective}", "^(?<adjective>.*)$")
})

test_that("template with a single part", {

  praise_check(
    "This is ${adjective}.",
    "^This is (?<adjective1>.*)\\.$"
  )

  praise_check(
    "This is ${adverb}.",
    "^This is (?<adverb1>.*)\\.$"
  )

  praise_check(
    "This is ${adverb_manner}.",
    "This is (?<adverb_manner1>.*)\\.$"
  )

  praise_check(
    "This was ${created}.",
    "^This was (?<created1>.*)\\.$"
  )

  praise_check(
    "This was ${creating}.",
    "^This was (?<creating1>.*)\\.$"
  )

  praise_check(
    "This was ${exclamation}.",
    "^This was (?<exclamation1>.*)\\.$"
  )

  praise_check(
    "This is a nice ${rpackage}!",
    "^This is a nice (?<rpackage1>.*)!$"
  )

})

test_that("templates with multiple parts of the same type", {

  praise_check("This is ${adjective} and ${adjective}.",
               "^This is (?<adjective1>.*) and (?<adjective2>.*)\\.$")

})

test_that("different part types in the same template", {

  praise_check(
    "You did this ${adverb_manner} and it got ${adjective}!",
    "^You did this (?<adverb_manner>.*) and it got (?<adjective>.*)!$"
  )

})

context("Capilatization")

test_that("templates are case insensitive", {

  praise_check("${AdjeCtiVe}", "^(?<adjective>.*)$")

  praise_check(
    "This is ${adjeCtive}.",
    "^This is (?<adjective1>.*)\\.$"
  )

  praise_check(
    "This is ${Adverb}.",
    "^This is (?<adverb1>.*)\\.$"
  )

  praise_check(
    "This is ${Adverb_Manner}.",
    "This is (?<adverb_manner1>.*)\\.$"
  )

  praise_check(
    "This was ${CREATED}.",
    "^This was (?<created1>.*)\\.$"
  )

  praise_check(
    "This was ${creatING}.",
    "^This was (?<creating1>.*)\\.$"
  )

  praise_check(
    "This was ${EXClamation}.",
    "^This was (?<exclamation1>.*)\\.$"
  )

  praise_check(
    "This is a nice ${rpACKage}!",
    "^This is a nice (?<rpackage1>.*)!$"
  )

  praise_check("This is ${Adjective} and ${Adjective}.",
               "^This is (?<adjective1>.*) and (?<adjective2>.*)\\.$")

  praise_check(
    "You did this ${adVERB_manner} and it got ${Adjective}!",
    "^You did this (?<adverb_manner>.*) and it got (?<adjective>.*)!$"
  )

})

test_that("first letter is capitalized", {
  for (i in 1:10) {
    pra <- praise("${Adjective}")
    expect_true(is_capitalized(pra))
    pra <- praise("${Adverb}")
    expect_true(is_capitalized(pra))
    pra <- praise("${Adverb_manner}")
    expect_true(is_capitalized(pra))
    pra <- praise("${Created}")
    expect_true(is_capitalized(pra))
    pra <- praise("${Creating}")
    expect_true(is_capitalized(pra))
    pra <- praise("${Exclamation}")
    expect_true(is_capitalized(pra))
    pra <- praise("${Rpackage}")
    expect_true(is_capitalized(pra))
  }
})

test_that("whole word is upper case", {
  for (i in 1:10) {
    pra <- praise("${ADJECTIVE}")
    expect_true(is_all_uppercase(pra), info = pra)
    pra <- praise("${ADVERB}")
    expect_true(is_all_uppercase(pra))
    pra <- praise("${ADVERB_MANNER}")
    expect_true(is_all_uppercase(pra))
    pra <- praise("${CREATED}")
    expect_true(is_all_uppercase(pra))
    pra <- praise("${CREATING}")
    expect_true(is_all_uppercase(pra))
    pra <- praise("${EXCLAMATION}")
    expect_true(is_all_uppercase(pra))
    pra <- praise("${RPACKAGE}")
    expect_true(is_all_uppercase(pra))
  }
})
