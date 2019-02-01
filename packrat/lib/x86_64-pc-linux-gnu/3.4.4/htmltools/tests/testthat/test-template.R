context("templates")

# Searches for an html dependency of format name[version], as in "d3[3.5.10]",
# within the html-dependencies script tag
findDep <- function(x, name, version) {
  deps <- sub(
    '.*<script type="application/html-dependencies">([^<]*)</script>.*',
    "\\1",
    x
  )
  grepl(paste0(name, "[", version, "]"), deps, fixed = TRUE)
}

test_that("Code blocks are evaluated and rendered correctly", {
  template <- htmlTemplate("template-document.html",
    x = div(class = "foo", "bar")
  )
  html <- renderDocument(template)

  expect_true(grepl('<div class="foo">bar</div>', html))

  # With text_ argument
  template <- htmlTemplate(text_ = "a {{ foo + 1 }} b", foo = 10)
  expect_identical(as.character(as.character(template)), "a \n11\n b")

  # Make char vectors are pasted together
  template <- htmlTemplate(text_ = c("a", "{{ foo + 1 }} b"), foo = 10)
  expect_identical(as.character(as.character(template)), "a\n\n11\n b")
})

test_that("UTF-8 characters in templates", {
  template <- htmlTemplate("template-document.html", x = "")
  html <- renderDocument(template)

  # Create the string 'Δ★😎', making sure it's UTF-8 encoded on all platforms.
  # These characters are 2, 3, and 4 bytes long, respectively.
  pat <- rawToChar(as.raw(c(0xce, 0x94, 0xe2, 0x98, 0x85, 0xf0, 0x9f, 0x98, 0x8e)))
  Encoding(pat) <- "UTF-8"
  expect_true(grepl(pat, html))

  # If template is passed text_ argument, make sure it's converted from native
  # to UTF-8.
  latin1_str <- rawToChar(as.raw(0xFF))
  Encoding(latin1_str) <- "latin1"
  text <- as.character(htmlTemplate(text_ = latin1_str))
  expect_identical(charToRaw(text), as.raw(c(0xc3, 0xbf)))
})

test_that("UTF-8 characters in template head but not body", {
  # On Windows, a string with "中文" will automatically be marked as UTF-8.
  ui <- tagList(
    tags$head(tags$script("alert('中文')")),
    "test"
  )
  html <- htmlTemplate("template-basic.html", body = ui)
  res <- renderDocument(html)
  expect_identical(Encoding(res), "UTF-8")
  expect_true(grepl("中文", res, fixed = TRUE))

  # On Windows, a string with "á" will automatically be marked as latin1.
  ui <- tagList(
    tags$head(tags$script("alert('á')")),
    "test"
  )
  html <- htmlTemplate("template-basic.html", body = ui)
  res <- renderDocument(html)
  expect_identical(Encoding(res), "UTF-8")
  expect_true(grepl("á", res, fixed = TRUE))
})

test_that("Dependencies are added properly", {
  dep <- htmlDependency("d3", "3.5.10", c(href="shared"), script = "d3.js")

  # Add dependency by inserting a tag with a dependency
  template <- htmlTemplate("template-document.html",
    x = attachDependencies(div(), dep)
  )
  html <- renderDocument(template)
  expect_true(findDep(html, "d3", "3.5.10"))
  expect_true(grepl('<script src="shared/d3.js"></script>', html, fixed = TRUE))

  # Add dependency via a renderDocument
  template <- htmlTemplate("template-document.html", x = "")
  html <- renderDocument(template, dep)
  expect_true(findDep(html, "d3", "3.5.10"))
  expect_true(grepl('<script src="shared/d3.js"></script>', html, fixed = TRUE))
})


test_that("Dependencies can be suppressed", {
  # The template includes suppressDependencies("jquery"), so we shouldn't see
  # this dependency in the final output.
  dep <- htmlDependency("jquery", "1.11.3", c(href="shared"), script = "jquery.js")

  # Add dependency by inserting a tag with a dependency
  template <- htmlTemplate("template-document.html",
    x = attachDependencies(div(), dep)
  )
  html <- renderDocument(template)
  expect_true(findDep(html, "jquery", "9999"))
  expect_false(grepl('<script[^>]+jquery[^>]+>', html))

  # Add dependency via a renderDocument
  template <- htmlTemplate("template-document.html", x = "")
  html <- renderDocument(template, dep)
  expect_true(findDep(html, "jquery", "9999"))
  expect_false(grepl('<script[^>]+jquery[^>]+>', html))
})

test_that("Errors for mismatched brackets", {
  # Error if unmatched opening brackets
  expect_error(htmlTemplate(text_ = "text {{ code"))
  # No error if we didn't open a code block
  expect_identical(
    as.character(htmlTemplate(text_ = "code }} text")),
    "code }} text"
  )

  # Error if unmatched brackets, when no leading or trailing space
  expect_error(htmlTemplate(text_ = "{{ code"))
  # No error if we didn't open a code block
  expect_identical(
    as.character(htmlTemplate(text_ = "code }}")),
    "code }}"
  )

})

test_that("Brackets at start or end of text", {
  # Code and text
  expect_identical(
    as.character(htmlTemplate(text_ = "text {{ code }} text", code = 1)),
    "text \n1\n text"
  )
  expect_identical(
    as.character(htmlTemplate(text_ = "text{{code}}text", code = 1)),
    "text\n1\ntext"
  )

  # No brackets
  expect_identical(
    as.character(htmlTemplate(text_ = "text", code = 1)),
    "text"
  )

  # No leading or trailing text
  expect_identical(
    as.character(htmlTemplate(text_ = "{{ code }}", code = 1)),
    "1"
  )
  expect_identical(
    as.character(htmlTemplate(text_ = " {{ code }}", code = 1)),
    " \n1"
  )
  expect_identical(
    as.character(htmlTemplate(text_ = "{{ code }} ", code = 1)),
    "1\n "
  )

  # Edge cases
  expect_identical(as.character(htmlTemplate(text_ = "")), "")
  expect_identical(as.character(htmlTemplate(text_ = "X")), "X")
  expect_identical(as.character(htmlTemplate(text_ = " ")), " ")
  expect_identical(as.character(htmlTemplate(text_ = "{{}}")), "")
  expect_identical(as.character(htmlTemplate(text_ = " {{}} ")), " \n ")
  expect_identical(as.character(htmlTemplate(text_ = "{{ }}")), "")
  expect_identical(as.character(htmlTemplate(text_ = "{{}}{{}}")), "")
  expect_identical(as.character(htmlTemplate(text_ = "{{1}}{{2}}")), "1\n2")
  expect_error(as.character(htmlTemplate(text_ = "{{")))
  expect_error(as.character(htmlTemplate(text_ = " {{")))
  expect_error(as.character(htmlTemplate(text_ = "{{ ")))
  expect_identical(as.character(htmlTemplate(text_ = "}}")), "}}")
  expect_identical(as.character(htmlTemplate(text_ = " }}")), " }}")
  expect_identical(as.character(htmlTemplate(text_ = "}} ")), "}} ")
})


test_that("Template DFA edge cases", {
  # Single quotes
  expect_identical(as.character(htmlTemplate(text_ = "{{ '' }}")), "")
  expect_identical(as.character(htmlTemplate(text_ = " {{ '' }} ")), " \n\n ")
  expect_identical(as.character(htmlTemplate(text_ = "{{ '\\'' }}")), "'")
  expect_identical(as.character(htmlTemplate(text_ = "{{ '\\\\' }}")), "\\")
  expect_identical(as.character(htmlTemplate(text_ = "{{ '}}' }}")), "}}")

  # Double quotes
  expect_identical(as.character(htmlTemplate(text_ = '{{ "" }}')), '')
  expect_identical(as.character(htmlTemplate(text_ = ' {{ "" }} ')), ' \n\n ')
  expect_identical(as.character(htmlTemplate(text_ = '{{ "\\"" }}')), '"')
  expect_identical(as.character(htmlTemplate(text_ = '{{ "\\\\" }}')), '\\')
  expect_identical(as.character(htmlTemplate(text_ = '{{ "}}" }}')), '}}')

  # Backticks in code
  expect_identical(as.character(htmlTemplate(text_ = "{{ `}}`<-1 }}")), "1")
  expect_identical(as.character(htmlTemplate(text_ = "{{ `x\\`x`<-1 }}")), "1")


  # Percent operator - various delimiters in percent operator
  expect_identical(
    as.character(htmlTemplate(text_ = "a{{ `%'%` <- function(x, y) 1; 2 %'% 3 }}b")),
    "a\n1\nb"
  )
  expect_identical(
    as.character(htmlTemplate(text_ = "a{{ `%}}%` <- function(x, y) 1; 2 %}}% 3 }}b")),
    "a\n1\nb"
  )

  # Comments
  expect_identical(
    as.character(htmlTemplate(text_ = "a{{ 1 #2 }}b")),
    "a\n1\nb"
  )
  expect_identical(
    as.character(htmlTemplate(text_ = "a{{ 1 #2\n3 }}b")),
    "a\n3\nb"
  )
  expect_identical(
    as.character(htmlTemplate(text_ = "a{{ 1 #2'3 }}b")),
    "a\n1\nb"
  )
  expect_identical(
    as.character(htmlTemplate(text_ = "a{{ 1 #2}3 }}b")),
    "a\n1\nb"
  )
})
