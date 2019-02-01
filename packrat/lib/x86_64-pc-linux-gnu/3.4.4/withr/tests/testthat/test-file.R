context("file")

describe("with_file", {
  it("can use unnamed arguments", {
    with_file("file1", {
      writeLines("foo", "file1")
      expect_equal(readLines("file1"), "foo")
      with_file("file2", {
        writeLines("bar", "file2")
        expect_equal(readLines("file1"), "foo")
        expect_equal(readLines("file2"), "bar")
      })
      expect_false(file.exists("file2"))
    })
    expect_false(file.exists("file1"))
  })

  it("can use named arguments", {
    with_file(list("file1" = writeLines("foo", "file1")), {
      expect_equal(readLines("file1"), "foo")
      with_file(list("file2" = writeLines("bar", "file2")), {
        expect_equal(readLines("file1"), "foo")
        expect_equal(readLines("file2"), "bar")
      })
      expect_false(file.exists("file2"))
    })
    expect_false(file.exists("file1"))
  })
  it("works with multiple files", {
    with_file(
      list("file1" = writeLines("foo", "file1"),
           "file2",
           "file3" = writeLines("baz", "file3")), {

      writeLines("bar", "file2")
      expect_equal(readLines("file1"), "foo")
      expect_equal(readLines("file2"), "bar")
      expect_equal(readLines("file3"), "baz")
    })
    expect_false(file.exists("file1"))
    expect_false(file.exists("file2"))
    expect_false(file.exists("file3"))
  })
})

describe("local_file", {
  it("works with unnamed arguments", {
    f <- function() {
      local_file("file1")
      writeLines("foo", "file1")
      expect_equal(readLines("file1"), "foo")
    }
    expect_no_output(f())
    expect_false(file.exists("file1"))
  })

  it("works with named arguments", {
    f <- function() {
      local_file(list("file1" = writeLines("foo", "file1")))
      expect_equal(readLines("file1"), "foo")
    }
    expect_no_output(f())
    expect_false(file.exists("file1"))
  })
})
