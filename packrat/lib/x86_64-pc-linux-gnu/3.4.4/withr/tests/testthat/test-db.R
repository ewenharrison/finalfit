context("db")

describe("with_db_connection", {
  #it("errors if connection is not named", {
    #expect_error({
      #with_db_connection(list(TRUE), TRUE)
    #}, "all(is.named(con)) is not TRUE", fixed = TRUE)
  #})

  #it("errors if connection is not a DBI connection", {
    #expect_error({
      #with_db_connection(list(con = TRUE), TRUE)
    #}, "all(vlapply(con, methods::is, \"DBIConnection\")) is not TRUE", fixed = TRUE)
  #})

  it("creates a single connection", {
    db <- tempfile()
    on.exit(unlink(db))
    expect_false(exists("con"))
    with_db_connection(
      list(con = DBI::dbConnect(RSQLite::SQLite(), db)), {
        DBI::dbWriteTable(con, "test", data.frame(a = 1:2, b = 3:4))
      })
    expect_false(exists("con"))
    con2 <- DBI::dbConnect(RSQLite::SQLite(), db)
    on.exit(DBI::dbDisconnect(con2), add = TRUE)
    expect_equal(DBI::dbReadTable(con2, "test"), data.frame(a = 1:2, b = 3:4))
  })

  it("creates multiple connections", {
    db <- tempfile()
    db2 <- tempfile()
    on.exit(unlink(c(db, db2)))
    expect_false(exists("con"))
    expect_false(exists("con2"))
    with_db_connection(
      list(con = DBI::dbConnect(RSQLite::SQLite(), db),
           con2 = DBI::dbConnect(RSQLite::SQLite(), db2)), {
        DBI::dbWriteTable(con, "test", data.frame(a = 1:2, b = 3:4))
        DBI::dbWriteTable(con2, "test", data.frame(c = 5:6, d = 7:8))
    })
    expect_false(exists("con"))
    expect_false(exists("con2"))
    con3 <- DBI::dbConnect(RSQLite::SQLite(), db)
    con4 <- DBI::dbConnect(RSQLite::SQLite(), db2)

    on.exit({
      DBI::dbDisconnect(con3)
      DBI::dbDisconnect(con4)
    }, add = TRUE)

    expect_equal(DBI::dbReadTable(con3, "test"), data.frame(a = 1:2, b = 3:4))
    expect_equal(DBI::dbReadTable(con4, "test"), data.frame(c = 5:6, d = 7:8))
  })
})

describe("local_db_connection", {
  it("creates a single connection", {
    db <- tempfile()
    on.exit(unlink(db))
    expect_false(exists("con"))

    (function() {
      con <- local_db_connection(DBI::dbConnect(RSQLite::SQLite(), db))
      DBI::dbWriteTable(con, "test", data.frame(a = 1:2, b = 3:4))
    })()
    expect_false(exists("con"))
    con2 <- DBI::dbConnect(RSQLite::SQLite(), db)
    on.exit(DBI::dbDisconnect(con2), add = TRUE)
    expect_equal(DBI::dbReadTable(con2, "test"), data.frame(a = 1:2, b = 3:4))
  })
})
