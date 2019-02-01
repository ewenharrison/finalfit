context("devices")

test_that("with_*device* functions create a plot file", {
  # A plot
  p <- lattice::xyplot(y ~ x, data.frame(x = -2:2, y = dnorm(-2:2)))

  # A directory to store the plots
  plot_dir <- tempfile("withr-test-plots-")
  dir.create(plot_dir)

  fn_names <- c("with_bmp", "with_cairo_pdf", "with_cairo_ps", "with_jpeg",
    "with_pdf", "with_png", "with_svg", "with_tiff", "with_xfig")

  fns <- mget(fn_names, envir = asNamespace("withr"))
  extensions <- c("bmp", "pdf", "ps", "jpg", "pdf", "png", "svg", "tiff", "xfig")
  for (i in seq_along(fns)) {
    filename <- file.path(plot_dir, paste0("test-", fn_names[i], ".", extensions[i]))
    info <- paste0("function = ", fn_names[i], "; filename = ", filename)
    if (fn_names[i] == "with_xfig") {
      # grDevices::xfig weirdly gives a warning with the default inputs
      expect_warning(
        fns[[i]](filename, print(p)),
        "will only return the last plot"
      )
    } else {
      expect_silent(fns[[i]](filename, print(p)))
    }
    expect_true(file.exists(filename), info = info)
    expect_gt(file.info(filename)$size, 0, label = info)
  }

  unlink(plot_dir)
})

test_that("local_device functions create a plot file", {
  # A plot
  p <- lattice::xyplot(y ~ x, data.frame(x = -2:2, y = dnorm(-2:2)))

  # A directory to store the plots
  plot_dir <- tempfile("withr-test-plots-local-")
  dir.create(plot_dir)

  fn_names <- c("local_bmp", "local_cairo_pdf", "local_cairo_ps", "local_jpeg",
    "local_pdf", "local_png", "local_svg", "local_tiff", "local_xfig")

  fns <- mget(fn_names, envir = asNamespace("withr"))
  extensions <- c("bmp", "pdf", "ps", "jpg", "pdf", "png", "svg", "tiff", "xfig")

  for (i in seq_along(fns)) {
    filename <- file.path(plot_dir, paste0("test-", fn_names[i], ".", extensions[i]))
    info <- paste0("function = ", fn_names[i], "; filename = ", filename)
    (function(i) {
      if (fn_names[i] == "local_xfig") {
        # grDevices::xfig weirdly gives a warning with the default inputs
        expect_warning(
          fns[[i]](filename),
          "will only return the last plot")
      } else {
        expect_silent(fns[[i]](filename))
      }
      print(p)
    })(i)
    expect_true(file.exists(filename), info = info)
    expect_gt(file.info(filename)$size, 0, label = info)
  }

  unlink(plot_dir)
})
