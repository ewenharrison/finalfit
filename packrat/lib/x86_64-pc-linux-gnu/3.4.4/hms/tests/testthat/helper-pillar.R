expect_known_pillar_shaft_display <- function(x, file, width) {
  object_quo <- rlang::quo(pillar::pillar_shaft(x))
  pillar::expect_known_display(!!object_quo, file = file.path("out", file))
}
