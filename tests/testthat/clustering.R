library(gtBase)

T <- function(expr, type = "base::int", attribute = a) {
  attribute <- as.character(substitute(attribute))
  expr <- substitute(expr)
  data <- list(schema = character())
  data$schema[[attribute]] <- attribute
  grokit <<- new.env()
  grokit$cluster <- list()
  grokit$cluster[[attribute]] <- list(lower = -Inf, upper = Inf, type = type)
  update.clustering(expr, data)
  interval <- as.numeric(grokit$cluster[[attribute]][1:2])
  gtBase:::.reset()
  interval
}

compare <- function(expr, interval) {
  expr <- substitute(expr)
  info <- eval(expr)
  eval(call("expect_equal", expr, interval, info = paste("LHS: ", deparse(info))))
}

## default clustering attribute is `a`
test_that("clustering is correct", {
  compare(T(a < 10 && b > 100), c(-Inf, 10))
  compare(T(a < 15 && a > 10), c(10, 15))
  compare(T(a < 10 && a > 15), c(15, 10))
  compare(T(15 > a && 10 < a), c(10, 15))
  compare(T(10 > a && 15 < a), c(15, 10))
  compare(T(a < 10 && a < 5 && a < 0), c(-Inf, 0))
  compare(T(a > 10 && a > 5 && a > 0), c(10, Inf))
  compare(T(a == 15), c(15, 15))
  compare(T(a < 10 && a > 15), T(a <= 10 && a >= 15))
  compare(T(a < 15 && (a == 0 || a == 1) && a > 10), c(10, 15))
})
