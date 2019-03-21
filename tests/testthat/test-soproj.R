context("test-soproj")

SOcrs("+proj=laea +datum=WGS84")
test_that("multiplication works", {
  expect_error(SOproj(cbind(147, -42), "+proj=laea +datum=WGS84"), "'y' is character, did you mean?")
#  expect_error(SOproj(147), "x and y must be provided unless")
})
