test_that("make_target works", {
  expect_equal(make_target(6, list(f1 = 1:3, f2 = 4:6))
               ,matrix(c(rep(NA_real_,3), rep(0,3)
                         ,rep(0,3),rep(NA_real_,3))
                       ,ncol = 2
                       ,dimnames = list(NULL,paste0("f",1:2))))
})
