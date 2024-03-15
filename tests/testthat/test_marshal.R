test_that("(un)marshal", {
  e = custom_env(1)
  cont = container(e, e)
  contm = marshal(cont)
  contr = unmarshal(contm)

  expect_true(identical(contr[[1]], contr[[2]]))
  expect_equal(cont, contr)


})
