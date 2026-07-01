test_that("io_forward_flow keeps only strictly forward transactions", {
  iotable <- read_iotable_dummy("regional_noncompetitive_import")
  order <- io_upstreamness(iotable)

  forward <- io_forward_flow(iotable, order)
  weight <- as.matrix(forward)
  raw_weight <- as.matrix(io_industry_network(iotable)$weight)

  order_values <- as.vector(as.matrix(order))
  kept <- outer(order_values, order_values, `<`)

  expect_equal(weight[kept], raw_weight[kept])
  expect_equal(weight[!kept], rep(0, sum(!kept)))
})

test_that("io_forward_flow zeros self-loops and same-level transactions", {
  iotable <- read_iotable_dummy("regional_noncompetitive_import")
  order <- io_trophic_level(iotable)

  forward <- io_forward_flow(iotable, order)
  forward_diag <- diag(as.matrix(forward))
  expect_equal(forward_diag, rep(0, length(forward_diag)))
})

test_that("io_forward_flow runs on multiregional tables", {
  iotable <- read_iotable_dummy("multiregional_competitive_import")
  order <- io_downstreamness(iotable)
  forward <- io_forward_flow(iotable, order)
  expect_true(all(is.finite(as.matrix(forward))))
})

test_that("io_forward_flow validates order", {
  iotable <- read_iotable_dummy("regional_noncompetitive_import")
  expect_snapshot(
    io_forward_flow(iotable, dibble::dibble(1:3, .dim_names = list(x = 1:3))),
    error = TRUE
  )
})
