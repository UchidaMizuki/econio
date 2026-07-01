test_that("io_spectral_embedding returns the Fiedler vector", {
  for (name in c(
    "regional_noncompetitive_import",
    "regional_competitive_import"
  )) {
    iotable <- read_iotable_dummy(name)

    embedding <- io_spectral_embedding(iotable)
    h <- as.numeric(as.matrix(embedding))

    expect_true(all(is.finite(h)))
    # The Fiedler vector is orthogonal to the constant vector.
    expect_equal(sum(h), 0)

    laplacian <- io_industry_network(iotable)$laplacian
    eigenvalues <- sort(eigen(laplacian, symmetric = TRUE)$values)
    # A unit eigenvector's Dirichlet energy equals its eigenvalue; the Fiedler
    # vector attains the smallest non-zero one.
    expect_equal(as.numeric(t(h) %*% laplacian %*% h), eigenvalues[[2]])
  }
})

test_that("io_spectral_embedding returns an industry by component array", {
  iotable <- read_iotable_dummy("regional_noncompetitive_import")
  embedding <- io_spectral_embedding(iotable, dims = 2)
  dim_names <- dimnames(embedding)
  expect_equal(names(dim_names), c("industry", "component"))
  expect_length(dim_names$component, 2)
})

test_that("io_spectral_embedding runs on multiregional tables", {
  iotable <- read_iotable_dummy("multiregional_competitive_import")
  h <- as.numeric(as.matrix(io_spectral_embedding(iotable)))
  expect_true(all(is.finite(h)))
})

test_that("io_spectral_embedding validates dims", {
  iotable <- read_iotable_dummy("regional_noncompetitive_import")
  expect_snapshot(io_spectral_embedding(iotable, dims = 0), error = TRUE)
})
