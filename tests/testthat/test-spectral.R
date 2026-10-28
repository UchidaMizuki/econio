test_that("io_spectral_embedding with normalized = FALSE returns the Fiedler vector", {
  for (name in c(
    "regional_noncompetitive_import",
    "regional_competitive_import"
  )) {
    iotable <- read_iotable_dummy(name)

    embedding <- io_spectral_embedding(iotable, normalized = FALSE)
    h <- as.numeric(as.matrix(embedding))

    expect_true(all(is.finite(h)))
    # The Fiedler vector is orthogonal to the constant vector.
    expect_equal(sum(h), 0)

    laplacian <- io_laplacian(iotable)$laplacian
    eigenvalues <- sort(eigen(laplacian, symmetric = TRUE)$values)
    # A unit eigenvector's Dirichlet energy equals its eigenvalue; the Fiedler
    # vector attains the smallest non-zero one.
    expect_equal(as.numeric(t(h) %*% laplacian %*% h), eigenvalues[[2]])
  }
})

test_that("io_spectral_embedding solves the normalized generalized eigenproblem", {
  for (name in c(
    "regional_noncompetitive_import",
    "multiregional_competitive_import"
  )) {
    iotable <- suppressMessages(read_iotable_dummy(name))
    laplacian <- suppressMessages(io_laplacian(iotable))
    total_weight <- laplacian$in_weight + laplacian$out_weight

    embedding <- suppressMessages(io_spectral_embedding(iotable, dims = 2))
    h <- matrix(as.numeric(as.matrix(embedding)), ncol = 2)

    expect_equal(as.vector(crossprod(total_weight, h)), c(0, 0))
    expect_equal(crossprod(h, total_weight * h), diag(2))

    eigenvalue <- diag(crossprod(h, laplacian$laplacian %*% h))
    expect_equal(
      laplacian$laplacian %*% h,
      total_weight * h %*% diag(eigenvalue)
    )

    eigenvalues <- sort(Re(
      eigen(
        laplacian$laplacian / total_weight,
        only.values = TRUE
      )$values
    ))
    expect_equal(eigenvalue, eigenvalues[2:3])
    expect_gte(min(eigenvalues), -1e-12)
    expect_lte(max(eigenvalues), 2)
  }
})

test_that("io_spectral_embedding aborts when normalizing an industry with no transactions", {
  iotable <- read_iotable_dummy("regional_noncompetitive_import")
  iotable[1, 1:3] <- 0
  iotable[1:3, 1] <- 0

  expect_snapshot(io_spectral_embedding(iotable), error = TRUE)
  expect_no_error(io_spectral_embedding(iotable, normalized = FALSE))
})

test_that("io_spectral_embedding validates normalized", {
  iotable <- read_iotable_dummy("regional_noncompetitive_import")
  expect_snapshot(
    io_spectral_embedding(iotable, normalized = "yes"),
    error = TRUE
  )
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

test_that("io_spectral_embedding agrees with numerically minimizing the Dirichlet energy", {
  iotable <- read_iotable_dummy("multiregional_competitive_import")
  network <- suppressMessages(io_laplacian(iotable))
  laplacian <- network$laplacian
  total_weight <- network$in_weight + network$out_weight

  for (normalized in c(TRUE, FALSE)) {
    constraint_weight <- if (normalized) {
      total_weight
    } else {
      rep(1, nrow(laplacian))
    }

    # Minimize the Rayleigh quotient h' L h / h' D h over h orthogonal to the
    # constant vector in the D inner product.
    project <- function(z) {
      z - sum(constraint_weight * z) / sum(constraint_weight)
    }
    quotient <- function(z) {
      h <- project(z)
      sum(h * (laplacian %*% h)) / sum(constraint_weight * h^2)
    }
    gradient <- function(z) {
      h <- project(z)
      norm <- sum(constraint_weight * h^2)
      grad_h <- 2 *
        (as.vector(laplacian %*% h) - quotient(z) * constraint_weight * h) /
        norm
      grad_h - sum(grad_h) * constraint_weight / sum(constraint_weight)
    }
    set.seed(1)
    optimum <- optim(
      rnorm(nrow(laplacian)),
      quotient,
      gradient,
      method = "BFGS",
      control = list(reltol = 1e-16, maxit = 1000)
    )
    expected <- project(optimum$par)
    expected <- expected / sqrt(sum(constraint_weight * expected^2))
    expected <- expected * sign(expected[which.max(abs(expected))])

    actual <- suppressMessages(as.numeric(as.matrix(io_spectral_embedding(
      iotable,
      normalized = normalized
    ))))
    expect_equal(sum(actual * (laplacian %*% actual)), optimum$value)
    expect_equal(actual, expected, tolerance = 1e-4)
  }
})
