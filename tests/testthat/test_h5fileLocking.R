library(rhdf5)

############################################################
context("Setting file locking")
############################################################

## record the current state of this, we'll set again at the end
curr <- Sys.getenv("HDF5_USE_FILE_LOCKING")

test_that("Disabling sets value", {
  h5disableFileLocking()
  expect_equal(Sys.getenv("HDF5_USE_FILE_LOCKING"), "FALSE")
})

test_that("Enabling removes value", {
  h5enableFileLocking()
  expect_equal(Sys.getenv("HDF5_USE_FILE_LOCKING"), "")
})

## set original value for environment variable
Sys.setenv(HDF5_USE_FILE_LOCKING = curr)
