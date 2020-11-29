context("test-cache-utils")


test_that("the cache is deleted",{
  expect_is(community_councils(ask = FALSE, force = TRUE), "sf")
  expect_true(delete_cache("community_councils"))
  expect_is(local_authorities(ask = FALSE, force = TRUE), "sf")
  expect_true(delete_cache("local_authorities.rds"))
  delete_cache()
  expect_equal(list.files(data_dir(), pattern = "\\.rds"), character(0))
})
