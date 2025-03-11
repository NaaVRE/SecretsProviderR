source("~/SecretsProviderR/R/SecretsProvider.R")

library(testthat)

# Create a temporary file for testing
test_env_path <- tempfile()

# Mock input function
mock_input_factory <- function(value) {
  function(prompt) {
    value
  }
}

test_that("test_set_secret_new_secret", {
  secret1_name <- "secret1"
  secret1_value <- "secret1_value"
  mock_input <- mock_input_factory(secret1_value)
  
  expect_equal(secret1_value, SecretsProvider(input_func = mock_input, secret_file_path = test_env_path)$get_secret(secret1_name))
  
  unlink(test_env_path)
})

test_that("test_set_secret_no_input", {
  secret1_name <- "secret1"
  mock_input <- mock_input_factory("")
  secretsProvider <- SecretsProvider(input_func = mock_input, secret_file_path = test_env_path)
  
  secretsProvider$set_secret(secret1_name)
  
  expect_equal(0, length(secretsProvider$read_file_to_dict()))
  
  unlink(test_env_path)
})

test_that("test_set_secret_space_input", {
  secret1_name <- "secret1"
  mock_input <- mock_input_factory(" ")
  secretsProvider <- SecretsProvider(input_func = mock_input, secret_file_path = test_env_path)
  
  secretsProvider$set_secret(secret1_name)
  
  expect_equal(0, length(secretsProvider$read_file_to_dict()))
  
  unlink(test_env_path)
})

test_that("test_set_secret_overwrite", {
  secret1_name <- "secret1"
  secret1_original_value <- "secret1_original_value"
  secret1_new_value <- "secret1_new_value"
  mock_input <- mock_input_factory(secret1_new_value)
  secretsProvider <- SecretsProvider(input_func = mock_input, secret_file_path = test_env_path)
  
  secretsProvider$set_secret(secret1_original_value)
  secretsProvider$set_secret(secret1_name)
  
  expect_equal(secret1_new_value, secretsProvider$read_file_to_dict()[[secret1_name]])
  
  unlink(test_env_path)
})

test_that("test_get_secret_nonexistent", {
  secret1_name <- "secret1"
  secret1_value <- "secret1_value"
  mock_input <- mock_input_factory(secret1_value)
  secretsProvider <- SecretsProvider(input_func = mock_input, secret_file_path = test_env_path)
  
  secret_value <- secretsProvider$get_secret(secret1_name)
  
  expect_equal(secret1_value, secretsProvider$read_file_to_dict()[[secret1_name]])
  
  unlink(test_env_path)
})

test_that("test_get_secret_existent", {
  secret1_name <- "secret1"
  secret1_value <- "secret1_value"
  mock_input <- mock_input_factory(secret1_value)
  secretsProvider <- SecretsProvider(input_func = mock_input, secret_file_path = test_env_path)
  
  secretsProvider$set_secret(secret1_name)
  
  expect_equal(secret1_value, secretsProvider$read_file_to_dict()[[secret1_name]])
  
  unlink(test_env_path)
})

test_that("test_remove_secret", {
  secret1_name <- "secret1"
  secret2_name <- "secret2"
  secret_value <- "secret_value"
  mock_input <- mock_input_factory(secret_value)
  secretsProvider <- SecretsProvider(input_func = mock_input, secret_file_path = test_env_path)
  
  secretsProvider$set_secret(secret1_name)
  secretsProvider$set_secret(secret2_name)
  secretsProvider$remove_secret(secret1_name)
  
  expect_equal(1, length(secretsProvider$read_file_to_dict()))
  
  unlink(test_env_path)
})

test_that("test_remove_last_secret", {
  secret1_name <- "secret1"
  secret_value <- "secret_value"
  mock_input <- mock_input_factory(secret_value)
  secretsProvider <- SecretsProvider(input_func = mock_input, secret_file_path = test_env_path)
  
  secretsProvider$set_secret(secret1_name)
  secretsProvider$remove_secret(secret1_name)

  expect_equal(0, length(secretsProvider$read_file_to_dict()))
  
  unlink(test_env_path)
})