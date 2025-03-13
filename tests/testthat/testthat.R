devtools::load_all()

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
  secrets_provider <- SecretsProvider(input_func = mock_input, secret_file_path = test_env_path)

  secrets_provider$set_secret(secret1_name)

  expect_equal(0, length(secrets_provider$read_file_to_dict()))

  unlink(test_env_path)
})

test_that("test_set_secret_space_input", {
  secret1_name <- "secret1"
  mock_input <- mock_input_factory(" ")
  secrets_provider <- SecretsProvider(input_func = mock_input, secret_file_path = test_env_path)

  secrets_provider$set_secret(secret1_name)

  expect_equal(0, length(secrets_provider$read_file_to_dict()))

  unlink(test_env_path)
})

test_that("test_set_secret_overwrite", {
  secret1_name <- "secret1"
  secret1_original_value <- "secret1_original_value"
  secret1_new_value <- "secret1_new_value"
  mock_input <- mock_input_factory(secret1_new_value)
  secrets_provider <- SecretsProvider(input_func = mock_input, secret_file_path = test_env_path)

  secrets_provider$set_secret(secret1_original_value)
  secrets_provider$set_secret(secret1_name)

  expect_equal(secret1_new_value, secrets_provider$read_file_to_dict()[[secret1_name]])

  unlink(test_env_path)
})

test_that("test_get_secret_nonexistent", {
  secret1_name <- "secret1"
  secret1_value <- "secret1_value"
  mock_input <- mock_input_factory(secret1_value)
  secrets_provider <- SecretsProvider(input_func = mock_input, secret_file_path = test_env_path)

  secret_value <- secrets_provider$get_secret(secret1_name)

  expect_equal(secret1_value, secrets_provider$read_file_to_dict()[[secret1_name]])

  unlink(test_env_path)
})

test_that("test_get_secret_existent", {
  secret1_name <- "secret1"
  secret1_value <- "secret1_value"
  mock_value <- "mock_value"

  mock_input <- mock_input_factory(secret1_value)
  secrets_provider <- SecretsProvider(input_func = mock_input, secret_file_path = test_env_path)

  secrets_provider$set_secret(secret1_name)

  secrets_provider$input_func <- mock_input_factory(mock_value)

  expect_equal(secret1_value,   secrets_provider$get_secret(secret1_name))

  unlink(test_env_path)
})

test_that("test_remove_secret", {
  secret1_name <- "secret1"
  secret2_name <- "secret2"
  secret_value <- "secret_value"
  mock_input <- mock_input_factory(secret_value)
  secrets_provider <- SecretsProvider(input_func = mock_input, secret_file_path = test_env_path)

  secrets_provider$set_secret(secret1_name)
  secrets_provider$set_secret(secret2_name)
  secrets_provider$remove_secret(secret1_name)

  expect_equal(1, length(secrets_provider$read_file_to_dict()))

  unlink(test_env_path)
})

test_that("test_remove_last_secret", {
  secret1_name <- "secret1"
  secret_value <- "secret_value"
  mock_input <- mock_input_factory(secret_value)
  secrets_provider <- SecretsProvider(input_func = mock_input, secret_file_path = test_env_path)

  secrets_provider$set_secret(secret1_name)
  secrets_provider$remove_secret(secret1_name)

  expect_equal(0, length(secrets_provider$read_file_to_dict()))

  unlink(test_env_path)
})

test_that("test_get_secret_does_not_overwrite_previous_value", {
  secret1_name <- "secret1"
  secret_value <- "secret_original_value"
  secret_new_value <- "secret_value_new_input_func"

  mock_input <- mock_input_factory(secret_value)
  secrets_provider <- SecretsProvider(input_func = mock_input, secret_file_path = test_env_path)
  secrets_provider$get_secret(secret1_name)

  secrets_provider$input_func <- mock_input_factory(secret_new_value)
  secrets_provider$get_secret(secret1_name)

  expect_equal(secret_value, secrets_provider$read_file_to_dict()[[secret1_name]])

  unlink(test_env_path)
})
