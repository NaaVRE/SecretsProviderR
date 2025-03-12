SecretsProvider <- function(secret_file_path = ".env", input_func = getPass::getPass) {
  self <- list()

  self$secret_file_path <- secret_file_path
  self$get_input <- input_func

  self$ensure_file_exists <- function() {
    if (!file.exists(self$secret_file_path)) {
      file.create(self$secret_file_path)
    }
  }

  self$ensure_file_exists()

  self$set_secret <- function(secret_name) {
    current_secret_value <- self$get_secret(secret_name)
    if (!is.null(current_secret_value)) {
      cat(paste0("\nThere is already a secret named ", secret_name, " stored as ",
                 secret_name, ". Entering a new value will overwrite the existing secret.\n"))
    }
    new_secret_value <- self$request_secret_value()
    self$store_secret(secret_name, new_secret_value)
  }

  self$read_file_to_dict <- function() {
    # Read the lines from the file
    lines <- readLines(self$secret_file_path)
    # Initialize an empty named list (dictionary)
    result_dict <- list()
    # Process each line
    for (line in lines) {
      line <- trimws(line)
      if (nchar(line) == 0) next
      parts <- strsplit(line, "=")[[1]]
      if (length(parts) == 2) {
        key <- trimws(parts[1])
        value <- gsub("'", "", trimws(parts[2]))
        result_dict[[key]] <- value
      }
    }
    result_dict
  }

  self$get_secret <- function(secret_name) {
    if (file.info(self$secret_file_path)$size == 0) {
      secret_dict <- self$read_file_to_dict()
      secret_value <- secret_dict$secret_name
    } else {
      secret_value <- NULL
    }
    if (is.null(secret_value)) {
      cat(paste0("\nA secret with name ", secret_name, " is not yet stored in ", self$secret_file_path, ".\n"))
      secret_value <- self$request_secret_value()
      self$store_secret(secret_name, secret_value)
    }
    secret_value
  }

  self$request_secret_value <- function() {
    self$get_input("Enter your Secret: ")
  }

  self$store_secret <- function(secret_name, secret_value) {
    if (nchar(trimws(secret_value)) > 0) {
      self$insert_key_value_into_file(secret_name, secret_value)
      cat(paste0("\nSecret has been stored: ", secret_name, ".\n"))
    } else {
      cat("\nNo value entered for the secret. Not storing.\n")
    }
  }

  self$write_dict_to_file <- function(dict) {
    file_conn <- file(self$secret_file_path, "w")
    for (key in names(dict)) {
      line <- paste0(key, "='", dict[[key]], "'")
      writeLines(line, file_conn)
    }
    close(file_conn)
  }

  self$insert_key_value_into_file <- function(secret_name, value) {
    existing_dict <- self$read_file_to_dict()
    existing_dict[[secret_name]] <- value
    self$write_dict_to_file(existing_dict)
  }

  self$remove_secret <- function(secret_name) {
    self$insert_key_value_into_file(secret_name, NULL)
    cat(paste0("\nSecret ", secret_name, " has been removed.\n"))
  }

  return(self)
}
