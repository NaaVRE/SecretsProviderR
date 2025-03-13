\name{SecretsProvider-package}
\alias{SecretsProvider}
\docType{package}
\title{
Save and Retrieve Name-Value Pairs to and from a File
}
\description{
Saves and retrieves secrets stored in a file. 
The secrets are not encrypted in the file. 
Manually add the file to gitignore.
The secrets can also be used by Python package SecretsProvider. 
}
\details{
        Arguments:
            
            secret_file_path Path to the file where secrets are stored. Defaults to ".env".
            
            input_func Function to get input from the user. Defaults to getPass::getPass
            
        Methods:
          
          set_secret(secret_name):
            Retrieves Saves a secret
            If there is already a secret with the name given. Entering a new value will overwrite the existing secret.
          
          get_secret(secret_name):
            Retrieves secret
            If this secret does not exist, it prompts the user to enter a secret and stores that.
          
          remove_secret(secret_name):
            Remove a secret
}
\author{
LifeWatch ERIC

Maintainer: LifeWatch ERIC VLIC
}
\examples{
# Create a SecretsProvider object
secretsProvider <- SecretsProvider(secret_file_path = tempfile(), 
                    input_func = function(prompt) { "mock_input"})

# Set a secret
secretsProvider$set_secret("Data_source_API_key")

# Get a secret
api_key <- secretsProvider$get_secret("Data_source_API_key")

# Remove a secret
secretsProvider$remove_secret("Data_source_API_key")
}
