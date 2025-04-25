#134780 4.2 C
# This allows us to process a plumber API
api <- plumber::plumb("PlumberAPI.R")

#Run the API on a specific port ----
# Specify a constant localhost port to use
api$run(host = "127.0.0.1", port = 5022)