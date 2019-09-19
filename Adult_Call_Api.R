library(plumber)
r <- plumb('Adult_Api.R')  # Where 'RegNB_Script.R' is the location of the file shown above
r$run(port=8000, swagger = TRUE)


