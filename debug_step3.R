# debug_step3.R
# Check if regression part works

library(stargazer)
library(lmtest)
library(sandwich)
library(dplyr)

message("Loading data...")
if (!file.exists("data/clean_data.rds")) {
  stop("File not found")
}
df <- readRDS("data/clean_data.rds")
message("Data loaded.")

# Basic DiD Test
message("Running test regression...")
tryCatch({
  ndidreg1 <- lm(df$`New_cases` ~ df$after * df$mandatory) # Use raw new cases to simpler check
  print(summary(ndidreg1))
  message("Regression ran.")
  
  stargazer(ndidreg1, type = "text")
  message("Stargazer ran.")
  
}, error = function(e) {
  message("Error in Regression/Stargazer:")
  print(e)
})
