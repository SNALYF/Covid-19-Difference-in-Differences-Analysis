# debug_data_check.R
# Check if 'day' and 'policygroup' are valid

df <- readRDS("data/clean_data.rds")

message("Checking 'day' column:")
if ("day" %in% colnames(df)) {
  print(summary(df$day))
  print(head(df$day))
  print(class(df$day))
} else {
  message("'day' column is MISSING!")
}

message("\nChecking 'policygroup' column:")
print(table(df$policygroup, useNA = "always"))

message("\nChecking New_cases/100 for NAs:")
print(summary(df$`New_cases/100`))
