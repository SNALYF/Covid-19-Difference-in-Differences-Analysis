library(foreign)
library(stargazer)
mydata = read.dta("http://dss.princeton.edu/training/Panel101.dta")

mydata$time <- ifelse(mydata$year >= 1994, 1, 0)
mydata$treated = ifelse(mydata$country == "E" |
                          mydata$country == "F" |
                          mydata$country == "G", 1, 0)
mydata$did = mydata$time * mydata$treated
didreg = lm(y ~ treated + time + did, data = mydata)
stargazer(didreg, type = "text")
didreg1 = lm(y ~ treated*time, data = mydata)
stargazer(didreg1, type = "text")

## y = beta_0 + beta_1*treated + beta_2*time + beta_3*treated*time
## new_cases = beta_0 + beta_1*china + beta_2*india + beta_3*time + beta_4*treated*time