rm(list = ls())
Sys.setlocale("LC_ALL", "English")
library(stargazer)
library(readr)
library(dplyr)
library("gridExtra")
library("cowplot")
library(lmtest)
library(zoo)
library(car)

### You may want to change the index of the CSV file.
WHO_COVID_19_global_data_selected <- read_csv("D:/Study/ECON 114/Emprical Project/WHO-COVID-19-global-data_selected.csv")
###
df <- WHO_COVID_19_global_data_selected
as.numeric(df$New_cases)
options(scipen = 1000)

##Subseting data frame for later uses.
##CANADA DATA:
attach(df)
Canada_data <- cbind(Date_reported[Country == "Canada"], 
                          New_cases[Country == "Canada"], 
                          Cumulative_cases[Country == "Canada"], 
                          New_deaths[Country == "Canada"], 
                          Cumulative_deaths[Country == "Canada"])
data_name <- cbind("Date_reported", 
                   "New_cases" , 
                    "Cumulative_cases", 
                    "New_deaths", 
                    "Cumulative_deaths")
colnames(Canada_data) <- data_name
Canada_data <- as.data.frame(Canada_data)

##China DATA:
China_data <- cbind(Date_reported[Country == "China"], 
                          New_cases[Country == "China"], 
                          Cumulative_cases[Country == "China"], 
                          New_deaths[Country == "China"], 
                          Cumulative_deaths[Country == "China"])
colnames(China_data) <- data_name
China_data <- as.data.frame(China_data)

##INDIA DATA:
India_data <- cbind(Date_reported[Country == "India"], 
                    New_cases[Country == "India"], 
                    Cumulative_cases[Country == "India"], 
                    New_deaths[Country == "India"], 
                    Cumulative_deaths[Country == "India"])
colnames(India_data) <- data_name
India_data <- as.data.frame(India_data)
##Japan DATA:
Japan_data <- cbind(Date_reported[Country == "Japan"], 
                    New_cases[Country == "Japan"], 
                    Cumulative_cases[Country == "Japan"], 
                    New_deaths[Country == "Japan"], 
                    Cumulative_deaths[Country == "Japan"])
colnames(Japan_data) <- data_name
Japan_data <- as.data.frame(Japan_data)
##UK DATA:
UK_data <- cbind(Date_reported[Country == "The United Kingdom"], 
                    New_cases[Country == "The United Kingdom"], 
                    Cumulative_cases[Country == "The United Kingdom"], 
                    New_deaths[Country == "The United Kingdom"], 
                    Cumulative_deaths[Country == "The United Kingdom"])
colnames(UK_data) <- data_name
UK_data <- as.data.frame(UK_data)
##US DATA:
US_data <- cbind(Date_reported[Country == "United States of America"], 
                 New_cases[Country == "United States of America"], 
                 Cumulative_cases[Country == "United States of America"], 
                 New_deaths[Country == "United States of America"], 
                 Cumulative_deaths[Country == "United States of America"])
colnames(US_data) <- data_name
US_data <- as.data.frame(US_data)


## Empirical part.
## Install ggplot2
library("ggplot2")

## Update Date as.Date
df$Date_reported <- as.Date(df$Date_reported)


## Line Graph Analysis of Daily New Cases:
dailynewcases1 <-ggplot(data = df,
                  aes(x = Date_reported,
                  y = New_cases,
                  group = Country,
                  color = Country)) + 
                  geom_line() +
                  scale_x_date(date_breaks = "months", date_labels = "%b") +
                  theme_minimal()
dailynewcases1

dailynewcases2 <-ggplot(data = df,
                       aes(x = Date_reported,
                           y = `New_cases/100`,
                           group = Country,
                           color = Country)) + 
  geom_line() +
  scale_x_date(date_breaks = "months", date_labels = "%b") +
  theme_minimal()
dailynewcases2

## Line Graph Analysis of Daily New Deaths:
dailynewdeaths1 <-ggplot(data = df,
                         aes(x = Date_reported,
                             y = New_deaths,
                             group = Country,
                             color = Country)) + 
                             geom_line() +
                             scale_x_date(date_breaks = "months", date_labels = "%b") +
                             theme_minimal()
dailynewdeaths1
min(df$New_deaths[Country == "United States of America"])

dailynewdeaths2 <-ggplot(data = df,
                        aes(x = Date_reported,
                            y = `New_deaths/100`,
                            group = Country,
                            color = Country)) + 
  geom_line() +
  scale_x_date(date_breaks = "months", date_labels = "%b") +
  theme_minimal()
dailynewdeaths2
#I notice that there is a negative new_deaths in U.S.
#Due to exceed deaths.

## Line Graph Analysis of Cumulative cases:
Cumulativecases1 <-ggplot(data = df,
                          aes(x = Date_reported,
                              y = Cumulative_cases,
                              group = Country,
                              color = Country,
                              scientific = FALSE)) + 
                              geom_line() +
  scale_x_date(date_breaks = "months", date_labels = "%b") +
                              theme_minimal()
Cumulativecases1

Cumulativecases2 <-ggplot(data = df,
                         aes(x = Date_reported,
                             y = `Cumulative_cases/100`,
                             group = Country,
                             color = Country,
                             scientific = FALSE)) + 
  geom_line() +
  scale_x_date(date_breaks = "months", date_labels = "%b") +
  theme_minimal()
Cumulativecases2

## Line Graph Analysis of Cumulative deaths:
Cumulativedeaths1 <-ggplot(data = df,
                         aes(x = Date_reported,
                             y = Cumulative_deaths,
                             group = Country,
                             color = Country,
                             scientific = FALSE)) + 
  geom_line() +
  scale_x_date(date_breaks = "months", date_labels = "%b") +
  theme_minimal()
Cumulativedeaths1

Cumulativedeaths2 <-ggplot(data = df,
                          aes(x = Date_reported,
                              y = `Cumulative_deaths/100`,
                              group = Country,
                              color = Country,
                              scientific = FALSE)) + 
  geom_line() +
  scale_x_date(date_breaks = "months", date_labels = "%b") +
  theme_minimal()
Cumulativedeaths2

## Overall graph:
  ## Normal:
    plot_grid(dailynewcases1, dailynewdeaths1, Cumulativecases1, Cumulativedeaths1, labels=c("1", "3", "2", "4"), ncol = 2, nrow = 2)
  ## Per 1 million people
    plot_grid(dailynewcases2, dailynewdeaths2, Cumulativecases2, Cumulativedeaths2, labels=c("1", "3", "2", "4"), ncol = 2, nrow = 2)
  ## Per 1 million people
    
## Test DIF-IN-DIF: 
    ## Test DIF-IN-DIF:
    ## The regression is y_it = beta_0 + beta_1*After_i + beta_2*Treatment_i + beta_3(After_i * Treatment_i) + mu_it
    
    ## Equation (1):
    ndidreg1 <- lm (df$`New_cases/100` ~ df$after*df$mandatory)
    ndidreg2 <- lm (df$`Cumulative_cases/100` ~ df$after*df$mandatory)
    ndidreg3 <- lm (df$`New_deaths/100` ~ df$after*df$mandatory)
    ndidreg4 <- lm (df$`Cumulative_deaths/100` ~ df$after*df$mandatory)
    
    ##Test in robust-standard error
    coeftest(ndidreg1, vcov = hccm)
    coeftest(ndidreg2, vcov = hccm)
    coeftest(ndidreg3, vcov = hccm)
    coeftest(ndidreg4, vcov = hccm)
    
    ##Output result in Latex:
    stargazer(ndidreg1, ndidreg2, ndidreg3, ndidreg4, type = "latex", single.row = TRUE, no.space = TRUE, column.sep.width = "0.01pt", font.size = "scriptsize")
    ##Output result in normal text form:
    stargazer(ndidreg1, ndidreg2, ndidreg3, ndidreg4, type = "text")
    ##Stargazer does not output long variable names, so I manually adjusted it in latex.
        
    ## Equation (2):
    ## Set up environment:
    df1 <- subset(df, subset=(df$nocontrol != 1))
    ## Regression estimation:
    ndidreg5 <- lm (df1$`New_cases/100` ~ df1$after*df1$mandatory)
    ndidreg6 <- lm (df1$`Cumulative_cases/100` ~ df1$after*df1$mandatory)
    ndidreg7 <- lm (df1$`New_deaths/100` ~ df1$after*df1$mandatory)
    ndidreg8 <- lm (df1$`Cumulative_deaths/100` ~ df1$after*df1$mandatory)
    
    ##Test in robust-standard error
    coeftest(ndidreg5, vcov = hccm)
    coeftest(ndidreg6, vcov = hccm)
    coeftest(ndidreg7, vcov = hccm)
    coeftest(ndidreg8, vcov = hccm)
    
    ##Output result in Latex:
    stargazer(ndidreg5, ndidreg6, ndidreg7, ndidreg8, type = "latex", single.row = TRUE, no.space = TRUE, column.sep.width = "0.01pt", font.size = "scriptsize")
    ##Output result in normal text form:
    stargazer(ndidreg5, ndidreg6, ndidreg7, ndidreg8, type = "text")
    ##Stargazer does not output long variable names, so I manually adjusted it in latex.
    
    ##Equation (3):
    df2 <- subset(df, subset=(df$mandatory != 1))
    ndidreg9 <- lm(df2$`New_cases/100` ~ df2$after*df2$optional)
    ndidreg10 <- lm(df2$`Cumulative_cases/100`~ df2$after*df2$optional)
    ndidreg11 <- lm(df2$`New_deaths/100` ~ df2$after*df2$optional)
    ndidreg12 <- lm(df2$`Cumulative_deaths/100` ~ df2$after*df2$optional)
    ##Output result in Latex:
    stargazer(ndidreg9, ndidreg10, ndidreg11, ndidreg12, type = "latex", single.row = TRUE, no.space = TRUE, column.sep.width = "0.01pt", font.size = "scriptsize")
    ##Output result in normal text form:
    stargazer(ndidreg9, ndidreg10, ndidreg11, ndidreg12, type = "text")
    ##Stargazer does not output long variable names, so I manually adjusted it in latex.
    
    ##Test in robust-standard error
    coeftest(ndidreg9, vcov = hccm)
    coeftest(ndidreg10, vcov = hccm)
    coeftest(ndidreg11, vcov = hccm)
    coeftest(ndidreg12, vcov = hccm)
    
    ##Test the reason why cumulative deaths are higher in optional-policy country
    df3 <- subset(df, subset=(df$mandatory != 1))
    df3 <- subset(df, subset=(df$Country != "India"))
    ##Test reg 1 exclude the country "India"
    testreg1 <- lm(df3$`Cumulative_deaths/100` ~ df3$after*df3$optional)
    stargazer(testreg1 ,type = "text")
    
    ##test reg2 environment set-up
    df4 <- subset(df, subset=(df$mandatory != 1))
    df4 <- subset(df, subset=(df$Country_code != "US"))
    ##Test reg 2 exclude the country "United States of America"
    testreg2 <- lm(df4$`Cumulative_deaths/100` ~ df4$after*df4$optional)
    stargazer(testreg2 ,type = "text")
    
    ## Graph Equation normal DID line trends:
    ## Set up environment:
    df$policygroup <- ifelse(df$mandatory == 1, 1, ifelse(df$optional == 1, 2, ifelse(df$nocontrol == 1, 3, 0)))
    ##Graph part
    attach(df)
    equation1graph <- ggplot(df, aes(day, 
                                     `New_cases/100`,
                                     color = policygroup,
                                     group = policygroup)) +
      geom_vline(xintercept = 0, linetype = 4) + 
      geom_vline(xintercept = 50, linetype = 4, color = "red") + 
      geom_smooth() +
      theme_minimal()
    equation1graph
    
    equation2graph <- ggplot(df, aes(day, 
                                     `Cumulative_cases/100`,
                                     color = policygroup,
                                     group = policygroup)) +
      geom_vline(xintercept = 0, linetype = 4) + 
      geom_vline(xintercept = 50, linetype = 4, color = "red") + 
      geom_smooth() +
      theme_minimal()
    equation2graph
    
    equation3graph <- ggplot(df, aes(day, 
                                     `New_deaths/100`,
                                     color = policygroup,
                                     group = policygroup)) +
      geom_vline(xintercept = 0, linetype = 4) + 
      geom_vline(xintercept = 50, linetype = 4, color = "red") + 
      geom_smooth() +
      theme_minimal()
    equation3graph
    
    equation4graph <- ggplot(df, aes(day, 
                                     `Cumulative_deaths/100`,
                                     color = policygroup,
                                     group = policygroup)) +
      geom_vline(xintercept = 0, linetype = 4) + 
      geom_vline(xintercept = 120, linetype = 4, color = "red") + 
      geom_smooth() +
      theme_minimal()
    equation4graph
    
    ## Overall graph:
    ## Normal:
    plot_grid(equation1graph, equation2graph, equation3graph, equation4graph, labels = c("daily cases/million", "cumulative cases/million", "daily deaths/million", "cumulative deaths/million"), ncol = 2, nrow = 2)

    ## The Regression is y_it = beta_0 + beta_1*China_i + beta_2*India_i + beta_3*after_i + beta_4*treat_i*after_i + mu_it
    didregn1 <- lm(df$`New_cases/100` ~ df$China + df$India + df$Canada + df$UK + df$US + df$Japan + I(df$after*df$mandatory) + I(df$after*df$optional) + df$after)
    didregn2 <- lm(df$`Cumulative_cases/100` ~ df$China + df$India + df$Canada + df$UK + df$US + df$Japan + I(df$after*df$mandatory) + I(df$after*df$optional) + df$after)
    didregn3 <- lm(df$`New_deaths/100` ~ df$China + df$India + df$Canada + df$UK + df$US + df$Japan + I(df$after*df$mandatory) + I(df$after*df$optional) + df$after)
    didregn4 <- lm(df$`Cumulative_deaths/100` ~ df$China + df$India + df$Canada + df$UK + df$US + df$Japan + I(df$after*df$mandatory) + I(df$after*df$optional) + df$after)
    
    summary(didregn1)
    
    ##Test in robust-standard error
    coeftest(didregn1, vcov = hccm)
    coeftest(didregn2, vcov = hccm)
    coeftest(didregn3, vcov = hccm)
    coeftest(didregn4, vcov = hccm)
    
    ##Output result in Latex:
    stargazer(didregn1, didregn2, didregn3, didregn4, type = "latex", single.row = TRUE, no.space = TRUE, column.sep.width = "0.01pt", font.size = "scriptsize")
    ##Output result in normal text form:
    stargazer(didregn1, didregn2, didregn3, didregn4, type = "text")  
    ##R Studio can not output too many dummy variables, but constant include the effect of the Japan.
    #I am able control time-invariant country specific factor so it will be less biased
    