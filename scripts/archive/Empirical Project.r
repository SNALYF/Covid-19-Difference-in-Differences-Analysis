rm(list = ls())
library(stargazer)
library(readr)
library(dplyr)
WHO_COVID_19_global_data_selected <- read_csv("D:/Study/ECON 114/Emprical Project/WHO-COVID-19-global-data_selected.csv")
df <- WHO_COVID_19_global_data_selected
as.numeric(df$New_cases)
options(scipen = 100)

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

## Install ggplot2
library("ggplot2")

## Update Date as.Date
df$Date_reported <- as.Date(df$Date_reported)

## Line Graph Analysis of Daily New Cases:
dailynewcases <-ggplot(data = df,
                  aes(x = Date_reported,
                  y = New_cases,
                  group = Country,
                  color = Country)) + 
                  geom_line() +
                  scale_x_date(date_labels = "%m")
dailynewcases
           
## Line Graph Analysis of Daily New Deaths:
dailynewdeaths <-ggplot(data = df,
                         aes(x = Date_reported,
                             y = New_deaths,
                             group = Country,
                             color = Country)) + 
                             geom_line() +
                             scale_x_date(date_labels = "%m") 
dailynewdeaths

min(data$New_deaths[Country == "United States of America"])
#I notice that there is a negative new_deaths in U.S.
#This is due to the exceed deaths.

## Line Graph Analysis of Cumulative cases:
Cumulativecases <-ggplot(data = df,
                          aes(x = Date_reported,
                              y = Cumulative_cases,
                              group = Country,
                              color = Country,
                              scientific = FALSE)) + 
                              geom_line() +
                              geom_point() +
                              scale_x_date(date_labels = "%m")
Cumulativecases

## Line Graph Analysis of Cumulative Cases of population < 100 million:
data_pop_smaller_100 <- subset(df, subset=(df$populationdummy_1 == 0))
Cumulativecases2 <-ggplot(data = data_pop_smaller_100,
                         aes(x = Date_reported,
                             y = Cumulative_cases,
                             group = Country,
                             color = Country)) + 
                             geom_line() +
                             geom_point() +
                             scale_x_date(date_labels = "%m")
Cumulativecases2

## Line Graph Analysis of Cumulative Cases of population > 100 million:
data_pop_bigger_100 <- subset(df, subset=(df$populationdummy_1 == 1))
Cumulativecases3 <-ggplot(data = data_pop_bigger_100,
                          aes(x = Date_reported,
                              y = Cumulative_cases,
                              group = Country,
                              color = Country)) + 
                              geom_line() +
                              scale_x_date(date_labels = "%m")
Cumulativecases3

## Test DIF-IN-DIF: (Wrong Section, try to fix)
      ## capital new cases ... Run regression of outcome, 
      ## The regression is y_it = beta_0 + beta_1*After_i + beta_2*Treatment_i + beta_3(After_i * Treatment_i) + mu_it
      didreg <- lm(df$`New_cases/100` ~ df$after + df$mandatory + I(df$after*df$mandatory), na.action = na.exclude)
      didreg1 <- lm (df$`New_cases/100` ~ df$after*df$mandatory)
      df1 <- subset(df, subset=(df$Country != "China"))
      didreg3 <- lm(df1$`New_cases/100` ~ df1$after*df1$optional)
      summary(didreg3)
      didreg$coef
      stargazer(didreg3, type = "text")
      
      ## The Regression is y_it = beta_0 + beta_1*China_i + beta_2*India_i + beta_3*after_i + beta_4*treat_i*after_i + mu_it
      didreg2 <- lm(df$`New_cases/100` ~ df$China + df$India + df$Canada + df$UK + df$US + df$Japan + I(df$after*df$mandatory) + I(df$after*df$optional) + df$after)
      didreg3 <- lm(df$`Cumulative_cases/100` ~ df$China + df$India + df$Canada + df$UK + df$US + df$Japan + I(df$after*df$mandatory) + I(df$after*df$optional) + df$after)
      didreg4 <- lm(df$`New_deaths/100` ~ df$China + df$India + df$Canada + df$UK + df$US + df$Japan + I(df$after*df$mandatory) + I(df$after*df$optional) + df$after)
      didreg5 <- lm(df$`Cumulative_deaths/100` ~ df$China + df$India + df$Canada + df$UK + df$US + df$Japan + I(df$after*df$mandatory) + I(df$after*df$optional) + df$after)
      
      stargazer(didreg2, didreg3, didreg4, didreg5, type = "text")      
      didreg2$coef
      #I am able control time-invariant country specific factor so it will be less biased

## Generate the new varibales:
      df <- 
      ##\ggplot
      ##\geom_line
      