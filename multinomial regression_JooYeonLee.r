
library(dplyr)                          ## Data manipulation
library(openxlsx)                       ## Open 'xlsx' Excel files
library(nnet)                           ## Multinomial regression fxns

data <- read.xlsx(file.choose())        ## Load dataL health.xlsx

md <- data %>%                          ## Data formatting and factoring
    filter(Gender != "Both") %>%
    mutate(
        Percentage = Percentage / 100,
        Year = factor(Year),
        Gender = factor(Gender, levels = c("Women", "Men")),
        Group = factor(Group, levels = c("Canadian", "Indigenous")),
        Category = factor(Category, levels = c("Fair", "Good", "Excellent"), ordered = TRUE)
    )

longData <- md %>%                      ## Convert data from wide to long
    group_by(Year, Gender, Category, Group) %>%
    summarize(response = rep(1, count)) %>%
    ungroup()


model1 <- multinom(Category ~ Gender * Group, data = longData)
modelReduced <- multinom(Category ~ Gender + Group, data = longData)

summary(model1)
anova(modelReduced, model1)
    ## Likelihood Ratio test for interaction