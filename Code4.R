
library(dplyr)                          
library(openxlsx)                      
library(nnet) 
library(readxl)
Canadian_Health <- read_excel("Proj_data.xlsx", sheet = "Canadian Perceived Health")
Indigenous_Health <- read_excel("Proj_data.xlsx", sheet = "Indigenous Perceived Health")
Conv_Data<-read_excel("Proj_data.xlsx", sheet = "Sheet1")

Health_Data  <- bind_rows(
  mutate(Indigenous_Health, Group = "Indigenous"),
  mutate(Canadian_Health, Group = "Canadian")
)

Perceived <- Health_Data %>%                         
  filter(Gender != "Both") %>%
  mutate(
    Percentage = Percentage / 100,
    Year = factor(Year),
    Gender = factor(Gender, levels = c("Women", "Men")),
    Group = factor(Group, levels = c("Canadian", "Indigenous")),
    Category = factor(Category, levels = c("Fair+Poor", "Good", "Excellent+Very good"),
                      ordered = TRUE)
  )

Ex_Data <- Perceived %>%
  filter(Category == "Excellent+Very good") %>%
  mutate(
    Year = factor(Year),
    Gender = factor(Gender, levels = c("Women", "Men")),
    Group = factor(Group, levels = c("Canadian", "Indigenous"))
  )

names(Conv_Data_F)[3]<-paste("Ex_G")
names(Conv_Data_F)[4]<-paste("F_P")

Conv_Data_F<- Conv_Data %>%
  mutate(
    Year = factor(Year),
    Period = factor(Period, levels =c("Pre","Post")),
    Gender = factor(Gender, levels = c("Women", "Men")),
    Group = factor(Group, levels = c("Canadian", "Indigenous"))
  )

model1<- glm(cbind(Count,Total-Count)~Gender*Group*Year, family=binomial, data=Ex_Data)
summary(model1)
model2<- glm(cbind(Count,Total-Count)~Gender*Group+Year, family=binomial, data=Ex_Data)
anova(model2,model1)
model3<- glm(cbind(Count,Total-Count)~Gender+Group+Year, family=binomial, data=Ex_Data)
summary(model3)
model4<- glm(cbind(Count,Total-Count)~Gender*Group*Year, family=binomial, data=Poor_Data)
anova(model2,model1, test="Chisq")
anova(model3,model1, test="Chisq")

Cov_model1 <-glm(cbind(Ex_G, F_P)~Gender*Group+Period, family=binomial, data=Conv_Data_F)
summary(Cov_model1)
Cov_model2 <-glm(cbind(Ex_G, F_P)~Gender*Group*Period, family=binomial, data=Conv_Data_F)
summary(Cov_model2)
Cov_model3 <-glm(cbind(Ex_G, F_P)~Gender*Group+Year, family=binomial, data=Conv_Data_F)
summary(Cov_model3)
Cov_model4 <-glm(cbind(Ex_G, F_P)~Gender*Group*Year, family=binomial, data=Conv_Data_F)
summary(Cov_model4)
anova(Cov_model1,Cov_model2, test="Chisq")

model1 <- multinom(Category ~ Gender * Group, data = longData)
summary(model1)
