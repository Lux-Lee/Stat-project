##Load Package and Data##
library(dplyr)                          
library(openxlsx)                      
library(nnet) 
library(readxl)
library(emmeans)
library(ggplot2)

Conv_Data<-read_excel("Proj_data.xlsx", sheet = "Sheet1")
##Convert Data Format##
Conv_Data_R<- Conv_Data %>%
  mutate(
    Ex_G = round(Ex_G, digits=0),
    F_P = round(F_P, digits=0)
  )
Conv_Data_R <- subset(Conv_Data_R, select=-c(Total, Percentage))

Conv_Data_F<- Conv_Data_R %>%
  mutate(
    Year = factor(Year),
    Period = factor(Period, levels =c("Pre","Post")),
    Gender = factor(Gender, levels = c("Women", "Men")),
    Group = factor(Group, levels = c("Canadian", "Indigenous"))
  )
##Binary Response Model with main and interaction##
glm1 <- glm(cbind(Ex_G, F_P) ~ Gender + Group * Period, data = Conv_Data_F, family = binomial)
summary(glm1)
####Estimated marginal means####
##Health care disparty change in groups##
emm2 <- emmeans(glm1, ~ Group, by = "Period", type = "response")
plot(emm2, xlab="Ex_G Proportion", ylab="Category")
summary(emm2)
##Change in genders##
emm4 <- emmeans(glm1, ~ Gender, by = "Period", type = "response")
plot(emm4, xlab="Ex_G Proportion", ylab="Category")
summary(emm4)
##Gender difference change among groups##
emm3<- emmeans(glm1, ~ Gender + Period, by="Group", type="response")
plot(emm3, xlab="Ex_G Proportion", ylab="Category")
summary(emm3)
emm3_D<- data.frame(emm3)
##Overall change##
emm1<- emmeans(glm1, ~Gender + Period + Group, type = "response")
pairs(emm1)
summary(emm1)
emm1_D<- data.frame(emm1)
plot(emm1, xlab="Ex_G Proportion", ylab="Category")
##Plotting##
emm1_D<-data.frame(emm1_D[1:4])
emm1_D <-emm1_D %>%
  mutate(Period = factor(Period, levels =c("Pre","Post")),
        Gender = factor(Gender, levels = c("Women", "Men")),
        Group = factor(Group, levels = c("Canadian", "Indigenous"))
)
emm1_D <-emm1_D %>%
  group_by(Gender, Group)
ggplot(data=emm1_D, aes(color=Gender,lty=Group, x=Period, y=prob)) +
  labs(title="COVID-19 impact on Perceived Health",
       x="Period", y="Probability of Excellent_Good") +
  geom_point() +
  geom_line(data=subset(emm1_D, Group=="Canadian"),aes(group=Gender)) +
  geom_line(data=subset(emm1_D, Group=="Indigenous"),aes(group=Gender))