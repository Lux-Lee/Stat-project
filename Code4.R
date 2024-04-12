
library(dplyr)                          
library(openxlsx)                      
library(nnet) 
library(readxl)
library(ggplot2)
Canadian_Health <- read_excel("Proj_data.xlsx", sheet = "Canadian Perceived Health")
Indigenous_Health <- read_excel("Proj_data.xlsx", sheet = "Indigenous Perceived Health")
Conv_Data<-read_excel("Proj_data.xlsx", sheet = "Sheet1")
Conv_Data_R<- Conv_Data %>%
  mutate(
    Ex_G = round(Ex_G, digits=0),
    F_P = round(F_P, digits=0)
  )
Conv_Data_R <- subset(Conv_Data_R, select=-c(Total, Percentage))

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

Conv_Data_F<- Conv_Data_R %>%
  mutate(
    Year = factor(Year),
    Period = factor(Period, levels =c("Pre","Post")),
    Gender = factor(Gender, levels = c("Women", "Men")),
    Group = factor(Group, levels = c("Canadian", "Indigenous"))
  )

names(Conv_Data_F)[3]<-paste("Ex_G")
names(Conv_Data_F)[4]<-paste("F_P")

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
Cov_model5 <-glm(cbind(Ex_G, F_P)~Gender*Group*Year, family=binomial(link=probit), data=Conv_Data_F)
summary(Cov_model5)
plot(Cov_model5)
anova(Cov_model5,Cov_model4, test="Chisq")

model1 <- multinom(Category ~ Gender * Group, data = longData)
summary(model1)

library(emmeans)
glm1 <- glm(cbind(Ex_G, F_P) ~ Gender + Group * Period, data = Conv_Data_F, family = binomial)
summary(glm1)
plot(glm1)
emm1<- emmeans(glm1, ~Gender + Period + Group, type = "response")
emm2 <- emmeans(glm1, ~Gender + Group, by = "Period", type = "response")
pairs(emm2)
summary(emm1)
plot(emm2, xlab="Ex_G Proportion")
data.frame(emm1)
glm2 <- glm(cbind(Ex_G, F_P) ~ Gender * Group * Year, data = Conv_Data_F, family = binomial)
glm3 <- glm(cbind(Ex_G, F_P) ~ Gender + Group * Year, data = Conv_Data_F, family = binomial)
anova(glm3,glm2, test="Chisq")
emm2 <- emmeans(glm3, ~Gender + Group, by="Year", type="response")
pairs(emm3)
plot(emm2, xlab="EMM", ylab="Gender:Group")

emm3<- emmeans(glm1, ~ Period + Gender, by="Group", type="response")
plot(emm3)
emm4<- emmeans(glm1, ~ Period + Group, by="Gender", type="response")
plot(emm4)
emm5 <- emmeans(glm1, ~ Period + Group, type="response")
plot(emm5)
emm6 <- emmeans(glm1, ~ Period + Gender, type="response")
plot(emm6)
emm7 <- emmeans(glm1, ~ Period + Gender + Group, type="response")
plot(emm7)

glm2<- glm(formula = Ex_G ~ offset(log(total)) + Gender + Group * Period, 
           family = poisson, data = Conv_Data_F)
summary(glm2)
glm3<-glm(cbind(Ex_G, F_P) ~ Gender + Group * Period, family = quasibinomial, 
          data = Conv_Data_F)
plot(glm3)
glm4<-glm.nb(cbind(Ex_G, F_P) ~ Gender + Group * Period, 
          family = binomial, data = Conv_Data_F)
summary(glm3)
####Estimated marginal means####
##Overall change##
emmp1<- emmeans(glm2, ~Gender + Period + Group, type = "response")
pairs(emm1)
summary(emmp1)
emm1_D<- data.frame(emm1)
plot(emmp1, xlab="Ex_G Proportion", ylab="Category", color="yellow") +
  theme_black() 
##Residual plot##
f1 <- fitted(glm1)
res<-residuals(glm1, "pearson")
resd<-residuals(glm1, type="deviance")
f2<- fitted(glm2)
resp<-residuals(glm2, "deviance")
plot(f1, resp)
residuals(glm1)
Res<-data.frame(f1,res)
plot(f1,res)
ggplot(data=Res, aes(x=f1, y=res)) +
  theme_black() + 
  labs(title="Residual vs Fitted",
       x="Predicted Values", y="Residual Values") +
  geom_point(data=df,aes(x=x, y=y), color="black") +
  geom_path(data=df, aes(x=x, y=y), color="#00FF0066", size=2)+
  geom_point(color="white", aes(x=f1, y=res))+
  geom_line(y=0, col="red", aes(x=f1, y=res))
halfnorm(residuals(glm1, type="deviance"), main="Half normal QQ plot")+
  theme_black()
library(faraway)
##Polygon##
df<- data.frame(x=c(0.87,0.75,0.85), y=c(-8,0,8))
dfp<-ggplot(df, aes(x,y)) +
  theme_black()+
  labs(title="Residual vs Fitted",
       x="Predicted Values", y="Residual Values") +
  geom_point(df,color="black") +
  geom_polygon(df, fill="#00FF0066")
##Each group##
glm3 <- glm(cbind(Ex_G, F_P) ~ Gender + Period, data = subset(Conv_Data_F, Group == "Indigenous"), family = binomial)
summary(glm3)
glm3I <- glm(cbind(Ex_G, F_P) ~ Gender * Period, data = subset(Conv_Data_F, Group == "Indigenous"), family = binomial)
summary(glm3I)
anova(glm3,glm3I, test="Chisq")
glm4 <- glm(cbind(Ex_G, F_P) ~ Gender + Period, data = subset(Conv_Data_F, Group == "Canadian"), family = binomial)
summary(glm4)
glm4I <- glm(cbind(Ex_G, F_P) ~ Gender * Period, data = subset(Conv_Data_F, Group == "Canadian"), family = binomial)
summary(glm4I)
anova(glm4,glm4I, test="Chisq")
##Each group Residual plot##
plot(glm3I)
plot(glm4)
f1 <- fitted(glm2)
res<-residuals(glm2, "pearson")
Res<-data.frame(f1,res)
plot(f1,res)
##Poisson##
glm_p1<- glm(Ex_G ~ offset(log(total)) + Gender + Group * Period, data = Conv_Data_F, family = poisson)
summary(glm_p1)
glm_p2<- glm(Ex_G ~ offset(log(total)) + Gender * Group * Period, data = Conv_Data_F, family = poisson)
summary(glm_p2)
anova(glm_p1, glm_p2, test="Chisq")
plot(glm_p1)
##Poisson each group##
glm_p3<- glm(Ex_G ~ offset(log(total)) + Gender + Period, data = subset(Conv_Data_F, Group == "Canadian"), family = poisson)
summary(glm_p3)
glm_p4<- glm(Ex_G ~ offset(log(total)) + Gender, data = subset(Conv_Data_F, Group == "Indigenous"), family = poisson)
summary(glm_p4)