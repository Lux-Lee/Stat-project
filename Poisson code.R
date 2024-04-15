##Poisson##
glm_p1<- glm(Ex_G ~ offset(log(total)) + Gender + Group * Period, data = Conv_Data_F, family = poisson)
summary(glm_p1)
glm_p2<- glm(Ex_G ~ offset(log(total)) + Gender * Group * Period, data = Conv_Data_F, family = poisson)
summary(glm_p2)
anova(glm_p1, glm_p2, test="Chisq")
##Quasi poisson##
glm_qp1<- glm(Ex_G ~ offset(log(total)) + Gender + Group * Period, data = Conv_Data_F, family = quasipoisson)
summary(glm_qp1)
plot(glm_qp1)
##Poisson EMM plot##
emm_p1<- emmeans(glm_p1, ~Gender + Period + Group, type = "response")
pairs(emm_p1)
summary(emm_p1)
emmp1_D<- data.frame(emm_p1)
plot(emm_p1, xlab="Ex_G Proportion", ylab="Category", color="yellow") +
  theme_black() 
emm_p2<- emmeans(glm_p2, ~Gender + Period + Group, type = "response")
pairs(emm_p2)
summary(emm_p2)
emmp2_D<- data.frame(emm_p2)
plot(emm_p2, xlab="Ex_G Proportion", ylab="Category", color="yellow") +
  theme_black() 
##Poisson Each Group##
glm_p3 <- glm(Ex_G ~ offset(log(total)) + Gender + Period, data = subset(Conv_Data_F, Group == "Indigenous"), family = poisson)
summary(glm_p3)
glm_p3I <- glm(Ex_G ~ offset(log(total)) + Gender * Period, data = subset(Conv_Data_F, Group == "Indigenous"), family = poisson)
summary(glm_p3I)
anova(glm_p3,glm_p3I, test="Chisq")
glm_p4 <- glm(Ex_G ~ offset(log(total)) + Gender + Period, data = subset(Conv_Data_F, Group == "Canadian"), family = poisson)
summary(glm_p4)
glm_p4I <- glm(Ex_G ~ offset(log(total)) + Gender * Period, data = subset(Conv_Data_F, Group == "Canadian"), family = poisson)
summary(glm_p4I)
anova(glm_p4,glm_p4I, test="Chisq")
##Poisson residual plot##
f2 <- fitted(glm_p1)
res2<-residuals(glm_p1, "pearson")
Res2<-data.frame(f2,res2)
plot(f2,res)
ggplot(data=Res2, aes(x=f2, y=res2)) +
  theme_black() + 
  labs(title="Pearson Residual vs Fitted of Poisson model",
       x="Predicted Values", y="Residual Values") +
  geom_point(color="white", aes(x=f2, y=res2))+
  geom_line(y=0, col="red", aes(x=f2, y=res2))
##negative binomial##
library(MASS)
nb_glm1 <- glm.nb(Ex_G ~ offset(log(total)) + Gender + Group * Period, 
                  control=glm.control(maxit=1000), data = Conv_Data_F)
summary(nb_glm1)
nb_glm2 <- glm.nb(Ex_G ~ offset(log(total)) + Gender * Group * Period, 
                  control=glm.control(maxit=1000), data = Conv_Data_F)
summary(nb_glm2)
anova(nb_glm1, nb_glm2, test="Chisq")
##EMM and plots##
emm_nb1<- emmeans(nb_glm1, ~Gender + Period + Group)
summary(nb_glm1)
emmnb1_D<- data.frame(emm_nb1)
plot(emm_nb1, xlab= "Estimated Marginal Mean",ylab="Category", color="yellow", type="response") +
  theme_black() 
emmnb1_D<-data.frame(emmnb1_D[1:4])
emmnb1_D <-emmnb1_D %>%
  mutate(Period = factor(Period, levels =c("Pre","Post")),
         Gender = factor(Gender, levels = c("Women", "Men")),
         Group = factor(Group, levels = c("Canadian", "Indigenous"))
  )
emmnb1_D <-emmnb1_D %>%
  group_by(Gender, Group)
ggplot(data=emmnb1_D, aes(color=interaction(Gender, Group), x=Period, y=emmean)) +
  theme_black() + 
  labs(title="COVID-19 impact on Perceived Health",
       x="Period", y="Estimated Marginal Mean", color="Gender.Group") +
  geom_point() +
  geom_line(data=subset(emmnb1_D, Group=="Canadian"),aes(group=Gender)) +
  geom_line(data=subset(emmnb1_D, Group=="Indigenous"),aes(group=Gender)) 
##Negative binomial Each Group##
glm_nb3 <- glm.nb(Ex_G ~ offset(log(total)) + Gender + Period, data = subset(Conv_Data_F, Group == "Indigenous")
                  , control=glm.control(maxit=1000))
summary(glm_nb3)
glm_nb3I <- glm.nb(Ex_G ~ offset(log(total)) + Gender * Period, data = subset(Conv_Data_F, Group == "Indigenous")
                   , control=glm.control(maxit=1000), init.theta = 100)
summary(glm_nb3I)
anova(glm_nb3,glm_nb3I, test="Chisq")
glm_nb4 <- glm.nb(Ex_G ~ offset(log(total)) + Gender + Period, data = subset(Conv_Data_F, Group == "Canadian")
                  , control=glm.control(maxit=1000))
summary(glm_nb4)
glm_nb4I <- glm.nb(Ex_G ~ offset(log(total)) + Gender * Period, data = subset(Conv_Data_F, Group == "Canadian")
                   , control=glm.control(maxit=1000))
summary(glm_nb4I)
anova(glm_nb4,glm_nb4I, test="Chisq")
##negative binomial residual plot##
f3 <- fitted(nb_glm1)
res3<-residuals(nb_glm1, "pearson")
Res3<-data.frame(f3,res3)
plot(f3,res3)
ggplot(data=Res3, aes(x=f3, y=res3)) +
  theme_black() + 
  labs(title="Pearson Residual vs Fitted of Negative Binomial model",
       x="Predicted Values", y="Residual Values") +
  geom_point(color="white", aes(x=f3, y=res3))+
  geom_line(y=0, col="red", aes(x=f3, y=res3))
