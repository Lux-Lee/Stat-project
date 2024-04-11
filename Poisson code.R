##Poisson##
glm_p1<- glm(Ex_G ~ offset(log(total)) + Gender + Group * Period, data = Conv_Data_F, family = poisson)
summary(glm_p1)
glm_p2<- glm(Ex_G ~ offset(log(total)) + Gender * Group * Period, data = Conv_Data_F, family = poisson)
summary(glm_p2)
anova(glm_p1, glm_p2, test="Chisq")
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
