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
    F_P = round(F_P, digits=0),
    total = Ex_G+F_P
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
glm2 <- glm(cbind(Ex_G, F_P) ~ Gender * Group * Period, data = Conv_Data_F, family = binomial)
summary(glm2)
anova(glm1, glm2, test="Chisq")
####Estimated marginal means####
##Health care disparty change in groups##
emm2 <- emmeans(glm2, ~ Group, by = "Period", type = "response")
plot(emm2, xlab="Ex_G Proportion", ylab="Category", color="yellow") +
  theme_black() 
summary(emm2)
##Change in genders##
emm4 <- emmeans(glm2, ~ Gender + Group, by = "Period", type = "response")
plot(emm4, xlab="Ex_G Proportion", ylab="Category", color="yellow") +
  theme_black() 
summary(emm4)
plot(emmeans(glm2, ~ Group + Period, type="response"), xlab="Ex_G Proportion", 
     ylab="Category", color="yellow") +
  theme_black()
##Gender difference change among groups##
emm3<- emmeans(glm2, ~ Gender + Period, by="Group", type="response")
plot(emm3, xlab="Ex_G Proportion", ylab="Category", color="yellow") +
  theme_black() 
summary(emm3)
emm3_D<- data.frame(emm3)
##Overall change##
emm1<- emmeans(glm2, ~Gender + Period + Group, type = "response")
pairs(emm1)
summary(emm1)
emm1_D<- data.frame(emm1)
plot(emm1, xlab="Ex_G Proportion", ylab="Category", color="yellow") +
  theme_black() 
##Plotting##
emm1_D<-data.frame(emm1_D[1:4])
emm1_D <-emm1_D %>%
  mutate(Period = factor(Period, levels =c("Pre","Post")),
        Gender = factor(Gender, levels = c("Women", "Men")),
        Group = factor(Group, levels = c("Canadian", "Indigenous"))
)
emm1_D <-emm1_D %>%
  group_by(Gender, Group)
ggplot(data=emm1_D, aes(color=interaction(Gender, Group), x=Period, y=prob)) +
  theme_black() + 
  labs(title="COVID-19 impact on Perceived Health",
       x="Period", y="Probability of Excellent_Good", color="Gender.Group") +
  geom_point() +
  geom_line(data=subset(emm1_D, Group=="Canadian"),aes(group=Gender)) +
  geom_line(data=subset(emm1_D, Group=="Indigenous"),aes(group=Gender)) 
##Model fit##
pchisq(206.13,11,lower.tail = F)
##Theme##
library(gridExtra)
theme_black <- function(base_size = 12, base_family = "") {
  
  theme_grey(base_size = base_size, base_family = base_family) %+replace%
    
    theme(
      line= element_line(color="white"),
      # Specify axis options
      axis.line = element_blank(),  
      axis.text.x = element_text(size = base_size*0.8, color = "white", lineheight = 0.9),  
      axis.text.y = element_text(size = base_size*0.8, color = "white", lineheight = 0.9),  
      axis.ticks = element_line(color = "white", size  =  0.2),  
      axis.title.x = element_text(size = base_size, color = "white", margin = margin(0, 10, 0, 0)),  
      axis.title.y = element_text(size = base_size, color = "white", angle = 90, margin = margin(0, 10, 0, 0)),  
      axis.ticks.length = unit(0.3, "lines"),   
      # Specify legend options
      legend.background = element_rect(color = NA, fill = "black"),  
      legend.key = element_rect(color = "white",  fill = "black"),  
      legend.key.size = unit(1.2, "lines"),  
      legend.key.height = NULL,  
      legend.key.width = NULL,      
      legend.text = element_text(size = base_size*0.8, color = "white"), 
      legend.axis.line = element_line(color="white"),
      legend.title = element_text(size = base_size*0.8, face = "bold", hjust = 0, color = "white"),  
      legend.position = "right",  
      legend.text.align = NULL,  
      legend.title.align = NULL,  
      legend.direction = "vertical",  
      legend.box = NULL, 
      # Specify panel options
      panel.background = element_rect(fill = "black", color  =  NA),  
      panel.border = element_rect(fill = NA, color = "white"),  
      panel.grid.major = element_line(color = "grey35"),  
      panel.grid.minor = element_line(color = "grey20"),  
      panel.margin = unit(0.5, "lines"),   
      # Specify facetting options
      strip.background = element_rect(fill = "grey30", color = "grey10"),  
      strip.text.x = element_text(size = base_size*0.8, color = "white"),  
      strip.text.y = element_text(size = base_size*0.8, color = "white",angle = -90),  
      # Specify plot options
      plot.background = element_rect(color = "black", fill = "black"),  
      plot.title = element_text(size = base_size*1.2, color = "white"),  
      plot.margin = unit(rep(1, 4), "lines")
    )
}

##Residual plot##
f1 <- fitted(glm2)
res<-residuals(glm2, "pearson")
Res<-data.frame(f1,res)
plot(f1,res)
ggplot(data=Res, aes(x=f1, y=res)) +
  theme_black() + 
  labs(title="Pearson Residual vs Fitted of full data",
       x="Predicted Values", y="Residual Values") +
  geom_point(data=df,aes(x=x, y=y), color="black") +
  geom_path(data=df, aes(x=x, y=y), color="#00FF0066", size=2)+
  geom_point(color="white", aes(x=f1, y=res))+
  geom_line(y=0, col="red", aes(x=f1, y=res))
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
##Poisson##
glm_p1<- glm(Ex_G ~ offset(log(total)) + Gender + Group * Period, data = Conv_Data_F, family = poisson)
summary(glm_p1)
glm_p2<- glm(Ex_G ~ offset(log(total)) + Gender * Group * Period, data = Conv_Data_F, family = poisson)
summary(glm_p2)
anova(glm_p1, glm_p2, test="Chisq")
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
