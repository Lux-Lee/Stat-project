G_data1<- subset(Health_Data, Gender_n <= 1)
F_data2<- subset(F_data, Gender_n == 1)
F_data<-bind_rows(F_data1, F_data2)
F_data$Year_n <- ifelse(F_data$Year == 2017, 0, 1)
F_model1 <- lm(log(Percentage)~Gender_n+Category_n+Year_n+Group_n, data=G_data1)
summary(lm(log(Percentage)~Gender_n+Category_n+Year_n+Group_n, data=G_data1))

Diff.data<-STAT_3510_Project
Diff.data$Diff.p<-Diff.data$Difference/100
Diff.model1<-lm(log(Difference)~Gender+Timeline, data=Diff.data)
Diff.model2<-lm(log(Difference)~Gender+Period, data=Diff.data)
Diff.model3<-lm(log(Difference)~Gender+Timeline+Period, data=Diff.data)
##############
Diff.model4<-lm(log(Difference)~Timeline+Period, data=Diff.data)
summary(Diff.model4)
anova(Diff.model4, Diff.model3)
#####Subset
Sub.model1<-lm(log(Difference)~Gender+Period, data=subset(Diff.data, Timeline=="Pre"))
Sub.model2<-lm(log(Difference)~Period, data=subset(Diff.data, Timeline=="Pre"))
summary(Sub.model1)
anova(Sub.model4,Sub.model3)
Sub.model3<-lm(log(Difference)~Gender+Period+Timeline, data=Diff.G)
Sub.model4<-lm(log(Difference)~Period+Timeline, data=Diff.G)
summary(Sub.model3)
summary(Sub.model4)

Diff.data1 <-subset(Diff.data, Gender=="Male")
Diff.data12 <-subset(Diff.data, Gender="Female")
Diff.G <- bind_rows(
  mutate(Diff.data1, Gender="Male"),
  mutate(Diff.data12, Gender="Female")
)

#############
Diff.data1 <- subset(Diff.data12, select = -c(Percentage) )
Diff.data1$Percentage<-Diff.data1$Canadian
Diff.data4<-aggregate(Difference ~ Timeline, subset(Diff.data12, Gender=="Both"), mean)
Diff.data4<-aggregate(Difference ~ Period, Diff.data12, mean)
Diff.data4$Timeline<- c()
for (i in 1:length(Diff.data4$Period)) {
  if(Diff.data4$Period[i] == "2007-2010"){
    Diff.data4$Timeline[i] <-"Pre"
  }
  else if(Diff.data4$Period[i] == "2011-2014"){
    Diff.data4$Timeline[i] <-"Pre"
  }
  else {Diff.data4$Timeline[i] <-"Post"
  }}
Diff.data1 <- bind_rows(
  mutate(Diff.data1, Gender="Male"),
  mutate(Diff.data3, Gender="Female"),
  mutate(Diff.data4, Gender="Both")
)
aggregate(Difference ~ Timeline, subset(Diff.data12, Gender=="Male"), mean)
Diff.model1<-lm(log(Difference)~Timeline, data=Diff.data4)
summary(Diff.model1)
subset(Diff.data12, Gender=="Male") %>% 
  group_by(Timeline) %>% 
  summarise(avg = mean(Difference))
Diff.data4<- subset(Diff.data12, Period=="2007-2010")
Diff.data5<- subset(Diff.data12, Period=="2011-2014")

Diff.data2 <- bind_rows(
  mutate(Diff.data2),
  mutate(Diff.data5)
)
library(dplyr)
Sub.model5<-lm(log(Percentage)~Group+Period+Gender+Timeline, data=Diff.GR)
Sub.model6<-lm(log(Percentage)~Group+Period, data=Diff.GR)

summary(Sub.model10)
Sub.model7<-lm(log(Percentage)~Group+Period, data=subset(Diff.GR, Gender=="Male"))
Sub.model8<-lm(log(Percentage)~Group+Period, data=subset(Diff.GR, Gender=="Female"))
Sub.model9<-lm(log(Percentage)~Group+Period, data=subset(Diff.GR, Gender=="Both"))
Sub.model10<-lm(log(Percentage)~Group+Period, data=subset(Diff.GR, Timeline=="Post"))
