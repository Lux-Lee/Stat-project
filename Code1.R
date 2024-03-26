Health_Data  <- bind_rows(
  mutate(Indigenous, group = "Indigenous"),
  mutate(Canadian, group = "Canadian")
)
Health_Data$Period<- ifelse(Health_Data$Year == 2017, "Pre", "Post")
Health_Data$Proportion<- Health_Data$Percentage/100


Health_Data$Year_n<-c()
for (i in 1:length(Health_Data$Year)) {
  if(Health_Data$Year[i] == "2017"){
    Health_Data$Year_n[i] <-"0"
  } else if(Health_Data$Year[i] == "2021") {
    Health_Data$Year_n[i] <-"1"
  } else if(Health_Data$Year[i] == "2022") {
    Health_Data$Year_n[i] <-"2"} 
  else {Health_Data$Year_n[i] <-"3"
  }}

Health_Data$Gender_n<-c()
for (i in 1:length(Health_Data$Gender)) {
  if(Health_Data$Gender[i] == "Women"){
    Health_Data$Gender_n[i] <-"0"
  }
  else if(Health_Data$Gender[i] == "Men"){
    Health_Data$Gender_n[i] <-"1"}
  else {Health_Data$Gender_n[i] <-"2"
  }}

Health_Data$Category_n<-c()
for (i in 1:length(Health_Data$Category)) {
  if(Health_Data$Category[i] == "Fair+Poor"){
    Health_Data$Category_n[i] <-"0"
  }
  else if(Health_Data$Category[i] == "Good"){
    Health_Data$Category_n[i] <-"1"}
  else {Health_Data$Category_n[i] <-"2"
  }}

year1_data <- subset(Health_Data, Year == 2017)
genders <- subset(Health_Data, Gender_n <=1)
Health_Data$Group_n <- ifelse(Health_Data$group == "Indigenous", 1, 0)
Model1 <-lm(log(Canadian)~Year_c+Indigenous+Category, data=subset(Project_Data, Gender == "Men"))
summary(Model1)
Model2 <-lm(log(Percentage)~Group_n+Category_n+Period, data=subset(Health_Data, Gender=="Men"))
summary(Model2)
Model3 <-lm(Percentage~Group_n+Category_n, data=subset(genders, Year == "2017"))
summary(Model3)

summary(lm(log(Percentage)~Gender_n+Group_n+Category_n+Year_n, data=subset(genders, Year_n == 0 & Year_n == 3)))

genders_data <-subset(Project_Data, Gender_n <=1)
F_model1<- lm(log(Canadian)~Year_n*Indigenous+Category_n, data=subset(Project_Data, Gender=="Women"))
summary(F_model1)
F_model2<- lm(log(Canadian)~Year_n*Indigenous+Category_n, data=Project_Data)
summary(F_model2)
F_model3<- lm(log(Canadian)~Year_n+Category_n+Gender_n, data=Project_Data)
summary(F_model3)
F_model4<- lm(log(Indigenous)~Year_n*Canadian+Category_n, data=Project_Data)
summary(F_model4)
F_model6<- lm(log(Canadian)~Category+Indigenous*Year_n, data=subset(Project_Data, Year_c=="Post"))
summary(F_model6)
summary(lm(log(Canadian)~Year_n*Indigenous+Category_n, data= subset(genders_data, Category_n == "2")))
anova(F_model6,F_model4)

colnames(STAT_3510_Project) <- c("Period", "Gender","Group", "Vaccination")
Gender_data  <- bind_rows(
  mutate(Gender_n, group = "Male"),
  mutate(Gender_f, group = "Female")
)

Cov_model <- lm(log(Vaccination)~Group+Period, data= Gender_data)
summary(Cov_model)
summary(lm(log(Canadian)~Year_n*Indigenous+Category_n, data=Project_Data))
