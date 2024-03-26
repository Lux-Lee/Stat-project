library(readxl)
Project_Data <- read_excel("STAT 3510 Project.xlsx", 
                                sheet = "Data", col_types = c("numeric", 
                                                              "text", "text", "numeric", "numeric", 
                                                              "skip", "skip", "skip", "skip", 
                                                              "skip"))
Project_Data$Year[3] <- 2017
Project_Data$Year[seq(29,36,1)] <-2023
Project_Data$Gender[seq(28,30,1)] <-"Both"
Project_Data$Gender[seq(31,33,1)] <-"Men"
Project_Data$Gender[seq(34,36,1)] <-"Women"

Project_Data$Year_n<-c()
for (i in 1:length(Project_Data$Year)) {
if(Project_Data$Year[i] == "2017"){
  Project_Data$Year_n[i] <-"0"
  }  else if(Project_Data$Year[i] == "2021") {
    Project_Data$Year_n[i] <-"1"
    } else if(Project_Data$Year[i] == "2022") {
    Project_Data$Year_n[i] <-"2"
    } else {Project_Year_n[i] <-"3"}
}

Project_Data$Category_n<-c()
for (i in 1:length(Project_Data$Category)) {
  if(Project_Data$Category[i] == "Fair+Poor"){
    Project_Data$Category_n[i] <-"0"
  }
  else if(Project_Data$Category[i] == "Good"){
    Project_Data$Category_n[i] <-"1"}
  else {Project_Data$Category_n[i] <-"2"
  }}

Project_Data$Gender_n<-c()
for (i in 1:length(Project_Data$Gender)) {
  if(Project_Data$Gender[i] == "Women"){
    Project_Data$Gender_n[i] <-"0"
  }
  else if(Project_Data$Gender[i] == "Men"){
    Project_Data$Gender_n[i] <-"1"}
  else {Project_Data$Gender_n[i] <-"2"
  }}

Project_Data1<- data.frame(Project_Data[4:8])
names(Project_Data)[4]<-paste("Indigenous")
names(Project_Data)[5]<-paste("Canadian")
Category_Ex <- subset(Project_Data, Category_n == 2)
Category_G <- subset(Project_Data, Category_n == 1)
Category_P <- subset(Project_Data, Category_n == 0)
Category_Male <- subset(Project_Data, Gender_n == 1)
Category_Female <- subset(Project_Data, Gender_n == 0)
Category_Gen <- subset(Project_Data, Gender_n <= 1)

Health_Data$Group_n <- ifelse(Health_Data$group == "Indigenous", 1, 0)
Model1 <-lm(Percentage~Group_n+Gender_n+Category_n+Period, data=Category_Ex)
summary(Model1)
Model_2 <-lm(Percentage~Group_n+Gender_n+Category_n+Period, data=Health_Data)
summary(Model_2)
Model_3 <-lm(Percentage~Group_n*Gender_n*Category_n*Period, data=Health_Data)
summary(Model_3)
