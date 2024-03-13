G_data1<- subset(Health_Data, Gender_n <= 1)
F_data2<- subset(F_data, Gender_n == 1)
F_data<-bind_rows(F_data1, F_data2)
F_data$Year_n <- ifelse(F_data$Year == 2017, 0, 1)
F_model1 <- lm(log(Percentage)~Gender_n+Category_n+Year_n+Group_n, data=G_data1)
summary(lm(log(Percentage)~Gender_n+Category_n+Year_n+Group_n, data=G_data1))

git config --global user.email "you@example.com"
git config --global user.name "Your Name"
git remote add origin https://github.com/Lux-Lee/Stat-project.git 