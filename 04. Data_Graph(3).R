table(X2023_STB_survey $Gender)

ECN <- table(X2023_STB_survey $Gender)
prop.table(ECN)

table(X2023_STB_survey $Gender, X2023_STB_survey $Grade)

 barplot(table(X2023_STB_survey $Nationality))
 barplot(table(X2023_STB_survey $`residential area`))
 entry <- table(X2023_STB_survey $Gender, X2023_STB_survey $Grade)
 barplot(entry, legend = TRUE)
 pie(table(X2023_STB_survey $Grade))
 hist(X2023_STB_survey$`Age`, main="Age histogram", col=terrain.colors(12))
 boxplot(X2023_STB_survey$`Grade`, X2023_STB_survey$`Age`, main="Grade별 Age를 비교하는 박스플롯", col="yellow", names = c("Grade","Age"))
 summary(X2023_STB_survey,na.rm=T)