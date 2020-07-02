#Firstly, open the covid19.txt file in word,then save as covid19.csv
#then import 

#install package  
install.packages("dplyr")

#Load package
library("dplyr")

#Import data
rData <- read.csv(file.choose(), header = FALSE)
View(rData)

#Replace comma ',' with '|' in rData for delimiting
rData_1 <- data.frame(gsub(', ','|',rData$V1))
View(rData_1)

#Delimiting
rData_2 <- data.frame(do.call('rbind', strsplit(as.character(rData_1$gsub............rData.V1.), ',', fixed = FALSE)))
View(rData_2)

#Export rData_2 into new directory
write.csv(rData_2, "Coivd19csv_exported", row.names = F)

#Import data again
rData_3 <- read.csv(file.choose(), header = TRUE, skip = 1, na.strings = c("","N/A","#NA"))
View(rData_3)

#Delete extra columns from the data
rData_ <- subset(rData_3, select = -c(33:38))
View(rData_)

#Convert columns according to there classas as factor,numeric,character & integer
class(rData_$ID)
rData_$ID <- as.integer(rData_$ID)
View(rData_)

#Change male and female for better understanding
rdata_6 <- rData_
View(rdata_6)
rdata_6$sex[rdata_6$sex == "male"] <- "Male"
rdata_6$sex[rdata_6$sex == "female"] <- "Female"
rdata_6$sex <- as.factor(rdata_6$sex)
View(rdata_6)



£check the distinct value
rData_7 <- rdata_6
View(rData_7)
class(rData_7$admin_id)
View(distinct(rData_7,admin_id))


# Using grepl to filter out unorganised rows by matching the unwanted value in the Column admin_id
# grepl returns TRUE when the string matches the argument provided
rData_7 <- filter(rData_7, !grepl('Chongqing',admin_id))
rData_7 <- filter(rData_7, !grepl('Rhineland-Palatinate',admin_id))
rData_7 <- filter(rData_7, !grepl('China',admin_id))
rData_7 <- filter(rData_7, !grepl('Xiangcheng District',admin_id))
rData_7 <- filter(rData_7, !grepl("Xi'an City",admin_id))
rData_7 <- filter(rData_7, !grepl('Tongnan District',admin_id))
rData_7 <- filter(rData_7, !grepl('South Korea',admin_id))
rData_7 <- filter(rData_7, !grepl('Zhejiang',admin_id))
rData_7 <- filter(rData_7, !grepl('Singapore',admin_id))
rData_7 <- filter(rData_7, !grepl('Ordos City',admin_id))
rData_7 <- filter(rData_7, !grepl('Jilin',admin_id))
rData_7 <- filter(rData_7, !grepl('Haikou City',admin_id))
rData_7 <- filter(rData_7, !grepl(10809,ID))
rData_7 <- filter(rData_7, !grepl(12045,ID))
View(rData_7)







#install package ggplot2 for graph
install.packages("ggplot2")
#load package
library("ggplot2")

##OUTCOMES
# Consistent data in the column
rData_7$outcome[rData_7$outcome == "died"] <- "death"
rData_7$outcome[rData_7$outcome == "death"] <- "death"
rData_7$outcome[rData_7$outcome == "discharge"] <- "discharged"
rData_7$outcome[rData_7$outcome == "Discharged"] <- "discharged"
rData_7$outcome <- as.factor(rData_7$outcome)
View(rData_7)

#count cases according to each outcome
outcomes <- aggregate(rData_7$outcome, list(rData_7$outcome), FUN = length)
View(outcomes)
#remove unwanted rows
outcomes <- outcomes[-c(1,2),]
#change column names
colnames(outcomes) <- c("outcome", "count")
View(outcomes)
#plot piechart for outcomes
piechart <- ggplot(outcomes ,aes(x = outcome, y = count, group = outcome, fill = outcome)) + geom_bar(width = 1, stat = 'identity') + coord_polar('y', start = 0) + theme(axis.text.y = element_blank())
piechart


##AFFECTED PROVINCE OF CHINA(TOP 10)
#count cases according to top 10 affected province_china 
province_china <- aggregate(rData_7$province, list(rData_7$province), FUN = length)
#change column name
colnames(province_china) <- c("provinceName", "casesCount")
View(province_china)
#arrange data 
province_china <- arrange(province_china, province_china$casesCount)
View(province_china)
#choose top 10 cases from the end of the data 
tail_pro. <- province_china[c(139:149),]
View(tail_pro.)
#plot bargraph for top 10 affected province of china
bargraph <- ggplot(tail_pro., aes(x = provinceName, y = casesCount, fill = provinceName, group = provinceName)) + geom_bar(stat = 'identity') + coord_flip() + labs(title = "Top 10 affected Provinces of China", x = "Provinces") 
bargraph


##AFFECTED PROVINCE OF CHINA(TOP 20)
#Top 20 effected province
aggregate8 <- aggregate(rData_7$province, list(rData_7$province), FUN = length)
View(aggregate8)
colnames(aggregate8) <- c("province","cases")
aggregate8 <- slice(aggregate8,-1)
aggregate8 <- arrange(aggregate8,aggregate8$cases)
View(aggregate8)
aggregate8 <- slice(aggregate8, 148:129)
View(aggregate8)
ggplot(aggregate8,aes(x=province,y=cases)) + geom_bar(stat = "identity" , color = "blue", fill="red") + labs(title = "top 20 affected province") + theme_minimal() 



##CASES ON EACH DAY
#Daily confirm case on each day
rData_7$date_confirmation <- as.Date(rData_7$date_confirmation, "%d.%m.%Y")
View(rData_7)
aggregate7 <- aggregate(rData_7$date_confirmation,list(rData_7$date_confirmation),FUN = length)
View(aggregate7)
colnames(aggregate7) <- c("date_confirm","cases")
View(aggregate7)
aggregate7 <- slice(aggregate7,-36,-37)
View(aggregate7)
ggplot(aggregate7, aes(date_confirm,cases)) + geom_line() + xlab("date_confirm") + ylab("cases") 



##History of wuhan
rData_7$lives_in_Wuhan[rData_7$lives_in_Wuhan == "No"] <- "NO"
rData_7$lives_in_Wuhan[rData_7$lives_in_Wuhan == "NO"] <- "NO"
rData_7$lives_in_Wuhan[rData_7$lives_in_Wuhan == "Yes"] <- "YES"
rData_7$lives_in_Wuhan[rData_7$lives_in_Wuhan == "YES"] <- "YES"
rData_7$lives_in_Wuhan <- as.factor(rData_7$lives_in_Wuhan)
View(rData_7)

in_wuhan <- aggregate(rData_7$lives_in_Wuhan,list(rData_7$lives_in_Wuhan), FUN = length)
View(in_wuhan)

colnames(in_wuhan) <- c("lives_in_wuhan","no_of_people")
in_wuhan <- slice(in_wuhan,-1)
View(in_wuhan)
ggplot(in_wuhan,aes(lives_in_wuhan,no_of_people)) + geom_bar(stat = "identity", fill = "yellow") +labs(title = "history of wuhan") + coord_flip() + geom_text(aes(label=no_of_people), vjust=-0.3, size=3.5)


##Top 10 affected country
country1 <- aggregate(rData_7$country,list(rData_7$country), FUN = length)
View(country1)
colnames(country1) <- c("country","cases")
country1 <- arrange(country1, country1$cases)
View(country1)
country1 <- slice(country1, 52:43)
View(country1)
ggplot(country1,aes(country,cases)) + geom_bar(stat = "identity", fill = "pink") +labs(title = "top 10 affected country") + coord_flip() + geom_text(aes(label=cases), vjust=-0.3, size=3.5)


##Discharge and death 
rData_7$date_death_or_discharge <- as.Date(rData_7$date_death_or_discharge, "%d.%m.%Y")
recover <- select(rData_7,country,date_death_or_discharge)
View(recover)

recover <- slice(recover,-5061:-5060)
View(recover)
ggplot(recover, aes(date_death_or_discharge,country)) + geom_line() + xlab("date_death_or_discharge") + ylab("country") 




#recover people on each day
recover1 <- aggregate(recover$date_death_or_discharge,list(recover$date_death_or_discharge),FUN = length)
View(recover1)
colnames(recover1) <- c("dates","recover_cases")
View(recover1)
ggplot(recover1, aes(dates,recover_cases)) + geom_line() + xlab("dates") + ylab("recover_cases") 


#Countries gender wise
gender <- select(rData_7,sex,country) 
View(gender)
gender1 <- gender %>% filter(!is.na(sex))
View(gender1)
gender2 <- slice(gender1,-c(198,217,571,987,1211))
View(gender2)
ggplot(gender2,aes(sex, fill = sex)) + geom_bar()
ggplot(gender2, aes(x = country , fill = sex)) + geom_bar(position = 'dodge') + labs(title = "country cases per gender") + coord_flip()























