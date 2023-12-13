#Hanis Athirah
#Placement Dataset - Observing academic & employability factors in influencing successful placement in a workforce.

#set working directory
setwd("~/Downloads")

#import data to global environment
df <- read.csv("PlacementData.csv", header = TRUE, sep = ",")

#print out the column names in the dataframe
names(df)

#give meaningful names to  each column in the dataset
df <- setNames(df, c("serial_number", "gender", "secondary_education_percentage", 
                     "secondary_education_board", "higher_secondary_education_percentage", 
                     "higher_education_board", "higher_secondary_specialization", "degree_percentage", 
                     "degree_field", "work_experience", "employability_test_percentage", 
                     "mba_specialization", "mba_percentage", "placement_status", "salary"))

#print out the column names in the dataframe after being altered above
names(df)

#view dataframe
View(df)

#print out top 6 rows in the dataframe
head(df)

#install packages
install.packages("webr")
install.packages("grid")
install.packages("tidyr")
install.packages("dplyr")
install.packages("plotly")
install.packages("ggplot2")
install.packages("tidyverse")
install.packages("patchwork")

#install libraries
library(webr)
library(grid)
library(tidyr)
library(dplyr)
library(plotly)
library(ggplot2)
library(tidyverse)
library(patchwork)

#have a glimpse of the dataframe
glimpse(df)

#check data type of each column
str(df)

#check the summary of the dataframe
summary(df)

#make a copy of the dataframe
dfCopy <- data.frame(df)

#create a dataframe for employed students
dfSalary <- df
dfSalary <- dfSalary %>% drop_na(salary)

#check if the dataframe is null
is.null(data)

#check each column for null values
colSums(is.na(df))

#list of unique values in a column
unique(df$gender)
unique(df$secondary_education_board)
unique(df$higher_education_board)
unique(df$higher_secondary_specialization)
unique(df$degree_field)
unique(df$mba_specialization)

#drop unnecessary column
dropSN <- c("serial_number")
df <- df[,!(names(df) %in% dropSN)]

#check some statistics of some columns
mean(df$secondary_education_percentage)
median(df$secondary_education_percentage)
sd(df$secondary_education_percentage)
quantile(df$secondary_education_percentage)
IQR(df$secondary_education_percentage)

mean(df$higher_secondary_education_percentage)
median(df$higher_secondary_education_percentage)
sd(df$higher_secondary_education_percentage)
quantile(df$higher_secondary_education_percentage)
IQR(df$higher_secondary_education_percentage)

mean(df$degree_percentage)
median(df$degree_percentage)
sd(df$degree_percentage)
quantile(df$degree_percentage)
IQR(df$degree_percentage)

mean(df$employability_test_percentage)
median(df$employability_test_percentage)
sd(df$employability_test_percentage)
quantile(df$employability_test_percentage)
IQR(df$employability_test_percentage)

mean(df$mba_percentage)
median(df$mba_percentage)
sd(df$mba_percentage)
quantile(df$mba_percentage)
IQR(df$mba_percentage)

mean(dfSalary$salary)
median(dfSalary$salary)
sd(dfSalary$salary)
quantile(dfSalary$salary)
IQR(dfSalary$salary)

#factor some of the columns which contain categorical values
gender_fct <- factor(df$gender)
summary(gender_fct)
levels(gender_fct)

secondary_education_board_fct <- factor(df$secondary_education_board)
summary(secondary_education_board_fct)
levels(secondary_education_board_fct)

higher_education_board_fct <- factor(df$higher_education_board)
summary(higher_education_board_fct)
levels(higher_education_board_fct)

higher_secondary_specialization_fct <- factor(df$higher_secondary_specialization)
summary(higher_secondary_specialization_fct)
levels(higher_secondary_specialization_fct)

degree_field_fct <- factor(df$degree_field)
summary(degree_field_fct)
levels(degree_field_fct)

work_experience_fct <- factor(df$degree_field)
summary(work_experience_fct)
levels(work_experience_fct)

mba_specialization_fct <- factor(df$mba_specialization)
summary(mba_specialization_fct)
levels(mba_specialization_fct)

placement_status_fct <- factor(df$placement_status)
summary(placement_status_fct)
levels(placement_status_fct)

#convert binary values in placement_status column into 0 or 1
dfCopy$placement_status <- ifelse(dfCopy$placement_status == "Placed", 1,0)

#calculate percentage students who got a placement
dfCopy %>% summarize(total = n(), percent_placed = mean(placement_status == 1))

#calculate percentage students who got a placement based on MBA specialization
dfCopy %>% group_by(mba_specialization) %>% summarize(total = n(), percent_placed = mean(placement_status == 1))

#pie chart to visualize the distribution of two genders in the dataset
ggplot(df, aes(x = "", y = gender, fill = gender)) + 
  geom_bar(stat = "identity", width = 1) + 
  coord_polar("y", start = 0) +
  theme_void() 

#pie chart to visualize the distribution of placement status in the dataset
ggplot(df, aes(x = "", y = placement_status, fill = placement_status)) + 
  geom_bar(stat = "identity", width = 1) + 
  coord_polar("y", start = 0) +
  theme_void() 

#bar plot to visualize distribution of two genders in the dataset
a <- df %>% 
  ggplot(aes(x = gender, fill = gender)) +
  geom_bar(width = 0.4) +
  theme_bw() +
  ggtitle("Genders Frequency") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "Genders", y = "Frequency")
ggplotly(a)

#bar plot to visualize distribution of two genders in the dataset
b <- df %>% 
  ggplot(aes(x = placement_status, fill = placement_status)) +
  geom_bar(width = 0.4) +
  theme_bw() +
  ggtitle("Placement Status Frequency") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "Placement Status", y = "Frequency")
ggplotly(b)

#stacked bar plot to visualize distribution of two genders based on the placement status
c <- df %>% 
  ggplot(aes(x = placement_status, fill = gender)) +
  geom_bar(width = 0.4) +
  theme_bw() +
  ggtitle("Placement Status & Genders Frequency") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "Placement Status", y = "Frequency")
ggplotly(c)

#bar plot to visualize distributions of secondary education board based on placement status
d <- df %>% 
  ggplot(aes(x = secondary_education_board, fill = placement_status)) +
  geom_bar(width = 0.4) +
  theme_bw() +
  ggtitle("Secondary Education Board VS Frequency") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "secondary_education_board", y = "Frequency")
ggplotly(d)

#bar plot to visualize distributions of higher education board based on placement status
e <- df %>% 
  ggplot(aes(x = higher_education_board, fill = placement_status)) +
  geom_bar(width = 0.4) +
  theme_bw() +
  ggtitle("Higher Education Board VS Frequency") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "higher_education_board", y = "Frequency")
ggplotly(e)

#combine two plots i.e. d and e
d + e

#box plots to visualize marks
f <- ggplot(df, aes(x = placement_status, y = secondary_education_percentage, fill = placement_status)) +
  geom_boxplot() +
  theme_bw() +
  labs(title = 'Placement Status VS Secondary Education Percentage')
ggplotly(f)
g <- ggplot(df, aes(x = placement_status, y = higher_secondary_education_percentage, fill = placement_status)) +
  geom_boxplot() +
  theme_bw() +
  labs(title = 'Placement Status VS Higher Education Percentage')
ggplotly(g)
h <- ggplot(df, aes(x = placement_status, y = degree_percentage, fill = placement_status)) +
  geom_boxplot() +
  theme_bw() +
  labs(title = 'Placement Status VS Degree Percentage')
ggplotly(h)
i <- ggplot(df, aes(x = placement_status, y = employability_test_percentage, fill = placement_status)) +
  geom_boxplot() +
  theme_bw() +
  labs(title = 'Placement Status VS Employability Test Percentage')
ggplotly(i)
j <- ggplot(df, aes(x = placement_status, y = mba_percentage, fill = placement_status)) +
  geom_boxplot() +
  theme_bw() +
  labs(title = 'Placement Status VS MBA Percentage')
ggplotly(j)

#combine several plots i.e. d + e + f + g + h
f + g + h + i + j

#line graph to visualize marks vs salary
k <- ggplot(data=salaryDF, aes(x=employability_test_percentage, y=salary, group=1)) +
  geom_line(color="blue") +
  geom_line(aes(x=mba_percentage), color="green") +
  geom_line(aes(x=degree_percentage), color="red") +
  geom_point(aes(x=degree_percentage)) +
  geom_point(aes(x=employability_test_percentage)) +
  geom_point(aes(x=mba_percentage)) +
  labs(title = 'Marks VS Salary') +
  xlab("Marks(%))") + ylab("Salary")

ggplotly(k)

#donut chart to visualize MBA Specialization and placement status
#building a table to use for the plot
PD <- df %>% group_by(placement_status, mba_specialization) %>% summarize(count=n())
#PD[PD == "Mkt&Fin"] <- "Markerting & Finance"
#PD[PD == "Mkt&HR"] <- "Markerting & Human Resources"
PD <- PD %>% rename(MBASpecialization = mba_specialization)
PieDonut(PD, aes(MBASpecialization, placement_status, count=count), title = "Donut Chart -  MBA Specialization VS Placement")

#donut chart to visualize degree field and placement status
PD <- df %>% group_by(placement_status, degree_field) %>% summarize(count=n())
PD <- PD %>% rename(DegreeField = degree_field)
PieDonut(PD, aes(DegreeField, placement_status, count=count), title = "Donut Chart -  Degree Field VS Placement")

#donut chart to visualize work experience and placement status
PD <- df %>% group_by(placement_status, work_experience) %>% summarize(count=n())
PD <- PD %>% rename(WorkExperience = work_experience)
PieDonut(PD, aes(WorkExperience, placement_status, count=count), title = "Donut Chart -  Work Experience VS Placement")

#scatter plot to visualize MBA Percentage VS Salary
l <- ggplot(dfSalary, aes(x=mba_percentage, y=salary, color = mba_specialization)) +
  geom_point(size=2)
ggplotly(l)

#box plot to visualize higher secondary education percentage vs higher education board respective to higher secondary specialization
ggplot(df, aes(higher_education_board, higher_secondary_education_percentage, fill =  higher_secondary_specialization)) + geom_boxplot()

#box plot to visualize degree percentage vs degree field respective to work experience
ggplot(df, aes(degree_field, degree_percentage, fill = work_experience)) + geom_boxplot()