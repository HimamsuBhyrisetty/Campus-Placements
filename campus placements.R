library(readxl)
pl_data <- read_excel("/Users/himamsu//Placement_Data_Full_Class.xlsx")
View(pl_data)
install.packages("dplyr")
install.packages("tidyverse")
install.packages("ggpubr")
install.packages("rstatix")
install.packages("ggplot2")
install.packages("datarium")
install.packages("FSA")
library(FSA)
library(tidyverse)
library(dplyr)
library(ggpubr)
library(rstatix)
library(datarium)
library(ggplot2)

#placement status and mba specialisation data exploration
SPECIALISATION1 <- c("MKT&HR", "MKT&FIN" )
valueS1<- c(53,95)
dataspec <- data.frame(SPECIALISATION1, valueS1)

ggplot(dataspec, aes (fill= SPECIALISATION1, y= valueS1, x = SPECIALISATION1))+
  geom_bar(position="dodge", stat="identity")+ggtitle("MBA specilisation")+theme (plot.title = element_text(hjust = 0.5))
placement_status <- c("Placed" ,"Not Placed" )
valuePS <- c(148,67)
dataPS <- data.frame(placement_status, valuePS)

ggplot(dataPS, aes (fill= placement_status, y =valuePS, x = placement_status))+
  geom_bar(position="dodge", stat="identity")+ggtitle("Placement status of all the students")+theme (plot.title = element_text(hjust = 0.5))


#GENDER VS PLACEMENT STATUS (chi-square)
genderplacement <- subset (pl_data, select = c("gender", "status of placement"))
genderplacement
chisq.test(genderplacement$gender, genderplacement$`status of placement`, correct = FALSE)

#DEGREE SPEC VS PLACEMENT STATUS (chi-square)
degreespec <- subset (pl_data, select = c("Field of degree education", "status of placement"))
View(degreespec)
chisq.test(degreespec$ `Field of degree education`, degreespec$`status of placement`, correct = FALSE)

#MBA Spec Vs Placement Status (chi-square)
mbaspec <- subset(pl_data, select = c("mba specialisation", "status of placement"))
View(mbaspec)
chisq.test(mbaspec$`mba specialisation`, mbaspec$`status of placement`, correct = FALSE)

#Work ex and placement status (chi-square)
workexp <- subset (pl_data, select = c("work experience", "status of placement"))
workexp
chisq.test(workexp$ `work experience`, workexp$`status of placement`, correct = FALSE)

#MBA spec vs their percentages
mkt_hr_50_60 <- pl_data %>% filter(`mba specialisation` == "Mkt&HR"&`mba percentage` >=50& `mba percentage` <=60) %>% select(`mba specialisation`, `mba percentage`)
mkt_fin_50_60 <- pl_data %>% filter (`mba specialisation`=="Mkt&Fin"& `mba percentage` >=50& `mba percentage` <=60) %>% select (`mba specialisation`, `mba percentage`)
count (mkt_fin_50_60)
count (mkt_hr_50_60)

mkt_hr_60_70 <-  pl_data %>% filter (`mba specialisation` == "Mkt&HR"&`mba percentage` >=60& `mba percentage` <=70) %>% select(`mba specialisation`, `mba percentage`)
mkt_fin_60_70 <- pl_data %>% filter (`mba specialisation` =="Mkt&Fin"&`mba percentage` >=60& `mba percentage` <=70) %>% select(`mba specialisation`, `mba percentage`)
count(mkt_hr_60_70)
count(mkt_fin_60_70)

mkt_hr_70_80 <- pl_data %>% filter (`mba specialisation` =="Mkt&HR"&"mba percentage" >=70& `mba percentage` <=80) %>% select (`mba specialisation`, `mba percentage`)
mkt_fin_70_80 <- pl_data %>% filter (`mba specialisation` == "Mkt&Fin"& `mba percentage` >=70& `mba percentage` <=80) %>% select (`mba specialisation`, `mba percentage`)
count(mkt_hr_70_80)
count (mkt_fin_70_80)

percentage <- c(rep("p50-60", 2), rep("p60-70", 2), rep("p70-80", 2))
SPECIALZATION<- rep(c("MKT&HR", "MKT&FIN" ), 3)
valueS_Percentage <- c(c(41,40),c(44,66), c(10,14))
data<- data.frame(percentage, SPECIALZATION, valueS_Percentage)

ggplot(data, aes (fill=SPECIALZATION, y=valueS_Percentage, x=percentage)) + 
  geom_bar(position="dodge", stat="identity")+ggtitle("MBA percentages vs specialisation")+theme (plot.title = element_text(hjust=0.5))

#Do ANOVA b/w employability test and placement status
shapiro.test(pl_data$ `Employability test percentage`)
ggqqplot(pl_data$`Employability test percentage`, title = "Employability test precentage")+theme (plot.title = element_text(hjust = 0.5))
hist(pl_data$ `Employability test percentage`)

result1= kruskal.test( pl_data$ `Employability test percentage`~ pl_data$ `status of placement`,
                       data=pl_data)
print(result1)

#Do ANOVA b/w percentages of degree percentage and placement status
ggqqplot(pl_data$ `degree percentage`, title = "Degree precentage")+theme (plot.title = element_text(hjust = 0.5))
shapiro.test(pl_data$ `degree percentage`)
hist(pl_data$ `degree percentage`)

result2= kruskal.test( pl_data$ `degree percentage`~ pl_data$ `status of placement`,
                       data= pl_data)
print(result2)

dunnTest(pl_data$ `degree percentage`~ pl_data$ `status of placement`,data= pl_data, method = "bonferroni")

#Do ANOVA b/w percentages of mba percentage and placement status
ggqqplot(pl_data$`mba percentage`,title = "MBA precentage")+theme (plot.title = element_text(hjust = 0.5))
hist (pl_data$`mba percentage`)
shapiro.test(pl_data$`mba percentage`)

result3= kruskal.test( pl_data$ `mba percentage`~ pl_data$ `status of placement`,data = pl_data)
print(result3)

#RANGE OF SALARY (3) VS MBA SPEC GROUPED BAR PLOT
placedstudents <-  na.omit(pl_data)
View(placedstudents) 

mba_salary <- subset(placedstudents, select = c("mba specialisation", "salary offered")) 
table(mba_salary$`mba specialisation`)

salaryoffered_2_31k <- placedstudents %>% filter(`salary offered`>=200000& `salary offered` <=300000) %>% select(`salary offered`, `mba specialisation`)
View(salaryoffered_2_31k)
count(salaryoffered_2_31k)

salaryoffered_3_4lk <-  placedstudents %>% filter (`salary offered` >=300001& `salary offered` <=400000) %>% select(`salary offered`, `mba specialisation`)
View(salaryoffered_3_41k)
count(salaryoffered_3_41k)

salaryoffered_4_51k <- placedstudents %>% filter(`salary offered` >=400001& `salary offered` <=500000) %>% select(`salary offered`, `mba specialisation`)
View(salaryoffered_4_51k)
count(salaryoffered_4_51k)


salaryoffered_5LKplus <- placedstudents %>% filter (`salary offered` >=500001& `salary offered` <=1000000) %>% select(`salary offered`, `mba specialisation`)
View(salaryoffered_5LKplus)
count(salaryoffered_5LKplus)

salary<- c(rep("s2-3", 2), rep("s3-4", 2), rep("s4-5", 2), rep("s5plus", 2))
SPECIALISATION<- rep(c("MKT&HR", "MKT&FIN" ), 4) 
values <- c(c(45,75),c(7,11), c(1, 6), c(0,3))
data<- data.frame(salary, SPECIALISATION, values)

ggplot(data, aes (fill=SPECIALISATION, y=values, x=salary)) +
  
  geom_bar(position="dodge", stat="identity")+ggtitle("salary ranges vs specialisation")+theme (plot.title = element_text(hjust= 0.5))

