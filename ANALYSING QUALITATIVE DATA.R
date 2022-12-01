#### 1) ANALYSIS AND COMPREHENSION OF QUALITATIVE DATA ####
### Using a survey dataset
### Setwd
setwd("E:/Documents/Personal Learning/Data Science/Qualitative Data-20221022T045223Z-001/Qualitative Data/Datasets/PST_Dec11_YouthEconomy_survey")
### Install necessary packages
install.packages("pewdata")
library(pewdata)
library(foreign)
library(tidyr)
library(dplyr)
library(tidyverse)

## Access the Dataset
## Sourced from Source: https://www.pewsocialtrends.org/dataset/youth-economy/
data <- read.spss("Dec11.sav", to.data.frame = TRUE)

#The survey focused on the following questions and variables
# “Compared to your parents when they were the age you are now, 
#do you think your own standard of living now is much better, 
#somewhat better, about the same, somewhat worse?" 
# To sum up, we are interested in 4 different variables:
#Personal Information
#code: “irace” -> This variable, specific for the race of the subject, can take 5 different values: “White”, “Black or African-American”, “Hispanic”, “Asian or Asian-American” and “Some other race”.
#code: “sex” -> This variable takes 2 main values: “Male” and “Female”.
#code: “age” -> This variable charachterizes the subject’s age in the interval from 18 to 92 years old.


## Generate a new dataset with only the selected variables
newdata <- dplyr::select(data,'psraid','age', 'sex', 'irace','q3')
Trial <- select(data, psraid, age)
## Clean/transform data to exclude missing information
## Specifically responses of "Don't know/Refused" or "N/A".
newdata2 <- subset(newdata, age != "Don't know/Refused" & sex != 'N/A' & irace!= 'N/A')

## Turn the "age" variable in to a numeric
newdata2$age <- as.numeric(newdata2$age) + 17

##Now look at the newly sorted dataset (subset)
head(newdata2)

#### Analysing the data to answer the question posed###
## compute the percentage of each race that characterize our sample to capture the origins of the subjects.
Black<- ((length(c(newdata2$irace[newdata2$irace == "Black or African-American"])))/(length(c(newdata2$irace))))*100
White <- ((length(c(newdata2$irace[newdata2$irace == "White"])))/(length(c(newdata2$irace))))*100
Hispanic <- ((length(c(newdata2$irace[newdata2$irace == "Hispanic"])))/(length(c(newdata2$irace))))*100
Asian <- ((length(c(newdata2$irace[newdata2$irace == "Asian or Asian-American"])))/(length(c(newdata2$irace))))*100
Others <- ((length(c(newdata2$irace[newdata2$irace == "Some other race"])))/(length(c(newdata2$irace))))*100

data.frame(row.names = "Percentage",
           Black,
           White,
           Hispanic,
           Asian,
           Others)

## Then identify the average age for each category of race in order to compare it with the average of the sample.
Black <- mean(newdata2$age[newdata2$irace =="Black or African-American"])
White <- mean(newdata2$age[newdata2$irace == "White"])
Hispanic <- mean(newdata2$age[newdata2$irace == "Hispanic"])
Asian <- mean(newdata2$age[newdata2$irace == "Asian or Asian-American"])
Others <- mean(newdata2$age[newdata2$irace == "Some other race"])
Sample.mean <- mean(newdata2$age)

#To view the output result
data.frame(row.names = "Mean",
           Black, 
           White, 
           Hispanic, 
           Asian,
           Others, 
           Sample.mean)

## Understand the number of Male and Female present in our sample.
Male <- (length(newdata2$sex[newdata2$sex == "Male"]))
Female <- (length(newdata2$sex[newdata2$sex == "Female"]))

#To view the output result
data.frame(row.names = "Gender number",
           Female, 
           Male)


##As we are interested in the relation between the current feeling and 
#the personal attribute of the subjects, we decided to create a graph 
#that clearly explain what is the proportion for each race group, answering a specific sentiment indicator.
# Create a joint table that aggregate each race with each answer to question 3.
Table1 <- table(newdata2$irace,newdata2$q3)
Q3_Percentage<- prop.table(Table1,1 )*100
#View the output result
Q3_Percentage

##To easily manipulate our data, we changed the qualitative answers into numeric observations from the best feeling 
#that takes value 6 to the worst feeling that takes value 1
answers_q3 <- dplyr::select(newdata2, 'q3')
answers_q3$q3 <- as.numeric( 
  as.character( 
    factor( 
      answers_q3$q3, 
      levels = c("Much better", "Somewhat better", "About the same","Somewhat worse","Much worse", "Don't know/Refused (VOL.)"), 
      labels = c("6","5","4","3","2","1"))))

##Finally create the table needed to create the graph
Table2 <- table(newdata2$irace,answers_q3$q3)
Q3_Percentage <- prop.table(Table2,1 )*100

##Finally, plot the bar chart.
barplot(
  Q3_Percentage, 
  main = "Percentage of races for each sentiment indicator", 
  beside = TRUE,
  ylim = c(0,40), 
  col = c("white","black","orange", "yellow", "blue", "grey"),
  xlab = "Sentiment indicator",
  ylab = "Race Percentage", 
  names.arg = c("Didn't answer", "Much worse", "SW worse", "About the same", "SW Better", "Much better"), 
  cex.names = 0.7
)

##Add a legend
legend("topleft",
       c("White","African - American", "Hispanic", "Asian/Asian - American", "Some other race"),
       fill = c("white", "black", "orange", "yellow", "blue"))

## Questions
#Using the msleep dataset in ggplot
ggplot2::msleep
?msleep
view(msleep)
# Design a research cycle (including problems, questions, hypotheses and objectives)
#Show the distribution of each variable (Descriptive statistics, central tendency)
#Display covariation between variables of interest (i.e. in reference to the research question)
# 
