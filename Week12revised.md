#Week 12 Ip
##By Samuel Gitau

##Overview
In this project, we will work with a Kenyan entrepreneur, who has created an online cryptography course and would want to advertise it on her blog. She currently targets audiences originating from various countries. In the past, she ran ads to advertise a related course on the same blog and collected data in the process. She would now like to employ your services as a Data Science Consultant to help her identify which individuals are most likely to click on her ads. 

##Analytics question
Our task is to help a Kenyan enterpreneur to identify a method to target individual to click on her ads to her crypotography course.

##Metrics for success
Our project will be considered successful if we can derive relevant and meaningful insight from the data provided

##Experimental design
Here is a step to step guide to our project
1. Loading and previewing the dataset
2. Studying the dataset properties
3. Data cleaning
4. EDA

#Loading the dataset
## Install important packages- we will import the packages we need to complete this project
```{r}
install.packages("dplyr")
library(dplyr)
install.packages("tidyr")
library(tidyr)
install.packages("ggplot2")
library(ggplot2)
install.packages("tibble")
library(tibble)
install.packages("Hmisc")
library('Hmisc')
install.packages("pander")
library(pander)
install.packages("corrplot")
library(corrplot)
install.packages("magrittr")
library(magrittr)
install.packages("sos")
library(sos)
findFn("select")
install.packages("data.table")
library(data.table)
install.packages("knitr")
library(knitr)
```
## Loading the dataset ----
`df <- read.csv("advertising.csv")`
###Previewing the first 6 rows 
`head(df)`
###Previewing the last 6 rows
`tail(df)`
###Checking shape of dataset
`dim(df)`
## Checking Information  our dataset----
###Checking number of columns 
`colnames(df)`
###Convert to dataframe to a tibble for knit output
`df <- as.tibble(df)`
`df`
###checking the datatypes of the dataframe
`glimpse(mtcars)`
`glimpse(df)`
###Checking summary of our dataset
`summary(df)`
## Cleaning the Dataset----
###Check for missing values
`colSums(is.na(df))`
###checking for duplicated values
```{r}
duplicated_rows <- df[duplicated(df),]
duplicated_rows
```
###Checking for outliers
####Getting the numerical columns
```{r}
df_num <- (df %>% select(c("Daily.Time.Spent.on.Site", "Area.Income", "Age", "Daily.Internet.Usage")))
```
###using  Boxplots to check for outliers
```{r}
par(mfrow=c(1,4))
boxplot(df_num$Daily.Time.Spent.on.Site, xlab = "Daily.Time.Spent.on.Site")
boxplot(df_num$Area.Income, xlab = "Area.Income")
boxplot(df_num$Age, xlab = "Age")
boxplot(df_num$Daily.Internet.Usage, xlab = "Daily.Internet.Usage")
```
###checking at the outliers
```{r}
out <- list(boxplot.stats(df_num$Area.Income)$out)
out
```
###Removing outliers
```{r}
df_new <- subset(df, Area.Income > 19000)
dim(df_new)
```
###Clean character columns
####remove Whitespaces and convert character columns lower case
```{r}
df_new %>%
  summarise_if(is.character, tolower) %>% trimws()
```
## Univariate Analysis----
###continuous variables
```{r}
df_sum <-(df_new %>% summarise_if(is.numeric, summary))
df_sum <- data.frame(df_sum)
df_sum$Index <- c('Min', '1st QUN', 'Median','Mean', '3rd QUN', 'Max' )
rownames(df_sum) <- df_sum$Index
df_sum %>% select(- c('Index', 'Male', 'Clicked.on.Ad'))
```
###Checking for variance and standard deviation of the numerical columns
```{r}
df_num %>%
  summarise_if( is.numeric, var)
df_num %>%
  summarise_if( is.numeric, sd)
```
###plot histogram to check distribution on numerical values
```{r}
par(mfrow=c(4,1))
h <- hist(df_num$Daily.Time.Spent.on.Site, main="Daily.Time.Spent.on.Site", col = 'black')
xfit <- seq(min(df_new$Daily.Time.Spent.on.Site),max(df_new$Daily.Time.Spent.on.Site),length=40)
yfit<-dnorm(xfit,mean=mean(df_new$Daily.Time.Spent.on.Site),sd=sd(df_new$Daily.Time.Spent.on.Site))
yfit <- yfit*diff(h$mids[1:2])*length(df_new$Daily.Time.Spent.on.Site)
lines(xfit, yfit, col="blue", lwd=2)
h <- hist(df_new$Age, main="AGE DISTIBUTION", col = 'red')
xfit <- seq(min(df_new$Age),max(df_new$Age),length=40)
yfit<-dnorm(xfit,mean=mean(df_new$Age),sd=sd(df_new$Age))
yfit <- yfit*diff(h$mids[1:2])*length(df_new$Age)
lines(xfit, yfit, col="blue", lwd=2)
h <- hist(df_new$Area.Income, main="Area.Income Distribution", col = 'green')
xfit <- seq(min(df_new$Area.Income),max(df_new$Area.Income),length=40)
yfit<-dnorm(xfit,mean=mean(df_new$Area.Income),sd=sd(df_new$Area.Income))
yfit <- yfit*diff(h$mids[1:2])*length(df_new$Area.Income)
lines(xfit, yfit, col="blue", lwd=2)
h <- hist(df_new$Daily.Internet.Usage, main="Daily.Internet.Usage Distibution", col = 'blue')
xfit <- seq(min(df_new$Daily.Internet.Usage),max(df_new$Daily.Internet.Usage),length=40)
yfit<-dnorm(xfit,mean=mean(df_new$Daily.Internet.Usage),sd=sd(df_new$Daily.Internet.Usage))
yfit <- yfit*diff(h$mids[1:2])*length(df_new$Daily.Internet.Usage)
lines(xfit, yfit, col="black", lwd=2)
```
### Display count of our Advert clicks
```{r}
ggplot(df_new) + geom_bar(aes(x = Clicked.on.Ad),fill = 'orange')
```
###Frequency  of countries that participated in the study
```{r}
df_grouped <- data.frame(table(df_new$Country))
sorted_by_county <- df_grouped[order(-df_grouped$Freq),][1:10,]
sorted_by_county
```
# Plot frequency  of countries that participated in the study
```{r}
ggplot(sorted_by_county, aes(x= Freq , y=Var1)) +
  geom_bar(stat="identity", fill="steelblue")+
  geom_text(aes(label=Freq), vjust=-0.3, size=3.5)+
  theme_minimal()
```
## Bivariate Anlysis----
###Group mean numerical columns by click on the click
```{r}
df_new %>%
  group_by(Clicked.on.Ad) %>%
  summarise_if(is.numeric ,mean)
```
###Relationship between daily time spent on website and Clicking on the add
```{r}
ggplot(df_new) +
  geom_point(aes(x = Age, y= Daily.Time.Spent.on.Site ,color = Clicked.on.Ad))
```
###Relationship between Gender and Clicking on the Advert
```{r}
ggplot(df_new) +
  geom_point(aes(x = Male, y= Daily.Internet.Usage ,color = Clicked.on.Ad))
```
###Relationship between Income and Clicking on the add
```{r}
ggplot(df_new) +
  geom_point(aes(x = Area.Income, y= Daily.Time.Spent.on.Site ,color = Clicked.on.Ad))
```
###Relationship between Timestamp and Clicking on the add
```{r}
ggplot(df_new) +
  geom_point(aes(x = Timestamp , y= Daily.Internet.Usage ,color = Clicked.on.Ad))
```
###Correlation Matrix
###find correlation between columns
####use rcorr package
```{r}
df_num <- data.frame(select_if(df_new, is.numeric) )
res <- rcorr(as.matrix(df_num))
corr <- data.frame(res$r)
corr
```
###Create a correlation plot
```{r}
corrplot(res$r, type = "upper", order = "hclust",
         tl.col = "black", tl.srt = 45)
```
###Get covariance between variables
```{r}
covv <- data.frame(cov(df_num))
covv
```