print("Fatima Nurmakhamadova")

#installing all packages
install.packages("FSA")
install.packages("FSAdata")
install.packages("knitr")
install.packages("magrittr")
install.packages("tidyr")
install.packages("plyr")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("ggrepel")
install.packages("tidyverse")
install.packages("hrbrthemes")
install.packages("esquisse")
install.packages("psych")
install.packages('epiDisplay')
install.packages("vcd")
install.packages("lubridate")
install.packages("ggpubr")
install.packages("fastDummies")
install.packages("ggiraph")
install.packages("ggiraphExtra")

library(FSA)
library(FSAdata)
library(knitr)
library(magrittr)
library(tidyr)
library(plyr)
library(dplyr)
library(ggplot2)
library(ggrepel)
library(tidyverse)
library(hrbrthemes)
library(data.table)
library(esquisse)
library(psych)
library(epiDisplay)
library(vcd)
library(lubridate)
library(ggpubr)
library(fastDummies)
library(ggiraph)
library(ggiraphExtra)

####Exploratory Data Analysis of wine dataset

#1 - Extract the dataset and create a data frame
wine <- read.csv("/Users/ms_fatishi/Downloads/Wine_tasting.csv",  header=TRUE, stringsAsFactors = TRUE)
#wine1 <- read.csv(file=file.choose(),header=TRUE, sep=",") #another new way 
wine_df <- data.frame(wine)
wine_df$points<- as.numeric(wine_df$points)
wine_df$price<- as.numeric(wine_df$price)

# data cleaning 
summary(wine_df)
colnames(wine_df)
rownames(wine_df)

#1- Selecting a subsets of interest
summary(wine_df$country)
#unique(wine_df$country) #another new way

#frequency table of country variable
tab1(wine_df$country, sort.group = "decreasing", cum.percent = TRUE, main="Wine frequency by Country")

#Subset of countries by New and Old World
new_world <- wine_df  %>% filter(country == "Argentina" | country == "Australia" | 
                                   country == "Canada" | country == "Chile" |country == "Mexico" | 
                                   country=="New Zealand" | country=="South Africa"| country=="US")
new_world <-na.omit(new_world)
#add ccolumn to distingiush by world
new_world <- new_world %>%
  add_column(World = "new_world",
             .after = "X") 

old_world <- wine_df  %>% filter(country == "Austria" | country == "France" | country=="Germany" | 
                                   country== "Greece" | country== "Hungary" | country=="Israel" | 
                                   country=="Italy" | country== "Portugal" | country== "Romania"| 
                                   country=="Spain")
old_world <-na.omit(old_world)
#add ccolumn to distingiush by world
old_world <- old_world %>%
  add_column(World = "old_world",
             .after = "X") 


#Combining Old and New World into one data frame
world <- rbind(old_world, new_world)
view(world)
world$World <- as.factor(world$World)


#Extracting Subsets of interest: World, points, and price
world1 <-subset(world, select = c(World, points, price))
world1 

#Grouping subsets by world and points, then summarizing by price
wine_world <- world1 %>% 
  group_by(World, points) %>% 
  summarise(across(everything(), sum))

#then sort a dataset by points
wine_world <- wine_world[order(wine_world$points),] 
wine_world <-na.omit(wine_world)
attach(wine_world)
wine_world$World <- as.factor(wine_world$World)

#Creating the ggplot bar of New and Old Worlds wines comparison points by price
ggplot(wine_world, aes(fill=World, y=price, x=points)) + 
  geom_bar(position = "dodge", stat="identity")+
  ggtitle("New and Old Worlds Wines Comparison")+
  theme(axis.text.x = element_text(size=10),
        axis.text.y = element_text(size=10),
        axis.title.x = element_text(size=14),
        axis.title.y = element_text(size=14),
        plot.title = element_text(face = "bold", hjust = 0.5, size=16))+
  scale_x_continuous(limits=c(80, 100),
                     breaks= scales::pretty_breaks(n=10))+
  scale_y_continuous(limits=c(0, 3300),
                     breaks= scales::pretty_breaks(n=15))

#Creating a density plot. Wine Density Shaded by Worlds
cols1 <- c("brown1", "darkturquoise") #creating color factor for two levels of shading for the “green” data points
cols1
cols_fac1 <- cols1[factor(wine_world$World)]
cols_fac1
x <- wine_world$points
y <- wine_world$price
overdense_plot= ggplot(data = wine_world, aes(x, y))+
  ggtitle("Wine Density Shaded by Worlds")+
  geom_point(col=cols_fac1, shape=25, show.legend = T)+ 
  xlab("points")+ylab("price")+
  theme(axis.text.x = element_text(size=10),
        axis.text.y = element_text(size=10),
        axis.title.x = element_text(size=14),
        axis.title.y = element_text(size=14),
        plot.title = element_text(face = "bold", hjust = 0.5, size=16))
overdense_plot

#descriptive statistics of the main dataset
summary(wine_df)
as_tibble(wine_df) #2nd option as str
describe(wine_df)

#descriptive statistics of the dataset of interest
summary(wine_world)
by(wine_world, wine_world$World, summary) #descriptive statistics by group
as_tibble(wine_world) #2nd option as str
describe(wine_world)


############20211219 Final project

###1 - Extract the dataset and create a data frame
wine <- read.csv(file=file.choose(),header=TRUE, sep=",")
wine_df <- data.frame(wine)
wine_df <-na.omit(wine_df)

#Pinot Noir from US, price/rank, price/location
Pinot_Noir <- wine_df  %>% filter(country == "US", variety == "Pinot Noir")

#choosing only needed columns
Pinot_Noir <- subset(Pinot_Noir, select = c('points','price', 'province', 'variety')) %>% droplevels

#checking the locations frequency of Pinot Noir 
tab1(Pinot_Noir$province, sort.group = "decreasing", cum.percent = TRUE, main="Pinot Noir frequency by province")
#checking the price frequency of Pinot Noir 

#Delete New York because it has only one wine. We will compare price in California and Oregon
PN_df <- Pinot_Noir[Pinot_Noir$province != "New York", ]  %>% droplevels
hist(PN_df$price)
summary(PN_df$price)

#ggplot and linear regression for price by ranking
plot_rank <- ggplot(PN_df,aes(y=price,x=points))+geom_point()+geom_smooth(method="lm",se=TRUE)+
  ggtitle("Price by Ranking")+theme(plot.title = element_text(face = "bold", hjust = 0.5, size=15))
plot_rank

#~~~~~~~~~~~~~~Analysis~~~~~~~~~~~~~~~~~~
##QUESTION 1:
#How does ranking (points) affect the price of Pinot Noir in the US?
##Step 1 - Two-sample t-test: Is there any significant difference in points between California and Oregon wine?
t.test(points ~ province, data = PN_df, var.equal = TRUE) #no significant difference

##Step 2 - Two-sample t-test: Is there any significant difference in price between California and Oregon wine?
t.test(price ~ province, data = PN_df, var.equal = TRUE) #no significant difference

##Step 3 - Regression testing of relationship between Price and points of Pinot Noir in the US
lm_points <- lm(PN_df,formula=price ~ points)
summary(lm_points) #R2 0.2255


#~~~~~~~~~~~~~~
##QUESTION 2: 
#How does location (province) affect the price of Pinot Noir in the US?
##Step 1- Regression testing of relationship between Price and both locations 
lm_province <- lm(PN_df,formula=price ~ province)
summary(lm_province) #location itself does not have any affect on the price

##Step 2-Regression test: Check how the ranking & locations affect the price
lm_prov_price <- lm(PN_df,formula=price ~ points+province)
summary(lm_prov_price)

##Step 3 - Multiple linear regression of price and ranking by locations
ggPredict(lm_prov_price, interactive = TRUE)


#~~~~~~~~~~~~~~
##QUESTION 3: 
#How does ranking affect the price in each location?

#Step 1 - making two subsets by province
PN_California <-  subset(Pinot_Noir, province == "California", select = c('points','price', 'province', 'variety'))
PN_Oregon <- subset(Pinot_Noir, province == "Oregon", select = c('points','price', 'province', 'variety'))

#Step 2 - regression test of price affect by points 
#in California 
lm_cal_price <- lm(PN_California,formula=price ~ points)
summary(lm_cal_price)

#in Oregon
lm_oreg_price <- lm(PN_Oregon,formula=price ~ points)
summary(lm_oreg_price)


