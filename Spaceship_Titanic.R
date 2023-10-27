#Make sure all needed R packages are installed
install.packages("readr")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("tidyverse")
install.packages("plotly")

#Make sure all needed R packages are loaded
library(readr)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(plotly)

#import data (as a variable)
test_data <- read_csv("HW-5_Spaceship_Titanic/test.csv")

#Read all data
test_data

#Read all data with view function
View(test_data)

# get some data summarization including data types
str(test_data)
#Note the data uses no "integers" only doubles even for age

#Find the max, mean, median, & std dev value for the numeric columns (min was 0 for each including age)
numeric_test_cols <- sapply(test_data, is.numeric)
summary_test <- sapply(test_data[, numeric_test_cols], function(x) c(Max_test = max(x), Avg_test = mean(x), Median_test = median(x), SD_test = sd(x)))
#Got NA for all values, likely because there were NA's in the data and I did not remove them when calculating
summary_test
#Attempt 2
summary_test <- sapply(test_data[, numeric_test_cols], function(x){
  c( 
    Max_test = max(x, na.rm = TRUE), Avg_test = mean(x, na.rm = TRUE), Median_test = median(x, na.rm = TRUE), SD_test = sd(x, na.rm = TRUE)
    )
  })
summary_test

#I noticed standard deviation was hard to tell how significant it is so I'm going to add a Coefficient of Variance column
summary_test <- sapply(test_data[, numeric_test_cols], function(x){
  c( 
    Max_test = max(x, na.rm = TRUE), Avg_test = mean(x, na.rm = TRUE), Median_test = median(x, na.rm = TRUE), SD_test = sd(x, na.rm = TRUE), CV_test = sd(x, na.rm = TRUE) / mean(x, na.rm = TRUE)
  )
})
summary_test

#I want to plot some graphs to see the interaction between different numeric and logical variables
#Additional notes: ID,cabin, & name are likely not useful. Cryo, VIP are logical. Home, dest, are sublime. The rest are relatively continuous (my not technically be, but they can be treated that way)
#Most people kept the majority of their spending to just FoodCourt or just RoomService, no noticeable impact from VIP status
ggplot(test_data, aes(x = RoomService, y = FoodCourt, color = VIP))  +
  geom_point()

#Now comparing RS vs SM and changed my logical to cryo
#similar to RS vs FC, but less people sticking entirly to one or the other, and it forms a better curve, Cryo seems to make no difference 
ggplot(test_data, aes(x = RoomService, y = ShoppingMall, color = CryoSleep))  +
  geom_point()

#same comparison and changed my logical back to VIP
#VIP's tended to spend a little more overall (
ggplot(test_data, aes(x = RoomService, y = ShoppingMall, color = VIP))  +
  geom_point()

#Now comparing FC vs SM and keeping my logical at VIP
#very strong correlation to people only spending on 1 of the 2, VIP's tended to spend a little more overall
ggplot(test_data, aes(x = FoodCourt, y = ShoppingMall, color = VIP))  +
  geom_point()

#Now comparing RS vs Spa and keeping my logical at VIP
#similar trend 
ggplot(test_data, aes(x = RoomService, y = Spa, color = VIP))  +
  geom_point()

#Now comparing RS vs Spa and keeping my logical at VIP
#similar trend 
ggplot(test_data, aes(x = ShoppingMall, y = Spa, color = VIP))  +
  geom_point()


#Now comparing VRdeck vs Spa and keeping my logical at VIP
#similar trend (although the curve is a little better filled out)
ggplot(test_data, aes(x = VRDeck, y = Spa, color = VIP))  +
  geom_point()

#Now comparing VRdeck vs ShoppingMall and keeping my logical at VIP
#very strong trend 
ggplot(test_data, aes(x = VRDeck, y = ShoppingMall, color = VIP))  +
  geom_point()

#Now comparing VRdeck vs RoomService and swapped logical back to CryoSleep
#very strong trend, no correlations with cryo and either factor
ggplot(test_data, aes(x = VRDeck, y = RoomService, color = CryoSleep))  +
  geom_point()

#the next 5 plots are comparing age to purchase amounts
#Now comparing age vs RoomService and swapped logical back to VIP
#nothing under age 13, and then after age 40 starts slowing down
ggplot(test_data, aes(x = Age, y = RoomService, color = VIP))  +
  geom_point()

#Now comparing age vs FC and kept logical at VIP
#nothing under age 13, and very slight diminishing spending after 45
ggplot(test_data, aes(x = Age, y = FoodCourt, color = VIP))  +
  geom_point()

#Now comparing age vs SM and kept logical at VIP
#nothing under age 13, and spending peaks at 16 and trends down
ggplot(test_data, aes(x = Age, y = ShoppingMall, color = VIP))  +
  geom_point()

#Now comparing age vs Spa and kept logical at VIP
#nothing under age 13, and then no strong trends
ggplot(test_data, aes(x = Age, y = Spa, color = VIP))  +
  geom_point()

#Now comparing age vs VRDeck and kept logical at VIP
#nothing under age 13, and weak downward trend
ggplot(test_data, aes(x = Age, y = VRDeck, color = VIP))  +
  geom_point()

#creating data for a graph to see how many categories people are spending in
#not graphing what I inteded (trying again below)
test_data$count_categories <- rowSums(test_data[, c("RoomService", "FoodCourt", "ShoppingMall", "Spa", "VRDeck" )])
ggplot(test_data, aes(x = count_categories)) +
  geom_bar()

#Attempt 2 create new column in my data to graph
test_counts <- test_data %>%
  rowwise() %>%
  mutate(categories_spent = sum(c("RoomService", "FoodCourt", "ShoppingMall", "Spa", "VRDeck" ) >0)) %>%
  group_by(categories_spent) %>%
  summarise(test_ids = n())
test_counts <- test_counts %>% complete(categories_spent, fill = list(test_ids = 0))
#now graph it (got a single bar, I wanted 6 bars)
ggplot(test_counts, aes(x= as.factor(categories_spent), y = test_ids)) +
  geom_bar(stat = "identity") +
  ylim(0,10000) +
  labs(x = "Number of Categories Spent in", y = "Number of Passengers")
#Attempt 3 create new column in my data to graph
test_counts_long <- test_data %>%
  gather(Category, Count, -PassengerId) %>%
  group_by(PassengerId) %>%
  mutate(Categories_spent = sum(Count >0))

test_counts <- test_counts_long %>%
  group_by(Categories_spent) %>%
  summarise(test_ids = n())
test_counts <- test_counts %>% complete(Categories_spent, fill = list(test_ids = 0))
#now graph it (closer, this time I counted total columns used and not just the ones related to spending)
ggplot(test_counts, aes(x= as.factor(Categories_spent), y = test_ids)) +
  geom_bar(stat = "identity") +
  ylim(0,10000) +
  labs(x = "Number of Categories Spent in", y = "Number of Passengers")


#Attempt 4 create new column in my data to graph - only using columns I care about -successful!!!
test_data <- test_data %>%
  select(PassengerId, RoomService, FoodCourt, ShoppingMall, Spa, VRDeck) %>%
  mutate(Categories_spent = rowSums(.[-1] > 0))

test_counts <- test_data %>%
  group_by(Categories_spent) %>%
  summarise(Num_Passengers = n())


# Create the bar chart 
#there is a difference in how excel and R calculate the data, with missing data, showing up as NA in R 
#Lots of 0's and then resembles a bell curve on 3 categories
ggplot(test_counts, aes(x = as.factor(Categories_spent), y = Num_Passengers)) +
  geom_bar(stat = "identity") +
  labs(x = "Number of Categories Spent In", y = "Number of Passengers")

table(test_data$Categories_spent)


#Bar graph comparing VIP vs spending
#VIP spent on average about 3x as much
test_data <- read_csv("HW-5_Spaceship_Titanic/test.csv")

VIP_spent <- test_data %>%
  group_by(VIP) %>%
  summarise(Average_Spent = mean(RoomService + FoodCourt + ShoppingMall + Spa + VRDeck, na.rm = TRUE)) #had to add na.rm = TRUE because initially showed no graphs


ggplot(VIP_spent, aes(x = VIP, y = Average_Spent)) +
  geom_bar(stat = "identity") +
  labs(x = "VIP status", y = "Average Spent")

str(test_data$VIP)

#Historgram by age group:
ggplot(test_data, aes(x = Age)) +
  geom_histogram(binwidth = 5, fill = "red") +
  labs(x = "Age", y = "Total in age group")

#Line graph by age group vs money spent:
average_spending <- test_data %>%
  group_by(Age) %>%
  # remember to close parenthesis before na.rm
  summarise(average_spending = mean(rowSums(select(., RoomService, FoodCourt, ShoppingMall, Spa, VRDeck), na.rm = TRUE)))

ggplot(average_spending, aes(x = Age, y = average_spending)) +
  geom_line(color = "blue") +
  labs(x = "Age", y = "Total Spending")

#Line graph attempt 2 
average_spending <- test_data %>%
  gather(Category, Count, RoomService:VRDeck) %>%
  replace_na(list(Count = 0)) %>%
  group_by(Age) %>%
  summarise(average_spending = mean(Count))

ggplot(average_spending, aes(x = Age, y = average_spending)) +
  geom_line(color = "blue") +
  labs(x = "Age", y = "Average Spending")

#Final graph pi chart, had to get plotly:
#largest 3 age_groups of VIP are 20-25, 25-30, &15-20, in total making up almost 50% of the VIP's
age_VIP <- test_data %>%
  filter(is.numeric(Age)) %>% #had to add filter to remove missnig values, might be better than using na.rm as above
  mutate(Age_Group = cut(Age, breaks = seq(0, 85, by =5), right= FALSE)) %>% #originally tried to use a formula for max age, which didn't work
  group_by(VIP, Age_Group) %>%
  summarise(Count = n())

plot_ly(data = age_VIP, labels = ~Age_Group, values = ~Count, type = 'pie', ids = ~VIP, names = ~VIP)
