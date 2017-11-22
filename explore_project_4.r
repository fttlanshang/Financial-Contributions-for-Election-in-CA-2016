getwd()
# setwd('../Desktop/project 4/')
setwd('D:/Udacity-DAND/project 4/')
list.files()

# IDEAS: I need to compare the result with the actual life, and read more about people's preferences in CA online
# Also, I need to learn the candidate, and their party
# Can I use financial support data to predict the support rate in CA,
# Maybe I'll combine the other candidates' data into two


# (1)load the data into environment, once had problems in row.names
financial <- read.csv('./P00000001-CA.csv')
# Maybe I should take a sample from this dataset and then apply it to the population dataset.
# There are 19 variables and 130,4346 observations in this dataset.
str(financial)
# When altering the row.names, found that the variable name and value can't match, meaning there is a shift in key and value
head(financial)
summary(financial)
#1. explore one variable at a time
library(ggplot2)
names(financial)
# bar plot shows how many times the candidate or committee have been contributed to
# 1.1 committee
ggplot(aes(x = cmte_id), data = financial) + geom_bar()
length(unique(financial$cmte_id)) # there are 25 different committees
# how to check committee id and candidate id are related one by one?
length(unique(c(financial$cmte_id, financial$cand_id))) # 25
financial$cmte_cand <- paste(financial$cmte_id, financial$cand_nm, sep = '_')
head(financial$cmte_cand)
unique(financial$cmte_cand)
# So each candidate has a committee, thus carrying less information

# 1.2 candidate
table(financial$cand_nm)
head(sort(table(financial$cand_nm), decreasing = TRUE), 5)
ggplot(aes(x = cand_nm), data = financial) + geom_bar()
# For committee and candidate and a lot other variables, like contbr_nm,
#it would be better to use table or bar plot to summarise

# 1.3 contribution city
table(financial$contbr_city)
head(sort(table(financial$contbr_city), decreasing = TRUE), 10)
# Maybe I should get information about the city's financial level and their rank
# I want to find the 10 cities with the contribution for 
# quite many times(here not for money)
ggplot(aes(x = contbr_city), data = financial) + geom_bar()
length(unique(financial$contbr_city)) # 2534 unique contributor cities

# 1.4 occupation bar chart
head(sort(table(financial$contbr_occupation), decreasing = TRUE), 20)
# Why there are so many retired men contributed
ggplot(aes(x = contbr_occupation), data = financial) + geom_bar()
# just like the city, I can compare the actual receipt amount due to their occupation

# 1.5 receipt amount histogram
# For CONTRIBUTION RECEIPT AMOUNT, there are many negative values, weird
# Also, I don't know the unit, I think at least it's thousands..
negative_contb_subset <- subset(financial, contb_receipt_amt <= 0)
length(negative_contb_subset) #16366 observations with contribution <= 0??
str(negative_contb_subset)
summary(negative_contb_subset)


ggplot(aes(x = contb_receipt_amt), 
       data = subset(financial, contb_receipt_amt > 0)) + 
  geom_histogram() +
  scale_x_log10()
# kind like a normal distribution in log scale

# 1.6 contribution time distribution
table(financial$contb_receipt_dt)
head(sort(table(financial$contb_receipt_dt), decreasing = TRUE), 20)
# wonder if there is a time contributed much larger than any other time,
# and it's related to the actual election event
# Here dt is considered as a factor variable, I need to transform it to datatime first
install.packages("lubridate")
library(lubridate)
financial$contb_receipt_dt <- dmy(financial$contb_receipt_dt)
head(financial$contb_receipt_dt)
# Second challenge: transform the time variable
# Why as.Date and strptime don't work as I expected, they only generated NA
# So I used a package called lubridate, very easy to use
# Now I can group into month
length(unique(year(financial$contb_receipt_dt)))
table(year(financial$contb_receipt_dt))
# Wow, the contribution even start on 2013, 
# maybe there will be multiple receipts from same person
financial$contb_receipt_dt_yr_mo <- strftime(financial$contb_receipt_dt, "%Y%m")
head(financial$contb_receipt_dt_yr_mo)
ggplot(aes(x = contb_receipt_dt_yr_mo), data = financial) + geom_bar()

names(financial)

# 1.7 form type
table(financial$form_tp)
ggplot(aes(x = form_tp), data = financial) + geom_bar()
# SA17A£º individual contributions
# SA18: transfers from authorized committees, does it mean negative numbers?
# SA28A: refunds to individuals'
# Still confused on what's going on here

# 1.8 election_tp, what does this variable mean
table(financial$election_tp)
ggplot(aes(x = election_tp), data = financial) + geom_bar()

## 2 Explore two variables
install.packages('dplyr')
library(dplyr)
# Here, I mean try connect the receipt amount and other variables

# 2.1 candidate & receipt amount
names(financial)
ggplot(aes(x = cmte_id, y = contb_receipt_amt), data = financial) +
  geom_boxplot()
by(financial$contb_receipt_amt, financial$cmte_id, sum)

financial.cand_id_amt <- financial %>%
  group_by(cand_nm) %>%
  summarise(total_amt = sum(contb_receipt_amt)) %>%
  arrange(cand_nm)
head(financial.cand_id_amt)
str(financial.cand_id_amt)
ggplot(aes(x = cand_nm, y = total_amt), data = financial.cand_id_amt) +
  geom_bar(stat = "identity") + scale_y_log10()
# should I use log10 scale here??
# Who received the most amount?
financial.cand_id_amt[which.max(financial.cand_id_amt$total_amt),]
# max sum is 93681171 and Clinton, Hillary Rodham receives it

# 2.2 city & receipt amount

ggplot(aes(x = contbr_city, y = contb_receipt_amt), data = financial) +
  geom_boxplot()
# since there are so many cities,
# it's not a good idea to draw boxplot using city as x axis

financial.city_amt <- financial %>%
  group_by(contbr_city) %>%
  summarise(total_amt = sum(contb_receipt_amt)) %>%
  arrange(contbr_city)

financial$contbr_city
head(financial$contbr_city, 50)
head(financial.city_amt, 50)
financial.city_amt
# Why there are so many invalid city names???
financial.city_amt[which.max(financial.city_amt$total_amt),]
# Can I get top 10 contribution cities in CA?
o <- order(financial.city_amt$total_amt, decreasing=TRUE)[1:10]
top_10_contributing_cities <- financial.city_amt[o,] 
top_10_contributing_cities
ggplot(aes(x = contbr_city, y = total_amt ), 
       data = top_10_contributing_cities) + 
  geom_bar(stat = 'identity')

# 2.3 occupation & receipt amount
financial.occupation_amt <- financial %>%
  group_by(contbr_occupation) %>%
  summarise(total_amt = sum(contb_receipt_amt)) %>%
  arrange(contbr_occupation)

head(financial.occupation_amt)

financial.occupation_amt[which.max(financial.occupation_amt$total_amt),]
# Wow, rich people are retired?!
# Can I get top 10 contribution occupations in CA?
o <- order(financial.occupation_amt$total_amt, decreasing=TRUE)[1:10]
top_10_contributing_occupations <- financial.occupation_amt[o,] 
top_10_contributing_occupations
ggplot(aes(x = contbr_occupation, y = total_amt ), 
       data = top_10_contributing_occupations) + 
  geom_bar(stat = 'identity')

# 2.4 contribution time & receipt amount
financial.dt_amt <- financial %>%
  group_by(contb_receipt_dt_yr_mo) %>%
  summarise(total_amt = sum(contb_receipt_amt)) %>%
  arrange(contb_receipt_dt_yr_mo)

head(financial.dt_amt)

financial.dt_amt[which.max(financial.dt_amt$total_amt),]
# Which month received most contributions? Is there a cyclic routine?
# like a candidate goes here for a speech and then contribution goes up
ggplot(aes(x = contb_receipt_dt_yr_mo, y = total_amt ), 
       data = financial.dt_amt) + 
  geom_bar(stat = 'identity')
# Obviously, 201607-201610 are the most intense periods.

## 3 ggpair to form scatterplot matrics
install.packages('GGally')
library(GGally)
# only 1 variable is numericial variable, 
# also I need to exclude some variables from this dataset
# I need to take samples from population
names(financial)
str(financial)
ggpairs(financial, cardinality_threshold = 25)
?ggpairs

# 4. I want to find out How do each candidate support change due to time?
ggplot(aes(x = contb_receipt_dt_yr_mo, y = contb_receipt_amt),
       data = financial) + geom_line() +
  facet_wrap(~cand_nm)

str(top_10_contributing_cities)
ggplot(aes(x = contb_receipt_dt_yr_mo, y = contb_receipt_amt),
       data = subset(financial, 
                     contbr_city %in% top_10_contributing_cities$contbr_city)) + 
  geom_line() +
  facet_wrap(~contbr_city)

names(financial)

# 5. draw contribution data on a map
names(financial)
head(financial$contbr_zip)

install.packages('zipcode')
library(zipcode)
install.packages('ggmap')
library(ggmap)

data(zipcode)
head(zipcode)

financial$modified_contbr_zip <- clean.zipcodes(financial$contbr_zip)
head(financial$modified_contbr_zip)
# I need to aggregate the same zipcode together, 
# I can first aggregate, then merge it with zipcode
financial.zip_amt <- financial %>%
  group_by(modified_contbr_zip) %>%
  summarise(total_amt = sum(contb_receipt_amt)) %>%
  arrange(modified_contbr_zip)
str(financial.zip_amt)

new_financial <- merge(financial.zip_amt, zipcode, 
                       by.x='modified_contbr_zip',
                       by.y='zip')
head(new_financial)
length(unique(new_financial$modified_contbr_zip)) #2128
table(new_financial$state) 
# Why there are a few contributions mapped to other state?
# Because they are in different state when they contributed??
# without map version
ggplot(data = subset(new_financial, state == 'CA')) + 
  geom_point(aes(x=longitude, y=latitude, size = total_amt),
             alpha = 1 / 4, color = 'orange')

# with map version
qmplot(longitude, latitude, size = total_amt,
       data = subset(new_financial, state == 'CA'), 
      color = I("red"), alpha = 1 / 20)
# still needs some adjustment
# maybe it's better to use color rather than size to 
# represent total_amt difference


# I think the most frequent way I'm gonna use is compare various variables on different candidates