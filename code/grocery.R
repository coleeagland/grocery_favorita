
library(mgcv)
library(sjmisc)
library(tidyverse)
library(lubridate)
library(fst)
library(data.table)
library(gridExtra)

setwd("E:/Grocery")

# Reading in all .csv files from folder and converting to data frames----
list.files(pattern="*.csv")

train        <- fread("train.csv")[, date := as.Date(fast_strptime(date, "%Y-%m-%d"))] #About 4.5GB
transactions <- fread("transactions.csv")[, date := as.Date(fast_strptime(date, "%Y-%m-%d"))]
items        <- fread("items.csv")
holidays     <- fread("holidays_events.csv")[, date := as.Date(fast_strptime(date, "%Y-%m-%d"))]
oil          <- fread("oil.csv")[, date := as.Date(fast_strptime(date, "%Y-%m-%d"))]
stores       <- fread("stores.csv")
calendar     <- fread("calendar_conversion.csv")[, date := as.Date(strp.date, "%m/%d/%Y")] %>% select(date,week,period,quarter,year,payday,payweek)


set.seed(822)
item.sample  <- sample(items$item_nbr, length(items$item_nbr)*0.1)
train <- train[item_nbr %in% item.sample]

write.fst(train,"train.fst", compress = 100)
train <- read.fst("train.fst", as.data.table = TRUE)[, date := as.Date(fast_strptime(date, "%Y-%m-%d"))]

# Functions ----

name.clean <- function(df) {
  names(df) <- gsub("_",".",names(df))
  names(df) <- tolower(names(df))
  names(df)
}


# The train data set is just too large with 16GB of RAM; will take sample
# Data cleanup (default to dplyr except on large train data set)

# Dates are in the correct format, but read in as character vectors
names(stores)        <- name.clean(stores)
names(test)          <- name.clean(test)  
names(train)         <- name.clean(train)
names(transactions)  <- name.clean(transactions)
names(items)         <- name.clean(items)
names(holidays)      <- name.clean(holidays)


# Exploratory Data Analysis ----
options(scipen=5)



###holidays
#Mostly character/factor variables, so an initial summary will return nothing.
holidays <- left_join(holidays,calendar) # Adding period/quarter/year

p1 <- ggplot(holidays %>% filter(year > 2012), aes(x=type, fill=type)) +
  geom_bar(fill=c("lightblue")) +
  ggtitle("Ecuador Holidays by type, 2013-2017") +
  theme_bw() +
  theme(legend.position = "none") 

p2 <- ggplot(holidays %>% filter(year > 2012), aes(x=locale, fill=locale)) +
  geom_bar(fill="lightblue") +
  ggtitle("Ecuador Holidays by locale, 2013-2017") +
  theme_bw() +
  theme(legend.position = "none") 

grid.arrange(p1,p2,ncol=2)

#We have 6 years of holiday data (Nearly - missing start of 2012).
#Most of the counts in the locale_name table are divisible by 6, with the exceptions of Cuenca,
#Guayaquil, Ibarra, and Quito. Let's look at these.

holidays %>% filter(locale_name %in% c("Cuenca","Guayaquil","Ibarra","Quito")) %>% arrange(locale_name)

#OK - in these four locales, the deviations are the result of transfers. And, though not directly relevant,
#each of these holidays is to celebrate/commemorate the founding of the locale.

#Being divisible by 6 isn't quite enough to confirm that the rest of the data is consistent. I'm really just trying to see whether
#there tend to be a lot of changes in Ecuador's holidays. So far, it seems like that's not the case. Let's look at the total by year:
ggplot(holidays %>% 
         filter(year > 2012) %>%
         group_by(year) %>%
         tally(), aes(x=year, y=n)) +
  geom_bar(stat="identity", fill="lightblue") +
  theme_bw() +
  ggtitle("Total Holidays by Year, 2013-2017")

#In fact, there is a lot of variation in the number of holidays each year! Let's compare 2013 (the first full year of holidays)
#with 2016, which has far more holidays than previous years. The table makes me suspect that 2014 and 2016 had something out
#of the ordinary. 2013/2015/2017 have 51, 52, and 53 holidays respectively.
holiday.viz <- holidays %>%
  filter(year > 2012) %>%
  group_by(period, year) %>%
  arrange(period) %>%
  tally() %>%
  mutate(year = factor(year))

ggplot(holiday.viz, aes(x=period, y=n, col=year)) +
  geom_line(size=1.5) +
  ggtitle("Ecuador Holidays by Period and Year, 2013 -2017") +
  theme_bw() +
  scale_x_continuous(breaks=seq(1,13,1)) +
  ylab("count") 

#Again, there's less consistency than I might expect. But, it's not unusual for the exact date of a holiday to move. What really
#stands out here are 2016, period 5 (33 holidays... in 4 weeks?) and 2014, period 7 (18 holidays in 4 weeks). That seems an unusually
#high number of holidays. But, at least it's narrowed down.

holidays %>% filter(year == 2014 & period == 7) #Of course! The 2014 world cup. But, these are not really holidays... they are events! 
#Now I understand the difference, and they must be treated differently. It's also nice to know what they really mean - this particular data
#is more than I expected to have on hand.

holidays %>% filter(year == 2016 & period == 5) #Again, an event, though a much less fun event. This is the April 16, 2016 earthquake and the
#30 days that followed (the days continue into period 6)

#Next, let's see what events occurred that were not the World Cup or the Earthquake
holidays %>% filter(type=="Event" & !grepl("Mundial de futbol", description) & !grepl("Terremoto Manabi", description)) %>%
  select(description) %>% unique()

holidays %>% filter(type=="Additional" & !grepl("Mundial de futbol", description) & !grepl("Terremoto Manabi", description)) #%>%
  select(description) %>% unique()

#Not many. Dia de la Madre (Mother's Day), plus Black Friday and Cyber Monday starting in 2014. Let's do another check of holidays,
#but without considering events.

ggplot(holidays %>%
         filter(type != "Event" & year > 2012) %>%
         group_by(year) %>%
         tally(), aes(x=year, y=n)) +
  geom_bar(stat="identity", fill="lightblue") +
  theme_bw() +
  ggtitle("Total Holidays by Year (Events Excluded), 2013-2017")


#Yes, much more consistent.

###items
table(items$family) %>% data.frame() %>% rename(Family = Var1, Frequency = Freq) %>% arrange(desc(Frequency)) %>% kable()

class.detail <- table(items$class) %>% data.frame() %>% rename(Class=Var1, Frequency = Freq)  %>% select(Frequency) %>%
  unlist() %>% unname() %>%  cut(breaks = c(0,1,2,3,4,5,10,20,50,100,133), include.lowest=TRUE) %>% table() %>% data.frame() %>%
  rename(NumItems = ".", NumClasses=Freq)

ggplot(class.detail, aes(x=NumItems, y=NumClasses)) +
  geom_bar(stat="identity", fill="lightblue") +
  scale_x_discrete(labels = c("1","2","3","4","5",">5 to 10",">10 to 20",">20 to 50",">50 to 100", ">100")) +
  labs(y="Number of Classes", x="Number of Items in Class") +
  ggtitle("Over 100 classes comprise only one or two items") +
  theme_bw()

oil <- oil %>% rename(price = dcoilwtico)

ggplot(oil, aes(x=date, y=price)) +
  geom_line(col="darkblue") +
  ggtitle("World Oil Prices, Jan 2013 - August 2017") +
  theme_bw()

library(sp)
ecuador <- readRDS("ECU_adm1.rds")
plot(ecuador)
ecuador[1]
###oil

###stores

table(stores$cluster)
table(stores$type)
table(stores$type,stores$cluster) %>% data.frame() %>% rename(Type = Var1, Cluster = Var2, Frequency = Freq) %>%
  spread(Cluster,Frequency)
spread(save, Cluster, Frequency)

cities <- read_csv("ecuadorcities.csv")

left_join(stores,cities) %>% arrange(desc(population))

###transactions

###train

# Supplementary Files

# Holidays

holidays.local <- filter(holidays, locale == "Local" & transferred == FALSE & type != "Work Day") %>%
                    select(date, locale.name, week, year) %>%
                    mutate(local.marker = 1)

holidays.regional<- filter(holidays, locale == "Regional" & transferred == FALSE & type != "Work Day") %>%
                    select(date, locale.name, week, year) %>%
                    mutate(regional.marker = 1)

holidays.national<- filter(holidays, locale == "National" & transferred == FALSE & type != "Work Day") %>%
                    select(date, locale.name, week, year) %>%
                    mutate(national.marker = 1)

earthquake <- holidays %>%
                select(date, earthquake) %>%
                filter(earthquake == 1) %>%
                rename(earthquake.marker = earthquake)

storeholidays <- data.frame(date=rep(seq(min(train$date), max(train$date), by = "days"), times = length(unique(stores$store.nbr))),
                            store.nbr = rep(unique(stores$store.nbr), each=length(seq(min(train$date), max(train$date), by = "days"))))
storeholidays <- left_join(storeholidays, select(stores,-type))
storeholidays <- left_join(storeholidays, select(holidays.local,-week,-year), by = c("date" = "date", "city" = "locale.name"))
storeholidays <- left_join(storeholidays, select(holidays.regional,-week,-year), by = c("date" = "date", "state" = "locale.name"))
storeholidays <- left_join(storeholidays, select(holidays.national,-week,-year,-locale.name), by = "date")
storeholidays <- storeholidays %>% left_join(earthquake)
storeholidays <- distinct(storeholidays) #Some duplicates created in join (from date/)

storeholidays <- storeholidays %>%
                  mutate_at(.vars = vars(ends_with("marker")), .funs = funs(replace_na), value=0)

storeholidays <- storeholidays %>% left_join(calendar)
storeholidays <- storeholidays %>%
                  group_by(year,week,store.nbr) %>%
                  summarize(local.holiday = max(local.marker),
                            regional.holiday = max(regional.marker),
                            national.holiday = max(national.marker),
                            earthquake.marker = max(earthquake.marker),
                            payweek = max(payweek))
storeholidays <- storeholidays %>% mutate(holiday = pmax(local.holiday,regional.holiday,national.holiday))


#OIL
oil
oil <- oil %>% rename(oil.price = dcoilwtico) #Making the name easier to read

oildat <- data.frame(date = seq(min(oil$date),max(oil$date),by = "days"))
oil <- left_join(oildat,oil) %>%
  mutate(day.of.week = lubridate::wday(date)) %>%
  data.table()
rm(oildat)

oil <- oil[, ':='(oil.price = ifelse(day.of.week == 7, lag(oil.price,1L),
                                     ifelse(day.of.week == 1, lag(oil.price,2L), oil.price)))][
                                       , oil.price := na.approx(oil.price, na.rm=FALSE)]

oil <- left_join(oil,calendar %>% select(date, week, year))
oil <- oil %>% group_by(year,week) %>% summarize_at(.vars = vars(starts_with("oil")), .funs = funs(median), na.rm=TRUE) %>% ungroup()
oil2$weekyear <- 1:nrow(oil2)
oil2



[
  , ':='(oil.7dprior  = lag(oil.price, 7L),
         oil.15dprior = lag(oil.price, 15L),
         oil.30dprior = lag(oil.price, 30L),
         oil.60dprior = lag(oil.price, 60L))]

library(gridExtra)
a<-ggplot(oil, aes(x=date, y=oil.price)) +
  geom_point(colour = "blue") +
  ggtitle("Daily Oil Price", y = "Oil Price ($)") +
  theme_bw()
b<-ggplot(oil2, aes(x=weekyear, y=oil.price)) +
  geom_point(colour = "blue") +
  ggtitle("Median Oil Price by Week") +
  labs(x = "Weeks From Start", y = "Oil Price ($)") +
  theme_bw()
grid.arrange(a,b)
a
b

class(oil)
class(oil2)
head(oil)
head(oil2)
#Data merge

length(seq(min(train$date), max(train$date), by = "days"))
train <- left_join(train,items)
train <- left_join(train,stores)
holidays.join <- holidays %>% select(date, holiday.type = type, locale,locale.name,transferred)
train <- left_join(train,holidays.join)
train <- left_join(train, oil)
train <- left_join(train, calendar)
train <- data.table(train)

