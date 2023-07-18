##loading dataset


## Travel <- read.csv("C:\\Users\\shrut\\Pythion Class\\Travel.csv")

Travel <- read.csv("C:\\Users\\shrut\\Travel1.csv")
Travel
head(Travel)
tail(Travel)

names(Travel)
colnames(Travel)

## rownames(Travel)

typeof(Travel)
str(Travel)
summary(Travel)
##Renaming column names
colnames(Travel)[1] <- ('TripID')
  
   ## colnames(Travel)[3:13] <- ('Start','End','Duration','Name','Age','Gender','Nationanlity','Acc_Type','Acc_cost','Trans_Type','Trans_Cost')

colnames(Travel)[3:13] <- c('Start','End','Duration','Name','Age','Gender','Nationanlity','Acc_Type','Acc_cost','Trans_Type','Trans_Cost')

colnames(Travel)


##################################################



##remove extra column Start in the end

Travel[3] == Travel[14]

drop(Travel[14]) ##DIDNOT WORK
colnames(Travel) 

Travel[,14]
Travel[,-14]


Travel <- Travel[,-14]
Travel

##################################################


##convert datatypes of columns
str(Travel)

## Travel$Start <- as.Date(Travel$Destination, format = "%m-%d-%Y")
## Travel$Start <- as.Date(Travel$Destination, format = "%m-%d-%Y")

## Travel$Start <- ymd(Travel$Start)

## Travel$Start <- dmy(Travel$Start)


library(lubridate)

Travel$Start <- mdy(Travel$Start)
class(Travel$Start)
Travel$Start

Travel$End <- mdy(Travel$End)
class(Travel$End)

################################################## 


##checking na values

is.na(Travel)

sum(is.na(Travel))

## drop_na(Travel, inplace = TRUE)

sum(is.na(Travel))

Travel <- na.omit(Travel)

sum(is.na(Travel))
## which(is.na(Travel))

## colSums(is.na(Travel))

## complete.cases(Travel)
## na.omit(Travel, inplace = TRUE)

sum(is.na(Travel)) ## why still sum is 8??

is.null(Travel)

sum(Travel$Age)
sum(Travel$Age,na.rm = TRUE) ##still we have null values


 ###got it





########################################################
##Currecny conversion in Trans Cost and Acc cost

## Travel$Trans_Cost = gsub('[$nUSD]','',Travel$Trans_Cost)
## Travel$Trans_Cost = str_replace(Travel$Trans_Cost,'USD','')



## Travel$Trans_Cost <- as.numeric(Travel$Trans_Cost)
unique(Travel$Trans_Cost)


Travel$Trans_Cost <- as.numeric(gsub("\\D", "", Travel$Trans_Cost)) ########IMPPPP
##above code, \\D means non-digit characters

Travel$Trans_Cost
mean(Travel$Trans_Cost,na.rm=TRUE)
class(Travel$Trans_Cost)


Travel$Acc_cost
class(Travel$Acc_cost)

Travel$Acc_cost <-  as.numeric(gsub("\\D","", Travel$Acc_cost))
class(Travel$Acc_cost)

mean(Travel$Acc_cost)

##2nd method
library(readr)

parse_number(Travel$Acc_cost)

########################################################


##replace column values for Trans Type

Travel$Trans_Type

unique(Travel$Trans_Type)

Travel$Trans_Type <- str_replace_all(Travel$Trans_Type, "Flight", "Plane")
Travel$Trans_Type <- str_replace_all(Travel$Trans_Type, "Airplane", "Plane")

##2nd way

# Travel %>% select(Trans_Type) %>%  mutate(Trans_Type = str_replace(Travel$Trans_Type, 'Airplane', 'Plane'))

###########################################################


##Split Destination in country, City and replace country where there is no country

unlist(strsplit(Travel$Destination,","))


## Travel %>% separate_wider_delim(Travel$Destination, ",", names = c("City", "Country"))

## ?separate_wider_delim

## Travel %>% separate(Travel$Destination, ",", into =  c("City", "Country"))

## Travel %>% separate(Travel$Destination, names = c("City", "Country"))


sum(is.na(Travel$Country))

Travel <- separate(Travel, col=Destination, into=c('City', 'Country'), sep=',')

names(Travel)

####################################################################

Travel %>% ggplot(aes(Age)) + geom_bar()
Travel %>%  ggplot(aes(Gender)) + geom_bar()
Travel %>% ggplot(aes(Age, fill=Gender)) + geom_bar()
Travel %>% ggplot(aes(Gender,Age)) + geom_boxplot()



Travel %>% ggplot(aes(Age,Acc_Type)) + geom_point()

Travel %>% ggplot(aes(Acc_Type,Acc_cost)) + geom_boxplot()
Travel %>% ggplot(aes(Trans_Type,Trans_Cost)) + geom_boxplot()


## Travel %>%ggplot(aes(Acc_cost,Trans_Cost)) + geom_point()



## barplot(Travel$Acc_cost, names.arg = Travel$Acc_Type, col = "red") 

##AccHist <- ggplot(data = Travel, aes(x = Acc_cost)) 
## AccHist + geom_histogram(binwidth = 0.2, color = "black", aes(fill = Acc_Type)) +   xlab("Acc Cost") +  ylab("Frequency") + ggtitle("Histogram of Acc cost vs type")

## TraHist <- ggplot(data = Travel, aes(x = Trans_Cost)) 
## TraHist + geom_histogram(binwidth = 0.2, color = "black", aes(fill = Trans_Type)) + xlab("Trans Cost") +  ylab("Frequency") + ggtitle("Histogram of Acc cost vs type")




##x = sort(table(Travel$Trans_Type), decreasing = TRUE)

## Travel %>% ggplot(aes(x)) + geom_bar()






## cit <- ggplot(data=Travel, aes(unique(Travel$City)))
## cit + geom_histogram(aes(Acc_Cost))




##which nationality spending more on acc
ggplot(Travel, aes(x=Acc_cost, y=Nationanlity,
                     group=interaction(Nationanlity, Acc_cost))) + 
    geom_point(size=3)
  
  



##which city has more avg acc cost
topcity <- Travel %>%
  group_by(City) %>%
  summarize(mean_accommodation_cost = mean(Acc_cost)) %>%
  top_n(10, mean_accommodation_cost)

ggplot(topcity, aes(City,mean_accommodation_cost)) +
  geom_bar(stat="identity") +
  labs(title = "Top 10 costliest countries (in terms of accommodation)",
       x = "City",
       y = "Accommodation Cost")


##which city has more acc cost
ggplot(Travel, aes(x=Acc_cost, y=City,
                   group=interaction(City, Acc_cost))) + 
  geom_point(size=3)




##which country has more tansp cost

TopCityTrans <- Travel %>% group_by(City) %>% summarize(meantranscost = mean(Trans_Cost)) %>% top_n(10,meantranscost)
ggplot(TopCityTrans, aes(City,meantranscost)) + geom_bar(stat = 'identity')




##what is most used trans type overall
TopTrans <- Travel %>%  count(Trans_Type) %>% arrange(desc(n))
pie(TopTrans$n, TopTrans$Trans_Type, main="Pie Chart of Transporatation")




##which nationality people are likely to travel more
TopNationality <- Travel %>%  count(Nationanlity) %>% arrange(desc(n)) %>% head(5)
pie(TopNationality$n,TopNationality$Nationanlity)


##which age group is travelling more

Travel %>% ggplot(aes(Age)) + geom_bar()

##which city is most visted
TopCity <- Travel %>%  count(City) %>% arrange(desc(n)) %>% head(15)
ggplot(TopCity, aes(TopCity$City,TopCity$n)) + geom_bar(stat = 'identity')


##avg duration
Dur <- Travel %>%  count(Duration) %>% arrange(desc(n)) %>% head(5)
ggplot(Dur, aes(Dur$Duration, Dur$n)) + geom_line()



#########################################################################################


cor(Travel$Trans_Cost,Travel$Acc_cost)
cor(Travel$Trans_Type, Travel$Trans_Cost)



Travel$Trans_Cost <- na.omit(Travel$Trans_Cost)


sum(is.na(Travel$Trans_Cost))
sum(is.na(Travel$Acc_cost))


unique(Travel$Acc_Type)
dcol <- dummy_cols(Travel, Travel$Acc_Type)

Travel$Acc_Type <- as.factor(Travel$Acc_Type)
levels(Travel$Acc_Type)
class(Travel$Acc_Type)
str(Travel)
Travel$Acc_Type <- as.integer(Travel$Acc_Type)
str(Travel$Acc_Type) 


##correlatio######

cor(Travel$Acc_Type, Travel$Acc_cost)



as.factor(Travel$Trans_Type)
Travel$Trans_Type <- as.integer(as.factor(Travel$Trans_Type))


cor(Travel$Trans_Cost,Travel$Trans_Type)

heatmap(Travel)
cor(Travel$Duration,Travel$Age)


forheatmap <- Travel[,c("Duration", "Age","Acc_Type","Acc_cost","Trans_Type","Trans_Cost")]
x <- scale(forheatmap)
x
heatmap(x)


forheatmap1 <- Travel[,c("Acc_Type","Acc_cost","Trans_Type","Trans_Cost")]
y <- scale(forheatmap1)
heatmap(y)


##############################################
check1 <- lm(Duration ~ Acc_Type + Acc_cost, data=Travel)
summary(check1) ##significant

check2 <- lm(Duration ~ Trans_Type + Trans_Cost, data=Travel)
summary(check2) ##not significant

check3 <- lm(Duration ~ Acc_Type + Acc_cost + Trans_Type + Trans_Cost, data=Travel)
summary(check3)

