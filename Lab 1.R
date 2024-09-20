EPI_data <- read.csv("C:/Users/aiden/OneDrive/Desktop/Data Analytics/Data/epi2024results06022024.csv") 
# Note: replace default data frame name – cannot start with numbers! Munging has begun! (ugh)
# Note: replace <path> with either a directory path or use: setwd(“<path>”) 
View(EPI_data)

attach(EPI_data) # sets the ‘default’ object 
fix(EPI_data) # launches a simple data editor – test it! 
EPI.new # prints out values EPI_data$EPI.new
tf <- is.na(EPI.new) # records True values if the value is NA 
E <- EPI.new[!tf] # filters out NA values, new array

summary(EPI.new) # stats 
fivenum(EPI.new,na.rm=TRUE) 
stem(EPI.new) # stem and leaf plot 
hist(EPI.new) # histogram

#Histogram with distribution line
hist(EPI.new, seq(20., 80., 1.0), prob=TRUE) 
lines(density(EPI.new,na.rm=TRUE,bw=1.)) # or try bw=“SJ” 
rug(EPI.new)

boxplot(EPI.new, APO.new) #Boxplot

#Histogram with smoother line
hist(EPI.new, seq(20., 80., 1.0), prob=TRUE)
lines(density(EPI.new, na.rm=TRUE, bw="SJ"))
rug(EPI.new) 

#Other Histogram with distribution lines
x<-seq(20,80,1) 
q<- dnorm(x,mean=42, sd=5,log=FALSE) 
lines(x,.4*q) 
ln<-dnorm(x,mean=65, sd=5,log=FALSE) 
lines(x,.12*ln)

#Excercise 2
#Cumulative density function? 
plot(ecdf(EPI.new), do.points=FALSE, verticals=TRUE) 
#Quantile-Quantile?
qqnorm(EPI.new); qqline(EPI.new) 
#Make a Q-Q plot against the generating distribution by: 
qqplot(rnorm(ppoints(250)), EPI.new, xlab = "Q-Q plot for norm dsn") 
qqline(EPI.new)
qqplot(rt(ppoints(250), df = 5), EPI.new, xlab = "Q-Q plot for t dsn") 
qqline(EPI.new)

#Excercise 3
ECO.new # prints out values EPI_data$EPI.new
tf <- is.na(ECO.new) # records True values if the value is NA 
E <- ECO.new[!tf] # filters out NA values, new array
summary(ECO.new) # stats 
fivenum(ECO.new,na.rm=TRUE) 
stem(ECO.new) # stem and leaf plot 
hist(ECO.new) # histogram
lines(density(ECO.new,na.rm=TRUE,bw='SJ'))
plot(ecdf(ECO.new), do.points=FALSE, verticals=TRUE) 
qqnorm(ECO.new); qqline(ECO.new) 
qqplot(rnorm(ppoints(250)), ECO.new, xlab = "Q-Q plot for norm dsn") 
qqline(ECO.new)
qqplot(rt(ppoints(250), df = 5), ECO.new, xlab = "Q-Q plot for t dsn") 
qqline(ECO.new)
