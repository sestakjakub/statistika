reddit <- read.csv('reddit.csv')
str(reddit)

table(reddit$employment.status)
summary(reddit)

levels(reddit$age.range)

install.packages('ggplot2', dependencies = T)
library(ggplot2)
qplot(data=reddit, x = age.range)

qplot(data=reddit, x = income.range)

factor(reddit(income.range)) 
