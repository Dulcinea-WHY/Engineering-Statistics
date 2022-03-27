#109612054
#2021.11.15_HW

#install.packages("rio") / select data Temperature
library(rio)

getwd()
setwd("C:/R/Engineering_Statistics/11.15")
d <- import("weatherdata.xlsx")
names(d)

#install packages("dplyr")
library(dplyr)
d.temp <- d %>%
  select(Temperature)
str(d.temp)

# log-normal
library(MASS)
?fitdistr
fit.lnorm <- fitdistr(d.temp$Temperature, "lognormal")
mean <- fit.lnorm$estimate[1]
sd <- fit.lnorm$estimate[2]
x <- seq(10,40,1)
?dlnorm
y <- dlnorm(x, meanlog = mean, sdlog = sd)
d.lnorm <- data.frame(x = x, y = y, label = "log normal")



# density curve
density <- density(d.temp$Temperature)
data.density <- data.frame(x = density$x,
                           y = density$y,
                           label = "density curve")

data.plot <- rbind.data.frame(d.lnorm, data.density)

#histogram
?geom_histogram
library(ggplot2)
ggplot(data = d.temp, aes(Temperature,..density..)) +
  geom_histogram(binwidth = 0.5,
                 fill = "black",
                 color = "white",
                 size =0.1,
                 alpha = 0.8)+
  geom_line(data = data.plot
            , aes(x,y, color = label)
            , size = 0.6)+
  labs(title = "June 01-Sept. 30 2020 Temperature",
       x = "Temperature",
       y = "Density")+
  scale_color_discrete(name = "Lines")+
  theme_bw()

library(moments)
skewness(d.temp)
kurtosis(d.temp)


#(2)
#sample size = 10
i <- 0
d.mean.10 <- 0.0
n <- 10
np <- length(d.temp$Temperature)
for(i in 1:1000) {
  d.index <- sample(1:np,n)
  d.sample <- d.temp$Temperature[d.index]
  d.mean.10[i] <- mean(d.sample)
}
d.mean.10 <- as.data.frame(d.mean.10)
summary(d.temp$Temperature)

#sample size = 50
i <- 0
d.mean.50 <- 0.0
n <- 50
np <- length(d.temp$Temperature)
for(i in 1:1000) {
  d.index <- sample(1:np,n)
  d.sample <- d.temp$Temperature[d.index]
  d.mean.50[i] <- mean(d.sample)
}
d.mean.50 <- as.data.frame(d.mean.50)
summary(d.temp$Temperature)


#sample size = 100
i <- 0
d.mean.100 <- 0.0
n <- 100
np <- length(d.temp$Temperature)

for(i in 1:1000) {
  d.index <- sample(1:np,n)
  d.sample <- d.temp$Temperature[d.index]
  d.mean.100[i] <- mean(d.sample)
}
d.mean.100 <- as.data.frame(d.mean.100)
summary(d.temp$Temperature)


#ggplot
library(ggplot2)
p1 <- ggplot(data = d.mean.10, aes(d.mean.10))+
  geom_histogram(bins = 30)+
  labs(x = "sample mean, random sampling n = 10, 1000 times")
p2 <- ggplot(data = d.mean.50, aes(d.mean.50))+
  geom_histogram(bins = 30)+
  labs(x = "sample mean, random sampling n = 50, 1000 times")
p3 <- ggplot(data = d.mean.100, aes(d.mean.100))+
  geom_histogram(bins = 30)+
  labs(x = "sample mean, random sampling n = 100, 1000 times")

library(gridExtra)
grid.arrange(p1,p2,p3,nrow =3)

