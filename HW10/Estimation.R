#109612054
#2021.12.20_HW


mu <- 50.0
sd <- 10.0
num.sample <- 100
n <- 20

#for loop
for (i in 1:num.sample){#normal distribution sample
  s.data <- rnorm(n,mu,sd)
  #sample mean
  sample.mean <- mean(s.data)
  # confidence interval, n < 30, t- distribution
  # lower, upper, Bound error (B)
  # B = t_star*sample.sd/sqrt(n)
  # confidence level 90%
  t.star <- qt(0.95, df = n-1)
  B <- t.star*sd(s.data)/sqrt(n)
  lower <- sample.mean - B
  upper <- sample.mean + B
  if (mu > lower && mu < upper){
    i.yes <- i.yes + 1
    num.yes[i.yes] <- i
    mean.yes[i.yes] <- sample.mean
    lower.yes[i.yes] <- lower
    upper.yes[i.yes] <- upper
  }else{
    i.no <- i.no + 1
    num.no[i.no] <- i
    mean.no[i.no] <- sample.mean
    lower.no[i.no] <- lower
    upper.no[i.no] <- upper
  }
}


data.yes <- data.frame(x = num.yes
                       , y1 = lower.yes
                       , y2 = upper.yes
                       , y3 = mean.yes)
data.no <- data.frame(x = num.no
                      , y1 = lower.no
                      , y2 = upper.no
                      , y3 = mean.no)

library(ggplot2)
ggplot(data = data.yes, aes(x))+
  geom_errorbar(aes(ymin = y1, ymax = y2))+
  geom_errorbar(data = data.no
                , aes(ymin = y1, ymax = y2)
                , color = "red")+
  geom_point(data = data.no, aes(x,y3))+
  geom_abline(intercept = mu, slope = 0,
            linetype = 2, 
            lwd = 1)+
  labs(title = "Understanding CL",
       x = "Numbers of Sample",
       y = "Confidence Intervals")

