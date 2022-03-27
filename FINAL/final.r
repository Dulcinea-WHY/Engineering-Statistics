# 工程統計期末報告
# Author : 土木大二-109612054-吳巽言
# Date : 2022/01/15
# Title : Old Faithful Geyser Data


#### look into the data

### import data
#install.packages("datasets")
data("faithful")
?faithful
str(faithful)

### min/1st Qu/median/mean/3rd Qu/max
summary(faithful)
### mode
as.numeric(names(table(faithful$eruptions)))[which.max(table(faithful$eruptions))]
as.numeric(names(table(faithful$waiting)))[which.max(table(faithful$waiting))] 
### Standard Deviation
sd(faithful$eruptions)
sd(faithful$waiting)

### histogram and density curve
#install.packages("ggplot2")
library(ggplot2)
#install.packages("gridExtra")
library(gridExtra)
## create dataframes
data <- data.frame(eruptions = faithful$eruptions, 
                   waiting = faithful$waiting) 
data.eden <- data.frame(x = density(faithful$eruptions)$x,
                        y = density(faithful$eruptions)$y)
data.wden <- data.frame(x = density(faithful$waiting)$x,
                        y = density(faithful$waiting)$y)
## eruptions
p1 <- ggplot() + 
  ## histograms
  geom_histogram(data= data, aes(x = eruptions, y = ..density..)) +
  ## density curve
  geom_line(data = data.eden, aes(x,y), color = "blue") +
  labs(title = "Eruption time [min]")
## waiting
p2 <- ggplot() +
  ## histograms
  geom_histogram(data = data, aes(x = waiting, y =..density..)) +
  ## density curve
  geom_line(data = data.wden, aes(x,y), color = "red") +
  labs(title = "Waiting time to next eruption [min]")
grid.arrange(p1,p2,nrow = 2)
 


### QQplot
qqnorm(faithful$eruptions) 
qqline(faithful$eruptions)
qqnorm(faithful$waiting)
qqline(faithful$waiting)

### skewness and kurtosis
#install.packages("moments")
library(moments)
cat("eruptions:","\n",
    "skewness = ", skewness(faithful$eruptions),"\n",
    "kurtosis = ", kurtosis(faithful$eruptions))
cat("waiting:","\n",
    "skewness = ", skewness(faithful$waiting),"\n",
    "kurtosis = ", kurtosis(faithful$waiting))

### Scatter plot
ggplot(data = data, aes(x = eruptions, y = waiting)) +
  geom_point() +
  geom_smooth() +
  labs(title = "Scatter plot of the Old Faithful Geyser data",
       x = "eruptions [min]",
       y = "waiting [min]")

#### Regression Analysis

### a straight fitting line
model.line <- lm(waiting~eruptions,data = data)
summary(model.line)
intercept <- model.line$coefficients[1]
slope <- model.line$coefficient[2]
## predict straight line
pre.x <- seq(1,5.3,0.3)
pre.y.line <- intercept + slope*pre.x
anova.line <- anova(model.line)
SSE <- anova.line$`Sum Sq`[2]
SSR <- anova.line$`Sum Sq`[1]
SSTo <- SSE + SSR
r2.line <- 1-SSE/SSTo
n <- length(faithful$eruptions)
se.line <- sqrt(SSE/(n-2))
text.slope <- as.character(round(slope,2))
text.intercept <- as.character(round(intercept,2))
text.r2.line <- as.character(round(r2.line,2))
text.se.line <- as.character(round(se.line,2))
label.line <- paste("waiting=",text.intercept,"+",
                    text.slope,"x eruptions")
paste("r^2:", text.r2.line,"se:",text.se.line,
                  "(line)")
cap.line <- data.line <- data.frame(x=pre.x, y=pre.y.line, label = label.line)

### power transformation
x <- log(faithful$eruptions)
y <- log(faithful$waiting)
d <- data.frame(x=x, y=y)
ggplot(data = d,aes(x,y))+
  geom_point()+
  labs(title = "Scatter Plot: ln(eruptions) v.s. ln(waiting)",
       x = "ln(eruptions), ln(min)",
       y = "ln(waiting), ln(min)")
model <- lm(y~x, data = d)
summary(model)
e <- exp(1)
a <- e^model$coefficients[1]
b <- model$coefficients[2]
SSE <- 0
SSTo <- 0
ymean <- mean(faithful$waiting)
for(i in 1:n){
  ypre <- a*faithful$eruptions[i]^b
  SSE = SSE + (faithful$waiting[i]-ypre)^2
  SSTo = SSTo + (faithful$waiting[i]-ymean)^2
}
se.curve <- sqrt(SSE/(n-2))
r2.curve <- 1 - SSE/SSTo
## predict curve line
pre.x <- seq(1,5.3,0.3)
pre.y <- a*(pre.x^b)
text.a <- as.character(round(a,2))
text.b <- as.character(round(b,2))
text.r2.curve <- as.character(round(r2.curve,2))
text.se.curve <- as.character(round(se.curve,2))
label.curve <- paste("waiting=",text.a,"x","eruptions^",text.b)
paste("r^2:",text.r2.curve,"se", text.se.curve,
                   "(curve)")
cap.curve <- data.curve <- data.frame(x=pre.x,y=pre.y,label=label.curve)

### scatter plot
ggplot(data = data, aes(eruptions,waiting))+
  geom_line(data = data.curve,
            aes(x,y,color = label),
            size = 1.2)+
  geom_line(data = data.line,
            aes(x,y,color = label),
            size = 1.2)+
  geom_point(data = data, aes(eruptions, waiting),
             alpha = 0.2,
             color = "black")+
  labs(title = "Scatter Plot: eruptions v.s. waiting",
       x = "eruptions [min]",
       y = "waiting [min]",
       subtitle = paste(cap.line,cap.curve))

