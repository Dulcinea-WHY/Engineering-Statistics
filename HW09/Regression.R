#109612054
#2021.12.13_HW

#(1) scatter plot / Pearson r
# load data
data(cars)
?cars
str(cars)

# scatter plots
r <- cor(cars$speed,cars$dist)
cat('Pearson correlation coefficient (r): ', r)
library(ggplot2)
ggplot(data = cars, aes(speed,dist))+
         geom_point()+
         labs(title = "Scatter Plot: Speed v.s. Distance",
              x = "Speed, mph",
              y = "Distance, ft")
      
#(2)
#a straight fitting line
n <- length(cars$speed)
model.line <- lm(dist~speed, data = cars)
summary(model.line)
intercept <- model.line$coefficients[1]
slope <- model.line$coefficient[2]
#predict straight line
pre.x <- seq(0,30,0.01)
pre.y.line <- intercept + slope*pre.x
anova.line <- anova(model.line)
SSE <- anova.line$`Sum Sq`[2]
SSR <- anova.line$`Sum Sq`[1]
SSTo <- SSE + SSR
r2.line <- 1-SSE/SSTo
se.line <- sqrt(SSE/(n-2))
text.slope <- as.character(round(slope,2))
text.intercept <- as.character(round(intercept,2))
text.r2.line <- as.character(round(r2.line,2))
text.se.line <- as.character(round(se.line,2))
label.line <- paste("dist=",text.intercept,"+",
                    text.slope,"x speed")
cap.line <- paste("r^2:", text.r2.line,"se:",text.se.line,
                  "(line)")
label.line
data.line <- data.frame(x=pre.x, y=pre.y.line, label = label.line)
#power transformation
x <- log(cars$speed)
y <- log(cars$dist)
d <- data.frame(x=x, y=y)
ggplot(data = d,aes(x,y))+
         geom_point()+
         labs(title = "Scatter Plot: ln(Speed) v.s. ln(Distance)",
              x = "ln(speed), ln(mph)",
              y = "ln(Distance), ln(ft)")
model <- lm(y~x, data = d)
summary(model)
e <- exp(1)
a <- e^model$coefficients[1]
b <- model$coefficients[2]
a
b
#compute SSE for curve line
SSE <- 0
SSTo <- 0
ymean <- mean(cars$dist)
for(i in 1:n){
  ypre <- a*cars$speed[i]^b
  SSE = SSE + (cars$dist[i]-ypre)^2
  SSTo = SSTo + (cars$dist[i]-ymean)^2
}
se.curve <- sqrt(SSE/(n-2))
r2.curve <- 1 - SSE/SSTo

#predict curve line
pre.x <- seq(0,30,0.01)
pre.y <- a*(pre.x^b)
text.a <- as.character(round(a,2))
text.b <- as.character(round(b,2))
text.r2.curve <- as.character(round(r2.curve,2))
text.se.curve <- as.character(round(se.curve,2))
label.curve <- paste("dist=",text.a,"x","speed^",text.b)
cap.curve <- paste("r^2:",text.r2.curve,"se", text.se.curve,
                   "(curve)")
data.curve <- data.frame(x=pre.x,y=pre.y,label=label.curve)

ggplot(data = cars, aes(speed,dist))+
         geom_line(data = data.curve,
                   aes(x,y,color = label),
                   size = 1.2)+
         geom_line(data = data.line,
                   aes(x,y,color = label),
                   size = 1.2)+
         geom_point(data = cars, aes(speed,dist),
                    alpha = 0.5,
                    color = "blue")+
         labs(title = "Scatter Plot: Speed v.s. Distance",
              x = "Speed, mph",
              y = "Distance, ft",
              subtitle = paste(cap.line,cap.curve))
      
