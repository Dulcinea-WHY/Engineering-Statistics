#109612054
#2021.11.8_HW

# 25個圓柱狀試體的彈性模數大小數據，單位：MPa
data <- c(37.0, 37.5, 38.1, 40.0, 40.2, 40.8, 41.0, 42.0, 43.1, 43.9, 44.1, 44.6, 45.0, 46.1, 47.0, 50.2, 55.0, 56.0, 57.0, 58.0, 62.0, 64.3, 68.8, 70.1, 74.5)

# (1) Weibull(red) / Normal(dashed) / density curve(solid)
library(MASS)
fit.wei <- fitdistr(data, "weibull")
shape <- fit.wei$estimate[1]
scale <- fit.wei$estimate[2]
x <- seq(20, 80, 0.1)
y.wei <- dweibull(x, shape = shape, scale = scale)

fit.nor <- fitdistr(data, "normal")
mean <- fit.nor$estimate[1]
sd <- fit.nor$estimate[2]
y.nor <- dnorm(x, mean = mean, sd =sd)

fit <- data.frame(x = x, ywei = y.wei, ynor = y.nor)

density <- density(data)
data.density <- data.frame(x = density$x, y = density$y)

ggplot(data = data.density, aes(x,y))+
  geom_line()+
  geom_line(data = fit, aes(x, y.wei),
            color = "red")+
  geom_line(data = fit, aes(x,ynor),
            linetype = 2)+
  labs(title = "Fitting distribution",
       x = "Material Strength",
       y = "Density")+
  theme_bw()

# (2) normal quantile plot
ggplot()+
  aes(sample = data)+
  stat_qq(distribution = qnorm)+
  stat_qq_line(line.p = c(0.25, 0.75))+
  labs(title = "QQplot",
       x = "Theoretical Normal Distribution Quantile",
       y = "Observed Data")+
  theme_bw()

# (3) Make comment on the plausibility of a normal population distribution.

