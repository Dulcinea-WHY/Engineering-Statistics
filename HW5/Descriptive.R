#109612054
#2021.11.1_HW

# 20個圓柱狀試體的彈性模數大小數據，單位：MPa
data <- c(37.0, 37.5, 38.1, 40.0, 40.2, 40.8, 41.0, 42.0, 43.1, 43.9, 44.1, 44.6, 45.0, 46.1, 47.0, 50.2, 55.0, 56.0, 57.0, 58.0, 62.0, 64.3, 68.8, 70.1, 74.5)

# 1) histogram with the density curve. (hist (breaks =5))
den <- density(data) #density curve : x, y
#convert data.frame
data.den <- data.frame(x = den$x, y = den$y) #for geom_line()
#original data convert to data.frame
data.hist <- data.frame(data = data)
#determine breaks for histogram
hist.tmp <- hist(data, breaks = 5, plot = FALSE)
#hist.tem$breaks

#ggplot for creating histogram
# Y axis unit = desity
library(ggplot2)
ggplot(data.hist, aes(data,..density..)) + 
  #geom histogram()
  geom_histogram(breaks = hist.tmp$breaks) +
  #density curve
  geom_line(data = data.den, aes(x,y), color = "red") +
  labs(title = "Histogram and Density Curve of data"
       , x = "Measurement data [MPa]"
       , y = "Density")

# 2) skewness / kurtosis values
library(moments)
skewness(data)
kurtosis(data)

# 3) boxplot (coef = 1.0)
ggplot(data = data.hist, aes(data)) +
  geom_boxplot(coef = 1.0) +
  scale_y_discrete() +
  labs(title = "Boxplot of data"
       , x = "Measurement data [MPa]")


