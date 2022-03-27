#(1)若該年度颱風形成數量為20個(n.typhoon)，則無任㇐颱風侵台的機率為多少? (數學計算，理論值)
#Probability of typhoon strike Taiwan
p <- 0.1
n.typhoon <- 20
#Probability of zero typhoon strike Taiwan
p0 <- (1-p)^n.typhoon
cat("P(n.trike == 0) = ",p0,'is a theoretical probability.')

#(2)請試著透過R程式設計模擬颱風侵台機率問題，重複8次(n=8)的抽數字(模擬20個颱風形成，單次抽取20個數字)，並計算完全沒有抽到00~09之間數字(代表為非侵台颱風)的次數m，則此次模擬無颱風侵台的機率為多少? (m/8) (注意若要每次計算相同結果，需使用set.seed)
n <- 8
n.typhoon <- 20
m <- 0
pp <- 0
set.seed(1)
for (i in 1:n){
  n.strike <- 0
  for(j in 1:n.typhoon){
    num <- sample(0:99,1)
    if (num <= 09){
      n.strike <- n.strike + 1
    }
  }
  if (n.strike == 0) m <- m + 1
}
cat(m,'of', n, 'are results of number of non-strike typhoon', '\n')
pp <- m / n
cat(pp,'is computed probability','Sample number is ', n, '\n')


#(3) repeated random samplinng n = 2^3, 2^4, , ..., 2^15
np <- 0 
px <- NULL
py <- NULL
for(k in 3:15){
  n <- 2^k
  n.typhoon <- 20
  m <- 0
  pp <- 0
  for(i in 1:n){
    n.strike <- 0
    for(j in 1:n.typhoon){
      num <- sample(0:99,1)
      if (num <= 09){
        n.strike <- n.strike + 1
      }
    }
    if  (n.strike == 0) m <- m+1
  }
  cat(k, '-power of two','\n')
  cat(m,'of',n,'are results of number of non-strike typhoon','\n')
  pp <- m/n
  cat(pp,'is computed probability', 'Sample number is', n, '\n')
  #save data for plot
  np <- np + 1
  px[np] <- n
  py[np] <- pp
}

#ggplot
data <- data.frame(x=px, y=py)
lpx <- c(2^3,2^15)  
lpy <- c(p0,p0)
data.line <- data.frame(x=lpx, y=lpy)
library(ggplot2)
ggplot(data = data, aes(x,y))+
  geom_point()+
  labs(title = "Probability of Typhoon Strike Taiwan",
       x = 'Number of Random Sampling',
       y = 'Probability of n.strike = 0')+
  geom_line(data = data.line, aes(x,y),
            linetype = 2,
            color = 'black', 
            size = 0.5)






