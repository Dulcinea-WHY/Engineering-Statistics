#109612054吳巽言
#2021.10.4 課堂練習

set.seed(1)
math.score <- sample(0:100, 80, replace = TRUE)
math.score

#(a): seat 1-30 / mean.sd
mean(math.score[1:30])
sd(math.score[1:30])

#(b): pass / number.seat number
pass <- which(math.score >= 60)
length(pass)
pass

#(c): highest.lowest / score.seat number
max(math.score)
min(math.score)
which.max(math.score)
which.min(math.score)

#(d): top10 (high to low) / mean.sd
top10 <- sort(math.score, decreasing = TRUE)[1:10]
mean(top10)
sd(top10)

#(e): 1st quantile
qu1st <-summary(math.score)
str(qu1st)
qu1st['1st Qu.']

