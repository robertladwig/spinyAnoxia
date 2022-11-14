
timelag <- read.csv(file = "robin-data/2022-08-03_time_lag_from_robert/timelag.csv")

plot(x = timelag$year, y = timelag$timelag, type = "l")

mylag <- list("pre" = timelag$timelag[timelag$year < 2010],
              "post" = timelag$timelag[timelag$year >= 2010])

boxplot(x = mylag, notch = T)
boxplot(x = mylag)

stripchart(x = mylag, vertical = T, method = "jitter", pch = 21)

t.test(x = mylag$pre, y = mylag$post, alternative = "two.sided")
