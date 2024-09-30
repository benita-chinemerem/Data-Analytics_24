EPI.new <- epi2024results06022024$EPI.new
#fitting a distribution beyond histograms

qqnorm(EPI.new) 
qqline(EPI.new)

#Make a Q-Q plot against the generating distribution by:
x <-seq(20., 80., 1.0)
qqplot(qnorm(ppoints(200)), x)
qqline(x)
qqplot(qnorm(ppoints(200)),EPI.new)
qqline(EPI.new)

#Cumulative density function
plot(ecdf(EPI.new), do.points=FALSE)
plot(ecdf(rnorm(1000, 45, 10)), do.points=FALSE) # ecdf of normal distr with mean=45, sd= 10
lines(ecdf(EPI.new))

#doing the same exploration and fitting for another Variable BDH

BDH.new <- epi2024results06022024$BDH.new

qqnorm(BDH.new) 
qqline(BDH.new)

#Make a Q-Q plot against the generating distribution by:
x <-seq(20., 80., 1.0)
qqplot(qnorm(ppoints(200)), x)
qqline(x)
qqplot(qnorm(ppoints(200)),BDH.new)
qqline(BDH.new)

#Cumulative density function
plot(ecdfBDH.new), do.points=FALSE)
plot(ecdf(rnorm(1000, 45, 10)), do.points=FALSE) # ecdf of normal distr with mean=45, sd= 10
lines(ecdf(BDH.new))