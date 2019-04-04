#1.1

library(alr4)

data <- alr4::UN11
#selittäjä ppgdp

# 1.2
plot(y=data$fertility, x=data$ppgdp)
# 1.3
plot(y=log(data$fertility), x=log(data$ppgdp) )


#2.1
data2 <- alr4::ftcollinstemp
syksy <- data2$fall
talvi <- data2$winter
plot(syksy, talvi)
#ei huomattavaa korrelaatiota
#2.2
?lm
regressio <- lm(talvi~ syksy)
summary(regressio)
abline(regressio)
#2.3
x <- sum((talvi - mean(talvi))^2)
r <- summary(regressio)$r.squared
vaihtelevuus <- r * x
vaihtelevuus

#2.4
pre <- subset(data2, year > 1900 || year < 1989)
after <- subset(data2, year>=1990)

alku <- lm(pre$winter ~ pre$fall)
loppu <- lm(after$winter ~after$fall)

plot(pre$fall, pre$winter)
abline(alku)
summary(alku)

plot(after$fall, after$winter)
abline(loppu)
summary(loppu)
#3
data3 <- alr4::Heights


#4.1
regressio2 <- lm(log(data$fertility) ~ log(data$ppgdp))
#4.2
plot( log(data$ppgdp) , log(data$fertility))
abline(regressio2)
#4.3 ja 4.4
summary(regressio2)
summary(regressio2)$r.squared
#0.526
#4.5
data2uusi <- data.frame(ppgdp = 1000)
predict(regressio2, data2uusi, level = 0.95)
#??????????

#5
regressio3 <- lm(log(data$fertility) ~ data$pctUrban)
plot(data$pctUrban , log(data$fertility))
abline(regressio3)
#6
