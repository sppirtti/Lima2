library(alr4)
#
#VKO3
#
#T1
#.1
data <- alr4::lathe1
scatterplotMatrix(~ jitter(Speed ,1)+ jitter(Feed,1) + Life, data = data)
#Nopeus ja syöttö vähentävät elinkaarta huomattavasti, kun taas syötön ja nopeuden väliset vaihtelevat
#.2
#.3
#############
#T.2
#.1
data2 <- alr4::cathedral
Roman <- subset(data2, data2$Type == 'Romanesque')
Goth <- subset(data2, data2$Type == 'Gothic')
plot(Roman$Length, Roman$Height)
abline(malli <- lm(Roman$Height ~ Roman$Length))
#ei suurta riippuvuutta pituudesta
plot(Goth$Length, Goth$Height)
abline(malli2 <- lm(Goth$Height ~ Goth$Length))
#korkeus kasvaa pituuden kanssa
#.2
summary(malli)
summary(malli2)
#
#########################
#T.3
data3 <- alr4::UN11
#.1
#NH Model: elinodote, funka ennustajina on log(ppgdp) ja suhde tietyn ryhmän ja log(ppgdp) välillä
#.2

unmalli <- lm(data3$lifeExpF ~ log(data3$ppgdp) + data3$group:log(data3$ppgdp))
unmalli2 <- lm(data3$lifeExpF ~ data3$group + log(data3$ppgdp) + data3$group:log(data3$ppgdp))
summary(unmalli)
summary(unmalli2)
anova(unmalli, unmalli2)
#
########
#T.4
data4 <- alr4::cakes
?I
m1 <- lm(Y ~ X1 + I(X1^2)+ I(X2^2))
#
#
##############
#T.5
data5 <- alr4::MinnLand
#

