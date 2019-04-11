##VKO2
#
#T1
#.1
library(alr4)
data <- alr4::UN11
?scatterplotMatrix
scatterplotMatrix(~ data$fertility + log(data$ppgdp) + data$pctUrban)

#.2

reg1 <- lm(data$fertility ~ log(data$ppgdp))
reg2 <- lm(data$fertility ~ data$pctUrban)
summary(reg1)           
summary(reg2)

#P-Arvo erittäin pieni, hylätään nollahypoteesi
#.3
reg3 <- lm(data$fertility ~ log(data$ppgdp) + data$pctUrban)
avPlots(reg3)
summary(reg3)
#Logaritmi ppgdp on vielä hyödyllinen, mutta pctUrban kulmakerroin lähellä nollaa joka poikkeaa paljon aikaisemmasta

reg4 <- lm(log(data$ppgdp) ~data$pctUrban)
reg5 <- lm (data $fertility ~data$pctUrban)
plot(resid(reg5) ~ resid(reg4))

residreg <- lm(resid(reg5) ~resid(reg4))
plot(resid(reg3) ~resid(residreg))

#asettuvat samalle viivalle

#####
#T.2
#.1
data2 <- alr4::water
scatterplotMatrix(~data2$BSAAM + data2$OPBPC + data2$OPRC + data2$OPSLAKE)
regvesi <- lm (data2$BSAAM ~ data2$OPBPC + data2$OPRC + data2$OPSLAKE)
cor(data2)

#Kaikki korreloivat keskenään ja korrelaatiot ovat positiivisia.
#.2
summary(regvesi)
#T-arvo pieni
#
####
#T.3
#.1
data3 <- alr4::MinnLand
boxplot(log(data3$acrePrice) ~ data3$year)
#.2
regasunto <- lm(log(data3$acrePrice) ~ factor(data3$year))

abline(regasunto, col = "red")
#.3
update(regasunto, ~factor(data3$year))
abline(regasunto, col = "red")
#???
#
#####
#T.4
#.1
#Ensimmäisessä tarkastellaan hinnan logaritmia ottaen huomioon vuoden ja alueen. Toisessa otetaan näiden lisäksi huomioon vielä vuoden ja alueen välinen suhde.
#.2
regasunto2 <- lm(log(data3$acrePrice) ~ data3$year + data3$region)
regasunto3 <- lm(log(data3$acrePrice) ~ data3$year + data3$region + data3$year:data3$region)

fit <- fitted(regasunto3)
summary(regasunto2)
summary(fit)


plot(data3$year~ data3$region)

##turhia
effects::allEffects(regasunto2)
plot.new()
plot(effect(log(data3$acrePrice) ~ data3$year + data3$region))

#####
#T.5
#####
#T.6
#.1
data4 <- alr4::salary
boxplot(data4$salary ~data4$sex)
boxplot(data4$salary ~data4$rank)
boxplot(data4$salary ~data4$degree)
boxplot(data4$salary ~data4$year)
#Naiset tienaavat miehiä vähemmän, Korkeampi arvoiset tienaavat enemmän,
#Maistereiden palkat vaihtelevat enemmän, yleisesti palkka kasvaa vuosien myötä

#.2
summary(lm(data4$salary ~ data4$sex))
#H0: Naiset tienaavat yleensä vähemmän kuin miehet
#.3
summary(lm(formula = salary ~., data = data4))
#Suuri P-arvo, eli voidaan sanoa että se että on nainen tarkoittaa huonompaa palkkaa
#.4
vika <- lm(formula = salary ~., data = data4)
summary(update(vika, ~. -rank ))
#huomataan että Rankin ollessa poissa huomioista, ei enään se että on nainen vaikuta niin paljoa palkkaan
