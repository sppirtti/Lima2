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

#P-Arvo eritt�in pieni, hyl�t��n nollahypoteesi
#.3
reg3 <- lm(data$fertility ~ log(data$ppgdp) + data$pctUrban)
avPlots(reg3)
summary(reg3)
#Logaritmi ppgdp on viel� hy�dyllinen, mutta pctUrban kulmakerroin l�hell� nollaa joka poikkeaa paljon aikaisemmasta

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

#Kaikki korreloivat kesken��n ja korrelaatiot ovat positiivisia.
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

#Ensimm�isess� tarkastellaan hinnan logaritmia ottaen huomioon vuoden ja alueen. Toisessa otetaan n�iden lis�ksi huomioon viel� vuoden ja alueen v�linen suhde.
#.2
regasunto2 <- lm(log(data3$acrePrice) ~ data3$year + data3$region)
regasunto3 <- lm(log(data3$acrePrice) ~ data3$year + data3$region + data3$year:data3$region)
summary(regasunto2)
summary(regasunto3)

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
#Naiset tienaavat miehi� v�hemm�n, Korkeampi arvoiset tienaavat enemm�n,
#Maistereiden palkat vaihtelevat enemm�n, yleisesti palkka kasvaa vuosien my�t�

#.2
summary(lm(data4$salary ~ data4$sex))
#H0: Naiset tienaavat yleens� v�hemm�n kuin miehet
#.3
summary(lm(formula = salary ~., data = data4))
#Suuri P-arvo, eli voidaan sanoa ett� se ett� on nainen tarkoittaa huonompaa palkkaa
#.4
vika <- lm(formula = salary ~., data = data4)
summary(update(vika, ~. -rank ))
#huomataan ett� Rankin ollessa poissa huomioista, ei en��n se ett� on nainen vaikuta niin paljoa palkkaan