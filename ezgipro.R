library("MASS")
library("Matching")
library("MatchIt", lib.loc="~/R/win-library/3.4")
library("optmatch", lib.loc="~/R/win-library/3.4")
library("stats")

as.factor(veri$CINSIYET)
as.factor(veri$YAS_GRUP)
as.factor(veri$EGITIM)
as.factor(veri$MEDENI_DURUM)
as.factor(veri$CALISMA_DURUM)
as.factor(veri$HANE_GELIR)
as.factor(veri$HASTALIK_SAGLIKDURUM)
as.factor(veri$TUTUNDUMAN_MARUZ)
as.factor(veri$ALKOLKULLANIM_DURUM)
as.factor(veri$TUTUNKULLANIM_DURUM)

yeniveri <- sort(sample(nrow(veri), nrow(veri)*0.7))
training <-veri[yeniveri,]
test <- veri[-yeniveri,]
g<-glm(TUTUNKULLANIM_DURUM ~ CINSIYET+YAS_GRUP+EGITIM+MEDENI_DURUM+CALISMA_DURUM+HANE_GELIR
       +HASTALIK_SAGLIKDURUM+BMI+TUTUNDUMAN_MARUZ+ALKOLKULLANIM_DURUM, family = binomial(link="logit"),data = training)
prediction <- predict(g,newdata=test, type="response")
prediction1 <- ifelse(prediction > 0.5, 1,0)
pp<-table(prediction1, test$TUTUNKULLANIM_DURUM)
accuracy <- (pp[01]+pp[04])/(pp[01]+pp[04]+pp[02]+pp[03])
sensitivity <- pp[01]/(pp[01]+pp[03])
specifity <- pp[04]/(pp[02]+pp[04])


############1:1 eslesme ######## 
m1_1=matchit(TUTUNKULLANIM_DURUM ~ CINSIYET+YAS_GRUP+EGITIM+MEDENI_DURUM+CALISMA_DURUM+HANE_GELIR
             +HASTALIK_SAGLIKDURUM+BMI+TUTUNDUMAN_MARUZ+ALKOLKULLANIM_DURUM,method="nearest", data=veri,ratio=1, "logit")
data1_1 = match.data(m1_1)
g1_1<-glm(TUTUNKULLANIM_DURUM ~ CINSIYET+YAS_GRUP+EGITIM+MEDENI_DURUM+CALISMA_DURUM+HANE_GELIR
          +HASTALIK_SAGLIKDURUM+BMI+TUTUNDUMAN_MARUZ+ALKOLKULLANIM_DURUM, family = binomial(link="logit"),data = training)
pred.prob1_1 <- predict(g1_1,newdata=data1_1, type="response")
pred.prob1_11 <- ifelse(pred.prob1_1 > 0.5, 1, 0)
pp1<-table(pred.prob1_11, data1_1$TUTUNKULLANIM_DURUM)
accuracy1_1=(pp1[01]+pp1[04])/(pp1[01]+pp1[04]+pp1[02]+pp1[03])
sensitivity1_1 <- pp1[01]/(pp1[01]+pp1[03])
specifity1_1 <- pp1[04]/(pp1[02]+pp1[04])

########caliper
mcaliper = matchit(TUTUNKULLANIM_DURUM ~ CINSIYET+YAS_GRUP+EGITIM+MEDENI_DURUM+CALISMA_DURUM+HANE_GELIR
                   +HASTALIK_SAGLIKDURUM+BMI+TUTUNDUMAN_MARUZ+ALKOLKULLANIM_DURUM,
                   method="nearest", "logit",ratio=1, data=veri,caliper=0.25)
datacaliper = match.data(mcaliper)
gcaliper<-glm(TUTUNKULLANIM_DURUM ~ CINSIYET+YAS_GRUP+EGITIM+MEDENI_DURUM+CALISMA_DURUM+HANE_GELIR
              +HASTALIK_SAGLIKDURUM+BMI+TUTUNDUMAN_MARUZ+ALKOLKULLANIM_DURUM, family = binomial(link="logit"),data = training) 
pred.probcaliper <- predict(gcaliper, newdata=datacaliper,type="response")
pred.probcaliper1 <- ifelse(pred.probcaliper > 0.5, 1, 0)
pp2<-table(pred.probcaliper1, datacaliper$TUTUNKULLANIM_DURUM)
accuracy_caliper=(pp2[01]+pp2[04])/(pp2[01]+pp2[04]+pp2[02]+pp2[03])
sensitivity_caliper <- pp2[01]/(pp2[01]+pp2[03])
specifity_caliper <- pp2[04]/(pp2[02]+pp2[04])

#####tabakali
mtab=matchit(TUTUNKULLANIM_DURUM ~ CINSIYET+YAS_GRUP+EGITIM+MEDENI_DURUM+CALISMA_DURUM+HANE_GELIR
             +HASTALIK_SAGLIKDURUM+BMI+TUTUNDUMAN_MARUZ+ALKOLKULLANIM_DURUM,
             data = veri , method = "subclass",subclass=5,distance = "logit",discard = "control")
datatab=match.data(mtab)
gtab<-glm(TUTUNKULLANIM_DURUM ~ CINSIYET+YAS_GRUP+EGITIM+MEDENI_DURUM+CALISMA_DURUM+HANE_GELIR
          +HASTALIK_SAGLIKDURUM+BMI+TUTUNDUMAN_MARUZ+ALKOLKULLANIM_DURUM, family = binomial(link="logit"), data =training) 
pred.probtab = predict(gtab,newdata=datatab, type="response")
pred.probtab1 = ifelse(pred.probtab > 0.5, 1, 0)
pp3<-table(pred.probtab1, datatab$TUTUNKULLANIM_DURUM)
accuracy_tab=(pp3[01]+pp3[04])/(pp3[01]+pp3[04]+pp3[02]+pp3[03])
sensitivity_tab <- pp3[01]/(pp3[01]+pp3[03])
specifity_tab <- pp3[04]/(pp3[02]+pp3[04])


######mahalanobis
mmah = matchit(TUTUNKULLANIM_DURUM ~ CINSIYET+YAS_GRUP+EGITIM+MEDENI_DURUM+CALISMA_DURUM+HANE_GELIR
               +HASTALIK_SAGLIKDURUM+BMI+TUTUNDUMAN_MARUZ+ALKOLKULLANIM_DURUM,
               method="nearest",ratio=1, data=veri,distance=	"mahalanobis")
datamah = match.data(mmah)
gmah<-glm(TUTUNKULLANIM_DURUM ~ CINSIYET+YAS_GRUP+EGITIM+MEDENI_DURUM+CALISMA_DURUM+HANE_GELIR
          +HASTALIK_SAGLIKDURUM+BMI+TUTUNDUMAN_MARUZ+ALKOLKULLANIM_DURUM, family = binomial(link="logit"),data = training) 
pred.probmah <- predict(gmah, newdata=datamah,type="response")
pred.probmah1 <- ifelse(pred.probmah > 0.5, 1, 0)
pp4<-table(pred.probmah1, datamah$TUTUNKULLANIM_DURUM)
accuracy_mmah=(pp4[01]+pp4[04])/(pp4[01]+pp4[04]+pp4[02]+pp4[03])
sensitivity_mmah <- pp4[01]/(pp4[01]+pp4[03])
specifity_mmah <- pp4[04]/(pp4[02]+pp4[04])

#####maha-ps
ps<-glm(TUTUNKULLANIM_DURUM ~ CINSIYET+YAS_GRUP+EGITIM+MEDENI_DURUM+CALISMA_DURUM+HANE_GELIR
        +HASTALIK_SAGLIKDURUM+BMI+TUTUNDUMAN_MARUZ+ALKOLKULLANIM_DURUM, family = binomial(link="logit"),data = veri)
veri$psvalue <-	predict(ps,type	=	"response")

mmah_ps = matchit(TUTUNKULLANIM_DURUM ~ CINSIYET+YAS_GRUP+EGITIM+MEDENI_DURUM+CALISMA_DURUM+HANE_GELIR
                  +HASTALIK_SAGLIKDURUM+BMI+TUTUNDUMAN_MARUZ+ALKOLKULLANIM_DURUM+psvalue,
                  mahvars=c("CINSIYET","YAS_GRUP","EGITIM","MEDENI_DURUM","CALISMA_DURUM","HANE_GELIR",
                            "HASTALIK_SAGLIKDURUM", "BMI","TUTUNDUMAN_MARUZ","ALKOLKULLANIM_DURUM","psvalue"),
                  caliper=0.25,data=veri,distance=	"mahalanobis")
datamahps1 = match.data(mmah_ps)
gmahps<-glm(TUTUNKULLANIM_DURUM ~ CINSIYET+YAS_GRUP+EGITIM+MEDENI_DURUM+CALISMA_DURUM+HANE_GELIR
            +HASTALIK_SAGLIKDURUM+BMI+TUTUNDUMAN_MARUZ+ALKOLKULLANIM_DURUM, family = binomial(link="logit"),data = training)
pred.probmahps = predict(gmahps, newdata=datamahps1,type="response")
pred.probmahps1 = ifelse(pred.probmahps > 0.5, 1, 0)
pp5<-table(pred.probmahps1, datamahps1$TUTUNKULLANIM_DURUM)
accuracy_mps=(pp5[01]+pp5[04])/(pp5[01]+pp5[04]+pp5[02]+pp5[03])
sensitivity_mps <- pp5[01]/(pp5[01]+pp5[03])
specifity_mps <- pp5[04]/(pp5[02]+pp5[04])

####ortalama doðru sýnýflama####
accuracy
accuracy1_1
accuracy_caliper
accuracy_tab  
accuracy_mmah
accuracy_mps

sensitivity
sensitivity1_1
sensitivity_caliper
sensitivity_tab
sensitivity_mmah
sensitivity_mps

specifity
specifity1_1
specifity_caliper
specifity_tab
specifity_mmah
specifity_mps


###eþleþtirmeden önce frekanslar###
table(veri$CINSIYET, veri$TUTUNKULLANIM_DURUM)
table(veri$YAS_GRUP, veri$TUTUNKULLANIM_DURUM)
table(veri$EGITIM, veri$TUTUNKULLANIM_DURUM)
table(veri$MEDENI_DURUM, veri$TUTUNKULLANIM_DURUM)
table(veri$CALISMA_DURUM, veri$TUTUNKULLANIM_DURUM)
table(veri$HANE_GELIR, veri$TUTUNKULLANIM_DURUM)
table(veri$HASTALIK_SAGLIKDURUM, veri$TUTUNKULLANIM_DURUM)
table(veri$TUTUNDUMAN_MARUZ, veri$TUTUNKULLANIM_DURUM)
table(veri$ALKOLKULLANIM_DURUM, veri$TUTUNKULLANIM_DURUM)
aggregate(veri$BMI, list(veri$TUTUNKULLANIM_DURUM), mean)
aggregate(veri$BMI, list(veri$TUTUNKULLANIM_DURUM), sd)
####1:1 eþleþtirme frekanslar######
table(data1_1$CINSIYET, data1_1$TUTUNKULLANIM_DURUM)
table(data1_1$YAS_GRUP, data1_1$TUTUNKULLANIM_DURUM)
table(data1_1$EGITIM, data1_1$TUTUNKULLANIM_DURUM)
table(data1_1$MEDENI_DURUM, data1_1$TUTUNKULLANIM_DURUM)
table(data1_1$CALISMA_DURUM, data1_1$TUTUNKULLANIM_DURUM)
table(data1_1$HANE_GELIR, data1_1$TUTUNKULLANIM_DURUM)
table(data1_1$HASTALIK_SAGLIKDURUM, data1_1$TUTUNKULLANIM_DURUM)
table(data1_1$TUTUNDUMAN_MARUZ, data1_1$TUTUNKULLANIM_DURUM)
table(data1_1$ALKOLKULLANIM_DURUM, data1_1$TUTUNKULLANIM_DURUM)
aggregate(data1_1$BMI, list(data1_1$TUTUNKULLANIM_DURUM), mean)
aggregate(data1_1$BMI, list(data1_1$TUTUNKULLANIM_DURUM), sd)
####caliper eþleþtirme frekanslar######
table(datacaliper$CINSIYET, datacaliper$TUTUNKULLANIM_DURUM)
table(datacaliper$YAS_GRUP, datacaliper$TUTUNKULLANIM_DURUM)
table(datacaliper$EGITIM, datacaliper$TUTUNKULLANIM_DURUM)
table(datacaliper$MEDENI_DURUM, datacaliper$TUTUNKULLANIM_DURUM)
table(datacaliper$CALISMA_DURUM, datacaliper$TUTUNKULLANIM_DURUM)
table(datacaliper$HANE_GELIR, datacaliper$TUTUNKULLANIM_DURUM)
table(datacaliper$HASTALIK_SAGLIKDURUM, datacaliper$TUTUNKULLANIM_DURUM)
table(datacaliper$TUTUNDUMAN_MARUZ, datacaliper$TUTUNKULLANIM_DURUM)
table(datacaliper$ALKOLKULLANIM_DURUM, datacaliper$TUTUNKULLANIM_DURUM)
aggregate(datacaliper$BMI, list(datacaliper$TUTUNKULLANIM_DURUM), mean)
aggregate(datacaliper$BMI, list(datacaliper$TUTUNKULLANIM_DURUM), sd)
####tabakalý eþleþtirme frekanslar######
table(datatab$CINSIYET, datatab$TUTUNKULLANIM_DURUM)
table(datatab$YAS_GRUP, datatab$TUTUNKULLANIM_DURUM)
table(datatab$EGITIM, datatab$TUTUNKULLANIM_DURUM)
table(datatab$MEDENI_DURUM, datatab$TUTUNKULLANIM_DURUM)
table(datatab$CALISMA_DURUM, datatab$TUTUNKULLANIM_DURUM)
table(datatab$HANE_GELIR, datatab$TUTUNKULLANIM_DURUM)
table(datatab$HASTALIK_SAGLIKDURUM, datatab$TUTUNKULLANIM_DURUM)
table(datatab$TUTUNDUMAN_MARUZ, datatab$TUTUNKULLANIM_DURUM)
table(datatab$ALKOLKULLANIM_DURUM, datatab$TUTUNKULLANIM_DURUM)
aggregate(datatab$BMI, list(datatab$TUTUNKULLANIM_DURUM), mean)
aggregate(datatab$BMI, list(datatab$TUTUNKULLANIM_DURUM), sd)
####mahalonobis eþleþtirme frekanslar######
table(datamah$CINSIYET, datamah$TUTUNKULLANIM_DURUM)
table(datamah$YAS_GRUP, datamah$TUTUNKULLANIM_DURUM)
table(datamah$EGITIM, datamah$TUTUNKULLANIM_DURUM)
table(datamah$MEDENI_DURUM, datamah$TUTUNKULLANIM_DURUM)
table(datamah$CALISMA_DURUM, datamah$TUTUNKULLANIM_DURUM)
table(datamah$HANE_GELIR, datamah$TUTUNKULLANIM_DURUM)
table(datamah$HASTALIK_SAGLIKDURUM, datamah$TUTUNKULLANIM_DURUM)
table(datamah$TUTUNDUMAN_MARUZ, datamah$TUTUNKULLANIM_DURUM)
table(datamah$ALKOLKULLANIM_DURUM, datamah$TUTUNKULLANIM_DURUM)
aggregate(datamah$BMI, list(datamah$TUTUNKULLANIM_DURUM), mean)
aggregate(datamah$BMI, list(datamah$TUTUNKULLANIM_DURUM), sd)
####mah-ps eþleþtirme frekanslar######
table(datamahps1$CINSIYET, datamahps1$TUTUNKULLANIM_DURUM)
table(datamahps1$YAS_GRUP, datamahps1$TUTUNKULLANIM_DURUM)
table(datamahps1$EGITIM, datamahps1$TUTUNKULLANIM_DURUM)
table(datamahps1$MEDENI_DURUM, datamahps1$TUTUNKULLANIM_DURUM)
table(datamahps1$CALISMA_DURUM, datamahps1$TUTUNKULLANIM_DURUM)
table(datamahps1$HANE_GELIR, datamahps1$TUTUNKULLANIM_DURUM)
table(datamahps1$HASTALIK_SAGLIKDURUM, datamahps1$TUTUNKULLANIM_DURUM)
table(datamahps1$TUTUNDUMAN_MARUZ, datamahps1$TUTUNKULLANIM_DURUM)
table(datamahps1$ALKOLKULLANIM_DURUM, datamahps1$TUTUNKULLANIM_DURUM)
aggregate(datamahps1$BMI, list(datamahps1$TUTUNKULLANIM_DURUM), mean)
aggregate(datamahps1$BMI, list(datamahps1$TUTUNKULLANIM_DURUM), sd)
