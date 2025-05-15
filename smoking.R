library(corrplot) #si
library(ggplot2)
library(gridExtra)
library(heplots)
library(MASS)
library(tidyverse)
library(car)
library(class)
library(caret)
library( ROCR)
library(mice)

smoking<-read.csv("C:/Users/giuli/OneDrive/Desktop/Università/3anno/Data mining/progetto/smoking.csv")
str(smoking)

#controllo la presenza dei valori mancanti
anyNA(smoking)

#trasformo le varibali in factor
smoking$Urine.protein<-as.factor(smoking$Urine.protein)
levels(smoking$Urine.protein)
nrow(smoking)-sum(smoking$Urine.protein==1) #3093
smoking$hearing.left.<-as.factor(smoking$hearing.left.)
levels(smoking$hearing.left.)
smoking$hearing.right.<-as.factor(smoking$hearing.right.)
levels(smoking$hearing.right.)
smoking$dental.caries<-as.factor(smoking$dental.caries)
levels(smoking$dental.caries)
smoking$tartar<-as.factor(smoking$tartar)
levels(smoking$tartar)<-c(0,1)
smoking$gender<-as.factor(smoking$gender)
str(smoking)
#rimuovo le varibili ID  e oral
smoking<-smoking[,-1]
smoking<-smoking[,-23]

#verifico la presenza della classe maggioritaria
sum(smoking$smoking==1)/nrow(smoking) #36.72% di persone che fumano
sum(smoking$smoking==0)/nrow(smoking) #63.27% di persone che non fumano

#divido il dataset in training e test
set.seed(220)
#training e test
dati_idx<-sample(nrow(smoking), nrow(smoking)*0.8)
#training
dati_trn<-smoking[dati_idx, ]
#test
dati_tst<- smoking[-dati_idx, ]
dati_tst2<- smoking[-dati_idx, ]
#calcolo validation
dati_idx_small<-sample(nrow(dati_trn), nrow(dati_trn)*0.75)
dati_smaller_trn<-dati_trn[dati_idx_small, ]
dati_vld<- dati_trn[-dati_idx_small,]

#lavoro sul training 
#Riduzione e trasformazione variabili
fac<-c(1,8,9,18,23,24)
correlazione<-cor(dati_smaller_trn[,-fac])
round(correlazione,2)
col <- colorRampPalette(c("#8B008B", "#FF1493", "#FFFFFF", "#00CED1", "#104E8B"))
par(mfrow=c(1,1))
corrplot(correlazione, method="color", col=col(200),  
         type="upper", order="hclust", 
         addCoef.col = "black", # Add coefficient of correlation
         tl.col="black", tl.srt=45, #Text label color and rotation
         # hide correlation coefficient on the principal diagonal
         diag=FALSE 
)


#elimino la variabile ALT
colnames(dati_smaller_trn)
dati_smaller_trn<-dati_smaller_trn[,-21]
#tolgo waist_cm
dati_smaller_trn<-dati_smaller_trn[,-5]


#IMPATTO DI smoking SULLE VARIABILI 

#test per le medie
trn_1<-dati_smaller_trn[dati_smaller_trn$smoking==1,]
trn_0<-dati_smaller_trn[dati_smaller_trn$smoking==0,]
fac<-c(1,7,8,17,21,22)
#pv<-t.test(trn_1[,t], trn_0[,t], paired=FALSE, mu=0)

# i pvalue sono tutti minori di 0.05 percui non tolgo nessuna variabili

#istogrammi
trn_1nf<-trn_1[,-fac]
trn_0nf<-trn_0[,-fac]
for (k in 1:(length(trn_1nf)-1)){
  par(mfcol=c(2,1))
  hist(trn_1nf[, k], main=colnames(trn_1nf)[k], breaks=20 )
  hist(trn_0nf[, k], main=colnames(trn_0nf)[k], breaks=20)
}


#ANALISI OUTLIER 
par(mfrow=c(1,1))
boxplot(dati_smaller_trn[,-fac])
? boxplot
#DATA LA PRESENZA ANCORA ALTA DI OUTLIER FACCIO DELLE ANALISI QUALITATIVE VEDENDO 
#QUALI SONO I VALORI NORMALI DI VARIAZIONE DELLE VARIABILI

#PRESSIONE ALTA DECIDO DI ELIMINARE I VALORI MAGGIORI DI 200
dati_smaller_trn<-dati_smaller_trn[-which(dati_smaller_trn$systolic>200),]

#FASTING BLOOD SUGAR
dati_smaller_trn<-dati_smaller_trn[-which(dati_smaller_trn$fasting.blood.sugar>400),]

#CHOLESTEROL
dati_smaller_trn<-dati_smaller_trn[-which(dati_smaller_trn$Cholesterol>400),]

#SERUM CREATININA
dati_smaller_trn<-dati_smaller_trn[-which(dati_smaller_trn$serum.creatinine>4),]

#AST
dati_smaller_trn<-dati_smaller_trn[-which(dati_smaller_trn$AST>300),]

#LDL 
dati_smaller_trn<-dati_smaller_trn[-which(dati_smaller_trn$LDL>500),]
#HDL
dati_smaller_trn<-dati_smaller_trn[-which(dati_smaller_trn$HDL>300),]
#TRIGLICERIDI
dati_smaller_trn<-dati_smaller_trn[-which(dati_smaller_trn$triglyceride>400),]


#DOPODICHE SVOLTO IL LOGARITMO DI ALCUNI CHE HANNO ANCORA OUTLIER
hist(dati_smaller_trn$Gtp, col="blue")
dati_smaller_trn$Gtp<-log(dati_smaller_trn$Gtp)
boxplot.stats(dati_smaller_trn$Gtp)
# DA PIU DI 3000 passo a 600

hist(dati_smaller_trn$AST, col="blue")
dati_smaller_trn$AST<-log(dati_smaller_trn$AST)
boxplot.stats(dati_smaller_trn$AST)
# da 2100 passo a 1200

# con triglicerici
hist(dati_smaller_trn$triglyceride, col="blue")
dati_smaller_trn$triglyceride<-log(dati_smaller_trn$triglyceride)
boxplot.stats(dati_smaller_trn$triglyceride)
boxplot(dati_smaller_trn$triglyceride)
# passo da 1200 a 27

#fasting blood sugar
#boxplot.stats(dati_smaller_trn$fasting.blood.sugar)
#circa 2000
#prova<-dati_smaller_trn
#prova$fasting.blood.sugar<-log(prova$fasting.blood.sugar)
#arrivo a 1700
# la trasformazione logaritmica non funziona
# percui utilizzo la funzione symbox per vedere quale trasformazione è più adatta
symbox(dati_smaller_trn$fasting.blood.sugar)

#STANDARDIZZAZIONE
medie<-c(0)
sdd<-c(0)
nf<-c(2:6,9:16,18:20)
for(t in nf){ #dati normalizzati
  medie[t]<-mean(dati_smaller_trn[,t])
  sdd[t]<-sd(dati_smaller_trn[,t])
  dati_smaller_trn[,t]<-(dati_smaller_trn[,t]-medie[t])/(sdd[t])
}

#VERIFICA ASSUNZIONI PER LDA/QDA
plot_box_all <-list()
variables <- colnames(dati_smaller_trn)[nf]

par(mfrow=c(4,2))
# Costruzione dei boxplot condizionati
dati_smaller_trn$smoking<-as.factor(dati_smaller_trn$smoking)
for (i in variables){
  plot_box_all[[i]] <- ggplot(dati_smaller_trn, aes_string(x = "smoking", y = i, col = "smoking", fill = "smoking")) + 
    geom_boxplot() + 
    theme(legend.position = "none") + 
    scale_color_manual(values = c("deeppink", "darkblue")) 
  scale_fill_manual(values = c("deeppink", "cyan"))
}

do.call(grid.arrange, c(plot_box_all, nrow = 2))

# le varianze sono comuni alla classe

#VERIFICO LA COVARIANZA PER CLASSI
# Costruzione del grafico per la valutazione delle covarianze
covEllipses(dati_smaller_trn[,nf], 
            factor(dati_smaller_trn$smoking), 
            fill = TRUE, 
            pooled = FALSE, 
            col = c("deeppink", "aquamarine3"), 
            variables = c(1:16, 16), 
            fill.alpha = 0.05)

#si nota che anche la covarianza è comune

#VERIFICO NORMALITA'
library(nortest)
ad.test(trn_0$relaxation)$p.value
ad.test(trn_1$relaxation)$p.value
# Costruzione dei qqplot per la verifica della normalità delle osservazioni condizionatamente alla classe.
# Classe: 1
trn_1<-dati_smaller_trn[dati_smaller_trn$smoking==1,]
trn_0<-dati_smaller_trn[dati_smaller_trn$smoking==0,]
trn_1nf<-trn_1[,nf]
trn_0nf<-trn_0[,nf]
par(mfrow = c(2, 2))
for(i in 1:16) {
  qqnorm(trn_1nf[, i], main = colnames(trn_1nf)[i], col=5); qqline(trn_1nf[, i], col = 2)
}

#Classe: 0
for(i in 1:16) {
  qqnorm(trn_0nf[, i], main = colnames(trn_0nf)[i], col=5); qqline(trn_0nf[, i], col = 2)
}

#Classe zero per alcune var per rappresentazione grafica
for(i in cd) {
  qqnorm(trn_1nf[, i], main = colnames(trn_1nf)[i], col=5); qqline(trn_1nf[, i], col = 2)
}

#Classe: 0
for(i in cd) {
  qqnorm(trn_0nf[, i], main = colnames(trn_0nf)[i], col=5); qqline(trn_0nf[, i], col = 2)
  
  
}
plot_density <- list()
# Costruzione curve di densità condizionate rispetto alla classe
dati_smaller_trn$smoking<-as.factor(dati_smaller_trn$smoking)
variabili<-colnames(dati_smaller_trn)[nf]
for(i in variabili){
  plot_density[[i]] <- ggplot(dati_smaller_trn, aes_string(x = i, y = "..density..", col="smoking")) + 
    geom_density(aes(y=..density..))+
    scale_color_manual(values = c("dodgerblue3", "deeppink3")) + 
    theme(legend.position = "none")
}
#ggplot(dati_smaller_trn$AST, aes_string(x=dati_smaller_trn$AST, y="..density"))
do.call(grid.arrange, c(plot_density, nrow = 4))

#nessuna delle variabili presenta una distribuzione normale percui la assunzione di base
#per LDA e QDA non sussiste percui non posso svolgere LDA e QDA

#finito le analisi preliminari

#REGRESSIONI LOGISTICA
model_glm<-glm(smoking~., data=dati_smaller_trn, family="binomial")
summary(model_glm)
#30775

step.model <- stepAIC(model_glm, direction = "both", trace = FALSE)
summary(step.model)
#30776
anova(step.model, test="Chisq")

par(mfrow=c(1,1))
influencePlot(model_glm, col="darkblue")

which(row.names(dati_smaller_trn)==43722) #16182
which(row.names(dati_smaller_trn)==54393)  #19758
which(row.names(dati_smaller_trn)==8441) #32537
which(row.names(dati_smaller_trn)==6835) #7560

influenti<-c(7560,32537,19758,16182)
trn_nuovo<-dati_smaller_trn[-influenti,]

#ricalcolo il modello senza putni influenti e vedo se il mio AIC migliora
#da applicare sul training gia modificato
model_glm2= glm(smoking ~. , data = trn_nuovo, 
                family = "binomial")
summary(model_glm2) #30750
step.model2 <- stepAIC(model_glm2, direction = "both", trace = FALSE)
summary(step.model2) #30741

anova(step.model2, test="Chisq")

#----
#grafico logit e variabili
# Calcolo fitted values
probabilities <- predict(step.model2, type = "response")
predictors <- c("systolic", "Cholesterol", "Fasting Blood Sugar", "AST")

# Costruzione delle log(p1/(1-p1))
supp <- dati_smaller_trn[-influenti, c(9,12,11,19)]
supp <- supp %>%
  mutate(logit = log(probabilities/(1-probabilities))) %>%
  gather(key = "predictors", value = "predictor.value", -logit)

# Costruzione dei grafici
ggplot(supp[, -9], aes(logit, predictor.value))+
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess") + 
  theme_bw() + 
  facet_wrap(~predictors, scales = "free_y")


#-----
#modifico il validation
#elimino la variabile ALT
colnames(dati_vld)
dati_vld<-dati_vld[,-21]
#tolgo waist_cm
dati_vld<-dati_vld[,-5]

#PRESSIONE ALTA DECIDO DI ELIMINARE I VALORI MAGGIORI DI 200
if (which(dati_vld$systolic>200)!=0){ dati_vld<-dati_vld[-which(dati_vld$systolic>200),]}

#FASTING BLOOD SUGAR
#if (as.numeric(which(dati_vld$fasting.blood.sugar>400)!=0){ dati_vld<-dati_vld[-which(dati_vld$fasting.blood.sugar>400),]}

#CHOLESTEROL
#dati_vld<-dati_vld[-which(dati_vld$Cholesterol>400),]

#SERUM CREATININA
dati_vld<-dati_vld[-which(dati_vld$serum.creatinine>4),]

#AST
dati_vld<-dati_vld[-which(dati_vld$AST>300),]

#LDL 
dati_vld<-dati_vld[-which(dati_vld$LDL>500),]
#HDL
#dati_vld<-dati_vld[-which(dati_vld$HDL>300),]
#TRIGLICERIDI
dati_vld<-dati_vld[-which(dati_vld$triglyceride>400),]

#DOPODICHE SVOLTO IL LOGARITMO DI ALCUNI CHE HANNO ANCORA OUTLIER

dati_vld$Gtp<-log(dati_vld$Gtp)

dati_vld$AST<-log(dati_vld$AST)

# con triglicerici
dati_vld$triglyceride<-log(dati_vld$triglyceride)

#STANDARDIZZAZIONE

nf<-c(2:6,9:16,18:20)
for(t in nf){ #dati normalizzati
  dati_vld[,t]<-(dati_vld[,t]-medie[t])/(sdd[t])
}

par(mfrow=c(1,1))
#----
#
pred_logit <- predict(step.model, dati_vld[, -23], type = "response")
pred_logit_class <- ifelse (pred_logit > 0.5, 1, 0) 
calc_class_err = function(actual, predicted) { mean(actual != predicted)
}

err_vld <- calc_class_err(actual = dati_vld$smoking, predicted = pred_logit_class)
err_vld
#0.2519
table("predicted" = pred_logit_class,
      "actual" = dati_vld$smoking)
acc_log_vld<- sum(diag(table(dati_vld$smoking, pred_logit_class)))/length(pred_logit_class)
acc_log_vld #74.8%

pred_roclogit <- prediction(pred_logit, dati_vld[, 23])
perf_logit <- performance(pred_roclogit,"tpr","fpr")
auc_logit <- performance(pred_roclogit, measure = "auc")@y.values
auc_logit #0.8301

#KNN
#divido il dataset in variabile target e covariate e tolgo le variabili qualitative
#training
x_dati_small<-dati_smaller_trn[,nf]
y_dati_small=dati_smaller_trn$smoking
#divido il vld
x_dati_vld<-dati_vld[,nf]
y_dati_vld=dati_vld$smoking

k_to_try = 2:50
err_k = rep(x = 0, times = length(k_to_try))
for (i in seq_along(k_to_try)) {
  pred = knn(train = x_dati_small,
             test = x_dati_vld, cl = y_dati_small,
             k = k_to_try[i])
  err_k[i] = calc_class_err(y_dati_vld,pred) }
#plot
par(mfrow=c(1,1))
plot(err_k, type = "b", col = "dodgerblue", cex = 1, pch = 20,
     xlab = "k, number of neighbors", ylab = "classification error", 
     main = "(Test) Error Rate vs Neighbors")
# Minimum error
abline(h = min(err_k), col = "darkorange", lty = 3)
#44

## Previsione con k migliore
pred = knn(train = x_dati_small,
           test = x_dati_vld, cl = y_dati_small,
           k = max(which(err_k == min(err_k))), prob = TRUE)

# error rate
errknnvld = calc_class_err(actual = y_dati_vld, predicted = pred)
errknnvld #0.2632


conf_matrix = table(pred, y_dati_vld)
conf_matrix

# Accuracy
accknn<-accuracy(conf_matrix) #73.67

#ROC
prob_knn <- attributes(pred)$prob
# Trasformazione delle proporzioni (proporzione di elementi su K che appartengono alla classe 0) che il KNN resitutisce in probabilità
prob_knn <- 2*ifelse(pred== "0", 1-prob_knn, prob_knn) - 1
pred_rocknn <- prediction(prob_knn, dati_vld[, 23])
perf_knn<- performance(pred_rocknn,"tpr","fpr")

auc_knn <- performance(pred_rocknn, measure = "auc")@y.values
auc_knn
#0.81

par(mfrow = c(1, 2))
plot(perf_logit, colworize = TRUE, main = "Regressione Logistica")
plot(perf_knn, colorize = TRUE, main = "5-NN")

#training completo
dati_smaller_trn1<-dati_trn[dati_idx_small, ]
dati_vld1<- dati_trn[-dati_idx_small,]
training<-rbind(dati_smaller_trn1, dati_vld1)

#elimino la variabile ALT
colnames(dati_smaller_trn)
training<-training[,-21]
#tolgo waist_cm
training<-training[,-5]

#PRESSIONE ALTA DECIDO DI ELIMINARE I VALORI MAGGIORI DI 200
training<-training[-which(training$systolic>200),]

#FASTING BLOOD SUGAR
training<-training[-which(training$fasting.blood.sugar>400),]

#CHOLESTEROL
training<-training[-which(training$Cholesterol>400),]

#SERUM CREATININA
training<-training[-which(training$serum.creatinine>4),]

#AST
training<-training[-which(training$AST>300),]

#LDL 
training<-training[-which(training$LDL>500),]
#HDL
training<-training[-which(training$HDL>300),]
#TRIGLICERIDI
training<-training[-which(training$triglyceride>400),]

#DOPODICHE SVOLGO IL LOGARITMO DI ALCUNI CHE HANNO ANCORA OUTLIER
hist(dati_smaller_trn$Gtp)
training$Gtp<-log(training$Gtp)
boxplot.stats(training$Gtp)

training$AST<-log(training$AST)
boxplot.stats(training$AST)
# da 2100 passo a 1200

# con triglicerici
training$triglyceride<-log(training$triglyceride)
boxplot.stats(training$triglyceride)

#STANDARDIZZAZIONE
mediec<-c(0)
sddc<-c(0)
nf<-c(2:6,9:16,18:20)
for(t in nf){ #dati normalizzati
  mediec[t]<-mean(training[,t])
  sddc[t]<-sd(training[,t])
  training[,t]<-(training[,t]-mediec[t])/(sddc[t])
}

#ricalcolo della logistica sul training completo
#REGRESSIONI LOGISTICA
model_glmc<-glm(smoking~., data=training, family="binomial")
summary(model_glmc)
#41274

step.modelc <- stepAIC(model_glmc, direction = "both", trace = FALSE) #AIC 76
summary(step.modelc)
#41226


anova(step.model, test="Chisq")
#weight, systilic, AST, urine protein

influencePlot(model_glmc)
#influenti rownames 6835,43722,54393,8441)
which(row.names(training)==43722) #16182
which(row.names(training)==54393)  #19758
which(row.names(training)==8441) #32537
which(row.names(training)==6835) #7560

influenti<-c(7560,32537,19758,16182)
trnc_nuovo<-training[-influenti,]
#ricalcolo il modello senza putni influenti e vedo se il mio AIC migliora
#da applicare sul training gia modificato
model_glm2c= glm(smoking ~. , data = trnc_nuovo, 
                family = "binomial")
summary(model_glm2c) #30750
res<-model_glm2c$residuals
plot(res)
step.model<- stepAIC(model_glm2c, direction = "both", trace = FALSE) 
summary(step.model) #30741

anova(step.model, test="Chisq")

#modifiche sul test
#elimino la variabile ALT
colnames(dati_tst)
dati_tst<-dati_tst[,-21]
#tolgo waist_cm
dati_tst<-dati_tst[,-5]

#PRESSIONE ALTA DECIDO DI ELIMINARE I VALORI MAGGIORI DI 200
dati_tst<-dati_tst[-which(dati_tst$systolic>200),]

#FASTING BLOOD SUGAR
#dati_tst<-dati_tst[-which(dati_tst$fasting.blood.sugar>400),]

#CHOLESTEROL
#dati_tst<-dati_tst[-which(dati_tst$Cholesterol>400),]

#SERUM CREATININA
dati_tst<-dati_tst[-which(dati_tst$serum.creatinine>4),]

#AST
dati_tst<-dati_tst[-which(dati_tst$AST>300),]

#LDL 
dati_tst<-dati_tst[-which(dati_tst$LDL>500),]
#HDL
#dati_tst<-dati_tst[-which(dati_tst$HDL>300),]
#TRIGLICERIDI
#dati_tst<-dati_tst[-which(dati_tst$triglyceride>400),]

#DOPODICHE SVOLTO IL LOGARITMO DI ALCUNI CHE HANNO ANCORA OUTLIER
dati_tst$Gtp<-log(dati_tst$Gtp)

dati_tst$AST<-log(dati_tst$AST)

# con triglicerici
dati_tst$triglyceride<-log(dati_tst$triglyceride)
boxplot.stats(training$triglyceride)

#STANDARDIZZAZIONE
nf<-c(2:6,9:16,18:20)
for(t in nf){ #dati normalizzati
  dati_tst[,t]<-(dati_tst[,t]-mediec[t])/(sddc[t])
}

#----
#
pred_logit <- predict(step.model, dati_tst[, -23], type = "response")
pred_logit_class <- ifelse (pred_logit > 0.5, 1, 0) 


calc_class_err = function(actual, predicted) { mean(actual != predicted)
}

err_tst <- calc_class_err(actual = dati_tst$smoking, predicted = pred_logit_class)
#0.2475
table("predicted" = pred_logit_class,
      "actual" = dati_tst$smoking)
acc_log_tst<- sum(diag(table(dati_tst$smoking, pred_logit_class)))/length(pred_logit_class)
acc_log_tst #0.7524

pred_roclogit <- prediction(pred_logit, dati_tst[, 23])
perf_logit <- performance(pred_roclogit,"tpr","fpr")
auc_logit <- performance(pred_roclogit, measure = "auc")@y.values
auc_logit #0.8355

confusionMatrix(factor(dati_tst[, 23]), factor(as.vector(pred_logit_class)))

x_dati_trn<-training[,nf]
y_dati_trn=training$smoking
#tst
x_dati_tst<-dati_tst[,nf]
y_dati_tst=dati_tst$smoking
best_k=44
best_k = max(which(err_k == min(err_k)))
pred = knn(train = x_dati_trn,
           test = x_dati_tst, cl = y_dati_trn,
           k = best_k, prob = TRUE)
confusionMatrix(factor(dati_tst[, 23]), pred)

# test error rate
test_error_rate = calc_class_err(actual = y_dati_tst, predicted = pred)
test_error_rate #0.2657

test_error_rate*100
# confusion matrix o matrice di confusione
conf_matrix = table(pred, y_dati_tst)
conf_matrix

# Accuracy
accuracy = function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}

accuracy(conf_matrix) #73.41

100-accuracy(conf_matrix)

prob_knn <- attributes(pred)$prob
# Trasformazione delle proporzioni (proporzione di elementi su K che appartengono alla classe 0) che il KNN resitutisce in probabilità
prob_knn <- 2*ifelse(pred== "0", 1-prob_knn, prob_knn) - 1
pred_rocknn <- prediction(prob_knn, dati_tst[, 23])
perf_knn<- performance(pred_rocknn,"tpr","fpr")

auc_knn <- performance(pred_rocknn, measure = "auc")@y.values
auc_knn #0.816

str(dati_smaller_trn)

#confronto graficamente le curve ROC
par(mfrow = c(1, 2))
plot(perf_logit, colworize = TRUE, main = "Regressione Logistica")
plot(perf_knn, colorize = TRUE, main = "44-NN")

#--- aggiunta dei nostri valori al test

Giorgia<-c(0,21,162,50,0.5,0.5,0,0,100,68,73,250,170,72,144,12.7,0,0.71,27,40,0,0,0)
Giulia<-c(0,21,161,54,1.5,2,0,0,110,72, 78,153,75,66,72,13.4,0,0.72,17,40,0,0,0)

Giorgia<-as.numeric(Giorgia)
Giulia<-as.numeric(Giulia)

#elimino la variabile ALT
colnames(dati_tst)
dati_tst2<-dati_tst2[,-21]
#tolgo waist_cm
dati_tst2<-dati_tst2[,-5]
dati_tst2<-rbind(dati_tst2,Giorgia,Giulia)
#PRESSIONE ALTA DECIDO DI ELIMINARE I VALORI MAGGIORI DI 200
dati_tst2<-dati_tst2[-which(dati_tst2$systolic>200),]

#FASTING BLOOD SUGAR
dati_tst2<-dati_tst2[-which(dati_tst2$fasting.blood.sugar>400),]

#CHOLESTEROL
dati_tst2<-dati_tst2[-which(dati_tst2$Cholesterol>400),]

#SERUM CREATININA
dati_tst2<-dati_tst2[-which(dati_tst2$serum.creatinine>4),]

#AST
dati_tst2<-dati_tst2[-which(dati_tst2$AST>300),]

#LDL 
dati_tst2<-dati_tst2[-which(dati_tst2$LDL>500),]
#HDL
#dati_tst<-dati_tst[-which(dati_tst$HDL>300),]
#TRIGLICERIDI
dati_tst2<-dati_tst2[-which(dati_tst2$triglyceride>400),]


#DOPODICHE SVOLTO IL LOGARITMO DI ALCUNI CHE HANNO ANCORA OUTLIER
dati_tst2$Gtp<-log(dati_tst2$Gtp)

dati_tst2$AST<-log(dati_tst2$AST)

# con triglicerici
dati_tst2$triglyceride<-log(dati_tst2$triglyceride)
boxplot.stats(training$triglyceride)

#STANDARDIZZAZIONE
nf<-c(2:6,9:16,18:20)
for(t in nf){ #dati normalizzati
  dati_tst2[,t]<-(dati_tst2[,t]-mediec[t])/(sddc[t])
}

pred_logit <- predict(step.model, dati_tst2[11142, -23], type = "response")
pred_logit_class <- ifelse (pred_logit > 0.5, 1, 0) 
dati_tst2[11123,21]<-"1"
dati_tst2[11123,1]<-"F"
dati_tst2[11123,8]<-"1"
dati_tst2[11123,7]<-"1"
dati_tst2[11123,17]<-"1"
#GIORGIA 0.15 NON FUMA
#GIULIA 0.099415 NON FUMA 

#-----
#PROVE PER ULTERIORI RISULTATI

 #prova con LDA con variabili normali
lda_model<-lda(smoking~Cholesterol+LDL+HDL+AST, data=dati_smaller_trn)
dati_vld$smoking<-as.numeric(dati_vld$smoking)-1
pred_lda<-predict(lda_model, dati_vld[,-23])
confusionMatrix(factor(dati_vld[,23]), pred_lda$class)

#PCA
train.pca <- prcomp(x_dati_small, center = TRUE, scale. = TRUE)
names(train.pca)
install.packages("factoextra")
library(factoextra)
fviz_eig(train.pca)
summary(train.pca)

train.pca.x <- train.pca$x[, 1:11]
train.new <- cbind(train.pca.x, y_dati_small)
train.new <- as.data.frame(trian.new)
train.new$y_dati_small<- train.new$y_dati_small-1

#regressione logistica 
model_glm<-glm(y_dati_small~., data=train.new, family="binomial")
summary(model_glm)
#34192

step.model <- stepAIC(model_glm, direction = "both", trace = FALSE) #AIC 
summary(step.model)
#34190
par(mfrow=c(1,1))
influencePlot(model_glm)

#punti influenti 
which(row.names(train.new)==30075) #1204
which(row.names(train.new)==48935)  #2477
which(row.names(train.new)==38709) #3592
which(row.names(train.new)==15425) #7348
which(row.names(train.new)==31258)#7758
which(row.names(train.new)==44105) #28079


influenti<-c(28079,7758,7348,3592,2477,1204)
trn_nuovo<-train.new[-influenti,]

model_glm2= glm(y_dati_small~. , data = trn_nuovo, 
                family = "binomial")
summary(model_glm2) 
step.model <- stepAIC(model_glm2, direction = "both", trace = FALSE) #AIC 76
summary(step.model) 


#pca su vld
#vld.x  <- as.data.frame(vld.x)
vld.pca.x <- predict(train.pca, x_dati_vld)
vld.pca.x <- vld.pca.x[, 1:11]
vld.pca.x <- as.data.frame(vld.pca.x)

pred_logit <- predict(step.model, vld.pca.x, type = "response")
pred_logit_class <- ifelse (pred_logit > 0.5, 1, 0) 
calc_class_err = function(actual, predicted) { mean(actual != predicted)
}

err_vld <- calc_class_err(actual = dati_vld$smoking, predicted = pred_logit_class)
#0.288
table("predicted" = pred_logit_class,
      "actual" = y_dati_vld)
acc_log_vld<- sum(diag(table(y_dati_vld, pred_logit_class)))/length(pred_logit_class)
acc_log_vld #71,19%

pred_roclogit <- prediction(pred_logit, y_dati_vld)
perf_logit <- performance(pred_roclogit,"tpr","fpr")
auc_logit <- performance(pred_roclogit, measure = "auc")@y.values
auc_logit #0.79

library(randomForest)
training$smoking<-as.factor(training$smoking)
rf <- randomForest(smoking~., data=training,importance=TRUE, ntree=500) 

p1 <- predict(rf, training)
confusionMatrix(p1, training$smoking)
dati_tst$smoking<-as.factor(dati_tst$smoking)
p2<-predict(rf, dati_tst2)
confusionMatrix(p2, dati_tst$smoking)

imp <- importance(rf)
options(repr.plot.width = 14, repr.plot.height = 10)
varImpPlot(rf, main="Variable Importance")

# Creating a dataframe of variable importance metrics
df_imp<- data.frame(variable=row.names(imp),
                    importance = imp[,'MeanDecreaseGini'],
                    accuracy= imp[,'MeanDecreaseAccuracy'])
noquote("Variable Importance Dataframe")
df_imp
# Visualizations of importance variables
options(repr.plot.width = 14, repr.plot.height = 10)
ggplot(df_imp, aes(x=reorder(variable,-importance),y=importance, fill=importance))+
  geom_col()+
  labs(x="Predictors", y ="Importance", title="Importance of predictor variables")+
  theme(plot.title=element_text(face="bold",hjust=0.5), legend.position = "bottom", axis.text.x = element_text(angle=45), text=element_text(size=18))+
  scale_fill_continuous(type="viridis")

#grafico hdl

options(repr.plot.width = 16, repr.plot.height = 12)
ggplot(subset(df,df$fasting.blood.sugar<300), aes(x=HDL, fill=as.factor(smoking)))+
  geom_bar(alpha=0.8)+
  labs(title="HDL and Smoking", fill="smoking")+
  theme(legend.position = "bottom", plot.title = element_text(face="bold", hjust=0.5), text=element_text(size=18))+
  scale_x_continuous(breaks=seq(0,200,20))+
  scale_fill_manual(labels=c("0","1"),values=c("aquamarine2","deeppink"))+
  geom_vline(aes(xintercept=mean(df$HDL)))
