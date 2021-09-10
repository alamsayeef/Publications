library(tidyverse)
library(glmnet)
library(riskRegression)
library(magrittr)
library(survminer)
library(ggplot2)
library(stargazer)
library(tab)

gd = read.csv(file.choose())
cd = read.csv(file.choose())

#transpose of gene data
tgd = setNames(data.frame(t(gd[,-1])),gd[,1])

#clinical data cleaning
cdc = select(cd, -c(X,X.Sample_characteristics_ch1,X.Sample_characteristics_ch1.14))
row.names(cdc) = cdc$ID
cdc = select(cdc,-c(ID))
cdc = cdc %>% rename(Gender = X.Sample_characteristics_ch1.1, 
                           Age = X.Sample_characteristics_ch1.2,
                           Ethnicity = X.Sample_characteristics_ch1.3, 
                           Vital_Status = X.Sample_characteristics_ch1.4,
                           Adjuvant_Chemo = X.Sample_characteristics_ch1.5, 
                           Adjuvant_RT = X.Sample_characteristics_ch1.6,
                           Disease_stage = X.Sample_characteristics_ch1.7, 
                           Relapse = X.Sample_characteristics_ch1.8,
                           PFS = X.Sample_characteristics_ch1.9, 
                           Months_to_last_cli_assess = X.Sample_characteristics_ch1.10,
                           OS =  X.Sample_characteristics_ch1.11, 
                           Smoking_history = X.Sample_characteristics_ch1.12,
                           Surgical_Margin = X.Sample_characteristics_ch1.13, 
                           Histological_grade =X.Sample_characteristics_ch1.15)

cdc = cdc %>% separate(Gender,c("Old1","Gender"),": ")
cdc = cdc %>% separate(Age,c("Old2","Age"),": ")
cdc = cdc %>% separate(Ethnicity,c("Old3","Ethnicity"),": ")
cdc = cdc %>% separate(Vital_Status,c("Old4","Vital_Status"),": ")
cdc = cdc %>% separate(Adjuvant_Chemo,c("Old5","Adjuvant_Chemo"),": ")
cdc = cdc %>% separate(Adjuvant_RT,c("Old6","Adjuvant_RT"),": ")
cdc = cdc %>% separate(Disease_stage,c("Old7","Disease_stage"),": ")
cdc = cdc %>% separate(Relapse,c("Old8","Relapse"),": ")
cdc = cdc %>% separate(PFS,c("Old9","PFS"),": ")
cdc = cdc %>% separate(Months_to_last_cli_assess,c("Old10","Months_to_last_cli_assess"),": ")
cdc = cdc %>% separate(OS,c("Old11","OS"),": ")
cdc = cdc %>% separate(Smoking_history,c("Old12","Smoking_history"),": ")
cdc = cdc %>% separate(Surgical_Margin,c("Old13","Surgical_Margin"),": ")
cdc = cdc %>% separate(Histological_grade,c("Old14","Histological_grade"),": ")
cdc = select(cdc,-c(Old1,Old2,Old3,Old4,Old5,Old6,Old7,Old8,Old9,Old10,Old11,Old12,Old13,Old14,Months_to_last_cli_assess))

cdc = cdc %>% mutate(Relapse, Relapse = ifelse(Relapse != "Yes","No","Yes"))
cdc = cdc %>% mutate(Smoking_history, Smoking_history = ifelse(Smoking_history == "--","Never smoked",ifelse(Smoking_history == "Unknown","Never smoked",Smoking_history)))
cdc = cdc %>% mutate(Surgical_Margin, Surgical_Margin = ifelse(Surgical_Margin == "--","Unknown",Surgical_Margin))
cdc = cdc %>% mutate(Histological_grade, Histological_grade = ifelse(Histological_grade == "--","Unknown",Histological_grade))
cdc = cdc %>% mutate(OS, OS = ifelse(OS == "--" | OS == "na",204.00,OS))
cdc = cdc %>% mutate(PFS, PFS = ifelse(PFS == "--" | PFS == "na",OS,PFS))

cdc = cdc %>% mutate_at(vars(Age, PFS, OS), as.numeric)
cdc = cdc %>% mutate_at(vars(Gender, Ethnicity, Vital_Status, Adjuvant_Chemo, Adjuvant_RT, Disease_stage, Relapse, Smoking_history, Surgical_Margin, Histological_grade), as.factor)
sapply(cdc, function(x) sum(is.na(x)))
summary(cdc)

#gene data cleaning
rownames(tgd) = 1:nrow(tgd)

#merge two datasets
tgd$ID = 1:nrow(tgd)
cdc$ID = 1:nrow(cdc)

md = merge(cdc,tgd,by = "ID")

#death from/ competing event variable
md$Event = NA
md$Event[md$Relapse == "No" & md$Vital_Status == "Alive" ] ="Alive"
md$Event[md$Relapse == "No" & md$Vital_Status == "Dead" ] ="Competing Risk"
md$Event[md$Relapse == "Yes" & md$Vital_Status == "Alive" ] ="Alive"
md$Event[md$Relapse == "Yes" & md$Vital_Status == "Dead" ] ="Cancer"

md$Event = as.factor(md$Event)

#subsetting patients dying from lung adenocarcinoma

md$Status[md$Relapse == "No"] = 0 
md$Status[md$Relapse == "Yes"] = 1 

md$Status2[md$Vital_Status == "Alive"] = 0 
md$Status2[md$Vital_Status == "Dead"] = 1 

cp = subset(md, Event != "Competing Risk")

#survival object
library(survival)
sobj = Surv(cp$OS, cp$Status2)

#for replication purposes
nFolds = 20
gmat = as.matrix(cp[,15:22215])
foldid = sample(rep(seq(nFolds), length.out = nrow(gmat)))
                
#cox lasso regression
lr1 = cv.glmnet(gmat, sobj, alpha=1, family = "cox", foldid = foldid)

pdf(file = "/Users/mdsayeefalam/Desktop/Reanalysis of VS/lr1.1.pdf", width = 8, height = 5) 
plot(lr1)
dev.off()

pdf(file = "/Users/mdsayeefalam/Desktop/Reanalysis of VS/lr1.2.pdf", width = 8, height = 5)
plot(lr1$glmnet.fit, "lambda", label = T)
dev.off()

pdf(file = "/Users/mdsayeefalam/Desktop/Reanalysis of VS/lr1.3.pdf", width = 8, height = 5) 
plot(lr1$glmnet.fit, "norm", label = T)
dev.off()

lr1$lambda.min
log(lr1$lambda.min)
lr1c = coef(lr1, s = lr1$lambda.min)
lr1lam = which(lr1c!=0)
lr1lam
lr1lamval = lr1c[lr1lam]
lr1lamval

rownames(coef(lr1, s = lr1$lambda.min))[coef(lr1, s = lr1$lambda.min)[,1]!= 0]

#cox elastic-net regression
er1 = cv.glmnet(gmat, sobj, alpha=0.5, family = "cox", foldid = foldid)

pdf(file = "/Users/mdsayeefalam/Desktop/Reanalysis of VS/er1.1.pdf", width = 8, height = 5) 
plot(er1)
dev.off()

pdf(file = "/Users/mdsayeefalam/Desktop/Reanalysis of VS/er1.2.pdf", width = 8, height = 5)
plot(er1$glmnet.fit, "lambda", label = T)
dev.off()

pdf(file = "/Users/mdsayeefalam/Desktop/Reanalysis of VS/er1.3.pdf", width = 8, height = 5) 
plot(er1$glmnet.fit, "norm", label = T)
dev.off()

er1$lambda.min
log(er1$lambda.min)
er1c = coef(er1, s = er1$lambda.min)
er1lam = which(er1c!=0)
er1lam
er1lamval = er1c[er1lam]
er1lamval

mdlr = as.data.frame(tgd[,lr1lam])
mdlr = cbind(md[,c(11,22232)],mdlr)

mder = as.data.frame(tgd[,er1lam])
mder = cbind(md[,c(11,22232)],mder)

temp = cp[,15:22229]
cplr = temp[,lr1lam]
cplr = cbind(cp[,c(11,22232)],cplr)
f1 = coxph(Surv(OS,Status2)~.,data=cplr,x=TRUE)

cper = temp[,er1lam]
cper = cbind(cp[,c(11,22232)],cper)
f2 = coxph(Surv(OS,Status2)~.,data=cper,x=TRUE)

f3 = coxph(Surv(OS,Status2)~.,data=cper,x=TRUE)

xscore = Score(list("Lasso" = f1),formula=Surv(OS,Status2)~1,data=mdlr,times=0:110, metrics="brier",conf.int = T,null.model = F)
pdf(file = "/Users/mdsayeefalam/Desktop/Reanalysis of VS/bp1.pdf", width = 8, height = 5)
plotBrier(xscore)
dev.off()

xscore1 = Score(list("ENet" = f2),formula=Surv(OS,Status2)~1,data=mder,times=0:110, metrics="brier",conf.int = T,null.model = F)
pdf(file = "/Users/mdsayeefalam/Desktop/Reanalysis of VS/bp2.pdf", width = 8, height = 5)
plotBrier(xscore1)
dev.off()

xscore2 = Score(list("ENet + Lasso" = f2),formula=Surv(OS,Status2)~1,data=mder,times=0:110, metrics="brier",conf.int = T,null.model = F)
pdf(file = "/Users/mdsayeefalam/Desktop/Reanalysis of VS/bp3.pdf", width = 8, height = 5)
plotBrier(xscore2)
dev.off()

mean(xscore$Brier$score$Brier)
mean(xscore$Brier$score$lower)
mean(xscore$Brier$score$upper)

mean(xscore1$Brier$score$Brier)
mean(xscore1$Brier$score$lower)
mean(xscore1$Brier$score$upper)

mean(xscore2$Brier$score$Brier)
mean(xscore2$Brier$score$lower)
mean(xscore2$Brier$score$upper)

names(cplr)[3:27] = c("GALNT3","COCH","ZC2HC1A","GAPDHS","CCL16",
                       "XPNPEP1","BECN1","ABAT","SWAP70","PSIP1",
                       "MRPL9","RCAN1","ITGB1","IL23A","PIAS1",
                       "PDXK","HILPDA","AFF4","LRFN4","RAB11FIP1",
                       "TESMIN","DAB1","GCM2","FAM117A","H2AW")

names(cper)[3:44] = c("GALNT3","STC2","STCRP","TERF1","WDHD1",
                       "COCH","ZC2HC1A","GAPDHS","CCL16","RAX",
                       "XPNPEP1","BECN1","ABAT","SWAP70","PSIP1",
                       "MRPL9","ARHGEF4","NUS1","RCAN1","MED24",
                       "ARFGEF2","MIA2","ITGB1","C1orf68","IL23A",
                       "PIAS1","PDXK","HILPDA","WDCP","AFF4",
                       "LRFN4","RAB11FIP1","PRRX2","TESMIN","PAQR5",
                       "DAB1","GCM2","FAM117A","MLNR","LEF1",
                       "H2AW","GALR3")

library(qgraph)
library(psych)
CorMat1 = cor_auto(cplr[,3:27])
Graph_pcor1 = qgraph(CorMat1,graph = "pcor",layout = "spring")

pdf(file = "/Users/mdsayeefalam/Desktop/Reanalysis of VS/cp1.pdf", width = 8, height = 5)
qgraph(CorMat1,graph = "pcor",layout = "spring")
dev.off()

CorMat2 = cor_auto(cper[,3:44])
Graph_pcor2 = qgraph(CorMat2,graph = "pcor",layout = "spring")

pdf(file = "/Users/mdsayeefalam/Desktop/Reanalysis of VS/cp2.pdf", width = 8, height = 5)
qgraph(CorMat2,graph = "pcor",layout = "spring")
dev.off()

cph1 = coxph(Surv(OS, Status2)~., data = cplr)
summary(cph1)

cph2 = coxph(Surv(OS, Status2)~., data = cper)
summary(cph2)

pdf(file = "/Users/mdsayeefalam/Desktop/Reanalysis of VS/fp1.pdf", width = 15, height = 12)
ggforest(cph1, 
         data = cplr, 
         main = "Adjusted hazard ratio",
         cpositions = c(0.01, 0.15, 0.35),
         fontsize = 0.8,
         refLabel = "reference",
         noDigits = 4)
dev.off()

pdf(file = "/Users/mdsayeefalam/Desktop/Reanalysis of VS/fp2.pdf", width = 15, height = 12)
ggforest(cph2, 
         data = cper, 
         main = "Adjusted hazard ratio",
         cpositions = c(0.01, 0.15, 0.35),
         fontsize = 0.8,
         refLabel = "reference",
         noDigits = 4)
dev.off()

library(broom)
write.csv(tabcoxph(cph1, decimals = 4),"/Users/mdsayeefalam/Desktop/Reanalysis of VS/cplr.csv")
write.csv(tabcoxph(cph2, decimals = 4),"/Users/mdsayeefalam/Desktop/Reanalysis of VS/cper.csv")
