#import data
library(readxl)
d = read_excel("/Users/mdsayeefalam/Documents/Working Papers/NRS/COVID SHEET final (1).xlsx", sheet = "mdata")

#categorising into alteration of TFTs
library(tidyverse)
d = d %>% mutate(abTSH = ifelse(TSH< 0.35, "Low", ifelse(TSH > 4.6, "Elevated", "Normal")))
d = d %>% mutate(abft3 = ifelse(fT3< 2.30, "Low", ifelse(fT3 > 4.20, "Elevated", "Normal")))
d = d %>% mutate(abtt3 = ifelse(TT3< 0.81, "Low", ifelse(TT3 > 1.87, "Elevated", "Normal")))
d = d %>% mutate(abft4 = ifelse(fT4< 0.89, "Low", ifelse(fT4 > 1.80, "Elevated", "Normal")))
d = d %>% mutate(abtt4 = ifelse(TT4< 4.66, "Low", ifelse(TT4 > 11.5, "Elevated", "Normal")))

#subsetting data according to severity
mildpt = subset(d, d$Severity == "Mild")
modpt = subset(d, d$Severity == "Moderate")
sevpt = subset(d, d$Severity == "Severe")

lowpt = subset(d, d$abft4 != "Elevated")
elevpt = subset(d, d$abft4 != "Low")

#check normality for TFT variables
shapiro.test(d$TSH)
shapiro.test(d$fT3)
shapiro.test(d$TT3)
shapiro.test(d$fT4)
shapiro.test(d$TT4)
shapiro.test(d$Ddimer)
shapiro.test(d$Ferritin)

#graphs
library(ggplot2)
library(gridExtra)
#kernel density plot together
p1 = ggplot(d, aes(x = TT3, color = Severity)) + geom_density()+labs(x = "TT3 (ng/ml)", y = "Kernel Density Estimation", title = "Distribution of TT3") + theme_minimal()
p2 = ggplot(d, aes(x = fT3, color = Severity)) + geom_density()+labs(x = "fT3 (pg/ml)", y = "Kernel Density Estimation", title = "Distribution of fT3") + theme_minimal()
p3 = ggplot(d, aes(x = TT4, color = Severity)) + geom_density()+labs(x = "TT4 (microg/dl)", y = "Kernel Density Estimation", title = "Distribution of TT4") + theme_minimal()
p4 = ggplot(d, aes(x = fT4, color = Severity)) + geom_density()+labs(x = "fT4 (ng/ml)", y = "Kernel Density Estimation", title = "Distribution of fT4") + theme_minimal()
p5 = ggplot(d, aes(x = TSH, color = Severity)) + geom_density()+labs(x = "Thyroid stimulating hormone (mIU/L)", y = "Kernel Density Estimation", title = "Distribution of serum thyroid stimulating hormone") + theme_minimal()

grid.arrange(p1, p2, p3, p4, p5, nrow = 3, ncol = 2)

#boxplot
bp1 = ggplot(d) +
  aes(x = "", y = Ddimer, fill = Severity, group = Severity) +
  geom_boxplot() +
  scale_fill_hue() +
  labs(x = " ", y = "D-Dimer (gm/ml)") +
  theme_minimal() +
  theme(legend.position = "top")

bp2 = ggplot(d) +
  aes(x = "", y = Ferritin, fill = Severity, group = Severity) +
  geom_boxplot() +
  scale_fill_hue() +
  labs(x = "", y = "Ferritin (ng/dl)") +
  theme_minimal() +
  theme(legend.position = "top")

bp3 = ggplot(d) +
  aes(x = "", y = TT3, fill = Severity, group = Severity) +
  geom_boxplot() +
  scale_fill_hue() +
  labs(x = " ", y = "TT3 (ng/ml)") +
  theme_minimal() +
  theme(legend.position = "top")

bp4 = ggplot(d) +
  aes(x = "", y = fT3, fill = Severity, group = Severity) +
  geom_boxplot() +
  scale_fill_hue() +
  labs(x = "", y = "fT3 (pg/ml)") + #, title = "Box Plot") +
  theme_minimal() +
  theme(legend.position = "top")

bp5 = ggplot(d) +
  aes(x = "", y = TT4, fill = Severity, group = Severity) +
  geom_boxplot() +
  scale_fill_hue() +
  labs(x = " ", y =  "TT4 (microg/dl)") +
  theme_minimal() +
  theme(legend.position = "top")

bp6 = ggplot(d) +
  aes(x = "", y = fT4, fill = Severity, group = Severity) +
  geom_boxplot() +
  scale_fill_hue() +
  labs(x = "", y = "fT4 (ng/ml)") +
  theme_minimal() +
  theme(legend.position = "top")

bp7 = ggplot(d) +
  aes(x = "", y = TSH, fill = Severity, group = Severity) +
  geom_boxplot() +
  scale_fill_hue() +
  labs(x = " ", y = "TSH (mIU/L)") +
  theme_minimal() +
  theme(legend.position = "top")

grid.arrange(bp1, bp2, bp3, bp4, bp5, bp6, bp7, nrow = 3, ncol = 3)

#q1,q2 and q3 of TFT according to severity
library(qwraps2)
attach(mildpt)
median_iqr(TSH, na_rm = T)
median_iqr(fT3, na_rm = T)
median_iqr(TT3, na_rm = T)
median_iqr(fT4, na_rm = T)
median_iqr(TT4, na_rm = T)
median_iqr(Ferritin, na_rm = T)
median_iqr(Ddimer, na_rm = T)
detach(mildpt)

attach(modpt)
median_iqr(TSH, na_rm = T)
median_iqr(fT3, na_rm = T)
median_iqr(TT3, na_rm = T)
median_iqr(fT4, na_rm = T)
median_iqr(TT4, na_rm = T)
median_iqr(Ferritin, na_rm = T)
median_iqr(Ddimer, na_rm = T)
detach(modpt)

attach(sevpt)
median_iqr(TSH, na_rm = T)
median_iqr(fT3, na_rm = T)
median_iqr(TT3, na_rm = T)
median_iqr(fT4, na_rm = T)
median_iqr(TT4, na_rm = T)
median_iqr(Ferritin, na_rm = T)
median_iqr(Ddimer, na_rm = T)
detach(sevpt)

#frequency tables
summarytools::freq(d$Sex)
summarytools::freq(d$abTSH)
summarytools::freq(d$abft3)
summarytools::freq(d$abtt3)
summarytools::freq(d$abft4)
summarytools::freq(d$abtt4)
summarytools::freq(d$Abnrml_Ddimer)
summarytools::freq(d$Abnrml_Fer)
summarytools::freq(d$AntiTPOPositive)

summarytools::freq(mildpt$Sex)
summarytools::freq(mildpt$abTSH)
summarytools::freq(mildpt$abft3)
summarytools::freq(mildpt$abtt3)
summarytools::freq(mildpt$abft4)
summarytools::freq(mildpt$abtt4)
summarytools::freq(mildpt$Abnrml_Ddimer)
summarytools::freq(mildpt$Abnrml_Fer)
summarytools::freq(mildpt$AntiTPOPositive)

summarytools::freq(modpt$Sex)
summarytools::freq(modpt$abTSH)
summarytools::freq(modpt$abft3)
summarytools::freq(modpt$abtt3)
summarytools::freq(modpt$abft4)
summarytools::freq(modpt$abtt4)
summarytools::freq(modpt$Abnrml_Ddimer)
summarytools::freq(modpt$Abnrml_Fer)
summarytools::freq(modpt$AntiTPOPositive)

summarytools::freq(sevpt$Sex)
summarytools::freq(sevpt$abTSH)
summarytools::freq(sevpt$abft3)
summarytools::freq(sevpt$abtt3)
summarytools::freq(sevpt$abft4)
summarytools::freq(sevpt$abtt4)
summarytools::freq(sevpt$Abnrml_Ddimer)
summarytools::freq(sevpt$Abnrml_Fer)
summarytools::freq(sevpt$AntiTPOPositive)

#crosstabs
summarytools::ctable(d$abTSH,d$Severity)
summarytools::ctable(d$abft3,d$Severity)
summarytools::ctable(d$abtt3,d$Severity)
summarytools::ctable(d$abft4,d$Severity)
summarytools::ctable(d$abtt4,d$Severity)
summarytools::ctable(d$Abnrml_Ddimer,d$Severity, useNA = "no")
summarytools::ctable(d$Abnrml_Fer,d$Severity, useNA = "no")
summarytools::ctable(d$AntiTPOPositive,d$Severity)

#anova and kruskal wallis test for difference among groups
anv4 = aov(TT4 ~ Severity , data = d)
summary(anv4)

anv1 = kruskal.test(TSH ~ Severity, data = d)
anv1
anv2 = kruskal.test(fT3 ~ Severity, data = d)
anv2 
anv3 = kruskal.test(TT3 ~ Severity, data = d)
anv3
anv5 = kruskal.test(fT4 ~ Severity, data = d)
anv5
anv6 = kruskal.test(Ddimer ~ Severity, data = d)
anv6
anv7 = kruskal.test(Ferritin ~ Severity, data = d)
anv7

#test for indepence
table1 = table(d$abTSH,d$Severity)
table2 = table(d$abft3,d$Severity)
table3 = table(d$abtt3,d$Severity)
table4 = table(d$abft4,d$Severity)
table5 = table(d$abtt4,d$Severity)
table6 = table(d$Abnrml_Ddimer,d$Severity)
table7 = table(d$Abnrml_Fer,d$Severity)

tblele = table(elevpt$abft4,elevpt$Severity)
tbllow = table(lowpt$abft4,lowpt$Severity)

fisher.test(table1)
fisher.test(table2)
fisher.test(table3)
fisher.test(table4)
fisher.test(table5)
fisher.test(table6)
fisher.test(table7)
fisher.test(tblele)
fisher.test(tbllow)