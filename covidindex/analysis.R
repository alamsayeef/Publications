install.packages("readxl")
install.packages("car")

library(readxl)
final = read_excel(file.choose(),sheet = "final")

library(car)
qqPlot(final$ts_dl)
shapiro.test(final$ts_dl)

qqPlot(final$ts_bl)
shapiro.test(final$ts_bl)

wilcox.test(final$ts_dl,final$ts_bl,alternative = "greater",conf.int = T)
boxplot(final$ts_dl,final$ts_bl)


