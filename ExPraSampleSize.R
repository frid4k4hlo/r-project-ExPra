install.packages("pwr")
library(pwr)

#Variante mit halbierter Effektgrösse des ursprünglichen Effekts:

pwr.chisq.test(w = 0.195, N = NULL, df = 4, sig.level = 0.05, power = 0.8)

#Variante mit der in der Originalstudie berichteten Effektgrösse

pwr.chisq.test(w = 0.39, N = NULL, df = 4, sig.level = 0.05, power = 0.8)

#Variante mit halbierter Effektgrösse des ursprünglichen Effekts:

pwr.chisq.test(w = 0.195, N = NULL, df = 7, sig.level = 0.05, power = 0.8)

#Variante mit der in der Originalstudie berichteten Effektgrösse

pwr.chisq.test(w = 0.39, N = NULL, df = 7, sig.level = 0.05, power = 0.8)

#Hennes het au mit 7 grechnet als df - also ischer fo 8 Antwortalternative usgange und da mier 8 und 5 hend sind df 4 und 7 

