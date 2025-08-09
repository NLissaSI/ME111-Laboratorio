source("http://www.openintro.org/stat/data/cdc.R")
names(cdc)
summary(cdc)
summary(cdc$height); summary(cdc$age)
table(cdc$gender)
table(cdc$gender)/20000
table(cdc$exerany)
table(cdc$exerany)/20000

table(cdc$gender, cdc$smoke100) #linhas = gênero; colunas = fumaram pelo menos 100 (1) ou não (0)
mosaicplot(table(cdc$gender, cdc$smoke100), color = TRUE)

cdc$age > 30
cdc$age < 30 
table(cdc$age < 30) #TRUE = pessoas com menos de 30 anos
table(cdc$age < 23) #TRUE = pessoas com menos de 23 anos

subset(cdc, cdc$smoke100 & cdc$age < 23) # & = e; pessoas com menos de 23 e que fumaram 100 cigarros (?)
abaixo23_e_fuma <- subset(cdc, cdc$smoke100 == 1 & cdc$age < 23)

subset(cdc, cdc$smoke100 | cdc$age < 23) # | = ou; pessoas com menos de 23 ou que fumaram 100 cigarros
table(cdc$age < 23)

boxplot(cdc$height)
boxplot(cdc$height ~ cdc$gender)

Saude <- cdc$genhlth
IMC <- (cdc$weight / cdc$height^2) * 703 
boxplot(IMC ~ Saude)

Genero <- cdc$gender
boxplot(IMC ~ Genero)    

Plano_de_saude <- cdc$hlthplan
boxplot(IMC ~ Plano_de_saude)

Idade <- cdc$age

boxplot(IMC ~ Idade)

