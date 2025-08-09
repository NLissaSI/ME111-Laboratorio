# Fixa os números gerados aleatoriamente
set.seed()

# barplot(height, names.arg = NULL)  # names.arg colocar o nome 
# legend () # legenda


## Questão 1
set.seed(1000) 
Cha_Primeiro_Acertos_1 = sample(0:10, size = 1000, replace = TRUE)
hist(Cha_Primeiro_Acertos_1,las = 1, ylab = "Frequência", ylim = c(0,200), col = "lightblue")

table(Cha_Primeiro_Acertos_1)
barplot(table(Cha_Primeiro_Acertos_1), las = 1, ylab = "Distribuição", xlab = "Chá Primeiro (Acertos)", col = "lightyellow")
fisher.test(B, y = NULL)

rhyper(1, 4, 4, 4) # 4 Chá primeiro, 4 Leite primeiro, escolheu 4 xícaras
set.seed(1)
Simulação =rhyper(1000, 10, 10, 10)
Cha_Primeiro_Acertos= table(Simulação)
barplot(Cha_Primeiro_Acertos, las = 1, 
        ylab = "Distribuição", xlab = "Chá Primeiro (Acertos)", 
        col = "lightyellow")



TeaTasting = matrix(c(6,4,4,6), nrow = 2,
       dimnames = list(Guess = c("Tea", "Milk"), # Nome das Linhas
                       Truth = c("Tea","Milk"))) # Nome das Colunas
fisher.test(TeaTasting, alternative = "greater")
# O texte deve ser feito com uma matriz         

Teste = matrix(c(6,4,4,6), nrow = 2,
       dimnames = list(c("Tea", "Milk"),
                       c("Tea","Milk")))



## Questão 3
Skittle = matrix(c(18, 9, 17, 15, 11, 13, 16, 19, 34, 8), 
                 ncol = 5, byrow = TRUE)

TesteSkittle = chisq.test(Skittle)
names(TesteSkittle)
TesteSkittle$p.value
TesteSkittle$observed
TesteSkittle$expected
TesteSkittle$statistic


## Questão 4
Prom = matrix(c(21, 3, 13, 11),
              ncol= 2, byrow = TRUE)
TesteProm = chisq.test(Prom)
names(TesteProm)
TesteProm$statistic
TesteProm$expected


## Questão 5
library(xlsx)
ME111Prob5Respostas = read.xlsx("ME111Prob5Respostas.xlsx", sheetIndex = 1) # Dados da parte 5 da lista

table(ME111Prob5Respostas$Genero)
table(ME111Prob5Respostas$MaoEscreve)
table(ME111Prob5Respostas$PeMaior)
table(ME111Prob5Respostas$MaoMaior)

summary(table(ME111Prob5Respostas$Genero) ~ table(ME111Prob5Respostas$MaoEscreve))
summary(ME111Prob5Respostas)


# Linhas (Pés) X Colunas (Mãos)
table(ME111Prob5Respostas$PeMaior, ME111Prob5Respostas$MaoMaior) 
Pés_e_Mãos = matrix(c(24,5,3,8,0,4,11,3,7), 
                    ncol = 3, byrow = TRUE)
chisq.test(Pés_e_Mãos)


#Linhas (Mão que escreve) X Colunas (Pés)
table(ME111Prob5Respostas$MaoEscreve, ME111Prob5Respostas$PeMaior) 
MãosD_e_Pés = matrix(c(1,0,2,31,12,19),
                    ncol = 3, byrow = TRUE)
chisq.test(MãosD_e_Pés)


# Linhas (Genero) X Colunas (Pés)
table(ME111Prob5Respostas$Genero, ME111Prob5Respostas$PeMaior) 
Gênero_e_Pés = matrix(c(10,5,10,22,7,11),
                      ncol=3, byrow = TRUE)
chisq.test(Gênero_e_Pés)


# Linhas (Genero) X Colunas (Mão que escreve)
table(ME111Prob5Respostas$Genero, ME111Prob5Respostas$MaoEscreve) 
Gênero_e_MãoD = matrix(c(0,25,3,37),
                       ncol = 2, byrow = TRUE)
Teste_Ind_4 = chisq.test(Gênero_e_MãoD, correct = FALSE)
names(chisq.test(Gênero_e_MãoD))
Teste_Ind_4$expected
Teste_Ind_4$statistic
