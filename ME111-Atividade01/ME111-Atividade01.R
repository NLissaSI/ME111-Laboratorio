# Parte 1 ----
#Primeira jogada
moeda = sample(0:1, size = 10, replace = TRUE)
Contagem = table(moeda)

#Segunda jogada
moeda1 = sample(0:1, size = 20, replace = TRUE)
Contagem1 = table(moeda1)


plot(table(moeda), type = "h")
hist(table(moeda), freq = TRUE)
barplot(table(moeda)) #...


#Terceira jogada
moeda2 = sample(0:1, size = 50, replace = TRUE)
Contagem2 = table(moeda2)

#Quarta jogada
moeda3 = sample(0:1, size = 100, replace = TRUE)
Contagem3 = table(moeda3)

#Quinta jogada
moeda4 = sample(0:1, size = 500, replace = TRUE)
Contagem4 = table(moeda4)

#Sexta jogada
moeda5 = sample(0:1, size = 1000, replace = TRUE)
Contagem5 = table(moeda5)

#Setima jogada
moeda6 = sample(0:1, size = 10000, replace = TRUE)
Contagem6 = table(moeda6)

barplot(Contagem3,las = 1, ylab = "Frequência")


Tabela =
rbind(
  cbind(length(moeda),sum(moeda),t(table(moeda)/length(moeda))),
  cbind(length(moeda1),sum(moeda1),t(table(moeda1)/length(moeda1))),
  cbind(length(moeda2),sum(moeda2),t(table(moeda2)/length(moeda2))), 
  cbind(length(moeda3),sum(moeda3),t(table(moeda3)/length(moeda3))),
  cbind(length(moeda4),sum(moeda4),t(table(moeda4)/length(moeda4))),
  cbind(length(moeda5),sum(moeda5),t(table(moeda5)/length(moeda5))),
  cbind(length(moeda6),sum(moeda6),t(table(moeda6)/length(moeda6)))
)

colnames(Tabela) = c("n","# caras","prop coroa","prop cara")

Tabela

summary(moeda6)
boxplot(moeda)

barplot(Contagem3,las = 1, ylab = "Frequência")
barplot(Contagem6,las = 1, ylab = "Frequência", col = "darkblue")
 ?barplot

barplot(Contagem3/sum(Contagem3),las = 1, ylab = "Proporção")
#Linha no gráfico
abline(h = 0.5, col = "red",  lty = 2)
barplot(Contagem6/sum(Contagem6),las = 1, ylab = "Proporção", col = "darkblue")
abline(h = 0.5, col = "red",  lty = 2)


# Parte 2 ----
#Lançamento 1
dado1 = sample(1:6, size = 10, replace = TRUE)
table(dado1)

#Lançamento 2
dado2 = sample(1:6, size = 30, replace = TRUE)
table(dado2)

#Lançamento 3
dado3 = sample(1:6, size = 50, replace = TRUE)
table(dado3)

#Lançamento 4
dado4 = sample(1:6, size = 100, replace = TRUE)
table(dado4)

#Lançamento 5
dado5 = sample(1:6, size = 600, replace = TRUE)
table(dado5)

#Lançamento 6
dado6 = sample(1:6, size = 1000, replace = TRUE)
table(dado6)

#Lançamento 7
dado7 = sample(1:6, size = 6000, replace = TRUE)
table(dado7)

#Lançamento 8
dado8 = sample(1:6, size = 10000, replace = TRUE)
table(dado8)

#A informação
dado1
#Mostra onde os 1 estão
which(dado1==1)
#Quantidade de 1 que tem no which
length(which(dado1==1))

Tabela2 =
rbind(
  cbind(length(which(dado1==1)),length(which(dado1==2)),length(which(dado1==3)),length(which(dado1==4)),length(which(dado1==5)),length(which(dado1==6)))/length(dado1),
  cbind(length(which(dado2==1)),length(which(dado2==2)),length(which(dado2==3)),length(which(dado2==4)),length(which(dado2==5)),length(which(dado2==6)))/length(dado2),
  cbind(length(which(dado3==1)),length(which(dado3==2)),length(which(dado3==3)),length(which(dado3==4)),length(which(dado3==5)),length(which(dado3==6)))/length(dado3),
  cbind(length(which(dado4==1)),length(which(dado4==2)),length(which(dado4==3)),length(which(dado4==4)),length(which(dado4==5)),length(which(dado4==6)))/length(dado4),
  cbind(length(which(dado5==1)),length(which(dado5==2)),length(which(dado5==3)),length(which(dado5==4)),length(which(dado5==5)),length(which(dado5==6)))/length(dado5),
  cbind(length(which(dado6==1)),length(which(dado6==2)),length(which(dado6==3)),length(which(dado6==4)),length(which(dado6==5)),length(which(dado6==6)))/length(dado6),
  cbind(length(which(dado7==1)),length(which(dado7==2)),length(which(dado7==3)),length(which(dado7==4)),length(which(dado7==5)),length(which(dado7==6)))/length(dado7),
  cbind(length(which(dado8==1)),length(which(dado8==2)),length(which(dado8==3)),length(which(dado8==4)),length(which(dado8==5)),length(which(dado8==6)))/length(dado8)
)
#O nomes das linhas (por isso não tem nome o n)
rownames(Tabela2) = c(length(dado1),length(dado2),length(dado3),length(dado4),length(dado5),length(dado6),length(dado7),length(dado8))
#Nome das colunas
colnames(Tabela2) = c("Face 1", "Face 2", "Face 3","Face 4","Face 5","Face 6")
#Arredondamento
round(Tabela2,4)



barplot(table(dado8))
prop.table(table(dado8))
barplot(prop.table(table(dado8)), col = "lightblue", ylab = "Proporção", las = 1)
abline(h = 0.166, col = "red", lty = 2)
