download.file("http://www.openintro.org/stat/data/ames.RData", destfile = "ames.RData")
load("ames.RData") 


names(ames)
str(ames) # Diz a estrutura dentro do objeto, uma alternativa para summary

area = ames$Gr.Liv.Area #Area em pés quadrados
summary(area)
hist(area, col = "lightpink", las = 1, 
     xlab = "Área (Pés)", ylab = "Frequência",
     main = "Distribuição da Área de Vendas")

set.seed(186028) ## troque o número 111111 pelo seu RA  Não é aleatório toda vez que rodar o sample
samp1 = sample(area, 50) 
hist(samp1, col = "lightpink", las = 1,
     xlab = "Samp1", ylab = "Frequência",
     main = "Distribuição da Área de Vendas")
summary(samp1)
mean(samp1)


set.seed(2020) # Eu acho que não precisava, mas ok...
samp2 = sample(area, 50)
mean(samp2)


means50 = replicate(5000, mean(sample(area, 50))) # Pega uma amostra de 50, o valor sendo a area, e então faz a média,
                                                  # depois faz a mesma coisa, no caso foram, no total, 5.000 vezes.
                                                  # São 5.000 amostras do means50, mas foram retirados 50 amostras do original.
                                                  # Então, replicate(Vezes, A informação que vai juntar / fazer)
hist(means50, breaks = 20, col = "lightpink", las = 1)


teste_means50000 = replicate(50000, mean(sample(area, 50))) # Geradas 50.000 amostras
hist(teste_means50000, breaks = 20)


means10 = replicate(5000, mean(sample(area, 10)))
means100 = replicate(5000, mean(sample(area, 100)))

par(mfrow = c(1, 3)) ## gráficos serão apresentados lado a lado (1 linha, 3 colunas) ## Segue para todo o resto, .rmd?
# Ordem dos Histogramas = means10, means50 e means100
hist(means10, col = "lightpink", las = 1, breaks = 20)
hist(means50, col = "lightpink", las = 1, breaks = 20)
hist(means100, col = "lightpink", las = 1, breaks = 20)

plot(means10) # Eixo x -> As amostras; Eixo y -> As médias
plot(means50)
plot(means100)

par(mfrow = c(1, 1)) ## 1 linha, 1 coluna  ## Teoricamente volta ao normal
names(ames)

price = ames$SalePrice
samp_price = sample(price, 50)

summary(price)
summary(samp_price)

means50_price = replicate(5000, mean(sample(price, 50)))
urve(dnorm(x, mean =180703.1, sd =11332.34), col = "red", add = TRUE)

hist(means50_price, col = "lightpink", breaks = 20, prob = TRUE)
curve(dnorm(x, mean =180703.1, sd =11332.34), col = "red", add = TRUE, lwd = 2)

mean(price)
sd(price)

hist(x, prob = TRUE) # Coloca as probabilidades ao inves da frequencia

sd(means50_price)
mean(means50_price)

means150_price = replicate(5000, mean(sample(price, 150)))
hist(means150_price, col = "lightpink", breaks = 20)
var(means150_price)
mean(means150_price)
sd(means150_price)



var(price) # n - 1
var(samp_price)

# curve()
# (length(.) - 1) / length(.) * var()
# Que sai o varianca populacional.
