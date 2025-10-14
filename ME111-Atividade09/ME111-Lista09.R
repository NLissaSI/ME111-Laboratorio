load("ames.RData")
area_pop = ames$Gr.Liv.Area

set.seed(09)
area = sample(area_pop, 60)
hist(area, breaks = 10, col = "orange")
summary(area)

media_area = mean(area)
mean(area_pop)

alpha = 0.05
z = qnorm(1 - alpha/2)
se_06 = sd(area)/sqrt(60)
margem_06 = z * se
lower_06 = media_area - margem
upper_06 = media_area + margem
ic_06 = c(lower, upper)

## Fazer atraves de "replicate"?
mean_50 = replicate(50, mean(sample(area_pop, 60)))
sd_50 = replicate(50, sd(sample(area_pop, 60)))

## Tentando usar "function"
ic = function(n = 60){
  amostra = sample(area_pop, n)
  m = mean(amostra)
  dp = sd(amostra)
  margem = 1.96*dp/sqrt(n)
  ic = c(m - margem, m + margem)
  return(ic)
}


Amostra_media = rep(NA, 50)
Amostra_desvio = rep(NA, 50)
n = 60
for(i in 1:50){ # "for" é um loop; de "i" variando de 1 a 50
  amostra = sample(area_pop, n)
  Amostra_media[i] = mean(amostra)  # O "[i]" é a posição
  Amostra_desvio[i] = sd(amostra)
}
Amostra_media - 1.96 * Amostra_desvio / sqrt(n)
Amostra_media + 1.96 * Amostra_desvio / sqrt(n)


plot_ci(Amostra_media - 1.96 * Amostra_desvio / sqrt(n), Amostra_media + 1.96 * Amostra_desvio / sqrt(n), mean(area_pop))

plot_ci(Amostra_media - 1.64 * Amostra_desvio / sqrt(n), Amostra_media + 1.64 * Amostra_desvio / sqrt(n), mean(area_pop))
