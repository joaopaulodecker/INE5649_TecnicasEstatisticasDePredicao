# 1 - Amostra
base = read.csv2("base.csv")
set.seed(07052006)
base1 = base[sample(nrow(base), 500),]



# 2 - Ajustando um modelo linear múltiplo
base1$Local <- as.factor(base1$Local)
modelo <- lm(Valor ~ Area + Idade + Energia + Local, data = base1)
summary(modelo)


# 3 - Teste estatístico para verificar relação linear
library(car)
Anova(modelo)



# 4 - Interpretação dos coeficientes do modelo das variáveis que permaneceram na análise
summary(modelo)



# 5 - Interpretação do Rˆ2 ajustado
summary(modelo)$adj.r.squared




# 6 - Análise da importância relativa de cada variável
library(relaimpo)
imp <- calc.relimp(modelo)
var.exp <- data.frame(round(imp$lmg * 100, 1))
colnames(var.exp) <- "imp.lmg"
nome <- rownames(var.exp)
var.exp <- data.frame(nome, var.exp)
# Gráfico
library(ggplot2)
ggplot(var.exp, aes(x = nome, y = imp.lmg)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_text(aes(label = imp.lmg), vjust = 1.5, colour = "white") +
  labs(x = "Variável", y = "Importância (%)", 
       title = "Importância relativa das variáveis no modelo") +
  theme_minimal()



# 7 - Análise dos resíduos do modelo (utilizando os 4 gráficos) e suas conclusões
par(mfrow = c(2, 2))
plot(modelo)


