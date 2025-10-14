# A - Amostra
base = read.csv2("selecao.csv")
set.seed(07052006)
base1 = base[sample(nrow(base), 500),]



# B - Seleção de variáveis - método backward
mc=lm( y ~ ., data=base1)
backward_model=step(mc, direction = "backward")
summary(backward_model)



# C - Análise de resíduos
plot(fitted(backward_model), rstandard(backward_model),
     main = "Gráfico de Resíduos do Modelo Original",
     xlab = "Valores Ajustados",
     ylab = "Resíduos Padronizados")
abline(h = 0, col = "red")

# Histograma dos resíduos
hist(rstandard(backward_model),
     main = "Histograma dos Resíduos Padronizados",
     xlab = "Resíduo Padronizado",
     col = "skyblue")

# foi verificada a possibilidade de variáveis quadráticas
# D - backward com variáveis quadráticas
mcq = lm(y ~ x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10 +
           I(x1^2) + I(x2^2) + I(x3^2) + I(x4^2) + I(x5^2) +
           I(x6^2) + I(x7^2) + I(x8^2) + I(x9^2) + I(x10^2),
         data = base1)

backward_model_quad = step(mcq, direction = "backward")
summary(backward_model_quad)

#  Análise de resíduos
plot(fitted(backward_model_quad), rstandard(backward_model_quad),
     main = "Gráfico de Resíduos do Modelo Original",
     xlab = "Valores Ajustados",
     ylab = "Resíduos Padronizados")
abline(h = 0, col = "red")

# Histograma dos resíduos
hist(rstandard(backward_model_quad),
     main = "Histograma dos Resíduos Padronizados",
     xlab = "Resíduo Padronizado",
     col = "skyblue")
