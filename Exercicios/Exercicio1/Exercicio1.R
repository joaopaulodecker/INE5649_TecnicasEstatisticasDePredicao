# 1 - Amostra
base = read.csv2("exercicio.csv")
set.seed(07052006)
base1 = base[sample(nrow(base), 800),]





# 2 - Gráfico de dispersão de cada váriavel relacionada com o preço
library(ggplot2)

# Gráfico 1: preco vs anos
ggplot(base1, aes(x = anos, y = preco)) +
  geom_point(color = "steelblue", alpha = 0.7) +
  labs(
    title = "Preço vs Anos de Uso",
    x = "Anos de uso",
    y = "Preço (R$)"
  ) +
  theme_minimal()

# Gráfico 2: preco vs km
ggplot(base1, aes(x = km, y = preco)) +
  geom_point(color = "darkgreen", alpha = 0.7) +
  labs(
    title = "Preço vs Quilometragem",
    x = "Km rodados",
    y = "Preço (R$)"
  ) +
  theme_minimal()

# Gráfico 3: preco vs potencia
ggplot(base1, aes(x = potencia, y = preco)) +
  geom_point(color = "firebrick", alpha = 0.7) +
  labs(
    title = "Preço vs Potência do Carro",
    x = "Potência (cv)",
    y = "Preço (R$)"
  ) +
  theme_minimal()

# Gráfico 4: preco vs tanque
ggplot(base1, aes(x = tanque, y = preco)) +
  geom_point(color = "darkorange", alpha = 0.7) +
  labs(
    title = "Preço vs Capacidade do Tanque",
    x = "Tanque (litros)",
    y = "Preço (R$)"
  ) +
  theme_minimal()





# 3 - Calculo do coeficiente de correlacao de cada variável quantitativa

vars_quant <- base1[, c("preco", "anos", "km", "potencia", "tanque")]
correlacoes <- cor(vars_quant, use = "complete.obs")
correlacoes["preco", -1]





# 4 - Intervalo de confiança da correlação entre potência e preço 

r <- cor(base1$preco, base1$potencia, use = "complete.obs")
n <- sum(complete.cases(base1$preco, base1$potencia))

# Transformação de Fisher
z <- 0.5 * log((1 + r) / (1 - r))
se <- 1 / sqrt(n - 3)

# Intervalo de confiança em z
z_low <- z - 1.96 * se
z_high <- z + 1.96 * se

# Voltar para escala de correlação
r_low <- (exp(2 * z_low) - 1) / (exp(2 * z_low) + 1)
r_high <- (exp(2 * z_high) - 1) / (exp(2 * z_high) + 1)

# Resultado
c("inferior" = r_low, "superior" = r_high)





# 5 - Modelo de regressão da variável preço x potência
modelo <- lm(preco ~ potencia, data = base1)
summary(modelo)

# a - o coeficiente de determinação e interprete o valor.
summary(modelo)$r.squared

# b - a análise de resíduos
par(mfrow = c(2, 2))
plot(modelo)

# c - teste de significancia do modelo
summary(modelo)$coefficients

# d - interpretacao dos coeficientes do modelo
coef(modelo)




# 6 - Gráfico de dispersão preco x potencia diferenciando transmissão automática e manual
library(ggplot2)

ggplot(base1, aes(x = potencia, y = preco, color = transm)) +
  geom_point(alpha = 0.7, size = 2) +
  labs(
    title = "Preço vs Potência com Tipo de Transmissão",
    x = "Potência (cv)",
    y = "Preço (R$)",
    color = "Transmissão"
  ) +
  theme_minimal()


