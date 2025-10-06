# a- Amostra
base = read.csv2("carros.csv")
set.seed(07052006)
base1 = base[sample(nrow(base), 800),]




# b- Criação da variável idade
library(dplyr)

base1 <- base1 |>
  mutate(idade = 2025 - ano)





# c- Criação do modelo linear
base1 <- base1 |>
  mutate(estado_conservacao = factor(estado_conservacao))

modelo_interacao <- lm(preco ~ idade * estado_conservacao, data = base1)

summary(modelo_interacao)





# d- Teste de interação 
library(car)
Anova(modelo_interacao)

modelo_sem_interacao <- lm(preco ~ idade + estado_conservacao, data = base1)
summary(modelo_sem_interacao)



# e- Análise de desvalorização



# f- Gráfico para representar interação do modelo
library(ggplot2)
library(dplyr)

# Garantir ordem desejada
base1 <- base1 |>
  mutate(
    estado_conservacao = factor(
      estado_conservacao,
      levels = c("Ruim", "Regular", "Bom", "Excelente")
    )
  )


ggplot(base1, aes(x = idade, y = preco, color = estado_conservacao)) +
  geom_point(alpha = 0.4, size = 1) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 1.2) +
  labs(
    title = "Relação entre Idade e Preço dos Carros por Estado de Conservação",
    x = "Idade do carro (anos)",
    y = "Preço de venda (R$)",
    color = "Estado de conservação"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.position = "right"
  )




# g- Intervalo de confiança e de predição
library(ggplot2)
library(dplyr)

# Intervalos de confiança e predição para os dados da base
IC1 <- predict(modelo_sem_interacao, interval = "confidence", level = 0.95)
IC2 <- predict(modelo_sem_interacao, interval = "predict", level = 0.95)

# Juntar resultados na mesma base
new <- data.frame(base1, IC1, IC2)

# Filtrar apenas carros em excelente estado (para o gráfico ilustrativo)
new_excelente <- new |> 
  filter(estado_conservacao == "Excelente")

# Gráfico dos intervalos
ggplot(new_excelente, aes(x = idade, y = preco)) +
  geom_point(alpha = 0.4) +
  geom_line(aes(y = fit), color = "blue", linewidth = 1) +            # linha ajustada
  geom_line(aes(y = lwr), color = "black") +                         # IC inferior
  geom_line(aes(y = upr), color = "black") +                         # IC superior
  geom_line(aes(y = lwr.1), color = "red", linetype = "dashed") +    # IP inferior
  geom_line(aes(y = upr.1), color = "red", linetype = "dashed") +    # IP superior
  labs(
    title = "Intervalos de Confiança (preto) e Predição (vermelho) — Estado Excelente",
    x = "Idade do carro (anos)",
    y = "Preço de venda (R$)"
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))



# Previsão para um carro de 5 anos e estado 'Excelente'
novo_carro <- data.frame(idade = 5, estado_conservacao = "Excelente")

# Intervalo de confiança (média esperada)
predict(modelo_sem_interacao, novo_carro, interval = "confidence", level = 0.95)

# Intervalo de predição (carro individual)
predict(modelo_sem_interacao, novo_carro, interval = "predict", level = 0.95)

