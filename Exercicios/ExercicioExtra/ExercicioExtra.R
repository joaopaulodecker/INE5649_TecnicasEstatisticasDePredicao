# LEITURA DA BASE
base <- read.csv2("turismo_floripa.csv")

# a - Série temporal
turismo <- ts(base$turistas,
              frequency = 12,
              start = c(2015, 1))

# Separação Treino (até 12/2023) e Teste (01/2024 a 12/2024)
treino <- window(turismo, end = c(2023, 12))
teste  <- window(turismo, start = c(2024, 1))




# b - AJUSTE DO MODELO
library(forecast)

# Arima
mod.arima <- auto.arima(treino, seasonal = FALSE)
summary(mod.arima)

# Sarima
mod.sarima <- auto.arima(treino, seasonal = TRUE)
summary(mod.sarima)

# Comparação de AICc
mod.arima$aicc
mod.sarima$aicc

# Previsão para 12 meses (01/2024 a 12/2024)
prev_sarima <- forecast(mod.sarima, h = 12)
summary(prev_sarima)




# c - DIAGNÓSTICO DE RESÍDUOS DO MODELO ESCOLHIDO
checkresiduals(mod.sarima)






# d - ANÁLISE GRÁFICA DO AJUSTE
library(ggplot2)

# Criação de data frames pro gráfico
df_treino <- data.frame(
  tempo = time(treino),
  valor = as.numeric(treino),
  serie = "Treino"
)

df_teste <- data.frame(
  tempo = time(teste),
  valor = as.numeric(teste),
  serie = "Teste"
)

df_prev <- data.frame(
  tempo = time(prev_sarima$mean),
  valor = as.numeric(prev_sarima$mean),
  serie = "Previsão"
)

# Gráfico 
ggplot() +
  geom_line(data = df_treino, aes(x = tempo, y = valor, color = "Treino")) +
  geom_line(data = df_teste, aes(x = tempo, y = valor, color = "Teste"),
            linetype = "dashed") +
  geom_line(data = df_prev, aes(x = tempo, y = valor, color = "Previsão")) +
  scale_color_manual(values = c("Treino" = "black",
                                "Teste" = "red",
                                "Previsão" = "blue")) +
  labs(title = "Treino, Teste e Previsão - Modelo SARIMA",
       x = "Ano",
       y = "Número de Turistas",
       color = "Séries") +
  theme_minimal()

# Gráfico com zoom 
ggplot() +
  geom_line(data = df_treino, aes(x = tempo, y = valor, color = "Treino")) +
  geom_line(data = df_teste, aes(x = tempo, y = valor, color = "Teste"),
            linetype = "dashed") +
  geom_line(data = df_prev, aes(x = tempo, y = valor, color = "Previsão")) +
  scale_color_manual(values = c("Treino" = "black",
                                "Teste" = "red",
                                "Previsão" = "blue")) +
  xlim(c(2022, 2025)) +
  labs(title = "Treino, Teste e Previsão - Modelo SARIMA",
       x = "Ano",
       y = "Número de Turistas",
       color = "Séries") +
  theme_minimal()





# e - GRÁFICO SOMENTE COM O TESTE E A PREVISÃO COM IC 95%
autoplot(prev_sarima) +
  autolayer(teste, series = "Real", color = "black") +
  labs(title = "Valores Reais e Previsão (IC 95%) - 2024",
       x = "Ano",
       y = "Número de Turistas") +
  xlim(c(2024, 2025)) +
  theme_minimal()



# f