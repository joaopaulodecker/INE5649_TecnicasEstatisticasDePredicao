
library(tidyverse)
library(forecast)
library(lubridate)

# a) Separação Treino/Teste

gold <- read_csv2("GoldUP.csv", show_col_types = FALSE)

indice_teste <- tail(1:nrow(gold), 12)   # últimos 12 meses (2024)
treino <- gold[-indice_teste, ]          # até dez/2023
teste  <- gold[indice_teste, ]           # jan–dez/2024

base_ts <- ts(treino$Gold_Price, frequency = 12, start = c(2015, 1))
autoplot(base_ts) +
  labs(title = "Preço do Ouro (USD/oz)", x = "Ano", y = "USD/oz")





# b) Modelo De Regressão Linear

modelo_reg <- lm(Gold_Price ~ Crude_Oil + Interest_Rate + USD_INR + CPI,
                 data = treino)
summary(modelo_reg)

# Previsão com o modelo de regressão
prev_reg <- predict(modelo_reg, newdata = teste)





# c) Modelos De Suavização Exponencial

SES <- ses(base_ts, h = 12)
HOLT <- holt(base_ts, h = 12)
HW_ad <- hw(base_ts, seasonal = "additive", h = 12)
HW_mult <- hw(base_ts, seasonal = "multiplicative", h = 12)
ETS <- ets(base_ts)
ETS_pred <- forecast(ETS, h = 12)






# d) Gráfico Comparando Modelos

teste <- cbind(
  teste,
  SES = SES$mean,
  HOLT = HOLT$mean,
  HW_ad = HW_ad$mean,
  HW_mult = HW_mult$mean,
  ETS = ETS_pred$mean,
  REG = prev_reg
)

teste$Date <- as.Date(teste$Date)

ggplot(teste, aes(x = Date)) +
  geom_line(aes(y = Gold_Price, color = "Real"), linetype = "solid") +
  geom_line(aes(y = SES, color = "SES"), linetype = "dashed") +
  geom_line(aes(y = HOLT, color = "Holt"), linetype = "solid") +
  geom_line(aes(y = HW_ad, color = "HW Aditivo"), linetype = "dashed") +
  geom_line(aes(y = HW_mult, color = "HW Multiplicativo"), linetype = "solid") +
  geom_line(aes(y = ETS, color = "ETS"), linetype = "solid") +
  geom_line(aes(y = REG, color = "Regressão Linear"), linetype = "dashed") +
  scale_color_manual(name = "Modelos",
                     values = c("Real" = "black", "SES" = "blue", "Holt" = "green",
                                "HW Aditivo" = "orange", "HW Multiplicativo" = "purple",
                                "ETS" = "red", "Regressão Linear" = "brown")) +
  theme_bw() +
  labs(title = "Previsões do preço do ouro (jan–dez/2024)",
       x = "Data", y = "USD/oz") +
  theme(legend.position = "bottom")









# e) Acurácia (EAM)

eam <- function(yreal, yprev) mean(abs(yreal - yprev))

eam_SES  <- eam(teste$Gold_Price, teste$SES)
eam_HOLT <- eam(teste$Gold_Price, teste$HOLT)
eam_HWad <- eam(teste$Gold_Price, teste$HW_ad)
eam_HWm  <- eam(teste$Gold_Price, teste$HW_mult)
eam_ETS  <- eam(teste$Gold_Price, teste$ETS)
eam_REG  <- eam(teste$Gold_Price, teste$REG)

acuracia <- tibble(
  Modelo = c("SES", "HOLT", "HW Aditivo", "HW Multiplicativo", "ETS", "Regressão Linear"),
  EAM = c(eam_SES, eam_HOLT, eam_HWad, eam_HWm, eam_ETS, eam_REG)
)

acuracia %>% arrange(EAM)
