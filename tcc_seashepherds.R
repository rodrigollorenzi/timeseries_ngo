# importar arquivos welight 21 e 22
welight_21 <- read.csv(file.choose())
welight_22 <- read.csv(file.choose())

summary(welight_21)
summary(welight_22)

# identificando dados a mais em um dos data sets
dif_col <- setdiff(colnames(welight_22), colnames(welight_21))
print(dif_col)

# Carregando o pacote dplyr
install.packages("dplyr")
library(dplyr)

# limpando colunas extras no _22
welight_22_clean <- welight_22 %>% select(-(Categoria.1:Banco))

#criando BD unificado 
welight_BD <- rbind(welight_21, welight_22_clean)

#checkando linhas duplicadas na BD
welightduplicados <- duplicated(welight_BD)

welight_BD[welightduplicados,]

summary(welight_BD)

#renomeando as colunas
welight_BD <- rename(welight_BD, id_transacao = Token, data = Data, valor = Valor..R..)

#filtrar linhas com status = paid
welight_BD_final <- subset(welight_BD, Status == "paid")

#inserir coluna nova tipo_entrada e atribuir valor de doação a todas as linhas
welight_BD_final$tipo_entrada <- "doacao"

#inserir nova coluna source para taguear como welight
welight_BD_final$source <- "welight"

#criar versão final otimizada dos dados de welight
dados_welight <- subset(welight_BD_final, select = c(id_transacao, data, valor, source, tipo_entrada))

#formatar data
dados_welight$data <- as.Date(dados_welight$data, format = "%d/%m/%Y", tryFormats = c("%Y-%m-%d"))
class(dados_welight$data)

####################################################PAG SEGURO##################################################

# importar arquivos pagseguro
pagseguro_21 <- read.csv(file.choose())
pagseguro_22 <- read.csv(file.choose())
  
# limpando colunas extras no _22
library(dplyr)
pagseguro_22_clean <- pagseguro_22 %>% select(-(Categoria.1:Banco))

#criando BD unificado 
pagseguro_BD <- rbind(pagseguro_21, pagseguro_22_clean)

#renomeando as colunas
pagseguro_BD <- rename(pagseguro_BD, id_transacao = Transacao_ID, data = Data_Transacao, valor = Valor_Bruto)

#inserir source 
pagseguro_BD$source <- "pagseguro"

#excluir tipos de transação de saída (Rendimento, Recarga e Saque)  
pagseguro_BD_filtrado <- subset(pagseguro_BD, Tipo_Transacao != "Rendimento" & Tipo_Transacao != "Recarga" & Tipo_Transacao != "Saque", select = c(id_transacao, data, valor, source, Tipo_Transacao))

#contando valores vazios
colSums(is.na(pagseguro_BD_filtrado))

#inserir coluna nova tipo_entrada e atribuir doação como label
pagseguro_BD_filtrado$tipo_entrada <- "doacao"

#criar versão final otimizada dos dados de pagseguro
dados_pagseguro <- subset(pagseguro_BD_filtrado, select = c(id_transacao, data, valor, source, tipo_entrada))

#exportar csv para limpar data
write.csv(dados_pagseguro, file = "dados_pagseguro.csv", row.names = FALSE) 

#contando valores vazios
colSums(is.na(dados_pagseguro))

#substituindo valores vazios por NA
dados_pagseguro2 <- dados_pagseguro 
dados_pagseguro2[dados_pagseguro2 == ''] <- NA
colSums(is.na(dados_pagseguro2))

#limpar dados NA

dados_pagseguro3 <- na.omit(dados_pagseguro2)
sum(is.na(dados_pagseguro3))

dados_pagseguro <- dados_pagseguro3

#tratando a data
install.packages("tidyr")
library(tidyr)

#split coluna de data e tempo
dados_pagseguro <- dados_pagseguro %>%
  separate(data, into = c('data', "tempo"), sep = " ")

#lubridate para formatação final
library(lubridate)

dados_pagseguro$data <- as.Date(dados_pagseguro$data, format = "%d/%m/%Y", tryFormats = c("%Y-%m-%d"))
class(dados_pagseguro$data)

#selecionar colunas necessárias
dados_pagseguro <- subset(dados_pagseguro, select = c(id_transacao, data, valor, source, tipo_entrada))





####################################################PAG HIPER##################################################
# importar arquivos 
paghiper_21 <- read.csv(file.choose())
paghiper_22 <- read.csv(file.choose())

#criando BD unificado 
paghiper_BD <- rbind(paghiper_21, paghiper_22)

#renomeando as colunas
library(dplyr)
colnames(paghiper_BD) <- gsub(" ", ".", colnames(paghiper_BD))
paghiper_BD <- rename(paghiper_BD, id_transacao = idTransacao, data = Data.da.Venda, valor = Valor.Bruto)

#selecionar colunas necessárias
dados_paghiper <- subset(paghiper_BD, select = c(id_transacao, data, valor))

#inserir source 
dados_paghiper$source <- "paghiper"

#classificar as entradas
dados_paghiper$tipo_entrada <- ifelse(dados_paghiper$valor %in% c(25, 30, 50, 100, 150, 200, 300),
  "doacao",
  "ecommerce"
)

#tratando a data
install.packages("tidyr")
library(tidyr)

#split coluna de data e tempo
dados_paghiper <- dados_paghiper %>%
  separate(data, into = c('data', "tempo"), sep = " ")

#lubridate para formatação final
library(lubridate)

dados_paghiper$data <- as.Date(dados_paghiper$data, format = "%d/%m/%Y", tryFormats = c("%Y-%m-%d"))
class(dados_paghiper$data)

#limpar dados NA
dados_paghiper <- subset(dados_paghiper, !is.na(valor))

#selecionar colunas necessárias
dados_paghiper <- subset(dados_paghiper, select = c(id_transacao, data, valor, source, tipo_entrada))


####################################################PAYPAL##################################################
# importar arquivos 
paypal_21 <- read.csv(file.choose())
paypal_22 <- read.csv(file.choose())

#criando BD unificado 
paypal_BD <- rbind(paypal_21, paypal_22)

#renomeando as colunas
library(dplyr)
colnames(paypal_BD) <- gsub(" ", ".", colnames(paypal_BD))
paypal_BD <- rename(paypal_BD, id_transacao = ID.de.referência.da.transação, data = Data, valor = Bruto)

#inserir source 
paypal_BD$source <- "paypal"

#filtrando dados desnecessários
paypal_BD <- subset(paypal_BD, Status == "Concluído")

paypal_BD_filtrado <- subset(
  paypal_BD,
  !(Tipo %in% c("retirada geral", "retirada geral de cartão de crédito", "Pagamento pré-aprovado do usuário da conta de pagamento")) &
  !grepl("retenção", Tipo)
)

#filtrando/excluindo os negativos
paypal_BD_filtrado <- subset(paypal_BD_filtrado, valor>0)

#classificar as entradas
paypal_BD_filtrado$tipo_entrada <- ifelse(paypal_BD_filtrado$Tipo == "Pagamento no site" & paypal_BD_filtrado$Assunto == "Shopping Cart", "ecommerce", "doacao")


#selecionar colunas necessárias
dados_paypal <- subset(paypal_BD_filtrado, select = c(id_transacao, data, valor, tipo_entrada, source))

# Gerar um ID randômico de 8 caracteres
install.packages("stringi")
library(stringi)
id_aleatorio <- stri_rand_strings(1, 8, pattern="[A-Za-z]")

#substituir blanks em id de transação por código randômico+ecomm (é o mesmo ID randômico em todas as células vazias, só pra marcação mesmo)
sum(dados_paypal$id_transacao == "")
dados_paypal$id_transacao <- ifelse(dados_paypal$id_transacao == "", id_aleatorio, dados_paypal$id_transacao)

#tratando a data
library(lubridate)

dados_paypal$data <- as.Date(dados_paypal$data, format = "%d/%m/%Y", tryFormats = c("%Y-%m-%d"))
class(dados_paypal$data)

#############################################################################################################################################
#############################################################################################################################################
############################################################SÉRIE TEMPORAL###################################################################
#############################################################################################################################################
#############################################################################################################################################

#MACRO DATABASE - COMBINANDO TODOS OS DADOS
dados_gerais <- rbind(dados_paghiper, dados_pagseguro, dados_paypal, dados_welight)

#tratando a data
dados_gerais$data <- as.Date(dados_geral$data, format = "%d-%m-%Y", tryFormats = c("%Y-%m-%d"))
class(dados_gerais$data)

#criando DB ecomm
db_ecomm <- subset(dados_gerais, tipo_entrada == "ecommerce")
print(summary(db_ecomm))
db_ecomm_dia <- group_by(db_ecomm,data) %>% summarise(total = sum(valor))
print(summary(db_ecomm_dia))
class(db_ecomm_dia$total)
db_ecomm_dia <- subset(db_ecomm_dia, select = c(data, total))

#criando DB doacao
db_doacao <- subset(dados_gerais, tipo_entrada == "doacao")
db_doacao_dia <- group_by(db_doacao,data) %>% summarise(total = sum(valor))
db_doacao_dia <- subset(db_doacao_dia, select = c(data, total))
summary(db_doacao_dia)

#instalando pacotes 

install.packages("readr")
install.packages("forecast")
install.packages("ggplot2")
install.packages("forecasting")
install.packages("tseries")
install.packages("fpp2")
install.packages("fabletools")
install.packages("zoo")
install.packages("tsibble")
install.packages("tidyverse")
library(tsibble)
library(tidyverse)
library(zoo)
library(fabletools)
library(readr)
library(fpp2)
library(lubridate)
library(forecast)
library(dplyr)
library(ggplot2)

#convertendo valor para interger
class(dados_gerais$valor)
dados_gerais$valor <- as.numeric(dados_gerais$valor)
sum(is.na(dados_gerais$valor))

#limpando valores NA
colSums(is.na(dados_gerais))
dados_gerais <- na.omit(dados_gerais)
colSums(is.na(dados_gerais))

#agregando valores por data
class(dados_gerais$valor)
dados_ts_dia <- group_by(dados_gerais,data) %>% summarise(total = sum(valor))
print(dados_ts_dia)
dados_ts_dia <- subset(dados_ts_dia, select = c(data, total))
dia <- dados_ts_dia
class(dia$data)

#agregando valores por semana
dados_ts_dia$semana <- yearweek(as.Date(dados_ts_dia$data))
class(dados_ts_dia$semana)

dados_ts_semana <- group_by(dados_ts_dia,semana) %>% summarise(total = sum(total))
plot(dados_ts_semana$total)
print(dados_ts_semana)
semana <- dados_ts_semana
class(semana$semana)

#agregando valores por mês
dados_ts_mes <- dados_ts_dia %>% 
  group_by(month = lubridate::floor_date(dados_ts_dia$data, 'month')) %>%
  summarize(total = sum(total))
class(dados_ts_mes$month)
dados_ts_mes$month <- format(dados_ts_mes$month, "%Y-%m")
mes <- dados_ts_mes
mes$month <- as.yearmon(mes$month)
class(mes$month)


#ordem cronológica
dados_gerais_agregados <- dados_gerais_agregados[order(as.Date(dados_gerais_agregados$data)), ]


#visualização dia
ggplot(dia, aes(x = data, y = total)) +
  geom_line()

par(mfrow= c(3,1), mar=c(3,3,3,3))
plot(ts_dia, main = "Série Diária")
plot(ts_semana, main = "Série Semanal")
plot(ts_mes, main = "Série Mensal")

par(mfrow= c(3,1), mar=c(3,3,3,3))
plot(ts_dia, main = "Série Diária Total")
plot(ts_doa, main = "Série Diária Doações")
plot(ts_ecom, main = "Série Diária E-commerce")

#visualizacao semana
ggplot(semana, aes(x = semana, y = total)) +
  geom_line()

#visualizacao mes
ggplot(mes, aes(x = month, y = total)) +
  geom_line()

#convertendo as 3 séries em TS

install.packages("xts")
library(xts)

#dia 1 (excluidno 2020 e 2023)
dia.zoo <- read.zoo(dia)
ts_dia_zoo <- as.ts(dia.zoo)
class(ts_dia_zoo)

dia <- dia[dia$data >= "2021-01-01" & dia$data <= "2022-12-31", ] #filtrando períodos antes e depois para regularizar os intervalos. 
ts_dia <- ts(dia[,"total"], start = c(2021, 1), end = c(2022, 365), frequency = 365)
class(ts_dia)

ts_dia2 <- ts(dia$total, frequency=365, start=c(2021,1))
dia_treino <- ts(dia[,"total"], start = c(2021, 1), end = c(2022, 146), frequency = 365)

#dia 2 (excluindo dezembro/20 apenas)

dia2 <- dados_ts_dia
dia2 <- dia2[dia2$data >= "2021-01-01", ]


#semana

class(semana$semana)
semana_filtrada <- subset(semana, semana >= as.Date("2021 W01", format = "%Y W%V") & 
                      semana <= as.Date("2022 W52", format = "%Y W%V"))
semana.zoo <- read.zoo(semana_filtrada)
ts_semana <- as.ts(semana.zoo)
class(ts_semana)
semana_filtrada <- semana[3:106,]
ts_semana

#mes
mes.zoo <- read.zoo(mes)
ts_mes <- as.ts(mes.zoo)
class(ts_mes)

#plotando autocorrelações DIA

dia_acf <- acf(dia$total, plot = FALSE)
dia_pacf <- pacf(dia$total, plot = FALSE)

autoplot(ts_dia)
autoplot(ts_semana)

par(mfrow= c(2,1), mar=c(3,3,3,3))
plot(dia_acf, main = "Função de Autocorrelação")
title(main="TS Dia")
plot(dia_pacf, main = "Autocorrelação Parcial")
title(main="TS Dia")

#plotando autocorrelações SEMANA

semana_acf <- acf(semana$total, plot = FALSE)
semana_pacf <- pacf(semana$total, plot = FALSE)

par(mfrow= c(2,1), mar=c(3,3,3,3))
plot(semana_acf, main = "Função de Autocorrelação")
title(main="TS Semana")
plot(semana_pacf, main = "Autocorrelação Parcial")
title(main="TS Semana")

#plotando autocorrelações MES

mes_acf <- acf(mes$total, plot = FALSE)
mes_pacf <- pacf(mes$total, plot = FALSE)

par(mfrow= c(2,1), mar=c(3,3,3,3))
plot(mes_acf, main = "Função de Autocorrelação")
title(main="TS Mês")
plot(mes_pacf, main = "Autocorrelação Parcial")
title(main="TS Mês")

#decompondo a série

#dia
ts_dia
decomp_dia <- decompose(ts_dia, "additive") #decomposição da ts_dia

plot(as.ts(decomp_dia$seasonal))
plot(as.ts(decomp_dia$trend))
plot(as.ts(decomp_dia$random))
plot(decomp_dia)
ts_dia

#semana
ts_semana
decomp_semana <- decompose(ts_semana, "additive") #decomposição da ts_semana nõa funcionou

plot(as.ts(decomp_semana$seasonal))
plot(as.ts(decomp_semana$trend))
plot(as.ts(decomp_semana$random))
plot(decomp_semana)

decomp_semana <- model(stl(ts_semana, s.window = "periodic", t.window = 52 )) #não rolou também

#mes
decomp_mes <- decompose(ts_mes, "additive")

plot(as.ts(decomp_mes$seasonal))
plot(as.ts(decomp_mes$trend))
plot(as.ts(decomp_mes$random))
plot(decomp_mes)
ts_mes

#separar base de treino e base de teste
#dia1

dia_treino_df <- dia[1:511,]
dia_teste_df <- dia[512:730,]

dia_treino <- ts(dia_treino_df$total, start=c(2021,1), frequency=365)
plot(dia_treino)
summary(dia_treino)

dia_teste <- ts(dia_teste_df$total, start=c(2022, 1))
plot(dia_teste)

#estimar modelo com base em treino

dia_arima <- auto.arima(dia_treino, trace = TRUE, approximation = FALSE, seasonal= FALSE)
print(summary(dia_arima))

#checando resíduos
checkresiduals(dia_arima)
checkresiduals(dia_sarima)

#comparando treino com ajuste do modelo
plot(dia_treino)
lines(dia_arima$fitted, col='blue')
lines(dia_sarima$fitted, col='blue')

#fazendo forecast
fcst_dia_validacao <- forecast(dia_arima, h=219)
print(fcst_dia_validacao)
autoplot(fcst_dia_validacao)
print(summary(fcst_dia_validacao))

###################dia 2###############################
#estimar modelo

dia_arima2 <- auto.arima(dia2$total, trace = TRUE, approximation = FALSE)
print(summary(dia_arima2))

#checando resíduos
checkresiduals(dia_arima2)

#fazendo forecast
fcst_dia2 <- forecast(dia_arima2, h=90)
print(fcst_dia2)
autoplot(fcst_dia2)
print(summary(fcst_dia2))

#acurácia? não funcionaram até então. 
accuracy(dia_arima, dia_arima2) %>% round(3)

###################dia - ecomm###############################
db_ecomm_dia

#transformar em ts
ts_ecom <- ts(db_ecomm_dia$total, start= c(2021, 1), end= c(2022, 365), frequency = 365)
ts_ecom

#decompor
ts_ecom
decomp_ecom <- decompose(ts_ecom, "additive")

plot(as.ts(decomp_ecom$seasonal))
plot(as.ts(decomp_ecom$trend))
plot(as.ts(decomp_ecom$random))
plot(decomp_ecom)

#ACF e PACF
ecom_acf <- acf(db_ecomm_dia$total, plot = FALSE)
ecom_pacf <- pacf(db_ecomm_dia$total, plot = FALSE)

autoplot(ecom_acf)
autoplot(ecom_pacf)

#estimar modelo

arima_ecomm <- auto.arima(db_ecomm_dia$total, trace = TRUE, approximation = FALSE, seasonal = FALSE)
print(summary(arima_ecomm))

sarima_ecomm <- auto.arima(db_ecomm_dia$total, trace = TRUE, approximation = FALSE, seasonal = TRUE)
print(summary(sarima_ecomm))

#checando resíduos
checkresiduals(arima_ecomm)

#fazendo forecast
fcst_ecom <- forecast(arima_ecomm, h=90)
print(fcst_ecom)
autoplot(fcst_ecom)
print(summary(fcst_ecom))


###################dia - doações###############################
db_doacao_dia

#transformar em ts
ts_doa <- ts(db_doacao_dia$total, start= c(2021, 1), end= c(2022, 365), frequency = 365)
ts_doa

#decompor
ts_doa
decomp_doa <- decompose(ts_doa, "additive")

plot(as.ts(decomp_doa$seasonal))
plot(as.ts(decomp_doa$trend))
plot(as.ts(decomp_doa$random))
plot(decomp_doa)

#ACF e PACF
doa_acf <- acf(db_doacao_dia$total, plot = FALSE)
doa_pacf <- pacf(db_doacao_dia$total, plot = FALSE)

autoplot(doa_acf)
autoplot(doa_pacf)

#estimar modelo

arima_doa <- auto.arima(db_doacao_dia$total, trace = TRUE, approximation = FALSE, seasonal = FALSE)
print(summary(arima_doa))

sarima_doa <- auto.arima(db_doacao_dia$total, trace = TRUE, approximation = FALSE, seasonal = TRUE)
print(summary(sarima_doa))

#checando resíduos
checkresiduals(arima_doa)

#fazendo forecast
fcst_doa <- forecast(arima_doa, h=90)
print(fcst_doa)
autoplot(fcst_doa)
print(summary(fcst_doa))

#ajuste holt-winters
períodos com muita informação não permitem ajuste holt-winters

##########################################################MES##############################################
#mes
mes.zoo <- read.zoo(mes)
ts_mes <- as.ts(mes.zoo)
class(ts_mes)

ts_mes <- ts(mes$total, start= c(2021, 1), end= c(2022, 12), frequency = 12)
ts_mes

#decompondo a série

decomp_mes <- decompose(ts_mes, "additive")

plot(as.ts(decomp_mes$seasonal))
plot(as.ts(decomp_mes$trend))
plot(as.ts(decomp_mes$random))
plot(decomp_mes)
ts_mes

#plotando autocorrelações MES

mes_acf <- acf(mes$total, plot = FALSE)
mes_pacf <- pacf(mes$total, plot = FALSE)

par(mfrow= c(2,1), mar=c(3,3,3,3))
plot(mes_acf, main = "Função de Autocorrelação")
title(main="TS Mês")
plot(mes_pacf, main = "Autocorrelação Parcial")
title(main="TS Mês")

#estimar modelo

arima_mes <- auto.arima(mes$total, trace = TRUE, approximation = FALSE, seasonal = FALSE)
print(summary(arima_mes))

sarima_mes <- auto.arima(mes$total, trace = TRUE, approximation = FALSE, seasonal = TRUE)
print(summary(sarima_mes))

#checando resíduos
checkresiduals(arima_mes)

#fazendo forecast
fcst_mes <- forecast(arima_mes, h=3)
print(fcst_mes)
autoplot(fcst_mes)
print(summary(fcst_mes))

#ajuste holt-winters
ajuste_mes <- hw(ts_mes, seasonal ='additive')
ajuste_mes_mult <- hw(ts_mes, seasonal = 'multiplicative')
plot(ts_mes)
lines(fitted(ajuste_mes), col = 'blue')
lines(fitted(ajuste_mes_mult), col = 'red')

#seleção do melhor método de suavização
modelo_ets_mes <- ets(ts_mes)
summary(modelo_ets_mes) #ETS (A, N, N) significa que o erro é aditivo (A) e não possui tendência (N), nem sazonalidade (N). 

plot(ts_mes)
lines(fitted(modelo_ets_mes), col='red')

############################SEMANA################################

#semana

class(semana$semana)
df_semana <- semana[3:106,]
semana <- df_semana
ts_semana <- ts(semana$total, start= c(2021, 1), end= c(2022, 52), frequency = 52)
ts_semana

#decomposição 
decomp_semana <- decompose(ts_semana, "additive")

plot(as.ts(decomp_semana$seasonal))
plot(as.ts(decomp_semana$trend))
plot(as.ts(decomp_semana$random))
plot(decomp_semana)
ts_semana


#plotando autocorrelações SEMANA

semana_acf <- acf(semana$total, plot = FALSE)
semana_pacf <- pacf(semana$total, plot = FALSE)

par(mfrow= c(2,1), mar=c(3,3,3,3))
plot(semana_acf, main = "Função de Autocorrelação")
title(main="TS Semana")
plot(semana_pacf, main = "Autocorrelação Parcial")
title(main="TS Semana")

#estimar modelo

arima_semana <- auto.arima(semana$total, trace = TRUE, approximation = FALSE, seasonal = FALSE)
print(summary(arima_semana))

sarima_semana <- auto.arima(semana$total, trace = TRUE, approximation = FALSE, seasonal = TRUE)
print(summary(sarima_semana))

#checando resíduos
checkresiduals(arima_semana)

#fazendo forecast
fcst_semana <- forecast(arima_semana, h=12)
print(fcst_semana)
autoplot(fcst_semana)
print(summary(fcst_semana))


