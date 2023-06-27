library(zoo)
library(dplyr)
library(tsibble)
library(rugarch)
library(readr)

# leitura dos dados
microsoft_df = quantmod::getSymbols("MSFT", src = "yahoo", auto.assign = FALSE,
                                    from = '2007-01-01', return.class = 'zoo')

log_retorno_dif = diff(log(microsoft_df[,6]))

df = fortify.zoo(log_retorno_dif)
df = df %>% as_tsibble(index = Index)

# leitura do modelo
# ARMA(2,2)-GARCH(1,1)
fit = readRDS(url("https://github.com/gabriel-alvaro/microsoft-garch-forecast/raw/main/modelagem/garch_fit.rds"))

# previsao
forecast = ugarchforecast(fit, data = df, n.ahead = 1)

######### primeira execucao #########
# gerando arquivo .csv

# forecast_data = data.frame(data = as.Date(character()),
#                            previsao_retorno = character(),
#                            previsao_sigma = character())
# 
# new_forecast = c(as.character(as.Date(colnames(forecast@forecast$seriesFor)) + 1),
#                  round(forecast@forecast$seriesFor[1], 4),
#                  round(forecast@forecast$sigmaFor[1], 4))
# 
# forecast_data[nrow(forecast_data)+1, ] = new_forecast
# forecast_data
# 
# write_csv(forecast_data, file = "../previsoes.csv", append = TRUE,
#           col_names = TRUE)

#####################################

# nova previsao
new_forecast = data.frame(data = as.Date(colnames(forecast@forecast$seriesFor)) + 1,
                          previsao_retorno = round(forecast@forecast$seriesFor[1], 4),
                          previsao_sigma = round(forecast@forecast$sigmaFor[1], 4))

# leitura da previsao antiga
forecast_data = read_csv("https://raw.githubusercontent.com/gabriel-alvaro/microsoft-garch-forecast/main/previsoes.csv",
                         show_col_types = FALSE)

# adiciona nova previsao a ultima linha do dataframe
if(as.data.frame(forecast_data)[nrow(forecast_data),1] != new_forecast[,1] && !is.na(forecast_data[1,1])){
  forecast_data = rbind(forecast_data, new_forecast)
}

if(is.na(forecast_data[1,1])){
  forecast_data = new_forecast
}

# salva o novo arquivo .csv
write_csv(forecast_data, file = "../previsoes.csv", append = FALSE, col_names = TRUE)

