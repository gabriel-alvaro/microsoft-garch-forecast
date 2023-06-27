library(zoo)
library(dplyr)
library(tsibble)
library(rugarch)
library(readr)

# leitura dos dados
microsoft_df = quantmod::getSymbols("MSFT", src = "yahoo", auto.assign = FALSE,
                                    from = '2007-01-01', to = Sys.Date() + 1, return.class = 'zoo')

log_retorno_dif = diff(log(microsoft_df[,6]))

df = fortify.zoo(log_retorno_dif)
df = df %>% as_tsibble(index = Index)

# leitura do modelo
# ARMA(2,2)-GARCH(1,1)
fit = readRDS(url("https://github.com/gabriel-alvaro/microsoft-garch-forecast/raw/main/modelagem/garch_fit.rds"))

# previsao
forecast = ugarchforecast(fit, data = df, n.ahead = 1)

# nova previsao
new_forecast = data.frame(data = as.Date(colnames(forecast@forecast$seriesFor)) + 1,
                          previsao_retorno = round(forecast@forecast$seriesFor[1], 4),
                          previsao_sigma = round(forecast@forecast$sigmaFor[1], 4))

# salva o novo arquivo .csv
write_csv(new_forecast, file = paste0(new_forecast$data, "_previsao.csv"), 
          append = FALSE, col_names = TRUE)

