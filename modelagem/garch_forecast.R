### previsão para proxima data
# leitura dos dados
microsoft_df = quantmod::getSymbols("MSFT", src = "yahoo", auto.assign = FALSE,
                                    from = '2007-01-01', return.class = 'zoo')

log_retorno_dif = diff(log(microsoft_df[,6]))

df = fortify.zoo(log_retorno_dif)
df = df %>% as_tsibble(index = Index)

# leitura do modelo
fit = readRDS(url("https://github.com/gabriel-alvaro/microsoft-garch-forecast/raw/main/modelagem/garch_fit.rds"))

# previsao
forecast = ugarchforecast(fit, data = df_tsibble, n.ahead = 1)

# previsao da serie
forecast@forecast$seriesFor

# previsao da volatilidade
forecast@forecast$sigmaFor
