library(ggfortify)
library(fpp3)
library(dygraphs)
library(tsibble)
library(zoo)
library(rugarch)
library(forecast)

# leitura dos dados
microsoft_df = quantmod::getSymbols("MSFT", src = "yahoo", auto.assign = FALSE,
                                    from = '2007-01-01', return.class = 'zoo')
microsoft = microsoft_df[,6]
microsoft_retorno = diff(microsoft)

log_retorno_dif = diff(log(microsoft))

df = fortify.zoo(log_retorno_dif)
df_tsible = df %>% as_tsibble(index = Index)

## cria modelos AR(1,1)-GARCH(1,1) até AR(2,2)-GARCH(2,2)
pars = expand.grid(ar = 1:2, ma = 1:2, alpha = 1:2, beta = 1:2)
models = list()

for(i in 1:nrow(pars)){
  models[[i]] = ugarchspec(mean.model = list(armaOrder = unlist(pars[i,1:2]), include.mean = TRUE),
                           variance.model = list(model = 'sGARCH', garchOrder = unlist(pars[i, 3:4])),
                           distribution = 'std')
}

##############################################################
######            CROSS VALIDATION (RUGARCH)            ###### 
##############################################################

fit = list()
fore = list()

## ajuste dos modelos
## note que o pacote ugarch faz um cross validation com rolling windows
## parametro out.sample retira as observações após 4000
## parametro n.roll indica que o conjunto de dados irá rolar até a ultima observação disponivel
##  (que foi deixada fora do conjunto de treino)

for (i in 1:length(models)){
  fit[[i]] = ugarchfit(models[[i]], df_tsible, solver = 'hybrid', 
                       out.sample = nrow(df_tsible)-4000)
  
  fore[[i]] = ugarchforecast(fit[[i]], data = df_tsible,
                             n.ahead = 1, n.roll = nrow(df_tsible)-4000, out.sample = nrow(df_tsible)-4000)
}

# calculando erros (predito - observado)
errors2_garch = data.frame(matrix(rep(0, 16*(nrow(df_tsible)-4000)), 
                                  nrow = 16))
colnames(errors2_garch) = paste0("errors2_garch", 1:(nrow(df_tsible)-4000))

for(j in 1:length(models)){
  for (i in 1:(nrow(df_tsible)-4000)){
    errors2_garch[j,] = ((fore[[j]]@forecast$seriesFor[1,] - tail(df_tsible, 146)[,2])^2)[,1]
  }
}

## calculando RMSE
rmse_garch = data.frame(modelo = paste0("Model", 1:length(models)),
                        RMSE = rep(0,16))

for(i in 1:length(models)){
  rmse_garch[i,2] = sqrt(mean(unlist(errors2_garch[i,])))
}

rmse_garch %>%
  arrange(RMSE)

## modelo 4, 8 e 16 foram dados como melhores (RMSE muito proximos)
## modelo 4 = AR(2,2)-GARCH(1,1)
## modelo 8 = AR(2,2)-GARCH(2,1)
## modelo 16 = AR(2,2)-GARCH(2,2)
## optamos pelo modelo 4 por ser mais simples e apresentar menor RMSE

# ajusta e salva o modelo final
fit = ugarchfit(models[[4]], df_tsible, solver = 'hybrid')
saveRDS(fit, file = "garch_fit.rds")

###############################################################
######      CROSS VALIDATION (IMPLEMENTACAO MANUAL)      ###### 
###############################################################

# ## cria conjuntos de treino e teste
# train = df_tsible %>%
#   stretch_tsibble(.step = 1, .init = 4100)
# 
# test = df_tsible %>% 
#   tail(nrow(df_tsible) - 4100)
#
# train %>% group_by(`.id`) %>% count()
# errors2 = data.frame(matrix(rep(0, 16*(nrow(df_tsible)-4100)),
#                             nrow = 16))
# 
# colnames(errors2) = paste0("errors2_", 1:(nrow(df_tsible)-4100))
# 
# for (j in 1:length(models)){
#   for(i in 1:(nrow(df_tsible)-4100)){
#     fit = ugarchfit(models[[j]],
#                     train %>% filter(`.id` == i) %>% select(-`.id`),
#                     solver = 'hybrid')
#     
#     fore = ugarchforecast(fit, n.ahead = 1)
#     
#     errors2[j,i] = (fore@forecast$seriesFor[1] - test[i,2])^2
#   }
# }
# 
# ## calculando RMSE
# rmse = data.frame(row.names = paste0("Model", 1:16))
# for(i in 1:length(models)){
#   rmse[i,1] = sqrt(mean(unlist(errors2[i,])))
# }
# 
# rmse = rmse %>%
#   rename(RMSE = V1) %>%
#   arrange(RMSE)
# 
# rmse
