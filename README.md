### ME607 - Trabalho Final
#### Previsão de Ações - Microsoft

Neste projeto, foram analisados dados das ações da Microsoft entre o período de 2007 à 2023, a fim de realizar previsões dos retornos esperados no dia seguinte.

Uma apresentação da série, bem como a previsão mais recente, podem ser encontradas aqui: *https://gabriel-alvaro.shinyapps.io/dashboard/*.

O modelo foi escolhido por meio de uma validação cruzada, que pode ser consultada em *[garch_crossvalidation.R](https://github.com/gabriel-alvaro/microsoft-garch-forecast/blob/main/modelagem/garch_crossvalidation.R)*.
A previsão do modelo é realizada automaticamente por meio de um workflow que executa o script em *[garch_forecast.R](https://github.com/gabriel-alvaro/microsoft-garch-forecast/blob/main/modelagem/garch_forecast.R)* e armazena os resultados em csv na raiz do repositório, no padrão "*data_previsao.csv*".
O diagnóstico do modelo, bem como a análise exploratória e descritiva, foi realizada em *[modelagem_microsoft.Rmd](https://github.com/gabriel-alvaro/microsoft-garch-forecast/blob/main/modelagem/modelagem_microsoft.Rmd)*.

***

**Projeto desenvolvido por:**

 - Gabriel Alvaro Batista
 - Geovani Reolon Sousa
 - Pedro Pimentel Cabrini
