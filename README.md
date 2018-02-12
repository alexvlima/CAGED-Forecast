# CAGED-Forecast
## Dados do CAGED
Os dados do Cadastro Geral e Empregados e Desempregas mostra as admissões e desligamentos do mês. A periodicidade é mensal e os microdados estão disponíveis no site do Ministério do Trabalho (ftp://ftp.mtps.gov.br/pdet/microdados/).

## Modelo de Séries Temporais
O script aqui proposto faz a predição a partir de um modelo ARIMA ajustado para cada atividade econômica.

## Cluster
Ao final é realizado um clustter tendo como base se a atividade econômica irá contratar ou demitir nos próximos meses. Esse resultado é apresentado em um gráfico dinâmico (blblioteca plotly).

