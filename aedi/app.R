# App AEDi (autoria de indicadores + atualizacao do lote) do projeto pndr_dashboard
# Launcher fina no padrao do deploy_admin(): atualizacoes chegam com o
# reinstall do pacote AEDi. Rodar da raiz do projeto com shiny::runApp("aedi").
# O setwd abaixo faz o app operar sobre a RAIZ do projeto (lote coleta/,
# controle_execucao com projeto = pndr_dashboard), como o admin ao lado.
# Credenciais do aedidb (variaveis de ambiente): user, password, host, dbname.
# ATENCAO: app de AUTORIA, escreve no banco e no lote — somente localhost/LAN;
# nao publicar em host acessivel externamente sem autenticacao na frente.
setwd("..")
options(golem.app.prod = TRUE)
AEDi::run_app()
