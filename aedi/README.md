# App AEDi do lote

Launcher fina do **app AEDi** (autoria de indicadores e aba "Atualização")
operando sobre o lote deste projeto: `coleta/`, `controle_execucao`
(projeto `pndr_dashboard`), banco `aedidb`. Gerada à mão no padrão do
`AEDi::deploy_admin()` — correções e novas abas chegam com o reinstall do
pacote AEDi, sem cópias derivadas para divergirem.

## Rodar localmente

```r
shiny::runApp("aedi")
```

O launcher faz `setwd("..")`, então o app enxerga a raiz do projeto como
raiz de trabalho (mesma convenção do `admin/`, que usa `raiz = ".."`).
Requisitos: pacote AEDi instalado e banco aedidb alcançável com as mesmas
credenciais do orquestrador (variáveis de ambiente `user`, `password`,
`host`, `dbname`).

## Segurança

Este app **escreve** no banco e no lote de coleta (define indicadores,
gera scripts, dispara atualizações). Não publicar em host acessível
externamente sem autenticação na frente (shinymanager, shinyproxy ou
VPN) — mais restritivo até que o `admin/`, que é somente leitura.
