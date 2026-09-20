# Painel de indicadores

App Shiny autonoma de consulta ao DW de indicadores (series por nivel
territorial, mapa municipal dinamico com slider de ano animado e globo
interativo das UFs), gerada por `AEDi::deploy_panel()` e
montada sobre o pacote AEDi. A paleta de cores pode ser Gov.br (azul) ou
preto e branco com o roxo da Distintive — o botao no topo troca a qualquer
momento e a escolha fica salva no navegador.

## Rodar localmente

```r
shiny::runApp("painel")
```

## Hospedar

- Shiny Server: aponte a configuracao do site para este diretorio;
- shinyapps.io / Posit Connect:

```r
rsconnect::deployApp("painel")
```

## Credenciais do DW

A conexao usa as variaveis de ambiente `user`, `password`, `host` e
`dbname` (defaults: aedi@127.0.0.1/aedidb). Em hospedagem, configure-as
no painel da plataforma ou no `.Renviron` lido pelo processo do Shiny.

Requisito: pacote AEDi instalado
('remotes::install_github("DistintiveLab/AEDi")').

