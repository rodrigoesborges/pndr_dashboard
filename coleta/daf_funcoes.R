#' @title Puxar Dados DAF de 1 Município e 1 Mês da API do BB
#'
#' @description Faz uma única requisição POST para a API
#'   demonstrativos.api.daf.bb.com.br para obter dados de arrecadação federal (DAF)
#'   para um município e um período de data específicos. Não faz loops nem consolida.
#'
#' @param city_code O código de município (caractere) a ser consultado.
#' @param anomes período (formato "YYYYMM").
#' @param fundo codigo do fundo (numerico, padrão 51 = simples nacional)
#'
#' @return Uma lista R contendo os dados JSON retornados pela API,
#'   ou NULL em caso de falha na requisição ou processamento.
#' @export
#'
#' @details **AVISO DE SEGURANÇA:** Esta função desabilita a verificação de
#'   certificado SSL para replicar o comportamento do script Python original.
#'   Isso torna a comunicação menos segura e não é recomendado em produção.
#'   Use por sua conta e risco, ou remova o argumento `config` das chamadas
#'   `httr::POST` se a API tiver um certificado SSL válido.
#'
#' @examples
#' \dontrun{
#' # Exemplo de uso (substitua o código e as datas conforme necessário):
#' # codigo_exemplo <- "303" # Exemplo: Alta Floresta d'Oeste - RO
#' # anomes_exemplo <- 202301
#' # dados_single_req <- baixa_daf_mes(codigo_exemplo,
#' #                                          anomes_exemplo)
#' #
#' # if (!is.null(dados_single_req)) {
#' #   print("Dados recebidos com sucesso:")
#' #   print(dados_single_req)
#' # } else {
#' #   message("Falha ao obter dados.")
#' # }
#' }
baixa_daf_mes <- function(city_code, anomes,fundo= 51) {
  # Verificar se os pacotes necessários estão instalados e carregados
  if (!requireNamespace("httr", quietly = TRUE)) {
    warning("Package \"httr\" needed. Please install it.", call. = FALSE)
    return(NULL)
  }
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    warning("Package \"jsonlite\" needed. Please install it.", call. = FALSE)
    return(NULL)
  }

  # URL do endpoint da API
  url <- "https://demonstrativos.api.daf.bb.com.br/v1/demonstrativo/daf/consulta"

  # Cabeçalhos da requisição (replicados do script Python)
  headers <- c(
    "Accept" = "application/json, text/plain, */*",
    "Accept-Language" = "pt-BR,pt;q=0.9,en;q=0.8,en-GB;q=0.7,en-US;q=0.6",
    "Connection" = "keep-alive",
    "Content-Type" = "application/json",
    "Origin" = "https://demonstrativos.apps.bb.com.br",
    "Referer" = "https://demonstrativos.apps.bb.com.br/",
    "Sec-Fetch-Dest" = "empty",
    "Sec-Fetch-Mode" = "cors",
    "Sec-Fetch-Site" = "same-site",
    "User-Agent" = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/126.0.0.0 Safari/537.36 Edg/126.0.0.0"
  )

  datain <- as.Date(paste0(anomes,"01"),tryFormats='%Y%m%d')
  datafin <- datain
  lubridate::month(datafin) <- lubridate::month(datafin)+1
  datafin <- datafin-1

  # Função para formatar a data (replicada do script Python)
  format_date_daf <- function(date_str) {
    # Assume que a entrada é "YYYY-MM-DD" e formata para "DD.MM.YYYY"
    date_obj <- as.Date(date_str, format = "%Y-%m-%d")
    format(date_obj, "%d.%m.%Y")
  }
  # Montar o payload da requisição
  payload <- list(
    codigoBeneficiario = as.character(city_code), # Garante que é string
    codigoFundo = fundo,
    dataInicio = format_date_daf(datain),
    dataFim = format_date_daf(datafin)
  )

  # Realizar a requisição POST
  # *** Desabilitando verificação SSL - inseguro! ***
  response <- tryCatch({
    httr::POST(url,
               httr::add_headers(.headers = headers),
               body = jsonlite::toJSON(payload, auto_unbox = TRUE),
               encode = "json",
               config = httr::config(ssl_verifypeer = 0, ssl_verifyhost = 0)
    )
  }, error = function(e) {
    # Captura erros na requisição (ex: problemas de conexão)
    warning(paste0("Erro na requisicao: ", e$message), call. = FALSE)
    return(NULL) # Retorna NULL em caso de erro
  })

  # Processar a resposta se a requisição foi bem sucedida (response não é NULL)
  if (!is.null(response)) {
    if (httr::status_code(response) == 200) {
      # Tentar parsear o JSON da resposta
      json_data <- tryCatch({
        httr::content(response, "parsed", encoding = "UTF-8")
      }, error = function(e) {
        # Captura erros ao parsear JSON
        warning(paste0("Erro ao parsear JSON: ", e$message), call. = FALSE)
        return(NULL) # Retorna NULL em caso de erro
      })

      # Retorna os dados JSON parseados ou NULL se o parse falhou
      resultado <-
        data.frame(
          ano_mes=datain,
          uf = gsub(".*-(..)$","\\1",json_data$quantidadeOcorrencia[[1]]$nomeBeneficio),
          codmun_bb = city_code,
          nomemun_bbdaf = gsub("\\s\\s+-..$","",json_data$quantidadeOcorrencia[[1]]$nomeBeneficio),
          siglafundo = gsub("\\s+.*$","",json_data$quantidadeOcorrencia[[3]]$nomeBeneficio),
          nomefundo = gsub(".*-\\s+([^ ].*)$","\\1",json_data$quantidadeOcorrencia[[3]]$nomeBeneficio),
          valor= readr::parse_number(
            gsub(".* ([^ ]+)C$","\\1",
                 json_data$quantidadeOcorrencia[[length(json_data$quantidadeOcorrencia)]]$nomeBeneficio),
          locale= readr::locale(decimal_mark = ",",grouping_mark = "."))
        )
      return(resultado)

    } else {
      # Status code não é 200 (ex: 404, 500)
      warning(paste0("Falha na API. Status Code: ", httr::status_code(response)), call. = FALSE)
      return(NULL) # Retorna NULL em caso de status code diferente de 200
    }
  } else {
    # Resposta NULL devido a erro na requisição capturado pelo tryCatch
    return(NULL) # Já tratado no tryCatch, mas explicitamente aqui
  }
}

baixa_ano_simples_mun <- \(codmun,ano){
  data.table::rbindlist(lapply(paste0(ano,sprintf("%02d",1:12)),\(x) baixa_daf_mes(codmun,x)))
                        }


consolida_simples_anual_mun <- \(codmun,anos) {
  simplesano <- \(ano) {
    baixa_ano_simples_mun(codmun,ano)|>
      dplyr::mutate(ano=lubridate::year(ano_mes))|>
      dplyr::group_by(ano,uf,codmun_bb,nomemun_bbdaf,siglafundo,nomefundo)|>
      dplyr::summarize(across(valor,sum))

  }
  data.table::rbindlist(lapply(anos,simplesano))
}

codigosmuns <- data.table::fread('coleta/cache/metadados_dafbb/codigos_municipios_beneficiarios.csv')

simples_muns2024 <-
  data.table::rbindlist(
    lapply(codigosmuns$codigo_beneficiario_saida[c(1:5570)],\(x){print(x)
      consolida_simples_anual_mun(x,2024)}))



mun2024_simp_prb <-
  data.table::rbindlist(
    lapply(codigosmuns$codigo_beneficiario_saida[c(645,795)],\(x){
      baixa_ano_simples_munprob(x,2024)|>
        dplyr::mutate(ano=lubridate::year(ano_mes))|>
        dplyr::group_by(ano,uf,codmun_bb,nomemun_bbdaf,siglafundo,nomefundo)|>
        dplyr::summarize(across(valor,sum))}))

todos_muns2022 <-
  data.table::rbindlist(
    lapply(codigosmuns$codigo_beneficiario_saida[1:100],\(x){print(x)
      consolida_simples_anual_mun(x,2022)}))

todos_muns2022_p1 <-
  data.table::rbindlist(
    lapply(codigosmuns$codigo_beneficiario_saida[1:99+100],\(x){print(x)
      consolida_simples_anual_mun(x,2022)}))

todos_muns2022_p2 <-
  data.table::rbindlist(
    lapply(codigosmuns$codigo_beneficiario_saida[200:500],\(x){print(x)
      consolida_simples_anual_mun(x,2022)}))

todos_muns2022_p3 <-
  data.table::rbindlist(
    lapply(codigosmuns$codigo_beneficiario_saida[501:600],\(x){print(x)
      consolida_simples_anual_mun(x,2022)}))

todos_muns2022_p3b <-
  data.table::rbindlist(
    lapply(codigosmuns$codigo_beneficiario_saida[601:640],\(x){print(x)
      consolida_simples_anual_mun(x,2022)}))

todos_muns2022_p3b2 <-
  data.table::rbindlist(
    lapply(codigosmuns$codigo_beneficiario_saida[641:644],\(x){print(x)
      consolida_simples_anual_mun(x,2022)}))

todos_muns2022_p3b3 <-
  data.table::rbindlist(
    lapply(codigosmuns$codigo_beneficiario_saida[646:650],\(x){print(x)
      consolida_simples_anual_mun(x,2022)}))


todos_muns2022_p3c <-
  data.table::rbindlist(
    lapply(codigosmuns$codigo_beneficiario_saida[651:749],\(x){print(x)
      consolida_simples_anual_mun(x,2022)}))

todos_muns2022_p3d <-
  data.table::rbindlist(
    lapply(codigosmuns$codigo_beneficiario_saida[750:789],\(x){print(x)
      consolida_simples_anual_mun(x,2022)}))

todos_muns2022_p3e <-
  data.table::rbindlist(
    lapply(codigosmuns$codigo_beneficiario_saida[790:794],\(x){print(x)
      consolida_simples_anual_mun(x,2022)}))

todos_muns2022_p3f <-
  data.table::rbindlist(
    lapply(codigosmuns$codigo_beneficiario_saida[796:800],\(x){print(x)
      consolida_simples_anual_mun(x,2022)}))

#Problemas em 645(07/2022) e 795
baixa_ano_simples_munprob <- \(codmun,ano){
  data.table::rbindlist(lapply(paste0(ano,sprintf("%02d",c(1:6,8:12))),\(x) baixa_daf_mes(codmun,x)))
}
todos_muns2022_pprob <-
  data.table::rbindlist(
    lapply(codigosmuns$codigo_beneficiario_saida[c(645,795)],\(x){
      baixa_ano_simples_munprob(x,2022)|>
        dplyr::mutate(ano=lubridate::year(ano_mes))|>
        dplyr::group_by(ano,uf,codmun_bb,nomemun_bbdaf,siglafundo,nomefundo)|>
        dplyr::summarize(across(valor,sum))}))



todos_muns_2022_p4 <-
  data.table::rbindlist(
    lapply(codigosmuns$codigo_beneficiario_saida[801:1600],\(x){print(x)
      consolida_simples_anual_mun(x,2022)}))

todos_muns_2022_p5 <-
  data.table::rbindlist(
    lapply(codigosmuns$codigo_beneficiario_saida[1601:4000],\(x){print(x)
      consolida_simples_anual_mun(x,2022)}))

todos_muns_2022_p6 <-
  data.table::rbindlist(
    lapply(codigosmuns$codigo_beneficiario_saida[4001:5570],\(x){print(x)
      consolida_simples_anual_mun(x,2022)}))


simples_mun_2022 <-
  data.table::rbindlist(mget(ls(pattern = "todos_muns_*")))
