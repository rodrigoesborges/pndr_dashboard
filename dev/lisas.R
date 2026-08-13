
rm(list = ls(all = TRUE))

options(scipen = 999L)

if(!"geobr" %in% rownames(installed.packages())){
  install.packages("geobr", dependencies=TRUE)
}
library(geobr)

if(!"sf" %in% rownames(installed.packages())){
  install.packages("sf", dependencies=TRUE)
}
library(sf)

if(!"readxl" %in% rownames(installed.packages())){
  install.packages("readxl", dependencies=TRUE)
}
library(readxl)

if(!"dplyr" %in% rownames(installed.packages())){
  install.packages("dplyr", dependencies=TRUE)
}
library(dplyr)

if(!"tidyr" %in% rownames(installed.packages())){
  install.packages("tidyr", dependencies=TRUE)
}
library(tidyr)

if(!"tmap" %in% rownames(installed.packages())){
  install.packages("tmap", dependencies=TRUE)
}
library(tmap)

if(!"rgeoda" %in% rownames(installed.packages())){
  install.packages("rgeoda", dependencies=TRUE)
}
library(rgeoda)

if(!"ggplot2" %in% rownames(installed.packages())){
  install.packages("ggplot2", dependencies=TRUE)
}
library(ggplot2)

if(!"writexl" %in% rownames(installed.packages())){
  install.packages("writexl", dependencies=TRUE)
}
library(writexl)

mdr <- DBI::dbConnect(
  RPostgres::Postgres(),
  user=Sys.getenv('userdb'),
  password = Sys.getenv('passwddbdev'),
  host = Sys.getenv('hostdbdev'),
  dbname=Sys.getenv('tdbname'))



# Leitura dos dados dos municípios para o ano de 2017 e criação de uma nova coluna com códigos municipais de 6 dígitos
muni_shp <- read_municipality(year = 2022)
muni_shp$code_muni6 <- substr(muni_shp$code_muni, 1, 6)
summary(muni_shp)

# Leitura dos dados da PNAD e dos estratos dos municípios
pnad_shp <- read_sf(paste0(path, "/1_data/0_geral/pnad_shp/pnad_estratos.shp"))
mun_estratos <- read.csv2(paste0(path, "/1_data/0_geral/Municipios_por_Estratos.csv"))
mun_estratos$code_muni6 <- substr(as.character(mun_estratos$Código.do.Município), 1, 6)

# Cálculo dos pesos de vizinhança do tipo Queen
queen_w <- queen_weights(muni_shp)
summary(queen_w)



#objetivo4



# Definição das variáveis correspondentes aos arquivos
variaveis = c( "objetivo4_")

titulos <- c(
  "Ind. Composto Objetivo 4",
  "Especialização agro", "Especialização mineração", "Divers. econômica"
)

# # Títulos das variáveis para uso em gráficos e tabelas
# titulos = c("Escolas com esgotamento (%)", "Escolas com internet (%)", "Matrículas EPT (/pop.)", "Ideb Educ. Básica",
#             "Empres. Biotec", "Emprego C&T", "Estabelecimentos C&T", "Patentes (por 100mil hab.)",
#             "Complexidade", "%_Industria", "Salário", "Escala Produtiva",
#             "Abastecimento (%)", "Internet alta veloc. (%)", "Internações - saneamento", "Inv. em Infraestrutura",
#             "Desnutrição (%)", "Baixa Renda no CadUnico (%)", "Distorção idade-série", "Diferencial salarial mulheres",
#             "Emp. reciclagem/resíduos", "Desmatamento (%)", "Variação desmatamento", "Emissões",
#             "Dirigentes púb. c/ ens. sup. (%)", "Servidores com ens. superior (%)", "Salário adm. pública", "Sustent. fiscal",
#             "Diferencial Renda", "Diferencial Educação", "Diferencial Saúde", "NA",
#             "Centralidade", "Primazia populacional", "Primazia econômica", "NA",
#             "Qualif. do emp. formal", "Salário médio", "Variação populacional", "NA",
#             "Especialização agro", "Especialização mineração", "Divers. econômica", "NA",
#             "Anos de estudo", "Cientistas e intelectuais (%)", "Taxa de desocupação", "Domicílios com internet", "Rendimento de todos os trabalhos", "Domicílios com esgotamento", "Escolaridade adm. pub. municipal")

# Variáveis cujos rankings devem ser invertidos (quanto maior valor, pior)
inverte_rank = c(
  #"infra3", "infra4", "dessoc", "sust2", "sust3", "sust4",
                 "objetivo4_1", "objetivo4_2"
                 #, "pnadc3"
                 )

# Títulos dos componentes para uso em gráficos e tabelas
titulos_comp = c(
  #"Educação", "C&T", "Desenv. Produtivo", "Infraestrutura",
  #               "Desenv. Social", "Sustentabilidade", "Cap. Governativas",
  #               "Convergência", "Cidades Intermediadoras", "Competitividade",
  "Diversificação")

# Anos a serem considerados na análise
anos = 2013:2024

# Lista para armazenar avisos
avisos = list()


indicadores_mapalisa <-
  c(paste0(variaveis,1:3),paste0("comp_",gsub("_","",variaveis)))

# Loop Principal ####
#for (i in seq_along(indicadores_mapalisa)) {
  # Leitura dos dados
#  data = readRDS(paste0(path, "/visualiza/", arquivos[i], ".RDS"))
#  cat("Summary de", arquivos[i], ":\n")
#  print(summary(data))
#  print(table(data$ano))
#  print(table(data$variavel))
#  cat("\n")

  data <- DBI::dbGetQuery(mdr,
                          paste0("select orig_name variavel,extract(year from refdate) ano, geoloc_id, value from data_values a left join mdata b on
                          a.mdata_id = b.mdata_id left join local c on a.local_id = c.local_id WHERE orig_name IN ('",
                                 paste0(indicadores_mapalisa,collapse="','"),"')"))


  # Obter o ano máximo para cada variável
  ano_max = aggregate(ano ~ variavel, data = data, FUN = max)
  ano_max$ano[ano_max$ano > max(anos)] <- max(anos)
  anos <- anos[anos >= min(data$ano)]

  # Transformar os dados de formato longo para formato largo
  data_w = data %>%
    pivot_wider(names_from = c(variavel, ano), values_from = value)
  summary(data_w)

  # Adicionar colunas faltantes para os anos definidos
  for (variable in colnames(data_w)[-1]) {
    suffix <- gsub("[0-9]*$", "", variable)
    for (year in anos) {
      if (!(paste0(suffix, year) %in% colnames(data_w))) {
        data_w[[paste0(suffix, year)]] <- NA
      }
    }
  }
  data_w <- data_w[, order(names(data_w))]

  # Substituir valores faltantes
  for (col in colnames(data_w)[-1]) {
    var_col <- var(data_w[[col]], na.rm = T)
    if (var_col == 0 || is.na(var_col)) {
      year_index <- which(grepl(col, colnames(data_w)))
      prev_year_col <- colnames(data_w)[year_index - 1]
      if (substring(prev_year_col, 1, regexpr("_\\d{4}", prev_year_col) - 1) ==
          substring(col, 1, regexpr("_\\d{4}", col) - 1)) {
        data_w[[col]] <- data_w[[prev_year_col]]
        avisos[[col]] = paste0("Valores de ", col, " foram substituídos por ", colnames(data_w)[year_index - 1])
      }
    }
  }

  # Condicional para merge dos dados
  # if (i <= 11) {
    data_shp <- merge(muni_shp, data_w, by.x = "code_muni", by.y = "geoloc_id")
  # } else {
  #   data_shp <- merge(pnad_shp, data_w, by.x = "Cdgdest", by.y = "cod_estrato")
  # }

  ## Mapas ####
 #for (i in seq_along(indicadores_mapalisa)) {
  for (k in seq_along(ano_max$variavel)) {
    # Criação do mapa para cada variável e ano máximo
    map <- tm_shape(data_shp) +
      tm_polygons(col = paste0(ano_max$variavel[k], "_", ano_max$ano[k]),
                  style = "fisher", n = 5, palette = "-RdYlBu",
                  title = paste0(titulos[ k], " (", ano_max$ano[k], ")"), border.col = NA) +
      tm_layout(legend.position = c("left", "bottom"))

    # Salvar o mapa em um arquivo PNG
    png_file <- paste0("visualiza/fig/map_", variaveis, k, ".png")
    png(png_file, width = 15, height = 15, units = "cm", res = 300, bg = "transparent")
    print(map)
    dev.off()
  }

  # LISA ####
  for (k in seq_along(ano_max$variavel)) {
    # Preparação dos dados para análise LISA
    data_shp_temp = #na.omit(
      data_shp[paste0(ano_max$variavel[k], "_", ano_max$ano[k])]
      #)
    queen_w <- queen_weights(data_shp_temp)

    # Cálculo da estatística LISA
    lisa <- local_moran(queen_w, data_shp_temp[paste0(ano_max$variavel[k], "_", ano_max$ano[k])])
    lisa_colors <- lisa_colors(lisa)
    lisa_labels <- c("Não significativo", "Alto-Alto", "Baixo-Baixo", "Baixo-Alto", "Alto-Baixo", "Indefinido", "Isolado")
    lisa_clusters <- lisa_clusters(lisa)
    lisa$labels <-  c("Não significativo", "Alto-Alto", "Baixo-Baixo", "Baixo-Alto", "Alto-Baixo", "Indefinido", "Isolado")

    data_ggplot <- data_shp_temp|>st_drop_geometry()|>mutate(code_muni=data_shp$code_muni)|>
      dplyr::mutate(cluster_num=lisa_clusters+1,cluster=factor(lisa_labels[cluster_num],levels=lisa_labels))|>
      right_join(data_shp|>select(code_muni,paste0(ano_max$variavel[k], "_", ano_max$ano[k])))|>st_as_sf()

    # Salvar o mapa LISA em um arquivo PNG
    png_file <- paste0("visualiza/fig/lisa_", variaveis, k, ".png")
    #png(png_file, width = 15, height = 15, units = "cm", res = 300, bg = "transparent")

    mapalisa <- ggplot(data_ggplot)+
      geom_sf(aes(fill=cluster),linewidth=0.1,color="lightgrey")+
      scale_fill_manual(values=c("white",RColorBrewer::brewer.pal(4,"RdBu"),"grey"))+
      theme_minimal()+
      theme(legend.position="bottom")+
      ggtitle(paste0(titulos[k]," (",ano_max$ano[k],")"),subtitle = paste0("I de Moran Global: ",round(mean(lisa$lisa_vals),2)))

      ggsave(filename=png_file, plot=mapalisa,width=15,height=15,units="cm",dpi=300,bg="transparent")
    # plot(st_geometry(data_shp), col = "#464646", border = NA)
    # plot(st_geometry(data_shp_temp),
    #      col = sapply(lisa_clusters, function(x) { return(lisa_colors[[x + 1]]) }),
    #      border = NA, add = TRUE)
    # title(main = paste0(titulos[k], " (", ano_max$ano[k], ")"), sub = paste0("I de Moran Global: ", round(mean(lisa$lisa_vals), 2)))
    # legend('bottomleft', legend = lisa_labels, fill = lisa_colors, border = "#eeeeee")

  #  dev.off()
  }

  # Criação de arquivos PNG para painéis LISA
  png_file <- paste0("visualiza/fig/lisa_panel_", variaveis, ".png")
#  if (i <= 11) {
    png(png_file, width = 14, height = 14, units = "cm", res = 300, bg = "transparent")
    par(mfrow = c(2, 2), mar = c(1, 1, 1, 1))
#  } else {
#    png(png_file, width = 28, height = 14, units = "cm", res = 300, bg = "transparent")
#    par(mfrow = c(2, 4), mar = c(1, 1, 1, 1))
#  }

  # Loop para gerar mapas LISA em painel
  for (k in seq_along(ano_max$variavel)) {
    data_shp_temp = na.omit(data_shp[paste0(ano_max$variavel[k], "_", ano_max$ano[k])])
    queen_w <- queen_weights(data_shp_temp)
    lisa <- local_moran(queen_w, data_shp_temp[paste0(ano_max$variavel[k], "_", ano_max$ano[k])])
    lisa_colors <- lisa_colors(lisa)
    lisa_labels <- c("Não significativo", "Alto-Alto", "Baixo-Baixo", "Baixo-Alto", "Alto-Baixo", "Indefinido", "Isolado")
    lisa_clusters <- lisa_clusters(lisa)

    plot(st_geometry(data_shp), col = "#464646", border = NA)
    plot(st_geometry(data_shp_temp),
         col = sapply(lisa_clusters, function(x) { return(lisa_colors[[x + 1]]) }),
         border = NA, add = TRUE)
    title(main = paste0(titulos[k], " (", ano_max$ano[k], ")"),
          sub = paste0("I de Moran Global: ", round(mean(lisa$lisa_vals), 2)), cex.main = 0.8)
    legend('bottomleft', legend = lisa_labels, fill = lisa_colors, border = "#eeeeee", cex = 0.6)
  }
  dev.off()

  # Indices Compostos ####
  rankings <- list()
  for (var in names(data_shp)[grepl(paste0("^", variaveis), names(data_shp))]) {
    # Calcula o ranking das variáveis
    rankings[[var]] <- rank(data_shp[[var]], na.last = "keep", ties.method = "min")
    rankings[[var]] <- ((rankings[[var]] - 1) / (max(rankings[[var]], na.rm = TRUE) - 1))

    # Inverte o ranking para variáveis específicas
    if (any(startsWith(var, inverte_rank))) {
      rankings[[var]] <- 1 - rankings[[var]]
    }
  }

  # Conversão da lista de rankings para um dataframe
  rankings <- as.data.frame(rankings)

#  if (i <= 11) { ## Municípios ####
    # Criação de um dataframe para armazenar as médias
    averages_df <- data.frame(matrix(nrow = nrow(rankings), ncol = 0))

    # Cálculo das médias por ano
    for (year in anos) {
      var_columns <- list()
      for (k in seq_along(ano_max$variavel)) {
        var_columns[[paste0("var", k, "_columns")]] <- grep(paste0(variaveis, k, "_", year), colnames(rankings), value = TRUE)
      }
      averages_df[paste0("comp_", variaveis, "_", year)] <- rowMeans(rankings[, unlist(var_columns)], na.rm = TRUE)
    }

    # Normalização das médias para o intervalo [0, 1]
    for (var in names(averages_df)) {
      averages_df[[var]] <- ((averages_df[[var]] - min(averages_df[[var]], na.rm = TRUE)) / (max(averages_df[[var]], na.rm = TRUE) - min(averages_df[[var]], na.rm = TRUE)))
    }

    # Combinação dos dados com os shapefiles
    data_shp2 <- cbind(data_shp, averages_df)

    # Criação do mapa composto
    map <- tm_shape(data_shp2) +
      tm_fill(col = paste0("comp_", variaveis, "_", min(ano_max$ano)), style = "fisher", n = 5, palette = "-RdYlBu",
              title = paste0(titulos_comp[k], " (", min(ano_max$ano), ")"), border.col = NA) +
      tm_layout(legend.position = c("left", "bottom"))

    # Salvar o mapa composto em um arquivo PNG
    png(paste0("visualiza/fig/map_comp_", variaveis, min(ano_max$ano), ".png"), width = 15, height = 15, units = "cm", res = 300, bg = "transparent")
    print(map)
    dev.off()

    # Análise LISA para o índice composto
    data_shp_temp = na.omit(data_shp2[paste0("comp_", variaveis, "_", min(ano_max$ano))])
    queen_w <- queen_weights(data_shp_temp)
    lisa <- local_moran(queen_w, data_shp_temp[paste0("comp_", variaveis, "_", min(ano_max$ano))])
    lisa_colors <- lisa_colors(lisa)
    lisa_labels <- c("Não significativo", "Alto-Alto", "Baixo-Baixo", "Baixo-Alto", "Alto-Baixo", "Indefinido", "Isolado")
    lisa_clusters <- lisa_clusters(lisa)

    # Salvar o mapa LISA composto em um arquivo PNG
    png_file <- paste0("visualiza/fig/lisa_comp_", variaveis, min(ano_max$ano), ".png")
    png(png_file, width = 15, height = 15, units = "cm", res = 300, bg = "transparent")
    plot(st_geometry(data_shp), col = "#464646", border = NA)
    plot(st_geometry(data_shp_temp),
         col = sapply(lisa_clusters, function(x) { return(lisa_colors[[x + 1]]) }),
         border = NA, add = TRUE)
    title(main = paste0(titulos_comp[k], " (", min(ano_max$ano), ")"), sub = paste0("I de Moran Global: ", round(mean(lisa$lisa_vals), 2)))
    legend('bottomleft', legend = lisa_labels, fill = lisa_colors, border = "#eeeeee")
    dev.off()


    #}

  ## Gráfico cidades selecionadas ####
  selected_munis <- c("Manaus", "Belo Horizonte", "Recife", "Porto Alegre", "Cuiabá", "São Paulo")
  filtered_data <- data_shp2 %>%
    subset(name_muni %in% selected_munis) %>%
    select(c("name_muni", colnames(data_shp2)[grep(paste0("^comp_", variaveis, "_"), colnames(data_shp2))]))

  long_data <- reshape(filtered_data,
                       varying = colnames(filtered_data)[grep(paste0("^comp_", variaveis, "_"), colnames(filtered_data))],
                       v.names = paste0("comp_", variaveis),
                       timevar = "year",
                       times = anos,
                       direction = "long")

  # Plot do gráfico de linhas
  png(paste0("visualiza/fig/graf_comp_", variaveis, ".png"), width = 15, height = 15, units = "cm", res = 300, bg = "transparent")
  map = ggplot(long_data, aes(x = year, y = get(paste0("comp_", variaveis)), group = name_muni, color = name_muni)) +
    geom_smooth(method = "loess", se = FALSE) +
    labs(title = paste("Evolução do Índice de", titulos_comp[i]),
         x = "Ano",
         y = titulos_comp[i],
         color = "Município") +
    scale_x_continuous(breaks = anos) +
    theme_minimal()
  print(map)
  dev.off()


  #}

# else { # PNAD: Índices compostos para os dados da PNAD ####
#   merge_by_code_muni6 <- function(df1, df2) { # Função para ler arquivos de indicadores municipais
#     merged <- merge(df1, df2, by = "code_muni6", all = TRUE)
#     return(merged)
#   }

  weighted_mean <- function(values, weights) { # Função para média ponderada
    sum(values * weights, na.rm = TRUE) / sum(weights, na.rm = TRUE)
  }

  list_of_compostos <- list()
  for (j in seq(1:7)) {
    data <- read_excel(paste0(path, "/visualiza/", arquivos[j], ".xlsx"))
    comp_data <- data %>% select(matches("^comp_|code_muni6"))
    list_of_compostos[[arquivos[j]]] <- comp_data
  }

  rankings$Cdgdest <- data_shp$Cdgdest
  pop <- readRDS(paste0(path, "/1_data/0_geral/pop_datasus_municipios.RDS")) %>%
    pivot_wider(names_from = ano, values_from = pop, names_prefix = "pop") %>%
    select(all_of(c("codmun", paste0("pop", anos))))

  compostos <- Reduce(merge_by_code_muni6, list_of_compostos) %>%
    merge(mun_estratos, by = "code_muni6") %>%
    merge(pop, by.x = "code_muni6", by.y = "codmun") %>%
    group_by(`Código.do.estrato`) %>%
    summarise(across(starts_with("comp_"),
                     .fns = ~ {
                       year <- as.numeric(sub(".*_(\\d+)$", "\\1", cur_column()))
                       if (year %in% anos) {
                         weight_col <- paste0("pop", year)
                         weighted_mean(., get(weight_col))
                       } else {
                         NA
                       }
                     },
                     .names = "{.col}")) %>%
    select(where(~ !all(is.na(.)))) %>%
    merge(rankings, by.x = "Código.do.estrato", by.y = "Cdgdest")

  # Criação de DataFrame para as Médias
  averages_df <- data.frame(Cdgdest = data_shp$Cdgdest)
  for (year in anos) {
    for (k in seq_along(ano_max$variavel)) {
      averages_df[paste0("comp_", variaveis, k, "_", year)] <-
        (compostos[paste0(variaveis, k, "_", year)] + 4 * compostos[paste0('comp_', variaveis, "_", year)]) / 5
    }
  }

  # Normalização das médias para o intervalo [0, 1]
  for (var in names(averages_df)[-1]) {
    averages_df[[var]] <-
      ((averages_df[[var]] - min(averages_df[[var]], na.rm = TRUE)) /
         (max(averages_df[[var]], na.rm = TRUE) - min(averages_df[[var]], na.rm = TRUE)))
  }

  # Combinação dos dados com os shapefiles
  data_shp2 <- merge(data_shp, averages_df, by = "Cdgdest")

  # Criação dos mapas compostos
  for (k in seq_along(ano_max$variavel)) {
    map <- tm_shape(data_shp2) +
      tm_fill(col = paste0("comp_", ano_max$variavel[k], "_", min(ano_max$ano)),
              style = "fisher", n = 5, palette = "-RdYlBu",
              title = paste0(titulos_comp[k], " (", min(ano_max$ano), ")"), border.col = NA) +
      tm_layout(legend.position = c("left", "bottom"))

    png(paste0("visualiza/fig/map_comp_", ano_max$variavel[k], "_", min(ano_max$ano), ".png"),
        width = 15, height = 15, units = "cm", res = 300, bg = "transparent")
    print(map)
    dev.off()

    # Análise LISA para o índice composto
    data_shp_temp <- na.omit(data_shp2[paste0("comp_", ano_max$variavel[k], "_", min(ano_max$ano))])
    queen_w <- queen_weights(data_shp_temp)
    lisa <- local_moran(queen_w, data_shp_temp[paste0("comp_", ano_max$variavel[k], "_", min(ano_max$ano))])
    lisa_colors <- lisa_colors(lisa)
    lisa_labels <- c("Não significativo", "Alto-Alto", "Baixo-Baixo", "Baixo-Alto", "Alto-Baixo", "Indefinido", "Isolado")
    lisa_clusters <- lisa_clusters(lisa)

    png_file <- paste0("visualiza/fig/lisa_comp_", ano_max$variavel[k], "_", min(ano_max$ano), ".png")
    png(png_file, width = 15, height = 15, units = "cm", res = 300, bg = "transparent")
    plot(st_geometry(data_shp), col = "#464646", border = NA)
    plot(st_geometry(data_shp_temp),
         col = sapply(lisa_clusters, function(x) { return(lisa_colors[[x + 1]]) }),
         border = NA, add = TRUE)
    title(main = paste0(titulos_comp[k], " (", min(ano_max$ano), ")"),
          sub = paste0("I de Moran Global: ", round(mean(lisa$lisa_vals), 2)))
    legend('bottomleft', legend = lisa_labels, fill = lisa_colors, border = "#eeeeee")
    dev.off()
  }

  # Exportação dos resultados para um arquivo Excel
  write_xlsx(data_shp2 %>% st_drop_geometry(), paste0(path, "/visualiza/", arquivos[i], ".xlsx"))

  # Salvando avisos em um arquivo de texto
  con <- file(paste0(path, "/visualiza/avisos.txt"), "w")
  for (key in names(avisos)) {
    cat(key, ":\n", avisos[[key]], "\n\n", file = con)
  }
  close(con)
}
