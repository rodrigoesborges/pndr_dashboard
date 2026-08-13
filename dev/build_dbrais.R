#curlftpfs -o cache=no -s -d -v -o uid=1001,gid=1001,allow_other,umask=002,direct_io ftp://ftp.mtps.gov.br/pdet/microdados/rais mte_rais



### funções auxiliares

# ajusta numericos
string_to_num_with_commas <- function( string ) {
  tryCatch( as.numeric( gsub( ",", "." , string ) ) , error=function(e) string )
}

# diretório comum
get_common_dir <- function(paths, delim = "/") {
  path_chunks <- strsplit(paths, delim)

  i <- 1
  repeat({
    current_chunk <- sapply(path_chunks, function(x) x[i])
    if(any(current_chunk != current_chunk[1])) break
    i <- i + 1
  })
  paste(path_chunks[[1]][seq_len(i - 1)], collapse = delim)

}

# recursive ftp scrape
link_scrape <- function( x ) {
  library(RCurl)
  custom_recodes <-
    c( "í" = "%ED" , "á" = "%E1" , "ã" = "%E3" , " " = "%20" , "ç" = "%E7" , "õ" = "%F5" , "ô" = "%F4" )
  if ( !is.null( x ) ){
    Sys.sleep(runif(1,.2,1))
    h <- getCurlHandle()
    this_text <- getURL( x , curl = h , .encoding = 'ISO-8859-1' , .opts = list( timeout = 240 , dirlistonly = TRUE , ftplistonly = TRUE , ftp.use.epsv = FALSE , ssl.verifypeer = FALSE ) )
    reset(h)
    this_text <- unlist( strsplit( this_text , ifelse( Sys.info()["sysname"] == "Windows" , "\r\n" , "\n" ) ) )
    this_text <- curlPercentEncode( this_text , codes = custom_recodes )
    # this_text <- URLencode( this_text , reserved = TRUE )
    paste0( x , this_text )
  } else {
    NULL
  }
}

getlisting <- function( these_urls ) {
  library(RCurl)
  library(future.apply)
  plan( "multiprocess" )
  these_urls <- ifelse( grepl( "\\/$" , these_urls ) , these_urls , paste0( these_urls , "/" ) )
  res <- future_lapply( these_urls , link_scrape )
  res <- unlist( res )
  res <- res[ !grepl( "\\/EEC" , res ) ] # drop EEC folder
  is.file <- grepl( "\\." , basename( res ) , ignore.case = TRUE )
  res[ !is.file ] <- ifelse( grepl( "\\/$" , res[ !is.file ] ) , res[ !is.file ] , paste0( res[ !is.file ] , "/" ) )
  list( dirs = if ( any(!is.file) ) { res[ !is.file ] } else { NULL } ,
        files = if ( any(is.file) ) { res[ is.file ] } else { NULL } )
}

recursive_ftp_scrape <- function( main_url , max.iter = Inf ) {

  final_files <- NULL

  directories <- main_url

  i=0
  while ( length(directories) > 0 ) {
    i=i+1
    if ( i > max.iter ) { break() }
    listing <- getlisting(directories)
    directories <- listing[[1]]
    files <- listing[[2]]
    final_files <- c(final_files, files )
    if ( length( directories) > 0 ) cat( length(directories) , "new directories found.\n" ) else cat( "done!\n" )
  }

  return( final_files )

}

remove_special_character <- function(x) {
  stripped_x <- iconv( x , to = "ascii//translit" , sub = "_" )
  stripped_x <- gsub( "('|~|\\^)" , "" , stripped_x , ignore.case = TRUE )
  stripped_x
}


###Funções apenas para RAIS
catalog_rais <- function( output_dir ,ftpmount="coleta/cache/mte_rais", ... ){


  rais_files <- list.files(ftpmount,recursive=T)


  # drop cumulated ?
  rais_files <- rais_files[ !grepl( "cumulado" , basename( rais_files ) , ignore.case = TRUE ) ]

  # separate file types layouts
  layout_files <- rais_files[ grepl( "layout|xls" , basename( rais_files ) , ignore.case = TRUE ) ]
  data_files <- rais_files[ grepl( "\\.(zip|7z)$" , basename( rais_files ) , ignore.case = TRUE ) ]

  # create catalog
  catalog <-
    data.frame(
      full_path = data_files ,
      stringsAsFactors = FALSE
    )

  # add types
  catalog$type <-"rais"
  # ifelse( grepl( "RAIS" , catalog$full_path ) , "rais" ,
  #         ifelse( grepl( "CAGED_AJUSTES" , catalog$full_path ) , "caged_ajustes" ,
  #                 ifelse( grepl( RCurl::curlEscape( "NOVO CAGED" ) , catalog$full_path ) , "novo_caged" ,
  #                         ifelse( grepl( "CAGED" , catalog$full_path ) , "caged" , NA )) ) )

  # add subtypes
  catalog$subtype <- ifelse( grepl( "estb|estabelecimento|estab" , catalog$full_path , ignore.case = TRUE ) , "estabelecimento" , "vinculo" )
  catalog$subtype[ catalog$type == "novo_caged" & catalog$subtype == "vinculo" ] <- "movimentacoes"
  catalog$subtype[ catalog$type %in% c("caged" , "caged_ajustes") ] <- NA

  # define filename
  #catalog$output_filename <- gsub( "ftp://ftp.mtps.gov.br/pdet/microdados/" , "" , tolower( catalog$full_path ) , ignore.case = TRUE , useBytes = TRUE )
  catalog$output_filename <- file.path( output_dir , catalog$full_path )
  catalog$output_filename <- gsub( "\\..*$" , ".gz" , catalog$output_filename , ignore.case = TRUE )
  catalog$output_filename <- sapply( catalog$output_filename , utils::URLdecode )

  # get name tag
  data_tags <- basename( catalog$full_path )
  data_tags <- sapply( data_tags , utils::URLdecode , USE.NAMES = FALSE )
  data_tags <- gsub( "\\..*" , "" , data_tags )
  data_tags[ catalog$type == "rais" ] <- basename( dirname( catalog$full_path ) )[ catalog$type == "rais" ]
  numeric_tags <- gsub( "\\D+" , "" , data_tags , ignore.case = TRUE , useBytes = TRUE )

  # add year
  catalog$year <- NULL
  catalog$year[ catalog$type == "caged" ] <- substr( numeric_tags , 3 , 6 )[ catalog$type == "caged" ]
  catalog$year[ catalog$type == "caged_ajustes" ] <- numeric_tags[ catalog$type == "caged_ajustes" ]
  catalog$year[ catalog$type == "caged_ajustes" ] <- ifelse( nchar( catalog$year ) > 4 , substr( catalog$year , 3 , 6 ) , catalog$year )[ catalog$type == "caged_ajustes" ]
  catalog$year[ catalog$type == "rais" ] <- numeric_tags[ catalog$type == "rais" ]
  catalog$year[ catalog$type == "novo_caged" ] <- substr( numeric_tags[catalog$type == "novo_caged"] , 1 , 4 )
  catalog$year <- as.numeric( catalog$year )

  # check years
  # table( catalog$year , useNA = "always" )

  # create tablename
  catalog$db_tablename <- paste0( catalog$type , ifelse( is.na( catalog$subtype ) , "" , paste0( "_" , catalog$subtype ) ) )
  catalog$db_tablename <- paste0( catalog$db_tablename , "_" , catalog$year )

  # database file
  catalog$dbfile <- file.path( output_dir , "rais.sqlite" )


  ##add ftpmount to full_path
  catalog$full_path <- paste0(ftpmount,"/",catalog$full_path)
  # return catalog
  catalog

}

datavault_rais <- function( catalog , datavault_dir , skipExist = TRUE ) {

  # get common directory
  basedirais <- get_common_dir( tolower( catalog$full_path ) )

  # create datavault links
  catalog$datavault_file <- paste0( datavault_dir , gsub( basedirais , "" , tolower( catalog$full_path ) , ignore.case = TRUE ) )

  # check for existing files
  existing_files <- file.exists( catalog$datavault_file )

  # if there is any non-downloaded, run download procedure:
  if ( any( !existing_files ) ) {

    # print message
    cat( sum( 1*!existing_files ) , "missing files detected.\nDownloading missing files only.\n")

    # download files to datavault directory
    for ( i in seq_len( nrow(catalog) )[ !existing_files ] ) {

      # skip existing file
      if( skipExist & existing_files[ i ] ) next()


      # copy to final destination
      dir.create( dirname( catalog$datavault_file[ i ] ) , showWarnings = FALSE , recursive = TRUE )
      file.copy( catalog$full_path[ i ] , catalog$datavault_file[ i ],copy.mode=F )



      # process tracker
      cat( "file" , i , "out of" , nrow( catalog ) , "downloaded/copied to" , datavault_dir , "\r")

    }

  }
  # print message
  cat( "\nmtps datavault was built at" , datavault_dir , "\n" )

  # return catalog
  catalog

}



build_rais <-
  function( catalog , skipExist = TRUE , chunk.size = as.integer(10^5) , ... ){

    # força inteiro
    chunk.size <- as.integer( chunk.size )

    # load libraries
    library( archive )
    library( data.table )
    library( R.utils )

    # create temporary
    tf <- tempfile()
    tf2 <- tempfile()
    td <- file.path( tempdir() , "unzips" )

    # preemptive clearing
    suppressWarnings( unlink( td , recursive = TRUE ) )

    for ( i in seq_len( nrow( catalog ) ) ){

      # skip existing
      if ( skipExist & file.exists( catalog[ i , "output_filename" ] ) ) {

        # count lines
        # nlines <- fread( file = catalog[ i , "output_filename" ] , sep = ";" , dec = "." , header = TRUE , data.table = TRUE , showProgress = FALSE , select = 1L )
        # nlines <- nrow( nlines )
        # nlines <- as.numeric( system( paste0("cat " , catalog[ i , "output_filename" ] ," | wc -l" ) , intern = TRUE ) ) - 1
        #
        # # add case count
        # catalog[ i , 'case_count' ] <- nlines

        # process tracker
        # cat( paste0( "catalog file '" , catalog[ i , "output_filename" ] , "' already exists. Skipping.\n" ) )
        next()

      }

      # download the file
      if ( is.null( catalog[ i , "datavault_file" ] ) ) {
        download.file( catalog[ i , "full_path" ] , tf , quiet = TRUE , mode = "wb" )
      } else if ( is.na( catalog[ i , "datavault_file" ] ) ) {
        download.file( catalog[ i , "full_path" ] , tf , quiet = TRUE , mode = "wb" )
      } else {
        file.copy( catalog[ i , "datavault_file" ] , tf )
      }

      # extract zipped file
      if( !grepl( ".7z$|.zip$" , catalog[ i , 'full_path' ] , ignore.case = TRUE ) ){
        this_data_file <- tf
      } else {
        archive_extract( normalizePath( tf ) , dir = td )
        this_data_file <- list.files( td , full.names = TRUE )
        this_data_file <- grep( "\\.csv|\\.txt$", this_data_file, value = TRUE, ignore.case = TRUE )
      }

      # count lines
      nlines <- fread( file = this_data_file , sep = ";" , dec = "," , header = TRUE , encoding = "Latin-1" , data.table = TRUE , showProgress = FALSE , select = 1L , fill = TRUE )
      nlines <- nrow( nlines )

      # define number of chunks
      nchunks <- ceiling( nlines / chunk.size )

      # get column names
      con <- file( this_data_file , encoding =  "LATIN1" )
      these_cols <- strsplit( readLines( con = con , n = 1L ) , ";" )[[1]]
      close( con )

      # convert all column names to lowercase
      these_cols <- tolower( these_cols )

      # remove trailing spaces
      these_cols <- trimws( these_cols , which = "both" )

      # remove special characters
      these_cols <- remove_special_character( these_cols )

      # change dots and spaces for underscore
      these_cols <- gsub( "[^[:alnum:][:space:]]" , "_" , these_cols )
      these_cols <- gsub( "[[:space:]]" , "_" , these_cols )

      # make unique names
      these_cols <- make.unique( these_cols , sep = "_" )

      # process chunks
      start_n = 1
      for ( this_chunk in seq_len( nchunks ) ) {

        # read file
        x <- fread( file = this_data_file ,
                    sep = ";" , dec = "," ,
                    header = FALSE ,
                    encoding = "Latin-1" ,
                    data.table = TRUE ,
                    skip = start_n ,
                    nrows = chunk.size,
                    showProgress = FALSE )

        # fix NAs
        for (j in seq_len(ncol(x))) {
          nas <- which( x[ , j , with=FALSE ] == -1 )
          # if ( length(nas) > 0 ) break()
          set( x , i = nas , j = j , NA  )
        }

        # rename columns
        names(x) <- these_cols

        # save data
        dir.create( dirname( catalog[ i , 'output_filename' ] ) , recursive = TRUE , showWarnings = FALSE )
        fwrite( x , file = tf2 , append = TRUE , showProgress = FALSE , compress = "gzip" , sep = ";" , dec = "." )

        # remove object
        rm( x ) ; gc()

        # process tracker
        cat( "chunk" , this_chunk , "of" , nchunks , "processed.\r")

        # recalculate starting line
        start_n <- start_n + chunk.size

      }

      # copy to output file
      file.copy( tf2 , catalog[ i , "output_filename" ] )
        file.remove( tf2 )

      # remove temporary
      suppressWarnings( unlink( td , recursive = TRUE ) )
      file.remove( tf )

      # add case count
      catalog[ i , 'case_count' ] <- nlines

      # process tracker
      cat( paste0( "catalog entry " , i , " of " , nrow( catalog ) , " stored at '" , catalog[ i , "output_filename" ] , "'\n" ) )

    }

    # return catalog
    catalog

  }


# cria base de dados monetdb
monetdb_rais <- function( catalog , chunk.size = as.integer( 10^6 ) ) {

  # load libraries
  library( DBI )
  library( MonetDBLite )
  library( data.table )

  # define folder address
  catalog[ , "dbfolder" ] <- file.path( dirname( unique( catalog[ , "dbfile" ] )[[1]] ) , "MonetDB" )

  # create folder
  dir.create( dirname( unique( catalog[ , "dbfolder" ] )[[1]] ) , recursive = TRUE , showWarnings = FALSE )

  # connect to database
  db <- dbConnect( MonetDBLite() , unique( catalog[ , "dbfolder" ] )[[1]] )

  # split by table
  list_catalog <- split( catalog , catalog$db_tablename )

  # process each table
  lapply ( list_catalog , function( this_catalog ) {

    # tabulate columns and formats
    these_dts <- lapply( this_catalog$output_filename , function(this_file) {

      # pula arquivos inexistentes
      if ( !file.exists( this_file ) ) return( NULL )

      # lê metadados
      x <- fread( this_file , sep = ";" , dec = "." , nrows = 1 )

      # coleta nome de colunas
      these_cols <- colnames( x )

      # coleta formato de colunas
      these_formats <- sapply( x , class )

      # monta data.table
      data.table( filename = this_file , column_name = these_cols , column_format = these_formats )

    } )

    # empilha dados
    data_structure <- rbindlist( these_dts , use.names = TRUE )

    # reformata estrutura
    data_structure <- dcast( data_structure , column_name ~ column_format , fill = "double" , drop = FALSE , value.var = "column_format" , fun.aggregate = unique )

    # define formato final da coluna
    data_structure <- as.data.frame( data_structure )
    data_structure$col_format <- apply( data_structure[ , -1 ] , 1 , function( y ) { ifelse( any( tolower(y) %in% c( "character", "date" ) ) , "character" ,"numeric" ) } )

    # cria estrutura final
    data_structure <- data_structure[ , c( "column_name" , "col_format" ) ]

    # load each file
    for ( i in seq_len( nrow( this_catalog ) ) ) {

      # count lines
      nlines <- fread( file = this_catalog[ i , "output_filename" ] , sep = ";" , dec = "." , header = TRUE , data.table = TRUE , showProgress = FALSE , select = 1L )
      nlines <- nrow( nlines )

      # define number of chunks
      nchunks <- ceiling( nlines / chunk.size )

      # get column names
      con <- file( this_catalog[ i , "output_filename" ] , encoding =  "LATIN1" )
      these_cols <- strsplit( readLines( con = con , n = 1L ) , ";" )[[1]]
      close( con )

      # process chunks
      start_n = 1
      for ( this_chunk in seq_len( nchunks ) ) {

        # read data
        x <- fread( file = this_catalog[ i , "output_filename" ] ,
                    sep = ";" , dec = "." ,
                    header = FALSE ,
                    encoding = "Latin-1" ,
                    data.table = TRUE ,
                    skip = start_n ,
                    nrows = chunk.size ,
                    showProgress = FALSE )

        # set names
        colnames( x ) <- these_cols

        # cria colunas ausentes
        missing_cols <- data_structure$column_name [ !is.element( data_structure$column_name , colnames(x) ) ]
        if ( length( missing_cols ) > 0 )  x[ , (missing_cols) := NA ]

        # altera formatos
        these_num_cols <- data_structure$column_name[ data_structure$col_format == "numeric" ]
        x[ , (these_num_cols) := lapply( .SD , as.numeric ) , .SDcols = these_num_cols ]
        these_char_cols <- data_structure$column_name[ data_structure$col_format == "character" ]
        x[ , (these_char_cols) := lapply( .SD , as.character ) , .SDcols = these_char_cols ]

        # reordena colunas
        setcolorder( x , data_structure$column_name )

        # write to database
        dbWriteTable( db , this_catalog[ i , "db_tablename" ] , x , append = TRUE )

        # remove object
        rm( x ) ; gc()

        # process tracker
        cat( "chunk" , this_chunk , "of" , nchunks , "processed.\r")

        # recalculate starting line
        start_n <- start_n + chunk.size

      }

      # process tracker
      # cat( paste0( "catalog entry " , i , " of " , nrow( catalog ) , " stored at '" , catalog[ i , "db_tablename" ] , "'\n" ) )
      cat( paste0( basename( this_catalog[ i , "output_filename" ] ) , " stored at '" , this_catalog[ i , "db_tablename" ] , "'\n" ) )


    }

  } )

  # disconnect from database
  dbDisconnect( db )

  # return catalog
  catalog

}

# cria base de dados postgres
postgres_rais <- function( catalog , chunk.size = as.integer( 10^6 ),userais="mte_rais",passwordrais="aEd1#man@gRpublicrais", hostprais="38.242.154.34") {

  # load libraries
  library( DBI )
  library( RPostgreSQL )
  library( data.table )

  # create folder
  dir.create( dirname( unique( catalog[ , "dbfile" ] )[[1]] ) , recursive = TRUE , showWarnings = FALSE )

  # connect to database
  db <- dbConnect(  RPostgreSQL::PostgreSQL(), dbname=unique( catalog[ , "dbfile" ] )[[1]], user=userais,password=passwordrais,
               host=hostprais,port=5432     )

  # split by table
  list_catalog <- split( catalog , catalog$db_tablename )

  # process each table
  lapply ( list_catalog , function( this_catalog ) {

    # tabulate columns and formats
    these_dts <- lapply( this_catalog$output_filename , function(this_file) {

      # pula arquivos inexistentes
      if ( !file.exists( this_file ) ) return( NULL )

      # lê metadados
      x <- fread( this_file , sep = ";" , dec = "." , nrows = 1 )

      # coleta nome de colunas
      these_cols <- colnames( x )

      # coleta formato de colunas
      these_formats <- sapply( x , class )

      # monta data.table
      data.table( filename = this_file , column_name = these_cols , column_format = these_formats )

    } )

    # empilha dados
    data_structure <- rbindlist( these_dts , use.names = TRUE )

    # reformata estrutura
    data_structure <- dcast( data_structure , column_name ~ column_format , fill = "double" , drop = FALSE , value.var = "column_format" , fun.aggregate = unique )

    # define formato final da coluna
    data_structure <- as.data.frame( data_structure )
    data_structure$col_format <- apply( data_structure[ , -1 ] , 1 , function( y ) { ifelse( any( tolower(y) %in% c( "character", "date" ) ) , "character" ,"numeric" ) } )

    # cria estrutura final
    data_structure <- data_structure[ , c( "column_name" , "col_format" ) ]

    # load each file
    for ( i in seq_len( nrow( this_catalog ) ) ) {

      # count lines
      nlines <- fread( file = this_catalog[ i , "output_filename" ] , sep = ";" , dec = "." , header = TRUE , data.table = TRUE , showProgress = FALSE , select = 1L )
      nlines <- nrow( nlines )

      # define number of chunks
      nchunks <- ceiling( nlines / chunk.size )

      # get column names
      con <- file( this_catalog[ i , "output_filename" ] , encoding =  "LATIN1" )
      these_cols <- strsplit( readLines( con = con , n = 1L ) , ";" )[[1]]
      close( con )

      # process chunks
      start_n = 1
      for ( this_chunk in seq_len( nchunks ) ) {

        # read data
        x <- fread( file = this_catalog[ i , "output_filename" ] ,
                    sep = ";" , dec = "." ,
                    header = FALSE ,
                    encoding = "Latin-1" ,
                    data.table = TRUE ,
                    skip = start_n ,
                    nrows = chunk.size ,
                    showProgress = FALSE )

        # set names
        colnames( x ) <- these_cols

        # cria colunas ausentes
        missing_cols <- data_structure$column_name [ !is.element( data_structure$column_name , colnames(x) ) ]
        if ( length( missing_cols ) > 0 )  x[ , (missing_cols) := NA ]

        # altera formatos
        these_num_cols <- data_structure$column_name[ data_structure$col_format == "numeric" ]
        x[ , (these_num_cols) := lapply( .SD , as.numeric ) , .SDcols = these_num_cols ]
        these_char_cols <- data_structure$column_name[ data_structure$col_format == "character" ]
        x[ , (these_char_cols) := lapply( .SD , as.character ) , .SDcols = these_char_cols ]

        # reordena colunas
        setcolorder( x , data_structure$column_name )

        # write to database
        dbWriteTable( db , this_catalog[ i , "db_tablename" ] , x , append = TRUE )

        # remove object
        rm( x ) ; gc()

        # process tracker
        cat( "chunk" , this_chunk , "of" , nchunks , "processed.\r")

        # recalculate starting line
        start_n <- start_n + chunk.size

      }

      # process tracker
      # cat( paste0( "catalog entry " , i , " of " , nrow( catalog ) , " stored at '" , catalog[ i , "db_tablename" ] , "'\n" ) )
      cat( paste0( basename( this_catalog[ i , "output_filename" ] ) , " stored at '" , this_catalog[ i , "db_tablename" ] , "'\n" ) )


    }

  } )

  # disconnect from database
  dbDisconnect( db )

  # return catalog
  catalog

}

  ##OBS RAIS 2020 - CBO  0000-1 INVALIDA DEIXA O CAMPO COMO TEXTO - MANUALMENTE AJUSTADO POSTGRESQL

con <-
  DBI::dbConnect(  RPostgreSQL::PostgreSQL(), dbname=unique( catalog[ , "dbfile" ] )[[1]], user=userais,password=passwordrais,
                   host=hostprais,port=5432     )

DBI::dbExecute(con, "UPDATE rais_vinculo_2020 SET cbo_ocupacao_2002 = 00001 WHERE cbo_ocupacao_2002 = '0000-1'")

DBI::dbExecute(con, "ALTER TABLE rais_vinculo_2020 ALTER COLUMN cbo_ocupacao_2002 TYPE NUMERIC USING cbo_ocupacao_2002::numeric")
#rais_dblocal <- build_rais(raiscatalogd|>dplyr::filter(!grepl("egado",full_path)))

#saveRDS(rais_dblocal,"data-raw/2023-1985-catalogo-completo-rais.rds")
