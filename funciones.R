# palavras ----
palavras_parada <- readr::read_lines("dados/stopwords_pt.txt")

# palavras irrelevantes ----
palavras_irrelevantes = c("brasil", "publicar", "comunidade", "pessoas",
                         "regiao",
                         "ano", "anos",
                         "pais", "pessoa", "comunicacao", "senhor",
                         "janeiro", "fevereiro", "marco", "abril", "maio", "junho", "julho", "agosto", "setembro", "outubro", "novembro", "dezembro",
                         "ler", "artigo", "completo",
                         "publicacao",
                         "detalhar"
)

palavras_remover = c(palavras_irrelevantes,
                    "right", "left", "top", "align", "gnews", "px", "twitter", "com", "pic", "font", "height", "width",
                    "pred", "fs", "us", "april", "flickr", "datawrapper", "data", "fried", "ftx", "medium", "exante", "server", "family", "loc", "lon", "mag", "prof", "lat", "gpt", "banner", "doacao",
                    "style")

# funções principais ----
executar <- function(script = "modulos/cron_elsiglo.r", 
                    esperar = TRUE) {
  caminho_log = script |> 
    stringr::str_replace("\\.r$", ".log") |> 
    stringr::str_replace("modulos/", "logs/")
  
  invisible(suppressWarnings(file.remove(caminho_log)))
  
  comando <- paste0("/usr/local/bin/Rscript ", script, " >> ", caminho_log, " 2>&1")
  
  Sys.sleep(0.1)
  system(comando, wait = esperar)
}

raspar_noticias <- function(script = "cron_radiopaulina.r",
                           caminho = "raspagem/fontes/") {
  message(glue::glue("iniciando {script} - {format(now(), '%d/%m/%y %H:%M:%S')}"))
  
  rstudioapi::jobRunScript(paste0(caminho, script), workingDir = getwd())
}

avisar_n <- function(x = 3) {
  walk(1:3, ~{beep(1); Sys.sleep(0.15)}) 
}

validar_elementos <- function(entrada, colapsar = FALSE) {
  if (colapsar == TRUE) {
    entrada2 <- paste(entrada, collapse = "\n")
  } else {
    entrada2 <- entrada
  }
  
  saida <- ifelse(length(entrada) == 0, NA_character_, entrada2)
  
  return(saida)
}

tentar <- function(x, nome = "teste") {
  tryCatch(x, 
           error = function(e) message("Erro em ", nome, ": ", e), 
           finally = message("OK ", nome))
}

revisar_resultados <- function(caminho) {
  walk(list.dirs(caminho, full.names = T, recursive = F), ~{
    Sys.sleep(0.05)
    pasta_x <- .x
    
    revisao <- pasta_x |> 
      list.files(full.names = T) |> 
      file.info() |> 
      tibble::tibble() |> 
      filter(size > 10000)
    
    if (max(revisao$ctime) |> as.Date() == lubridate::today()) {
      message(pasta_x |> stringr::str_extract("\\w+$"), " OK")
    } else {
      message("ERRO ", pasta_x |> stringr::str_extract("\\w+$"))  
    }
  })
}

continuar_se_tem_links <- function(links, num = 3) {
  if (length(links) <= num) {
    message("links insuficientes, terminando")
    return(FALSE)
  } else {
    message(glue("{length(links)} links obtidos"))
    return(TRUE)
  } 
}

verificar_url <- function(url) {
  estado <- url |> 
    httr::GET() |> 
    httr::status_code() |> 
    try()
  
  if (class(estado) != "integer") return(NULL)
  
  message(url, " (estado: ", estado, ")") |> try()
  
  if (estado != 200) {
    message(glue("erro http em {url}"))
    return(NULL)
  } else {
    return(estado)
  }
}

limpar_texto <- function(x) {
  x |> 
    textclean::strip() |> 
    str_trim() |> 
    str_squish()
}

limpar_texto_pouco <- function(x) {
  x |> 
    str_replace_all("dfp:|\\n|\\r", " ") |> 
    str_trim() |> 
    str_squish()
}

revisar_raspagem <- function(dados) {
  try({
    message(paste("pronto", deparse(substitute(dados)), "-", lubridate::now()))
    if ("tbl" %in% class(dados)) message(paste(nrow(dados), "noticias obtidas"))
  })
}

recodificar_fontes <- function(dados) {
  dados |> 
    mutate(fonte = case_match(fonte,
                             "24horas" ~ "24 Horas",
                             "adnradio" ~ "ADN Radio",
                             # ... resto do código permanece igual
                             .default = fonte))
}

formatar_data <- function(x) { 
  mes = month(x)
  mes_t = recode(mes, 
                 "1" = "janeiro",
                 "2" = "fevereiro",
                 "3" = "marco",
                 "4" = "abril",
                 "5" = "maio",
                 "6" = "junho",
                 "7" = "julho",
                 "8" = "agosto",
                 "9" = "setembro",
                 "10" = "outubro", 
                 "11" = "novembro",
                 "12" = "dezembro")
  
  data_etiqueta = paste(day(x), "de", mes_t)
  return(data_etiqueta)
}

mes_para_numero <- function(x) {
  recode(x, 
         "janeiro" = "1",
         "fevereiro" = "2",
         "marco" = "3",
         "abril" = "4",
         "maio" = "5",
         "junho" = "6",
         "julho" = "7",
         "agosto" = "8",
         "setembro" = "9", 
         "outubro" = "10",
         "novembro" = "11",
         "dezembro" = "12")
}

notificacao <- function(titulo = "Título", texto = "texto") {
  message(titulo, ": ", texto)
  
  system(
    paste0("osascript -e 'display notification \"", texto, "\" with title \"", titulo, "\"'")
  ) |> try()
}

aleatorio <- function() {
  sample(1111:9999, 1)
}

caminho_resultado <- function(fonte = "latercera", hist = "", formato = "rds") {
  if (class(hist)[1] == "function") hist <- ""
  glue::glue("raspagem/dados/{fonte}/{fonte}_cron_{aleatorio()}_{lubridate::today()}{hist}.{formato}")
}

modulos_n <- function() {
  fs::dir_ls("raspagem/dados") |> length()
}

sem_mudancas_hoje <- function() {
  diretorios <- fs::dir_info("raspagem/dados") |> 
    arrange(desc(modification_time))
  
  sem_mudancas <- diretorios |> 
    filter(modification_time < lubridate::today()) |> 
    mutate(fonte = stringr::str_extract(path, "raspagem/dados/\\w+") |> stringr::str_remove("raspagem/dados/")) |> 
    select(fonte, size, modification_time)
  
  return(sem_mudancas)
}

estimar_tempo <- function(amostra, estimativa = 4.9) {
  message(paste("tempo aproximado de processamento:", round((amostra * estimativa)/60/60, 1), "horas")) 
}

parada_manual <- function() {
  read.delim("outros/parar.txt", header = FALSE)[[1]] == "parar"
}

mensagem_segundos <- function(palavras, tempo) {
  segundos = seconds(round(tempo, 1)) |> as.numeric()
  palavras_segundos = round(palavras/segundos, 0)
  
  message(" (",  segundos, " segundos, ",
          palavras_segundos, " palavras/segundo)")
