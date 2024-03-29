## Aqui fica o código que RODA os scrapers

# sugiro desenvolver tudo por aqui primeiro, e depois migrar os códigos para as
# funções da pasta R.

ssl <- httr::config(ssl_verifypeer = FALSE)
u_sei <- "https://sei.economia.gov.br/sei/modulos/pesquisa/md_pesq_processo_pesquisar.php?acao_externa=protocolo_pesquisar&acao_origem_externa=protocolo_pesquisar&id_orgao_acesso_externo=0"
r_sei <- httr::GET(u_sei, ssl)
html_sei <- xml2::read_html(r_sei)

# captcha -----------------------------------------------------------------
sei_all_numbers <- function() {
  c(
    48:57, #0:9
    65:90, #A-Z
    97:122 #a-z
  )
}

sei_all_chars <- function() {
  c(0:9, LETTERS, letters)
}

num_to_char <- function(x) {
  nums <- sei_all_numbers()
  char <- sei_all_chars()
  charx <- character(length(x))
  for(xi in seq_along(x)) {
    if (length(char[nums == x[xi]]) > 0) {
      charx[xi] <- char[nums == x[xi]]
    } else {
      charx[xi] <- NA_character_
    }
  }
  charx
}

captcha_classify_sei <- function(x, y) {
  l1 <- num_to_char(x)
  l2 <- num_to_char(y)
  l3 <- num_to_char(ceiling((x + y - 48 * 2) / 2 + 48))
  l4 <- num_to_char(floor((x + y - 48 * 2) / 2 + 48))
  if (is.na(l3)) l3 <- ifelse(x > y, l2, l1)
  if (is.na(l4)) l4 <- ifelse(x > y, l1, l2)
  paste(c(l1, l2, l3, l4), collapse = "")
}

r0 <- html_sei
u_captcha_endpoint <- r0 |>
  xml2::xml_find_all("//img[contains(@src,'captcha')]") |>
  xml2::xml_attr("src")
u_captcha <- paste0("https://sei.economia.gov.br", u_captcha_endpoint)
xy <- u_captcha_endpoint |>
  urltools::param_get("codetorandom") |>
  stringr::str_split("-") |>
  unlist() |>
  as.numeric()
ans <- captcha_classify_sei(xy[1], xy[2])

# id ----------------------------------------------------------------------

id <- "10372100225201917"

# body --------------------------------------------------------------------

body <- list(
  "txtProtocoloPesquisa" = id,
  "txtCaptcha" = ans,
  "sbmPesquisar" = "Pesquisar",
  "q" = "",
  "chkSinProcessos" = "",
  "txtParticipante" = "",
  "hdnIdParticipante" = "",
  "txtUnidade" = "",
  "hdnIdUnidade" = "",
  "selTipoProcedimentoPesquisa" = "",
  "selSeriePesquisa" = "",
  "txtDataInicio" = "",
  "txtDataFim" = "",
  "txtNumeroDocumentoPesquisa" = "",
  "txtAssinante" = "",
  "hdnIdAssinante" = "",
  "txtDescricaoPesquisa" = "",
  "txtAssunto" = "",
  "hdnIdAssunto" = "",
  "txtSiglaUsuario1" = "",
  "txtSiglaUsuario2" = "",
  "txtSiglaUsuario3" = "",
  "txtSiglaUsuario4" = "",
  "hdnSiglasUsuarios" = "",
  "hdnSiglasUsuarios" = "",
  "partialfields" = glue::glue("prot_pesq:*{id}*"),
  "requiredfields" = "",
  "as_q" = "",
  "hdnFlagPesquisa" = "1")

# POST --------------------------------------------------------------------

path <- "data-raw/processos"
f <- fs::path(path, id, ext = "html")

httr::POST(u_sei, ssl, body = body,
                   httr::write_disk(f,overwrite = TRUE))

html_resultado <- xml2::read_html(f)

endpoint_resultado <- html_resultado |>
  xml2::xml_find_first("//table") |>
  xml2::xml_find_first(".//a") |>
  xml2::xml_attr("href")

u_id <- glue::glue("https://sei.economia.gov.br/sei/modulos/pesquisa/{endpoint_resultado}")

# id ----------------------------------------------------------------------

r_id <- httr::GET(u_id, ssl, httr::write_disk(glue::glue("data-raw/processos/{id}.html"), overwrite = TRUE))
html_id <- xml2::read_html(glue::glue("data-raw/processos/{id}.html"))

pegar_id <- function(url) {

  url |>
    httr::GET(ssl) |>
    xml2::read_html() |>
    xml2::xml_find_first("//table/tr[2]") |>
    xml2::xml_text() |>
    stringr::str_remove_all("[a-zA-Z\\.\\:\\/\\-]")

}

tables_id <- html_id |>
  xml2::xml_find_all("//table")

tables_id[2] |>
  rvest::html_table() |>
  as.data.frame() |>
  tibble::as.tibble()

readr::write_rds(table_id2, glue::glue("data-raw/movs/movs_{id}.rds"))

# proximos passos ---------------------------------------------------------
# não precisa fazer uma função para baixar a página r0
# fazer uma função para baixar a página principal com o link (armazenar as páginas principais com os links)
# uma segunda função para receber um arquivo html e pegar o link. Ela retorna o link
# uma terceira função para baixar a página do processo a partir do link (armazenar essas páginas também)
# e uma quarta função para processar o html e retornar uma tibble
# não precisa ness quarta função salvar as movs
# depois disso a gente usa o purrr::map_dfr() para empilhar todas as movs, o map ja vai deixar a gente criar uma coluna nova para cada processo
# # no parâmetro .id a gente coloca o nome do arquivo como o nome da coluna
#
# Estrutura dos arquivos
# 1. Colocar o output dentro do data-raw
# 2. Fazer pastas pareadas com os nomes das funções
# Digamos que a função que baixa o nome dos links seja "download-processos", então a pasta vai se chamar "processos". E eu coloco os brutos lá.
# E digamos que a função os links seja "download-movimentacoes", então a pasta se chama "movs".
#
# Depois disso, eu faço a iteração
# A iteração eu faço direto no data-raw. Pra ter controle de erros.


# testando individualmente ------------------------------------------------

id <- "10372100225201917"

file <- download_processo(id)
url <- parse_processo(file)
download_mov(id)
# iteração ----------------------------------------------------------------

processos <- c("10372100225201917", "10372100141201894")
path_processos <- "data-raw/processos"
path_movs <- "data-raw/movs"
# passo 1: download_processo() --------------------------------------------

purrr::walk(processos, download_processo, path_processos)
# files <- purrr::map_chr(processos, download_processo)

# passo 2: parse_processo() -----------------------------------------------

dados_processos <- path_processos |>
  fs::dir_ls() |>
  purrr::map_dfr(parse_processo, .id = "files")

# é melhor não usar este jeito abaixo, em que eu parto do "files"
# porque pode ser que nem todas as files deem certo

# dados_processos <- files |>
#   purrr::set_names() |>
#   fs::path_common() |>
#   fs::dir_ls() |> # não entendi porque só o "files" não funciona
#   purrr::map_dfr(parse_processo, .id = "files")

# passo 3: download_mov() -------------------------------------------------
dados_processos$url |>
  purrr::walk(download_mov, path_movs)

# passo 4: parse_mov()
da_movs <- path_movs |>
  fs::dir_ls() |>
  purrr::map_dfr(parse_mov, .id = "files") |>
  janitor::clean_names() |>
  dplyr::transmute(
    id = stringr::str_extract(files, "[0-9]+"),
    data = lubridate::dmy_hms(data_hora),
    data = lubridate::date(data),
    unidade,
    descricao)

