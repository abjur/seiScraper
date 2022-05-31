## Aqui fica o código que RODA os scrapers

# sugiro desenvolver tudo por aqui primeiro, e depois migrar os códigos para as
# funções da pasta R.

ssl <- httr::config(ssl_verifypeer = FALSE)
u_sei <- "https://sei.economia.gov.br/sei/modulos/pesquisa/md_pesq_processo_pesquisar.php?acao_externa=protocolo_pesquisar&acao_origem_externa=protocolo_pesquisar&id_orgao_acesso_externo=0"
r_sei <- httr::GET(u_sei, ssl, httr::write_disk("output/sei.html", overwrite = TRUE))
html_sei <- xml2::read_html("output/sei.html")

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

body_full <- list(
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

r_id <- httr::POST(u_sei, ssl, body = body_full,
                   httr::write_disk(paste0("output/processos/", id, ".html"),overwrite = TRUE))


