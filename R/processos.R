# download processos ----------------------------------------------------
download_processo <- function(id, path = "data-raw/processos") {
  # o id é o id do processo que eu quero
  # o path é o path do data-raw>processos

  # descobrindo o captcha
  ssl <- httr::config(ssl_verifypeer = FALSE)
  u_sei <- "https://sei.economia.gov.br/sei/modulos/pesquisa/md_pesq_processo_pesquisar.php?acao_externa=protocolo_pesquisar&acao_origem_externa=protocolo_pesquisar&id_orgao_acesso_externo=0"
  r_sei <- httr::GET(u_sei, ssl)
  html_sei <- xml2::read_html(r_sei)

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

  # body
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

  # POST
  file <- fs::path(path, id, ext = "html")
  httr::POST(u_sei, ssl, body = body,
             httr::write_disk(file,overwrite = TRUE))

  invisible(file)

}

# parse processos ----------------------------------------
parse_processo <- function(file) {
  # o arquivo html é o resultado do download processos

  html_resultado <- xml2::read_html(file)

  endpoint_resultado <- html_resultado |>
    xml2::xml_find_first("//table//a") |>
    xml2::xml_attr("href")

  url <- glue::glue("https://sei.economia.gov.br/sei/modulos/pesquisa/{endpoint_resultado}")
  # tem que retornar uma tibble cuja coluna é url
  tibble::tibble(
    url = url
  )
}


