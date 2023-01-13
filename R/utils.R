sei_body <- function(ans, tipo_processo) {
  list(
    "txtProtocoloPesquisa" = "",
    "q" = "",
    "chkSinProcessos" = "P",
    "txtParticipante" = "",
    "hdnIdParticipante" = "",
    "txtUnidade" = "",
    "hdnIdUnidade" = "" ,
    "selTipoProcedimentoPesquisa" = tipo_processo,
    "selSeriePesquisa" = "",
    "txtDataInicio" = "",
    "txtDataFim" = "",
    "txtCaptcha" = ans,
    "sbmPesquisar" = "Pesquisar",
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
    "hdnCaptchaMd5" = cli::hash_md5(ans),
    "partialfields" = glue::glue("id_tipo_proc:{tipo_processo} AND sta_prot:P"),
    "requiredfields" = "",
    "as_q" = "",
    "hdnFlagPesquisa" = "1"
  )
}

u_sei <- function() {
  "https://sei.economia.gov.br/sei/modulos/pesquisa/md_pesq_processo_pesquisar.php?acao_externa=protocolo_pesquisar&acao_origem_externa=protocolo_pesquisar_paginado&id_orgao_acesso_externo=0"
}

sei_pag <- function(pag, ans, tipo, path) {
  id_pag <- (pag-1)*10
  body <- sei_body(ans, tipo)
  fs::dir_create(path)
  fname <- sprintf("%s/pag_%04d.html", path, pag)
  r <- httr::POST(
    u_sei(),
    query = list(inicio = id_pag),
    body = sei_body(ans, tipo),
    httr::write_disk(fname, TRUE)
  )
}
