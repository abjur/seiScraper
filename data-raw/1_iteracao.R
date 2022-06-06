# preparação ----------------------------------------------------------------

processos <- c("10372100225201917", "10372100141201894")

rx_sei <- "(19957|10372|00783)\\.?[0-9]{6}[/-]?20[0-9]{2}-?[0-9]{2}"

processos <- obsMC::da_conselho_cvm |>
  dplyr::mutate(sei = stringr::str_extract(resumo, rx_sei)) |>
  dplyr::pull(sei) |>
  abjutils::clean_cnj()
  unique()

path_processos <- "data-raw/processos"
path_movs <- "data-raw/movs"

# passo 1: download_processo() --------------------------------------------

purrr::walk(processos, download_processo, path_processos)

# passo 2: parse_processo() -----------------------------------------------

dados_processos <- path_processos |>
  fs::dir_ls() |>
  purrr::map_dfr(parse_processo, .id = "files")

# passo 3: download_mov() -------------------------------------------------
dados_processos$url |>
  purrr::walk(purrr::possibly(download_mov, NULL), path_movs)

# passo 4: parse_mov()
dados_movs <- path_movs |>
  fs::dir_ls() |>
  purrr::map_dfr(parse_mov, .id = "files") |>
  janitor::clean_names() |>
  dplyr::transmute(
    id = stringr::str_extract(files, "[0-9]+"),
    data_hora,
    unidade,
    descricao
  )

# 10372000023201615

usethis::use_data(dados_movs, overwrite = TRUE)


