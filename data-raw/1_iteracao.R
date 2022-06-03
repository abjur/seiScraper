# preparação ----------------------------------------------------------------

processos <- c("10372100225201917", "10372100141201894")
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
  purrr::walk(download_mov, path_movs)

# passo 4: parse_mov()
dados_movs <- path_movs |>
  fs::dir_ls() |>
  purrr::map_dfr(parse_mov, .id = "files") |>
  janitor::clean_names() |>
  dplyr::transmute(
    id = stringr::str_extract(files, "[0-9]+"),
    data = lubridate::dmy_hms(data_hora),
    data = lubridate::date(data),
    unidade,
    descricao
  )
