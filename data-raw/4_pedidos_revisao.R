devtools::load_all()

# setup inicial -----------------------------------------------------------

path_pags <- "data-raw/pedidos_revisao/pags"
path_processos <- "data-raw/pedidos_revisao/processos"
path_movs <- "data-raw/pedidos_revisao/movs"

u_sei <- "https://sei.economia.gov.br/sei/modulos/pesquisa/md_pesq_processo_pesquisar.php?acao_externa=protocolo_pesquisar&acao_origem_externa=protocolo_pesquisar_paginado&id_orgao_acesso_externo=0"
r0 <- httr::GET(u_sei)
ans <- sei_captcha(r0)

# esse é o tipo processo de interesse, com pedidos de revisão
tipo <- "100000506"
r <- httr::POST(u_sei, body = sei_body(ans, tipo))

# quantidade de paginas

n_pags <- r |>
  xml2::read_html() |>
  xml2::xml_find_all("//*[@class='pesquisaBarraD']") |>
  xml2::xml_text() |>
  stringr::str_extract("[0-9]+$") |>
  as.numeric() |>
  magrittr::divide_by(10) |>
  ceiling()

# download e parse das paginas --------------------------------------------

purrr::walk(
  seq_len(n_pags),
  \(pag) sei_pag(pag, ans, tipo, path_pags),
  .progress = TRUE
)

ids_baixados <- fs::dir_ls(path_pags) |>
  purrr::map(parse_sei) |>
  purrr::list_rbind() |>
  dplyr::pull(id)

# download e parse dos processos ------------------------------------------

fs::dir_create(path_processos)

purrr::walk(
  ids_baixados,
  download_processo,
  path = path_processos,
  .progress = TRUE
)

da_processos <- fs::dir_ls(path_processos) |>
  purrr::map(parse_processo) |>
  purrr::list_rbind(names_to = "file")

# download e parse das movs -----------------------------------------------

fs::dir_create(path_movs)

purrr::walk(
  da_processos$url,
  download_mov,
  path = path_movs,
  .progress = TRUE
)

da_pedidos_revisao <- fs::dir_ls(path_movs) |>
  purrr::map(parse_mov) |>
  purrr::list_rbind(names_to = "id") |>
  tibble::as_tibble()


# analise -----------------------------------------------------------------

dados_movs <- da_pedidos_revisao |>
  janitor::clean_names()

# aux_dt_primeira ---------------------------------------------------------
aux_dt_primeira <- dados_movs |>
  dplyr::transmute(
    id,
    data = substr(data_hora, 0,10),
    data = lubridate::dmy(data),
    unidade,
    descricao
  ) |>
  dplyr::group_by(id) |>
  dplyr::summarise(
    dt_primeira = min(data)
  ) |>
  dplyr::ungroup()

# aux_dt_autuacao --------------------------------------------------------------
aux_dt_autuacao <- dados_movs |>
  dplyr::transmute(
    id,
    data = substr(data_hora, 0,10),
    data = lubridate::dmy(data),
    unidade,
    descricao
  ) |>
  dplyr::mutate(
    autuacao = stringr::str_detect(descricao, stringr::regex("autuado no CRSFN", TRUE)),
    dt_autuacao = NA_real_,
    dt_autuacao = dplyr::case_when(
      autuacao ~ as.integer(data)
    )
  ) |>
  dplyr::group_by(id) |>
  dplyr::slice(which.min(dt_autuacao)) |>
  dplyr::ungroup() |>
  dplyr::mutate(dt_autuacao = lubridate::as_date(dt_autuacao)) |>
  dplyr::select(id, dt_autuacao) |>
  dplyr::full_join(todos_processos)

# aux_dt_dist1 -------------------------------------------------------------
aux_dt_dist1 <- dados_movs |>
  dplyr::transmute(
    id,
    data = substr(data_hora, 0,10),
    data = lubridate::dmy(data),
    unidade,
    descricao
  ) |>
  dplyr::mutate(
    dist = stringr::str_detect(descricao, stringr::regex("sorteio", TRUE)),
    dt_dist1 = NA_real_,
    dt_dist1 = dplyr::case_when(
      dist ~ as.integer(data)
    )
  ) |>
  dplyr::group_by(id) |>
  dplyr::slice(which.min(dt_dist1)) |>
  dplyr::ungroup() |>
  dplyr::mutate(dt_dist1 = lubridate::as_date(dt_dist1)) |>
  dplyr::select(id, dt_dist1) |>
  dplyr::full_join(todos_processos)

# aux_dt_dist2 ------------------------------------------------------------
aux_dt_dist2 <- dados_movs |>
  dplyr::transmute(
    id,
    data = substr(data_hora, 0,10),
    data = lubridate::dmy(data),
    unidade,
    descricao
  ) |>
  dplyr::mutate(
    dist = stringr::str_detect(descricao, stringr::regex("sorteio", TRUE)),
    dt_dist2 = NA_real_,
    dt_dist2 = dplyr::case_when(
      dist ~ as.integer(data)
    )
  ) |>
  dplyr::group_by(id) |>
  dplyr::slice(which.max(dt_dist2)) |>
  dplyr::ungroup() |>
  dplyr::mutate(dt_dist2 = lubridate::as_date(dt_dist2)) |>
  dplyr::select(id, dt_dist2) |>
  dplyr::full_join(todos_processos)

# aux_dt_pauta ------------------------------------------------------------
aux_dt_pauta <- dados_movs |>
  dplyr::transmute(
    id,
    data = substr(data_hora, 0,10),
    data = lubridate::dmy(data),
    unidade,
    descricao
  ) |>
  dplyr::mutate(
    pauta = stringr::str_detect(descricao, stringr::regex("incluído na pauta", TRUE)),
    dt_pauta = NA_real_,
    dt_pauta = dplyr::case_when(
      pauta ~ as.integer(data)
    )
  ) |>
  dplyr::group_by(id) |>
  dplyr::slice(which.max(dt_pauta)) |>
  dplyr::ungroup() |>
  dplyr::mutate(dt_pauta = lubridate::as_date(dt_pauta)) |>
  dplyr::select(id, dt_pauta) |>
  dplyr::full_join(todos_processos)


# aux_dt_julgamento -------------------------------------------------------
aux_dt_julgamento <- dados_movs |>
  dplyr::transmute(
    id,
    data = substr(data_hora, 0,10),
    data = lubridate::dmy(data),
    unidade,
    descricao
  ) |>
  dplyr::mutate(
    julgamento = stringr::str_detect(descricao, stringr::regex("julgado na", TRUE)),
    dt_julgamento = NA_real_,
    dt_julgamento = dplyr::case_when(
      julgamento ~ as.integer(data)
    )
  ) |>
  dplyr::group_by(id) |>
  dplyr::slice(which.max(dt_julgamento)) |>
  dplyr::ungroup() |>
  dplyr::mutate(dt_julgamento = lubridate::as_date(dt_julgamento)) |>
  dplyr::select(id, dt_julgamento) |>
  dplyr::full_join(todos_processos)


# aux_parecer_teve --------------------------------------------------------
aux_parecer_teve <- dados_movs |>
  dplyr::mutate(
    parecer = stringr::str_detect(descricao, stringr::regex("PGFN", TRUE))
  ) |>
  dplyr::group_by(id) |>
  dplyr::summarise(
    parecer_teve = any(parecer)
  ) |>
  dplyr::ungroup()

# aux_parecer_numero ------------------------------------------------------
aux_parecer_numero <- dados_movs |>
  dplyr::mutate(
    parecer_numero = stringr::str_extract(descricao, stringr::regex("pgfn.+", TRUE)),
    parecer_numero = stringr::str_extract(parecer_numero, stringr::regex("[0-9\\.]+/[0-9\\-\\.]+", TRUE))
  ) |>
  dplyr::filter(!is.na(parecer_numero)) |>
  dplyr::select(id, parecer_numero) |>
  dplyr::group_by(id) |>
  tidyr::nest() |>
  dplyr::ungroup() |>
  dplyr::rename(parecer_numero = data)

# aux_dt_parecer ----------------------------------------------------------
aux_dt_parecer <- dados_movs |>
  dplyr::transmute(
    id,
    data = substr(data_hora, 0,10),
    data = lubridate::dmy(data),
    unidade,
    descricao
  ) |>
  dplyr::mutate(
    parecer = stringr::str_detect(descricao, stringr::regex("PGFN", TRUE)),
    dt_parecer = NA_real_,
    dt_parecer = dplyr::case_when(
      parecer ~ as.integer(data)
    )
  ) |>
  dplyr::group_by(id) |>
  dplyr::slice(which.min(dt_parecer)) |>
  dplyr::ungroup() |>
  dplyr::mutate(dt_parecer = lubridate::as_date(dt_parecer)) |>
  dplyr::select(id, dt_parecer) |>
  dplyr::full_join(todos_processos)

# join --------------------------------------------------------------------

da_pedidos_revisao <- list(
  aux_dt_primeira,
  aux_dt_autuacao,
  aux_dt_dist1,
  aux_dt_dist2,
  aux_dt_pauta,
  aux_dt_julgamento,
  aux_parecer_teve,
  aux_parecer_numero,
  aux_dt_parecer
) |> purrr::reduce(dplyr::left_join, by = "id")

# salvar ------------------------------------------------------------------

usethis::use_data(da_pedidos_revisao, overwrite = TRUE, compress = "xz")

