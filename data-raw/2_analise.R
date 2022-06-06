load("data/dados_movs.rda")

dt_dist <- dados_movs |>
  dplyr::transmute(
    id,
    data = substr(data_hora, 0,10),
    data = lubridate::dmy(data),
    unidade,
    descricao
  ) |>
  dplyr::mutate(
    primeira_mov = stringr::str_detect(descricao, stringr::regex("Processo restrito gerado", TRUE)),
    autuado = stringr::str_detect(descricao, stringr::regex("autuado no CRSFN", TRUE)),
    dist = stringr::str_detect(descricao, stringr::regex("sorteio", TRUE)),
    pauta = stringr::str_detect(descricao, stringr::regex("incluído na pauta", TRUE)),
    julgado = stringr::str_detect(descricao, stringr::regex("julgado na", TRUE))
  ) |>
  dplyr::group_by(id) |>
  dplyr::mutate(
    dt_primeira = dplyr::case_when(
      primeira_mov ~ data,
      TRUE ~ NA_real_
    ),
    dt_auto = dplyr::case_when(
      autuado ~ data,
      TRUE ~ NA_real_
    ),
    dt_dist = dplyr::case_when(
      dist ~ data,
      TRUE ~ NA_real_
    ),
    dt_pauta = dplyr::case_when(
      pauta ~ data,
      TRUE ~ NA_real_
    ),
    dt_julgado = dplyr::case_when(
      julgado ~ data,
      TRUE ~ NA_real_
    )
  ) |>
  dplyr::ungroup()

processos_com_dist <- dt_dist |>
  dplyr::group_by(id) |>
  dplyr::summarise(processo_tem_dist = any(dist)) |>
  dplyr::ungroup() |>
  dplyr::filter(processo_tem_dist) |>
  dplyr::pull(id)


processos_datas <- dt_dist |>
  dplyr::mutate()

