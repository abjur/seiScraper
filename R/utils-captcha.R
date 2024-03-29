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

sei_captcha <- function(r0) {
  html_sei <- xml2::read_html(r0)
  u_captcha_endpoint <- html_sei |>
    xml2::xml_find_all("//img[contains(@src,'captcha')]") |>
    xml2::xml_attr("src")
  u_captcha <- paste0("https://sei.economia.gov.br", u_captcha_endpoint)
  xy <- u_captcha_endpoint |>
    urltools::param_get("codetorandom") |>
    stringr::str_split("-") |>
    unlist() |>
    as.numeric()
  ans <- captcha_classify_sei(xy[1], xy[2])
  ans
}
