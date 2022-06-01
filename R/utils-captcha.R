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
