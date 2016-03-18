findDate <- function(x) {
  y <- substr(x, 1, 10)
  ndate <- as.Date(y, "")
}

# conversion functions for bytes to megabytes, gigabytes, and terabytes
bytes2mb <- function(x){
  x / (1000 ^ 2)
}
bytes2gb <- function(x){
  x / (1000 ^ 3)
}
bytes2tb <- function(x){
  x / (1000 ^ 4)
}