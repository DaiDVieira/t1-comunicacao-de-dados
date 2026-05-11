NRZL <- function(x){
  res <- vector("numeric", length = 2 * length(x))
  pos <- 1
  for(i in 1:length(x)){
    if(x[i] == 0){  # 0 : positivo
      res[pos] <- 1
      res[pos+1] <- 1
    }
    else{   # 1 : negativo
      res[pos] <- -1
      res[pos+1] <- -1
    }
    pos <- pos+2
  }
  return(res)
}

NRZI <- function(x){
  res <- vector("numeric", length = 2 * length(x))
  if(x[1] == 0){
    res[1] <- res[2] <- 1
  }
  else{
    res[1] <- res[2] <- -1
  }
  pos <- 3
  for(i in 2:length(x)){
    if(x[i] == 0){  # 0 : nao muda
      res[pos] <- res[pos+1] <- res[pos-1]
    }
    else{   # 1 : muda
      res[pos] <- res[pos+1] <- -1 * res[pos-1]
    }
    pos <- pos+2
  }
  return(res)
}

AMI <- function(x){
  res <- vector("numeric", length = 2 * length(x))
  pos <- 1
  ult1 <- -1    #marca se o ultimo 1 e positivo ou negativo
  for(i in 1:length(x)){
    if(x[i] == 0){  # 0 : continua 0
      res[pos] <- res[pos+1] <- 0
    }
    else{   # 1 : alterna entre 1 e -1
      res[pos] <- res[pos+1] <- ult1 * -1
      ult1 <- ult1 * -1
    }
    pos <- pos+2
  }
  return(res)
}

Pseudoternario <- function(x){
  res <- vector("numeric", length = 2 * length(x))
  pos <- 1
  ult0 <- -1    #marca se o ultimo 0 e positivo ou negativo
  for(i in 1:length(x)){
    if(x[i] == 0){  # 0 : alterna entre 1 e -1
      res[pos] <- res[pos+1] <- ult0 * -1
      ult0 <- ult0 * -1
    }
    else{   # 1 : valor 0
      res[pos] <- res[pos+1] <- 0
    }
    pos <- pos+2
  }
  return(res)
}

Manchester <- function(x){
  res <- vector("numeric", length = 2 * length(x))
  pos <- 1
  for(i in 1:length(x)){
    if(x[i] == 0){  # 0 : descida
      res[pos] <- 1
      res[pos+1] <- -1
    }
    else{   # 1 : subida
      res[pos] <- -1
      res[pos+1] <- 1
    }
    pos <- pos+2
  }
  return(res)
}

ManchesterDiferencial <- function(x){
  res <- vector("numeric", length = 2 * length(x))
  if(x[1] == 0){
    res[1] <- -1
    res[2] <- 1
  }
  else{
    res[1] <- 1
    res[2] <- -1
  }
  pos <- 3
  for(i in 2:length(x)){
    if(x[i] == 0){  # 0 : muda - inversao
      res[pos] <- -1 * res[pos-1]
      res[pos+1] <- res[pos-1]
    }
    else{   # 1 : nao muda - sem inversao
      res[pos] <- res[pos-1]
      res[pos+1] <- res[pos-1] * -1
    }
    pos <- pos+2
  }
  return(res)
}

ZR <- function(x){
  res <- vector("numeric", length = 2 * length(x))
  pos <- 1
  for(i in 1:length(x)){
    if(x[i] == 0){  # 0 : positivo
      res[pos] <- 1
      res[pos+1] <- 0
    }
    else{   # 1 : negativo
      res[pos] <- -1
      res[pos+1] <- 0
    }
    pos <- pos+2
  }
  return(res)
}

cod_2B1Q <- function(x){
  aux <- x
  if(length(aux) %% 2 == 1){    #se o tamanho da sequencia for impar, alguns adicionam 0 no final
    aux <- c(x, 0)
  }
  res <- vector("numeric", length = 2 * length(aux))
  pos <- 1
  n_ant <- 1    #supondo-se nivel anterior positivo
  for(i in seq(1, length(aux), by = 2)){      #a cada dois bits
    if(aux[i] == 0 && aux[i+1] == 0){  # 00
      if(n_ant >= 0)     #se o anterior foi positivo
        res[pos] <- res[pos+1] <- res[pos+2] <- res[pos+3] <- 1
      else
        res[pos] <- res[pos+1] <- res[pos+2] <- res[pos+3] <- -1
    }
    else if(aux[i] == 0 && aux[i+1] == 1){  # 01
      if(n_ant >= 0)     #se o anterior foi positivo
        res[pos] <- res[pos+1] <- res[pos+2] <- res[pos+3] <- 3
      else
        res[pos] <- res[pos+1] <- res[pos+2] <- res[pos+3] <- -3
    }
    else if(aux[i] == 1 && aux[i+1] == 0){  # 10
      if(n_ant >= 0)     #se o anterior foi positivo
        res[pos] <- res[pos+1] <- res[pos+2] <- res[pos+3] <- -1
      else
        res[pos] <- res[pos+1] <- res[pos+2] <- res[pos+3] <- 1
    }
    else{         # 11
      if(n_ant >= 0)     #se o anterior foi positivo
        res[pos] <- res[pos+1] <- res[pos+2] <- res[pos+3] <- -3
      else
        res[pos] <- res[pos+1] <- res[pos+2] <- res[pos+3] <- 3
    }
    n_ant <- res[pos]
    pos <- pos+4
  }
  return(res)
}

MLT_3 <- function(x){
  res <- vector("numeric", length = 2 * length(x))
  pos <- 3
  n_ant <- -1
  if(x[1] == 0){
    res[1] <- res[2] <- 0
  }
  else{
    res[1] <- res[2] <- 1
    n_ant <- 1
  }
  for(i in 2:length(x)){
    if(x[i] == 0){  # 0 : nao tem transicao
        res[pos] <- res[pos+1] <- res[pos-1]
    }
    else{   # 1 : depende do estado anterior
      if(res[pos-1] == 0){
        res[pos] <- res[pos+1] <- n_ant * -1
        n_ant <- n_ant * -1
      }
      else{
        res[pos] <- res[pos+1] <- 0 
      }
    }
    pos <- pos+2
  }
  return(res)
}
