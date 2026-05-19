library(tidyverse)

#Leitura da sequencia a ser transmitida
repeat{
  erro <- F
  sequencia <- scan()
  if(length(sequencia) >= 1){
    for(i in 1:length(sequencia)){
      if(sequencia[i] != 0 && sequencia[i] != 1){
        erro <- T
      }
    }
  }
  else erro <- T
  if(!erro) 
    break
  else{
    print("Digite somente 0 ou 1")
  }
}

repeat{
  erro <- F
  sequencia <- readline()
  sequencia <- as.numeric(str_split(sequencia, "", simplify = TRUE))
  if(length(sequencia) >= 1){
    for(i in 1:length(sequencia)){
      if(is.na(sequencia[i]) || (sequencia[i] != 0 && sequencia[i] != 1)){
        erro <- T
      }
    }
  }
  else erro <- T
  if(!erro) 
    break
  else{
    print("Digite somente 0 ou 1")
  }
}

cat("Escolha 1, 2 ou 3 Codificações de Linha para apresentar digitando o numero correspondente:
1- NRZ-L  \t2- NRZ-I  \t3- AMI  \t4- Pseudoternário \n5- Manchester  \t6- Manchester Diferencial
7- ZR  \t\t8- 2B1Q \t9- MLT-3\n")
escolha <- scan(n = 3)


#Codificacao
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
  res <- res[-length(res)]
  res <- res[-length(res)]
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


seq_codificada <- matrix(0, nrow = length(escolha), ncol = 2 * length(sequencia))
codificacao <- vector("character", 3)
extra <- vector("numeric", 2 * length(escolha))   #para as sequencias NRZ-I, Manchester Diferencial e MLT-3

for (i in 1:length(escolha)){
  if(escolha[i] == 1) {
    seq_codificada[i,] <- NRZL(sequencia)
    codificacao[i] <- "NRZ-L"
  }
  else if(escolha[i] == 2){
    seq_codificada[i,] <- NRZI(sequencia)
    codificacao[i] <- "NRZ-I"
    extra[i*2] <- 1    #sempre inicia em positivo
  }
  else if(escolha[i] == 3){
    seq_codificada[i,] <- AMI(sequencia)
    codificacao[i] <- "AMI"
  }
  else if(escolha[i] == 4){
    seq_codificada[i,] <- Pseudoternario(sequencia)
    codificacao[i] <- "Pseudoternário"
  }
  else if(escolha[i] == 5){
    seq_codificada[i,] <- Manchester(sequencia)
    codificacao[i] <- "Manchester"
    extra[i*2] <- 1
  }
  else if(escolha[i] == 6){
    seq_codificada[i,] <- ManchesterDiferencial(sequencia)
    codificacao[i] <- "Manchester Diferencial"
    extra[i*2] <- 1
  }
  else if(escolha[i] == 7){
    seq_codificada[i,] <- ZR(sequencia)
    codificacao[i] <- "ZR"
  }
  else if(escolha[i] == 8){
    seq_codificada[i,] <- cod_2B1Q(sequencia)
    codificacao[i] <- "2B1Q"
  }
  else if(escolha[i] == 9){
    seq_codificada[i,] <- MLT_3(sequencia)
    codificacao[i] <- "MLT-3"
    extra[i*2] <- 0
  }
  else{
    print("Escolha de codigo de linha invalida")
  }
  extra[i*2-1] <- 0
  if(escolha[i] == 1 || escolha[i] == 3 || escolha[i] == 4 || escolha[i] == 7 || escolha[i] == 8){
    extra[i*2] <- seq_codificada[i, 1]
  }
}

#Transformacao para o grafico da sequencia codificada

pontos_seq <- matrix(0, nrow = 4 * length(sequencia), ncol = 2 * length(escolha))   
#matriz vertical, duas primeiras colunas para a primeira sequencia codificada e as outras duas para a segunda
#uma coluna para ponto x e a outra para ponto y. Cada bit deve ser composto por quatro pontos no grafico

x_ini <- 0
for(j in 1:ncol(seq_codificada)){   #pontos_seq tem o dobro de linhas da quantidade de colunas de seq_codificada
  ind_ini <- 2 * j - 1
  for(i in 1:nrow(seq_codificada)){   #para cada codigo escolhido
    if(j == 1){
      pontos_seq[1, 1] = x_ini
    }
    else{
      pontos_seq[ind_ini, 2*i-1] = pontos_seq[ind_ini-1, 2*i-1]    #x: linhas ímpares pegam os valores anteriores
    }
    pontos_seq[ind_ini+1, 2*i-1] =  pontos_seq[ind_ini, 2*i-1] + 0.5  #x: linhas pares criam um novo valor
    
    pontos_seq[ind_ini, 2*i] = seq_codificada[i,j]    
    pontos_seq[ind_ini+1, 2*i] = seq_codificada[i,j]   
  }
}
max_y <- max(abs(seq_codificada))


#Sinal transmitido
texto_sequencia <- data.frame(x_pos = seq(0.5, length(sequencia), by = 1),
                              y_pos = max_y + 0.5, 
                              label = sequencia)

if(length(codificacao) == 2){
  colnames(pontos_seq) <- c("x", codificacao[1], "x.1", codificacao[2])
} else if(length(codificacao) == 3){
  colnames(pontos_seq) <- c("x", codificacao[1], "x.1", codificacao[2], "x.2", codificacao[3])
}

#Um metodo de codificacao (o primeiro)

df <- as.data.frame(pontos_seq) %>%
  select(x = 1, y = 2)
df <- df %>% add_row(x = extra[1], y = extra[2], .before = 1)

ggplot(df, aes(x = x, y = y)) +
  geom_vline(xintercept = 0:length(sequencia), col = "grey50") +      #separacao dos bits
  geom_hline(yintercept = 0, col = "grey50") +        #eixo 0
  geom_line(linewidth = 0.75) +
  labs(title = paste("Código de Linha", codificacao, sep = " "), x = "Tempo", y = "Sinal") +
  geom_text(data = texto_sequencia, aes(x = x_pos, y = y_pos-0.2, label = label), size = 4) +
  scale_x_continuous(n.breaks = length(sequencia)) +
  scale_y_continuous(limits = c(-(max_y+0.5), max_y+0.5)) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())


#comparação entre 2 metodos de codificacao

df <- as.data.frame(pontos_seq) %>%
  select(1, 2, 4) %>%
  pivot_longer(cols = c(codificacao[1], codificacao[2]), names_to = "Codificacao", values_to = "y") %>% 
  add_row(x = extra[1], y = extra[2], Codificacao = codificacao[1], .before = 1) %>% 
  add_row(x = extra[3], y = extra[4], Codificacao = codificacao[2], .before = 2)


ggplot(df, aes(x = x, y = y)) +
  geom_vline(xintercept = 0:length(sequencia), col = "grey50") +      #separacao dos bits
  geom_hline(yintercept = 0, col = "grey50") +        #eixo 0
  geom_line(linewidth = 0.75) +
  facet_wrap(~ Codificacao, nrow = 2) +       #separa em dois quadros
  labs(title = paste("Comparação de Codigos de Linha:", codificacao[1], "e", codificacao[2], sep = " "), 
       x = "Tempo", y = "Sinal") +
  geom_text(data = texto_sequencia, aes(x = x_pos, y = y_pos, label = label), size = 4) +
  scale_x_continuous(n.breaks = length(sequencia)) +
  scale_y_continuous(limits = c(-(max_y+0.5), max_y+0.5), expand = expansion(add = c(0, 0.5))) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())



#comparacao entre 3 metodos de codificacao de linha
df <- as.data.frame(pontos_seq) %>%
  select(1, 2, 4, 6) %>%
  pivot_longer(cols = codificacao, names_to = "Codificacao", values_to = "y")
df <- df %>% 
  add_row(x = extra[1], y = extra[2], Codificacao = codificacao[1], .before = 1) %>%
  add_row(x = extra[3], y = extra[4], Codificacao = codificacao[2], .before = 2) %>%
  add_row(x = extra[5], y = extra[6], Codificacao = codificacao[3], .before = 3)


ggplot(df, aes(x = x, y = y)) +
  geom_vline(xintercept = 0:length(sequencia), col = "grey50") +      #separacao dos bits
  geom_hline(yintercept = 0, col = "grey50") +        #eixo 0
  geom_line(linewidth = 0.75) +
  facet_wrap(~ Codificacao, nrow = 3) +       #separa em tres quadros
  labs(title = paste0("Comparação de Codigos de Linha: ", codificacao[1], ", ", codificacao[2], " e ", 
                     codificacao[3]), x = "Tempo", y = "Sinal") +
  geom_text(data = texto_sequencia, aes(x = x_pos, y = y_pos, label = label), size = 4) +
  scale_x_continuous(n.breaks = length(sequencia)) +
  scale_y_continuous(limits = c(-(max_y+0.5), max_y+0.5), expand = expansion(add = c(0, 0.5))) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())