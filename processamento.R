#Codificacao equivalente ao numero escolhido

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
