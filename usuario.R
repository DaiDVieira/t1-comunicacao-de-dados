#opcao 1 de leitura
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

#opcao 2 de leitura
library(stringr)
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
7- ZR  \t8- 2B1Q \t9- MLT-3\n")
escolha <- scan(n = 3)
