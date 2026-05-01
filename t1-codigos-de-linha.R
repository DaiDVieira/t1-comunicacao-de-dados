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


cat("Escolha 1 ou 2 Codificações de Linha para apresentar digitando o numero correspondente:
1- NRZ-L  \t2- NRZ-I  \t3- AMI  \t4- Pseudoternário \n5- Manchester  \t6- Manchester Diferencial
7- RZ  \t8-  \t9- \n")
escolha <- scan(n = 2)

#Codificacao
NRZL <- function(x){
  res <- vector("numeric", length = 2 * length(x))
  pos <- 1
  for(i in 1:length(x)){
    if(x[i] == 0){  # 0 - positivo
      res[pos] <- 1
      res[pos+1] <- 1
    }
    else{   # 1 - negativo
      res[pos] <- -1
      res[pos+1] <- -1
    }
    pos <- pos+2
  }
  return(res)
}

NRZI <- function(x){
  
}

#Codificacao equivalente ao numero escolhido
seq_codificada <- matrix(0, nrow = length(escolha), ncol = 2 * length(sequencia))
codificacao <- vector("character", 2)
for (i in 1:length(escolha)){
  if(escolha[i] == 1) {
    seq_codificada[i,] <- NRZL(sequencia)
    codificacao[i] <- "NRZ-I"
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

#Sinal transmitido - geracao do grafico
texto_sequencia <- data.frame(x_pos = seq(0.5, length(sequencia), by = 1),
                              y_pos = max(df$y) + 0.5, 
                              label = sequencia)

#Um metodo de codificacao
plot(pontos_seq[,1], pontos_seq[,2], type = 'l')

df <- as.data.frame(pontos_seq) %>%
  select(x = 1, y = 2)

ggplot(df, aes(x = x, y = y)) +
  geom_vline(xintercept = 0:length(sequencia), col = "grey50") +      #separacao dos bits
  geom_hline(yintercept = 0, col = "grey50") +        #eixo 0
  geom_line(linewidth = 0.75) +
  labs(title = paste("Codigo de Linha", codificacao, sep = " "), x = "Tempo", y = "Sinal") +
  geom_text(data = texto_sequencia, aes(x = x_pos, y = y_pos-0.2, label = label), size = 4) +
  scale_x_continuous(n.breaks = length(sequencia)) +
  scale_y_continuous(limits = c(-1.5, 1.5)) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())


#comparação entre 2 metodos de codificacao
colnames(pontos_seq) <- c("x_cod1", "y_cod1", "x_cod2", "y_cod2")
df <- as.data.frame(pontos_seq) %>%
  select(x = x_cod1, Codigo_1 = y_cod1, Codigo_2 = y_cod2) %>%
  pivot_longer(cols = c("Codigo_1", "Codigo_2"), 
               names_to = "Codificacao", 
               values_to = "y")

ggplot(df, aes(x = x, y = y)) +
  geom_vline(xintercept = 0:length(sequencia), col = "grey50") +      #separacao dos bits
  geom_hline(yintercept = 0, col = "grey50") +        #eixo 0
  geom_line(linewidth = 0.75) +
  facet_wrap(~ Codificacao, nrow = 2) +       #separa em dois quadros
  labs(title = paste("Comparação de Codigos de Linha:", codificacao[1], "e", codificacao[2], sep = " "), 
       x = "Tempo", y = "Sinal") +
  geom_text(data = texto_sequencia, aes(x = x_pos, y = y_pos, label = label), size = 4) +
  scale_x_continuous(n.breaks = length(sequencia)) +
  scale_y_continuous(limits = c(-1.5, 1.5), expand = expansion(add = c(0, 0.5))) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())


