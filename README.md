# Trabalho 1 de Comunicação de Dados
## Estudo e Representação de Sinais Digitais por meio de Códigos de Linha variados

Esse repositório foi criado para guardar a parte prática do trabalho 1 da disciplina de Comunicação de Dados, da Universidade Federal de Santa Maria (UFSM).

Feito pelas alunas Daiane Dias Vieira e Giovana Borelli.

Pode-se testar um código de linha por vez ou comparar duas/três sequências codificadas diferentes para uma mesma sequência de bits inicial.

O arquivo [t1-codigos-de-linha.R](t1-codigos-de-linha.R) une todos os códigos. Porém dividiu-se ele em 4 arquivos separados para melhor organização de funcionalidades:
* [usuario.R](usuario.R): possui duas formas de leituras do terminal da sequência de bits e possui a leitura de quais os códigos que serão aplicados.
* [codigos_de_linha.R](codigos_de_linha.R): possui as funções de aplicação de cada código de linha.
* [processamento.R](processamento.R): aplica os códigos de linha e transforma as sequências lidas em pontos para a geração do gráfico.
* [resultados.R](resultados.R): possui os códigos para rodar cada quantidade de códigos de linha aplicados (1, 2 ou 3).

## Compilação recomendada do código
Deve-se compilar algumas partes do código separadamente, por causa da leitura feita pelo terminal. Abaixo há a sequência de códigos recomendada:

### Biblioteca utilizada:
```
library(tidyverse)
```

### Leitura da Sequência de Bits
Somente é possível digitar sequências de 0 e 1, de qualquer tamanho. Pode ser feita por dois métodos.

1. A leitura é feita bit a bit. A parada da leitura é feita pela tecla enter numa linha vazia:
```
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
```

2. A leitura é feita numa só linha:
```
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
```

### Escolha de Codificação
Pode-se escolher uma ou duas das seguintes codificações de linha, por meio do código abaixo:
1. NRZ-L
2. NRZ-I
3. AMI
4. Pseudoternário
5. Manchester
6. Manchester Diferencial
7. ZR
8. 2B1Q
9. MLT-3

```
cat("Escolha 1 ou 2 Codificações de Linha para apresentar digitando o numero correspondente:
1- NRZ-L  \t2- NRZ-I  \t3- AMI  \t4- Pseudoternário \n5- Manchester  \t6- Manchester Diferencial
7- RZ  \t8- 2B1Q  \t9- MLT-3 \n")

escolha <- scan(n = 2)
```

### Funções de Codificação
Há uma função (function) para cada código de linha, então, para diminuir a extensão desta explicação, ela estará somente no código deste repositório e separadas no arquivo [codigos_de_linha.R](codigos_de_linha.R).

### Codificação da Sequência de bits
Realiza-se a transformação da sequência de bits. Para códigos não bifásicos, cada valor do bit é duplicado. O vetor extra é necessário para os códigos NRZ-I, Manchester Diferencial e MLT-3, pois iniciam em uma posição fixa.

```
seq_codificada <- matrix(0, nrow = length(escolha), ncol = 2 * length(sequencia))
codificacao <- vector("character", 3)
extra <- vector("numeric", 2 * length(escolha))

for (i in 1:length(escolha)){
  if(escolha[i] == 1) {
    seq_codificada[i,] <- NRZL(sequencia)
    codificacao[i] <- "NRZ-L"
  }
  else if(escolha[i] == 2){
    seq_codificada[i,] <- NRZI(sequencia)
    codificacao[i] <- "NRZ-I"
    extra[i*2] <- 1 
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

```

### Matriz de Pontos
Cria-se uma matriz com os pontos para a geração dos gráficos que representam visualmente os valores do sinal digital.

```
pontos_seq <- matrix(0, nrow = 4 * length(sequencia), ncol = 2 * length(escolha))
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
max_y <- max(seq_codificada)
```

### Geração dos gráficos
Criação do texto representativo dos bits da sequência:

```
texto_sequencia <- data.frame(x_pos = seq(0.5, length(sequencia), by = 1),
                              y_pos = max(df$y) + 0.5, 
                              label = sequencia)
```

* Gráfico de um único código de linha:
```
df <- as.data.frame(pontos_seq) %>%
  select(x = 1, y = 2)
df <- df %>% add_row(x = extra[1], y = extra[2], .before = 1)

ggplot(df, aes(x = x, y = y)) +
  geom_vline(xintercept = 0:length(sequencia), col = "grey50") + 
  geom_hline(yintercept = 0, col = "grey50") + 
  geom_line(linewidth = 0.75) +
  labs(title = paste("Código de Linha", codificacao, sep = " "), x = "Tempo", y = "Sinal") +
  geom_text(data = texto_sequencia, aes(x = x_pos, y = y_pos-0.2, label = label), size = 4) +
  scale_x_continuous(n.breaks = length(sequencia)) +
  scale_y_continuous(limits = c(-(max_y+0.5), max_y+0.5)) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
```

* Gráfico Comparativo entre dois códigos de linha:
```
df <- as.data.frame(pontos_seq) %>%
  select(x = 1, Codigo_1 = 2, Codigo_2 = 4) %>%
  pivot_longer(cols = c("Codigo_1", "Codigo_2"), names_to = "Codificacao", values_to = "y") %>% 
  add_row(x = extra[1], y = extra[2], Codificacao = "Codigo_1", .before = 1) %>% 
  add_row(x = extra[3], y = extra[4], Codificacao = "Codigo_2", .before = 2)

ggplot(df, aes(x = x, y = y)) +
  geom_vline(xintercept = 0:length(sequencia), col = "grey50") + 
  geom_hline(yintercept = 0, col = "grey50") +   
  geom_line(linewidth = 0.75) +
  facet_wrap(~ Codificacao, nrow = 2) +  
  labs(title = paste("Comparação de Codigos de Linha:", codificacao[1], "e", codificacao[2], sep = " "), 
       x = "Tempo", y = "Sinal") +
  geom_text(data = texto_sequencia, aes(x = x_pos, y = y_pos, label = label), size = 4) +
  scale_x_continuous(n.breaks = length(sequencia)) +
  scale_y_continuous(limits = c(-(max_y+0.5), max_y+0.5), expand = expansion(add = c(0, 0.5))) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
```

* Gráfico Comparativo entre três códigos de linha:
```
df <- as.data.frame(pontos_seq) %>%
  select(x = 1, Codigo_1 = 2, Codigo_2 = 4, Codigo_3 = 6) %>%
  pivot_longer(cols = c("Codigo_1", "Codigo_2", "Codigo_3"), names_to = "Codificacao", values_to = "y")
df <- df %>% 
  add_row(x = extra[1], y = extra[2], Codificacao = "Codigo_1", .before = 1) %>%
  add_row(x = extra[3], y = extra[4], Codificacao = "Codigo_2", .before = 2) %>%
  add_row(x = extra[5], y = extra[6], Codificacao = "Codigo_3", .before = 3)

ggplot(df, aes(x = x, y = y)) +
  geom_vline(xintercept = 0:length(sequencia), col = "grey50") +      #separacao dos bits
  geom_hline(yintercept = 0, col = "grey50") +        #eixo 0
  geom_line(linewidth = 0.75) +
  facet_wrap(~ Codificacao, nrow = 3) +       #separa em dois quadros
  labs(title = paste("Comparação de Codigos de Linha:", codificacao[1], "e", codificacao[2], "e", 
                     codificacao[3], sep = " "), x = "Tempo", y = "Sinal") +
  geom_text(data = texto_sequencia, aes(x = x_pos, y = y_pos, label = label), size = 4) +
  scale_x_continuous(n.breaks = length(sequencia)) +
  scale_y_continuous(limits = c(-(max_y+0.5), max_y+0.5), expand = expansion(add = c(0, 0.5))) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
```
