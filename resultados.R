library(tidyverse)

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
  scale_x_continuous(n.breaks = length(sequencia)/2) +
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

