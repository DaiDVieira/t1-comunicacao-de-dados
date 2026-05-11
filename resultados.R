library(tidyverse)

#Sinal transmitido
texto_sequencia <- data.frame(x_pos = seq(0.5, length(sequencia), by = 1),
                              y_pos = max_y + 0.5, 
                              label = sequencia)

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
  select(x = 1, Codigo_1 = 2, Codigo_2 = 4) %>%
  pivot_longer(cols = c("Codigo_1", "Codigo_2"), names_to = "Codificacao", values_to = "y") %>% 
  add_row(x = extra[1], y = extra[2], Codificacao = "Codigo_1", .before = 1) %>% 
  add_row(x = extra[3], y = extra[4], Codificacao = "Codigo_2", .before = 2)


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

