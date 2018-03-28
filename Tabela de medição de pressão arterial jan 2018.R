library(tidyverse)
# Fazendo uma tabela de pressão arterial para transformar em grã¡fico.
# Criando as colunas com 24 + 1 linhas.

d <- c("1º dia", "2º dia", "3º dia", "4º dia")
dia <- c(rep(d, each = 6))

# Coluna data.

s <- c("2018-01-08", "2018-01-09", "2018-01-10", "2018-01-11")
data <- c(rep(as.POSIXlt(s), each = 6))

# Coluna turno.
t <- c("manhã 1", "manhã 2", "manhã 3", "tarde 1", "tarde 2", "tarde 3")
turno <- c(rep(t,4))

# Coluna exercã?cio aerã³bico.
e <- c("sim", "não")
exer_aerobico <- c("não", "não", "não", "não", "não", "não", "não", "não", "não",
                   "não", "não", "não", "não", "não", "não", "não", "não", "não",
                   "não", "não", "não", "não", "não", "não")

# Coluna SIS. Vetor com 24 nãºmeros inteiros.
SIS <- c(134, 131, 130, 135, 130, 123, 124, 125, 120, 128, 122, 121, 
         122, 141, 125, 127, 121, 127, 119, 130, 122, 120, 113, 121)

# Coluna DIA. vetor com 24 nãºmeros inteiros.
DIA <- c(90, 89, 86, 88, 90, 86, 84, 89, 85, 86, 77, 79, 89, 99, 86,
         87, 81, 87, 81, 87, 90, 75, 79, 80)

# Criando o df. 
df_press_art <- data.frame(dia, data, turno, exer_aerobico, SIS, DIA, stringsAsFactors = FALSE)

# Reunindo as colunas SIS e DIA em tipo_press e colocando seus valores em valor_press.

df_press_arterial <- df_press_art %>% gather(key = tipo_press,
                                             value = valor_press, 5:6)
# Criando o grã¡fico.
ggplot(df_press_arterial, aes(x = factor(turno), y = valor_press, fill = factor(tipo_press))) +
  geom_bar(stat = "identity", position = "dodge", colour = "black", size = 1.2) + 
  geom_text(aes(label = valor_press), position = position_dodge(0.9),
            vjust = 2, size = 5.0, colour="black")+
  geom_hline(yintercept = 80, linetype = "dashed")+
  geom_hline(yintercept = 120, linetype = "dashed")+
  scale_y_continuous( breaks = c(80, 120))+
  facet_wrap( ~ data, ncol = 2)+
  theme_bw() + scale_fill_manual(values=c("red", "#9999FF")) +
  theme(panel.grid.major = element_line(linetype = "blank"), 
        panel.grid.minor = element_line(linetype = "blank"), 
        plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        panel.background = element_rect(fill = "aliceblue"), 
        strip.text.x = element_text(size = 20),  
        strip.background = element_rect(colour = "black",
                                        fill = "green")) + 
  labs(x = NULL, y = NULL, subtitle = "Média Final")  + 
  theme(axis.ticks = element_line(linetype = "blank"), 
        panel.grid.minor = element_line(linetype = "blank"), 
        axis.text.x = element_text(size = 13), 
        axis.text.y = element_text(size = 10)) + labs(subtitle = NULL) +
  theme(axis.ticks = element_line(colour = "mediumpurple4"), 
        legend.position = "none")




