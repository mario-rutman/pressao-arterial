library(tidyverse)


# o objetivo é criar uma função que fará a tabela de pressão arterial 
# apenas substituindo os vetores SIS e DIA

func_press_art <- function(a, b, c){
  
#coluna dia.  
  d <- c("1º dia", "2º dia", "3º dia", "4º dia")
dia <- c(rep(d, each = 6))

# Coluna data.
s <- c
data <- c(rep(as.POSIXlt(s), each = 6))

# Coluna turno.
t <- c("manhã 1", "manhã 2", "manhã 3", "tarde 1", "tarde 2", "tarde 3")
turno <- c(rep(t,4))

# Coluna exercício aeróbico.
e <- c("sim", "não")
exer_aerobico <- rep(c(rep(e, each = 6)), 2)

# Coluna SIS.
SIS <- a

# Coluna DIA
DIA <- b

# Criando o df. 
df_press_art <- data.frame(dia, data, turno, exer_aerobico, SIS, DIA, stringsAsFactors = FALSE)

# Reunindo as colunas SIS e DIA em tipo_press e colocando seus valores em valor_press.

df_press_arterial <- df_press_art %>% gather(key = tipo_press,
                                             value = valor_press, 5:6)

# Criando o gráfico.
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
}

j <- c(139,129,123,113,120,118,
       131,132,120,112,119,131,
       136,127,131,122,122,118,
       135,126,125,118,129,127) #SIS

h <- c(96,91,88,79,80,79, 
       87,85,85,78,80,89,
       90,85,88,78,77,71,
       93,94,80,78,81,84) #DIA

w <- c("2018-04-15", "2018-04-16", "2018-04-17", "2018-04-18") #data

func_press_art(a = j, b = h, c = w)

