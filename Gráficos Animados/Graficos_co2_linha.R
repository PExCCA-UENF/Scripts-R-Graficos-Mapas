# VERSÃO GRÁFICO DE LINHA DO co2

# PACOTES UTILIZADOS
library(tidyverse)
library(dplyr)
library(ggthemes)
library(tidyr)
library(ggplot2)
library(plotly)
library(gganimate)
library(gifski)
library(av)
library(RColorBrewer)
library(timetk)  # transforma em tibble
library(zoo)   # extrai colunas de meses da série temporal
library(htmlwidgets)  # exporta arquivos em formato html


# Transformando o Time-Series co2 em data.frame (esse processo contou com a ajuda de Nícolas)
co2_df <- data.frame(ANO = rep(1959:1997, times=1, each=12), 
                     MES = rep(1:12, times=39, each=1),
                     CO2 = as.numeric(co2))
View(co2_df)


co2_df$MES <- as.character(co2_df$MES)
View(co2_df)

# Indicando a ordem dos meses para que seja plotado corretamente na legenda do gráfico
co2_df$MES <- factor(co2_df$MES, levels = c('1', '2', '3', '4', '5', '6', '7', '8', '9', '10', '11', '12'))

# Criando um line plot
gr <- ggplot(co2_df, aes(x=ANO, y=CO2, color=MES)) + 
  geom_line() +
  scale_color_brewer(labels=c('Jan', 'Fev', 'Mar', 'Abr', 'Mai', 'Jun', 'Jul', 'Ago', 'Set', 'Out', 'Nov', 'Dez'), palette = "Paired") +
  labs(title = "Concentração atmosférica de CO2 (ppm) de 1959 até 1997") +
  xlab("Anos") +
  ylab("CO2 (ppm)") +
  theme_bw() +
  geom_point(alpha=0.7)
gr

# OBS. Se eu quisesse mudar apenas o label dos itens, sem definir uma outra paletta de cores, eu poderia usar a função scale_color_hue():
##  scale_color_hue(labels=c('Jan', 'Fev', 'Mar', 'Abr', 'Mai', 'Jun', 'Jul', 'Ago', 'Set', 'Out', 'Nov', 'Dez')) 


ggplotly(gr)  # não reconhece a legenda do `gr` com scale_color_hue() nem scale_color_brewer() - só as cores, não sei pq


# Definindo as informações de visualização da animação e chamando o resultado
gr_anim_line <- gr + transition_reveal(ANO) +
  view_follow(fixed_y = TRUE)
gr_anim_line

# Definindo as configurações de renderização da animação e chamando o resultado
gr_anim_line1 <- animate(gr_anim_line, height = 500, width = 800, fps = 30, duration = 5, res = 100)
gr_anim_line1

# Salvando a animação como gif
anim_save("anim_co2_line2.gif")

# Salvando a animação como vídeo
av::av_encode_video(gr_anim_line1, output = "anim_co2_line6s.mp4")
