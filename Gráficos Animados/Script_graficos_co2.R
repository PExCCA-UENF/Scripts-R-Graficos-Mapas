# ANIMANDO E TORNANDO INTERATIVO O NOVO DF CO2_DF

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


# ANIMANDO E TORNANDO INTERATIVO O NOVO DF CO2_DF  --- FUNCIONA

# VERSÃO CONTÍNUA DO SCATTER PLOT
# Transformando o Time-Series co2 em data.frame (esse processo contou com a ajuda de Nícolas)
co2_df <- data.frame(ANO = rep(1959:1997, times=1, each=12), 
                     MES = rep(1:12, times=39, each=1),
                     CO2 = as.numeric(co2))
View(co2_df)

# Criando um vetor com o nome "Mes" com os valores da coluna "MES" de co2_df
Mes <- co2_df$MES

# Criando um scatter plot contínuo
gr <- ggplot(co2_df, aes(x=ANO, y=CO2, color=Mes)) + 
  geom_jitter(alpha = 0.7, size=2) +
  scale_color_continuous_tableau(palette = "Orange") +
  labs(title = "Atmospheric CO2 concentration (ppm) from 1959 to 1997") +
  xlab("Anos") +
  ylab("CO2 (ppm)")+
  theme_bw()
gr

# Visualização Interativa
ggplotly(gr)

# Definindo as informações de visualização da animação e chamando o resultado
gr_animate <- gr + transition_time(ANO) +
  labs(subtitle = "ANO: {frame_time}") +
  shadow_wake(wake_length = 0.1)
gr_animate

# Definindo as configurações de renderização da animação e chamando o resultado
gr_animate1 <- animate(gr_animate, height = 500, width = 800, fps = 30, duration = 10, res = 100)
gr_animate1

# Salvando a animação
anim_save("anim_atm_co2.gif")


#__________________________________________________________________________
# VERSÃO DISCRETA DO SCATTER PLOT | Faltam alguns ajustes

# A paletta de cores de MES tem que ser discreta, por isso vamos convertê-la para as.character()
Mes_c <- as.character(Mes)


# Criando um scatter plot discreto
gr_disc <- ggplot(co2_df, aes(x=ANO, y=CO2, color=Mes_c)) + 
  geom_jitter(alpha = 0.7, size=2) +
  scale_color_brewer(palette="Paired") +
  labs(title = "Atmospheric CO2 concentration (ppm) from 1959 to 1997") +
  xlab("Anos") +
  ylab("CO2 (ppm)")+
  theme_bw()
gr_disc

# Visualização Interativa
ggplotly(gr_disc)

# Definindo as informações de visualização da animação e chamando o resultado
gr_disc_animate <- gr_disc + transition_time(ANO) +
  labs(subtitle = "ANO: {frame_time}") +
  shadow_wake(wake_length = 0.1)
gr_disc_animate

# Definindo as configurações de renderização da animação e chamando o resultado
gr_disc_animate1 <- animate(gr_disc_animate, height = 500, width = 800, fps = 30, duration = 10, res = 100)
gr_disc_animate1

# Salvando a animação
anim_save("anim_atm_co2_disc.gif")

# NOTA: ainda falta conseguir ordenar os valores em character de "Mes_c" para que eles apareçam na ordem crescente na legenda

# SOBRE O PROCESSO: 
# nos dois primeiros dias eu consegui criar o gráfico interativo e o gráfico animado, porém descobri
# por acaso, conversando com o Nícolas, que estava manuseando os dados errados. Eu estava trabalhando com o CO2, quando
# em realidade era para estar trabalhando com o co2 (com caps baixa).

# em seguida comecei a trabalhar com os dados certos, do co2, entretando ele estava no formato Time-Series e, por nunca ter
# manuseado esse tipo de dado antes, também tive dificuldade. Com a ajuda de Nícolas, consegui converter co2 de TS para
# data.frame. A partir daí começou de verdade o trabalho.

# Nos dois dias seguintes eu passei a tentar criar um scatter plot e torná-lo interativo e gerar uma animação com ele.
# Tive alguns problemas com o plotly e descobri que tinha a ver com estar usando o formato markdown, apesar de ainda não
# ter entendido muito bem o porque. Também tive alguns outputs de erro na hora de gerar a animação, mas depois descobri que
# era devido ao fato de estar tentando gerar um plot com mais de uma geometria (eu queria adicionar uma linha de regressão
# geom_smooth() ao meu scatter plot geom_jitter()). Pra frente quero descobrir como realizar animações com mais de uma geome
# tria no mesmo plot. 

# Por fim eu consegui atingir em 90% do que eu queria, que era gerar um gráfico interativo e uma animação com os dados de CO2.
# Eu fiz duas versões, uma utilizando uma classificação de cor que levasse em conta os meses como dados contínuos (essa deu
# tudo certo), e outra versão com uma classificação discreta dos meses (essa faltou eu resolver alguns detalhes de ordena_
# mento dos números).

# De resto, a maior parte da descrição do processo está presente no arquivo markdown "Graficos_co2.Rmd" e "Graficos_co2_updt.Rmd".

#__________________________________________________________________________
# DAQUI PRA FRENTE É APENAS UM REGISTRO PARCIAL DE ERROS NO CÓDIGO, ALGUNS FORAM RESOLVIDOS E OUTROS AINDA NÃO ENTENDI O QUE SIGNIFICAM

# TÁ DANDO ERRO N SEI PQQQQ 
# Algumas questões para tentar entender...
co2_df <- data.frame(ANO = rep(1959:1997, times=1, each=12), 
                     MES = rep(1:12, times=39, each=1),
                     CO2 = as.numeric(co2))


gr <- ggplot(co2_df, aes(x=ANO, y=CO2, colour = CO2)) + 
  geom_jitter(shape=16, alpha = 0.5, size = 2, show.legend = FALSE) +
  scale_color_continuous_tableau(palette = "Orange") +
  geom_smooth(lwd=0.5, col="darkred") +
  labs(title = "Monthly atmospheric CO2 concentration in ppm from 1959 to 1997") +
  xlab("Anos") +
  ylab("CO2 (ppm)")

print(gr)
ggplotly(gr)  # estava dando erro no arquivo markdown, n sei pq

#TÁ DANDO ERRO | Não sei pqqq
htmlwidgets::saveWidget(widget = gr, 
   file = "gr.html", 
   selfcontained = TRUE )


# TÁ DANDO ERRO | Update: o erro é pq nao pode ter mais de uma geometria na mesma animação (geom_jitter + geom_smooth)
gr_animate <- gr + transition_states(ANO) +
  #labs(subtitle = ": ANO{frame}") +
  shadow_wake(wake_length = 0.1)
gr_animate

animate(gr_animate, height = 500, width = 800, fps = 30, duration = 10, res = 100) # Error in `$<-.data.frame`(`*tmp*`, "group", value = "") :
                                                                                        # replacement has 1 row, data has 0

anim_save("gr_animate.gif")



