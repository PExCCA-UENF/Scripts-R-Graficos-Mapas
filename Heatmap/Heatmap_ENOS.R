#==============================================================================#
#          EXTENSÃO UNIVERSITÁRIA EM CIÊNCIAS CLIMÁTICAS E AMBIENTAIS          #
#          PROJETO "PROCESSAMENTO E ANÁLISE DE DADOS AMBIENTAIS COM R"         #
#                        Contato: pexcca.lamet@uenf.br                         #
#                       https://linktr.ee/pexcca.lamet                         #
#                       Script Atualizado em 31/05/2023                        #
#==============================================================================#

#                  HEATMAP - EL NIÑO OSCILAÇÃO SUL (ENOS)                      #

# Bibliotecas (Pacotes) --------------------------------------------------------
## Para instalar e carregar as bibliotecas necessárias, use os comandos abaixo:
for (p in c("tidyverse", "showtext", "ggplot2")) {
  if (!require(p, character.only = T)) {
    install.packages(p, character = T)
  }
  library(p, quietly = T, character.only = T)
}

# Importação e organização dos dados -------------------------------------------
## Índice Oceânico Niño (Oceanic Niño Index – ONI).
## O ONI é definido pela média móvel de três meses das anomalias de TSM na região 
## Niño 3.4, onde anomalias iguais ou maiores que 0,5°C estão associadas a El Niño  
## e iguais ou menores que -0,5°C estão associadas a La Niña. As anomalias são coloridas  
# em vermelho e azul quando o limite é atingido por no mínimo cinco valores consecutivos. 


ONI <- 
  read.table(
    file = "https://www.cpc.ncep.noaa.gov/data/indices/oni.ascii.txt",
    header = T
  )
head(ONI)

ONI <- ONI %>%
  mutate(Episódios = NA)

for (i in 5:nrow(ONI)) {
  if ((ONI$ANOM[i-4] >= 0.5) & 
      (ONI$ANOM[i-3] >= 0.5) & 
      (ONI$ANOM[i-2] >= 0.5) & 
      (ONI$ANOM[i-1] >= 0.5) & 
      (ONI$ANOM[i] >= 0.5)) {
    ONI$Episódios[i-4] <- "El Niño"
    ONI$Episódios[i-3] <- "El Niño"
    ONI$Episódios[i-2] <- "El Niño"
    ONI$Episódios[i-1] <- "El Niño"
    ONI$Episódios[i] <- "El Niño"
  } else if ((ONI$ANOM[i-4] <= -0.5) & 
             (ONI$ANOM[i-3] <= -0.5) & 
             (ONI$ANOM[i-2] <= -0.5) & 
             (ONI$ANOM[i-1] <= -0.5) & 
             (ONI$ANOM[i] <= -0.5)) {
    ONI$Episódios[i-4] <- "La Niña"
    ONI$Episódios[i-3] <- "La Niña"
    ONI$Episódios[i-2] <- "La Niña"
    ONI$Episódios[i-1] <- "La Niña"
    ONI$Episódios[i] <- "La Niña"
  }
}

ONI <-
  ONI %>%
  mutate(ANOM2 =
           if_else(is.na(Episódios), NA, ANOM)) %>%
  mutate(SEAS = 
           factor(SEAS,
                  levels = 
                    c("DJF", "JFM", "FMA",
                      "MAM", "AMJ", "MJJ",
                      "JJA", "JAS", "ASO",
                      "SON", "OND", "NDJ"))
  )
head(ONI, n = 12)

# Fontes das Letras ------------------------------------------------------------
fonte_l <- "Roboto Mono"
font_add_google(
  name = fonte_l,
  family = fonte_l)
showtext_auto()

# Heatmap ----------------------------------------------------------------------
ggplot(
  data = ONI %>% filter(YR >= 1997),
  mapping = aes(x = SEAS, y = YR, fill = ANOM2, label = ANOM)
) +
  geom_tile(
    color = rgb(0,0,0,0.3),
    lwd = 0.5,
    show.legend = T
  ) +
  geom_text(
    mapping = aes(family = fonte_l),
    color = "black",  fontface = "italic",
    size = 3.8) +
  scale_x_discrete(
    position = "top"
  ) +
  scale_y_continuous(
    breaks = 1997:2023,
    trans = "reverse"
  ) +
  scale_fill_gradient2(
    low = "#0571b0",
    high = "#ca0020",
    mid = "#f7f7f7",
    midpoint = 0,
    na.value = "#f7f7f7",
    limits = c(min(ONI$ANOM), max(ONI$ANOM)),
    breaks = c(min(ONI$ANOM), 0, max(ONI$ANOM)),
    guide = "legend",
    labels = c("La Niña", "Neutralidade","El Niño")) +
  labs(x = "", y = "", fill = "",
       title = "Índice Oceânico Niño (ONI)", 
       subtitle = "Média móvel trimestral das anomalias de TSM na região Niño 3.4 (ºC)",
       caption = "Fonte de dados: NCEP/NOAA") +
  theme_void() +
  theme(
    text = element_text(family = fonte_l, colour = "gray20"),
    # plot.background = element_rect(fill = "#faf7f2"),
    plot.title = element_text(hjust = 0.5, vjust = 1, size = 26, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, vjust = 4, size = 15),
    plot.caption = element_text(hjust = 0.99, vjust = 26, size = 14, color = "#53738c", face = "italic"),
    axis.text = element_text(size = 18),
    axis.text.x = element_text(face = "italic", vjust = -5),
    axis.text.y = element_text(margin = margin(0, 0, 0, 5)),
    legend.key.size = unit(0.3, "cm"),
    legend.text = element_text(size = 12),
    legend.position = "bottom",
    legend.justification = "right",
    legend.box.margin = margin(-10, 5, 0, 0),
    legend.spacing.x = unit(0.1, 'cm'),
  ) +
  coord_fixed(ratio = 1/3)

ggsave(filename = "ONI.png",
       width = 1220, height = 1100, units = "px", 
       bg = "#faf7f2"
)
#------------------------https://linktr.ee/pexcca.lamet------------------------#
