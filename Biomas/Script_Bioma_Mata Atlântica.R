#==============================================================================#
#          EXTENSÃO UNIVERSITÁRIA EM CIÊNCIAS CLIMÁTICAS E AMBIENTAIS          #
#          PROJETO 'PROCESSAMENTO E ANÁLISE DE DADOS AMBIENTAIS COM R'         #
#                        Contato: pexcca.lamet@uenf.br                         #
#                       https://linktr.ee/pexcca.lamet                         #
#==============================================================================#

#--------------------------MAPA - BIOMA MATA ATLÂNTICA-------------------------#
#                       Elaboração: Camila Totti Andrade                       #
#           Revisão: Nícolas C. Nogueira e Profa. Eliane B. Santos             #
#                      Script Atualizado em 20/10/2023                         #
#------------------------------------------------------------------------------#

# Bibliotecas (Pacotes) --------------------------------------------------------
## Para instalar e carregar as bibliotecas necessárias, use os comandos abaixo:
for (p in c('sf', 'geobr', 'ggspatial', 'tidyverse', 'magick')) {
  if (!require(p, character.only = T)) {
    install.packages(p, character = T)
  }
  library(p, quietly = T, character.only = T)
}

# Importação dos dados --------------------------------------------------------#
# Vamos fazer o donwload de uma base de dados do Catálogo de Metadados da Agência
# Nacional de Águas e Saneamento Básico (ANA). 

# Primeiro, vamor criar uma pasta denominada 'Biomas':
if(!dir.exists('Biomas/')){
  dir.create('Biomas')
}

# Agora, vamos fazer o download do arquivo zipado:
download.file(url = 'https://metadados.snirh.gov.br/geonetwork/srv/api/records/7cfd53c4-b4e1-4aba-a79b-857a19649df6/attachments/GEOFT_PAIS.zip',
              destfile = 'Biomas/America.zip')

# Extraindo o arquivo zipado:
unzip(zipfile = 'Biomas/America.zip', exdir = 'Biomas/')   

#  Leitura do arquivo .shp:
America <- read_sf('Biomas/GEOFT_PAIS/GEOFT_PAIS.shp')

# Para o mapa do Brasil e do bioma Mata Atlântica, vamos usar o pacote 'geobr':
BR <- geobr::read_country(year = 2020)

Mata_atlan <-
  geobr::read_biomes() %>%
  filter(name_biome == 'Mata Atlântica')

# Produção do Mapa -------------------------------------------------------------
mapa <-
  ggplot() +
  # América do Sul:
  geom_sf(
    data = America$geometry,
    mapping = aes(fill =  'América do Sul'),
    color = 'gray60',
    size = 3
  ) +
  # Brasil:
  geom_sf(
    data = BR,
    mapping = aes(fill =  'Brasil'),
    color = 'gray70',
    size = 2
  ) +
  # Mata Atlântica:
  geom_sf(
    data = Mata_atlan,
    mapping = aes(fill = name_biome),
    color = '#4D5C12',
    alpha = 0.8,
    size = 4
  ) +
  # Definindo as cores:
  scale_fill_manual(
    values = c('#DDDDC7', '#F2F2E6', '#4D5C12'),
    name = NULL
  ) +
  # Legendas
  geom_label(
    mapping = aes(x = -65, y = -20, label = 'América do Sul'),
    fill = '#FFFFFF',
    color = 'gray30',
    size = 7,
    alpha = 0.3
  ) +
  geom_label(
    mapping = aes(x = -55, y = -10, label = 'Brasil'),
    fill = '#FFFFFF',
    color = 'gray30',
    size = 7,
    alpha = 0.3
  ) +
  geom_label(
    mapping = aes(x = -40, y = -22, label = 'Mata Atlântica'),
    fill = '#FFFFFF',
    color = 'gray30',
    size = 7,
    alpha = 0.3
  ) +
  # Informações das Coordenadas Geográficas:
  geom_label(
    mapping = aes(x = -68, y = -32),
    label = 'DATUM SIRGAS 2000 \n Sistema de Coordenadas Geográficas \n Fonte: IBGE (2019; 2020); ANA (2010) \n Elaborado por @proamb.r', 
    fill = '#FFFFFF',
    size = 4,
    alpha = 0.5
  ) +
  # Título
  labs(
    title = 'BIOMA MATA ATLÂNTICA - BRASIL',
    x = NULL,
    y = NULL
  ) +
  # Tema do plot:
  theme(
    plot.title =
      element_text(
        face = 'bold',
        size = 20,
        lineheight = 0.9,
        hjust = 0.5
      ),
    legend.position = 'none',
    legend.background = element_rect(fill = "#FFFFFF",
                                     linetype = "solid",
                                     colour = "#000000"),
        plot.caption = element_text(size = 10),
    panel.background = element_rect(fill = '#557E84'),
    panel.grid.major = element_line(
      colour = '#486C71',
      linetype = 'twodash'
    )
  ) +
  guides(fill = guide_legend(reverse = TRUE)) +
  # Limites x e y do plot:
  coord_sf(
    xlim = c(-73, -35),
    ylim = c(5, -34)
  ) +
  # Escala gráfica:
  annotation_scale(
    location = 'bl',
    line_col = 'gray30',
    text_col = 'grey30',
    style = 'ticks',
    text_cex = 1,
    text_face = 'bold',
    line_width = 4
  ) +
  # Seta norte:
  annotation_north_arrow(
    location = 'tr',
    pad_x = unit(1.0, 'cm'),
    pad_y = unit(1.0, 'cm'),
    height = unit(2.0, 'cm'),
    width = unit(2.0, 'cm'),
    style = north_arrow_fancy_orienteering(
      fill = c('#A9A98F', '#F2F2E6'),
      line_col = 'grey30'
    )
  )

# Exportando o Mapa ------------------------------------------------------------
ggsave(filename = 'Biomas/Mapa_Mata_Atlantica.png',
       plot = mapa,
       width = 1080,
       height = 1080,
       units = 'px',
       scale = 3)

#------------------------https://linktr.ee/pexcca.lamet------------------------#
