#==============================================================================#
#          EXTENSÃO UNIVERSITÁRIA EM CIÊNCIAS CLIMÁTICAS E AMBIENTAIS          #
#          PROJETO 'PROCESSAMENTO E ANÁLISE DE DADOS AMBIENTAIS COM R'         #
#                        Contato: pexcca.lamet@uenf.br                         #
#                       https://linktr.ee/pexcca.lamet                         #
#==============================================================================#

#------------------MAPA - PRINCIPAIS CURSOS D'ÁGUA DO BRASIL-------------------#
#                      Elaboração: Camila Totti Andrade                        #
#                       Revisão: Profa. Eliane B. Santos                       #
#                      Script Atualizado em 22/10/2023                         #
#------------------------------------------------------------------------------#

# Bibliotecas (Pacotes) --------------------------------------------------------
## Para instalar e carregar as bibliotecas necessárias, use os comandos abaixo:
for (p in c('sf', 'geobr', 'ggspatial', 'tidyverse', 'showtext', 'rnaturalearth')) {
  if (!require(p, character.only = T)) {
    install.packages(p, character = T)
  }
  library(p, quietly = T, character.only = T)
}

# Importação dos dados --------------------------------------------------------#

## América do Sul:
AS <- ne_countries(continent = "South America", 
                             returnclass = "sf") %>%
  st_transform(., 'EPSG:4674')

## Brasil:
BR <- AS %>% 
  filter(sovereignt == 'Brazil')

## Sombreamento:
semBR <- AS %>% 
  filter(sovereignt != 'Brazil')

## Agora, vamos fazer o download de uma base de dados do Catálogo de Metadados da 
## Agência Nacional de Águas e Saneamento Básico (ANA). 

## Primeiro, vamor criar uma pasta denominada 'Cursos de Água':
if(!dir.exists('Cursos de Água/')){
  dir.create('Cursos de Água')
}

## Download do arquivo zipado dos cursos d'água:
download.file(url = 'https://metadados.snirh.gov.br/geonetwork/srv/api/records/5dd8982f-afe3-4bf0-88d1-73fd53bc196c/attachments/GEOFT_BHO_REF_CURSO_DAGUA.zip',
              destfile = 'Cursos de Água/CursosdAgua.zip')

## Extraindo o arquivo zipado:
unzip(zipfile = 'Cursos de Água/CursosdAgua.zip', exdir = 'Cursos de Água/') 

##  Leitura do arquivo .shp:
CursosdAgua <- 
  st_read('Cursos de Água/GEOFT_BHO_REF_CURSO_DAGUA.shp')

# Verificando o Sistema de referência de coordenadas (SRC):
st_crs(AS)
st_crs(CursosdAgua)

st_crs(CursosdAgua) <- 'EPSG:4674'   # Atribuindo um SRC.

CursosdAgua$NUORDEMCDA %>%   # Verificando os elementos exclusivos.
  unique() 

## Filtrando os dados:
CursosdAgua1 <- 
  CursosdAgua %>% filter(NUORDEMCDA == 1)

CursosdAgua2 <- 
  CursosdAgua %>% filter(NUORDEMCDA == 2)

CursosdAgua3 <- 
  CursosdAgua %>% filter(NUORDEMCDA == 3)

# Fontes das Letras -----------------------------------------------------------#
fonte_l <- "Roboto Mono"
font_add_google(
  name = fonte_l,
  family = fonte_l)
showtext_auto()

# Produção do Mapa ------------------------------------------------------------#
mapa <-
  ggplot() +
  # América do Sul:
  geom_sf(data = AS, fill = 'gray20', color='seashell3') +
  # Cursos d'agua 3° ordem:
  geom_sf(data = CursosdAgua3, color = 'lightblue4', linewidth = 0.1) +
  # Cursos d'agua 2° ordem:
  geom_sf(data = CursosdAgua2, color = 'lightblue3', linewidth = 0.2) +
  # Cursos d'agua 1° ordem:
  geom_sf(data = CursosdAgua1, color = 'lightblue1', linewidth = 1) +
  # Sombramento fora do Brasil
  geom_sf(data = semBR, fill ='gray60', alpha=0.3) +
  # Limite Brasil:
  geom_sf(data = BR, fill = NA, color='gray20', linewidth = 1.2) +
  # Título e Informações das Coordenadas Geográficas: 
  labs(title = "PRINCIPAIS CURSOS D'ÁGUA DO BRASIL",
       caption = 'Sistema de Coordenadas Geográficas | DATUM SIRGAS 2000 | Fonte: IBGE (2020); ANA (2013) | Elaboração: Camila Totti (@proamb.r)',
       x = NULL,
       y = NULL)+
  coord_sf(xlim = c(-73, -35),
           ylim = c(5, -35))+
  theme(plot.title = element_text(face = 'bold',
                                  size = 70, 
                                  colour = 'gray20',
                                  lineheight = 0.9,
                                  family = fonte_l,
                                  hjust = 0.5),
        legend.position = 'bottom',
        plot.caption = element_text(size = 28,
                                    family = fonte_l, 
                                    hjust = 0.5),
        axis.text = element_text(size = 25, 
                                 family = fonte_l),
        panel.background = element_rect(fill = '#557E84'),
        panel.grid.major = element_line(colour = '#486C71',
                                        linetype = 'twodash'))+
# Escala gráfica:
  annotation_scale(
    location = 'br',      
    line_col = '#F2F2E6',
    text_col = '#F2F2E6',
    style = 'ticks',
    text_cex = 4,
    text_face = 'bold',
    line_width = 3) +
# Seta norte:
  annotation_north_arrow(
    location = 'tr',     
    pad_x = unit(1.0, 'cm'),     
    pad_y = unit(1.0, 'cm'),     
    height = unit(2.0, 'cm'),     
    width = unit(2.0, 'cm'),      
    style = north_arrow_fancy_orienteering(    
      fill = c('#A9A98F', '#F2F2E6'),             
      line_col = 'grey30'))

# Exportando o Mapa -----------------------------------------------------------#
ggsave(filename = 'Cursos de Água/Mapa_CursosÁgua_Brasil.png',
       plot = mapa,
       width = 1080,
       height = 1080,
       units = 'px',
       scale = 3)

#------------------------https://linktr.ee/pexcca.lamet------------------------#
