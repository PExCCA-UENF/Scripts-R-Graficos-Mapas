## IMPORTANDO E MANIPULANDO DADOS DA PLATAFORMA SEEG DE EMISSÃO DE CO2 POR CATEGORIA

## INSTALANDO PACOTES NECESSÁRIOS
#install.packages("readxl")
#install.packages("tidyverse")
#install.packages("dplyr")

## IMPORTANDO PACOTES NECESSÁRIOS
library(readxl)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(plotly)
library(gganimate)

## A PLATAFORMA SEEG:
# O Sistema de Estimativas de Emissões e Remoções de Gases de Efeito Estufa (SEEG) é uma iniciativa do Observatório do Clima
# que compreende a produção de estimativas anuais das emissões de gases de efeito estufa (GEE) no Brasil, documentos analíticos
# sobre a evolução das emissões e um portal na internet para disponibilização de forma simples e clara dos métodos e dados do
# sistema. (http://seeg.eco.br/)

# Dados: https://plataforma.seeg.eco.br/total_emission#
# Link do drive com os dados importados: https://drive.google.com/drive/folders/1OeVff6kksPQNZvsD42D1gQ78-38GK_3y?usp=sharing


# Por fora, convertemos o arquivo excel em csv separado por vírgulas
# isso porque apesar de alguns pacotes permitirem a leitura direta de arquivos excel, não são a melhor opção

## Importando o arquivo csv e atribuindo à variável "Emissco2"
Emissco2 <- read_excel("~/Estudos/Projetos/Processamento de Dados Ambientais com R - UA UENF/Atividades/PlataformaSEEG_dadosCO2/Arquivos_SEEG/Emissao_CO2e_t _GWP_AR5_1990-2020_Brasil.xlsx")
View(Emissco2)


Emissco2 <- pivot_longer(Emissco2, '1990':'2020',
             names_to = "Ano",
             values_to = "Emissao")
View(Emissco2)


# Analisando "Emissco2"
class(Emissco2) # é um data.frame
dim(Emissco2)   # possui 6 linhas e 32 colunas
str(Emissco2)
View(Emissco2)

# Criando um vetor só com os valores da coluna "Total"
Emissco2_total <- Emissco2$Total
View(Emissco2_total)

# Removendo a coluna "Total"
Emissco2 <- Emissco2 %>% select(-Total)
View(Emissco2)

# Removendo as linhas da Categoria "Total"
linhas_total <- c(156:186)
Emissco2 <- Emissco2[-linhas_total,]
View(Emissco2)

# Substituindo pontos dos chacteres numericos da coluna "Emissão" e transformando os valores em "numeric"
Emissco2$Emissao <- str_replace_all(Emissco2$Emissao, fixed("."), "")
Emissco2$Emissao <- as.numeric(Emissco2$Emissao)
View(Emissco2)
str(Emissco2)

# Criando um vetor com os valores da Categoria "Mudança de Uso da Terra e Florestas"
mutf <- Emissco2 %>%
  filter(Categoria == "Mudança de Uso da Terra e Florestas")
View(mutf)

# Criando um vetor com os valores da Categoria "Energia" e "Agropecuária"
enagro <- Emissco2 %>%
  filter(Categoria %in% c("Energia", "Agropecuária"))
View(enagro)

# Criando colunas "Ano_num" -- anos numéricos ao invés de character
Emissco2 <- Emissco2 %>% mutate(Ano_num = as.numeric(Ano))

mutf <- mutf %>% mutate(Ano_num = as.numeric(Ano))

enagro <- enagro %>% mutate(Ano_num = as.numeric(Ano))

# GRÁFICOS -----------------------
# GRÁFICOS DE LINHA
# Linha: Mudança de uso da terra e florestas
gr_linha_mutf <- ggplot(mutf, aes(x=Ano, y=Emissao, group=Categoria, color=Categoria)) +
  geom_line() +
  theme(axis.text.x = element_text(angle = 30, vjust = .5)) +
  labs(title = "Emissão de CO2 do setor de Mudança de Uso da Terra e Florestas por Ano no Brasil") +
  xlab("Anos") +
  ylab("Emissao") +
  geom_point(alpha=0.7)
gr_linha_mutf

ggplotly(gr_linha_mutf)

# Linha: Energia + Agropecuária
gr_linha_enagro <- ggplot(enagro, aes(x=Ano, y=Emissao, group=Categoria, color=Categoria)) +
  geom_line() +
  theme(axis.text.x = element_text(angle = 30, vjust = .5)) +
  labs(title = "Emissão de CO2 dos setores de Energia e Agropecuária por Ano no Brasil") +
  xlab("Anos") +
  ylab("Emissao") +
  geom_point(alpha=0.7)
gr_linha_enagro

ggplotly(gr_linha_enagro)

# Definindo as informações de visualização da animação e chamando o resultado
gr_linha_anim <- gr_linha_enagro + transition_reveal(Ano_num)
gr_linha_anim


# GRÁFICOS DE BARRAS
Emissco2_barplt <- ggplot(Emissco2, aes(x=Categoria, y=Emissao, color=Categoria, fill=Categoria)) +
  geom_bar(stat="identity") +
  theme(axis.text.y = element_text(angle = 30, vjust = .5)) +
  coord_flip()
Emissco2_barplt

# Definindo as informações de visualização da animação e chamando o resultado
Emissco2_barplt_anim <- Emissco2_barplt + transition_reveal(Ano_num)
Emissco2_barplt_anim

# Definindo as configurações de renderização da animação e chamando o resultado
# height = 600, width = 1500, fps = 30, duration = 5, res = 100
Emissco2_barplt_anim2 <- animate(Emissco2_barplt_anim, height = 1200, width = 3000, fps = 30, duration = 5, res = 200)
Emissco2_barplt_anim2
