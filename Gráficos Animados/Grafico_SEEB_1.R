## IMPORTANDO E MANIPULANDO DADOS DA PLATAFORMA SEEG DE EMISSÃO DE CO2 POR CATEGORIA

## INSTALANDO PACOTES NECESSÁRIOS
#install.packages("readxl")
#install.packages("tidyverse")
#install.packages("dplyr")

## IMPORTANDO PACOTES NECESSÁRIOS
library(readxl)
library(tidyverse)
library(dplyr)

## A PLATAFORMA SEEG:
# O Sistema de Estimativas de Emissões e Remoções de Gases de Efeito Estufa (SEEG) é uma iniciativa do Observatório do Clima
# que compreende a produção de estimativas anuais das emissões de gases de efeito estufa (GEE) no Brasil, documentos analíticos
# sobre a evolução das emissões e um portal na internet para disponibilização de forma simples e clara dos métodos e dados do
# sistema. (http://seeg.eco.br/)

# Dados: https://plataforma.seeg.eco.br/total_emission#

## Importando o arquivo excel como tibble e atribuindo à variável "Emissco2"
Emissco2 <- read_excel("~/Estudos/Projetos/Processamento de Dados Ambientais com R - UA UENF/Atividades/PlataformaSEEG_dadosCO2/Arquivos_SEEG/Emissao_CO2e_t _GWP_AR5_1990-2020_Brasil.xlsx",
                       sheet = "Sheet1"
                       )
View(Emissco2)


# Definindo como data.frame
Emissco2 <- Emissco2 %>% as.data.frame(Emissco2)
View(Emissco2)


# Analisando "Emissco2"
class(Emissco2) # é um data.frame
dim(Emissco2)   # possui 6 linhas e 32 colunas
str(Emissco2)
View(Emissco2)

# Removendo a coluna "Total"
Emissco2 <- Emissco2 %>% select(-Total)
View(Emissco2)

# Convertendo os valores de character para numéricos -- vai dar erro !
Emissco2 %>% mutate(`2020` = as.numeric(`2020`)) %>%  # os valores da coluna são convertidos em NAs
  str()
# Possível explicação: https://acervolima.com/como-converter-a-coluna-dataframe-de-character-para-numeric-em-r/#:~:text=A%20convers%C3%A3o%20pode%20ser%20feita,numeric().



