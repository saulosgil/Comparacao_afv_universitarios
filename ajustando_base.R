# Caregando pacotes ---------------------------------------------------------------------------
library(readxl)
library(tidyverse)
library(janitor)

# Lendo a base -------------------------------------------------------------------------------

df <- read_excel("data/df.xlsx")

# Arrumando a base ----------------------------------------------------------------------

# Arrumando nomes

df <- clean_names(df)

# Pegando as colunas do SF36

sf36 <- df[153:171] |>
  select(-capacidade_funcional,
         -aspecto_fisico,
         -dor,
         -estado_geral_de_saude,
         -vitalidade,
         -aspectos_sociais,
         -aspectos_emocionais,
         -saude_mental) |>
  rename(
    capacidade_funcional = raw_scale,
    aspecto_fisico = raw_scale2,
    dor = raw_scale3,
    estado_geral_saude = raw_scale4,
    vitalidade = raw_scale5,
    aspectos_sociais = raw_scale6,
    aspectos_emocionais = raw_scale7,
    saude_mental = raw_scale8,
    sf36_fisico = aspecto_fisico_2,
    sf36_mental = aspecto_mental
  )



# Pegando as outras variáveis que serão analisadas

df_ajustado <-
  df |>
  select( # Selecionando as variaveis
    idade,
    sexo,
    o_seu_curso_e_de_qual_area_do_conhecimento,
    qual_seu_curso,
    qual_semestre_esta_cursando,
    presencial_ou_ead,
    cor_raca,
    voce_esta_fazendo_algum_tratamento,
    voce_faz_uso_de_cigarros,
    voce_faz_uso_de_alcool,
    voce_teve_covid_19,
    ha_quantos_meses_voce_teve_covid_19,
    fez_exercicios_fisicos_sistematizados_nos_ultimos_6_meses_isso_inclui_programas_de_musculacao_natacao_danca_ou_qualquer_outro_tipo_de_atividades_feitos_como_parte_da_sua_rotina,
    comportamento_sedentario,
    mvpa_semanal,
    ansiedade_soma,
    soma_depressao
  ) |> # Renomeando algumas variáveis
  rename(
    area_conhecimento = o_seu_curso_e_de_qual_area_do_conhecimento,
    curso = qual_seu_curso,
    semestre = qual_semestre_esta_cursando,
    modalidade = presencial_ou_ead,
    raca = cor_raca,
    tratamento_atual = voce_esta_fazendo_algum_tratamento,
    tabagismo = voce_faz_uso_de_cigarros,
    consumo_alcool = voce_faz_uso_de_alcool,
    teve_covid = voce_teve_covid_19,
    quando_teve_covid = ha_quantos_meses_voce_teve_covid_19,
    realiza_treinamento = fez_exercicios_fisicos_sistematizados_nos_ultimos_6_meses_isso_inclui_programas_de_musculacao_natacao_danca_ou_qualquer_outro_tipo_de_atividades_feitos_como_parte_da_sua_rotina,
    mvpa = mvpa_semanal,
    ansiedade = ansiedade_soma,
    depressao = soma_depressao
  ) |>
  bind_cols(sf36) |> # juntanto com as colunas do SF36
  mutate(
    area_conhecimento = case_when(area_conhecimento == 1 ~ "Saude",
                                  area_conhecimento == 2 | area_conhecimento == 4 ~ "Exatas",
                                  area_conhecimento == 3 ~ "Humanas"),
    raca = case_when(raca == 1 ~ "Preto",
                     raca == 2 ~ "Branco",
                     raca == 3 ~ "Amarelo",
                     raca == 4 ~ "Pardo",
                     raca == 5 ~ "Indigena"),
    tratamento_atual = case_when(tratamento_atual == 1 ~ "Nenhum",
                                 TRUE ~ as.character(tratamento_atual)),
    tabagismo = case_when(tabagismo == 1 ~ "nunca",
                          tabagismo == 2 ~ "ex_tab",
                          tabagismo == 3 ~ "sim"),
    consumo_alcool = case_when(consumo_alcool == 1 ~ "nao",
                               consumo_alcool == 2 ~ "sim"),
    realiza_treinamento = case_when(realiza_treinamento == 1 ~ "sim",
                                    realiza_treinamento == 2 ~ "nao"),
    teve_covid = case_when(teve_covid == 1 ~ "sim",
                           teve_covid == 2 ~ "nao"),
    z_score_mvpa = (mvpa-mean(mvpa))/sd(mvpa),
    z_score_comportamento_sedentario = (comportamento_sedentario - mean(comportamento_sedentario))/sd(comportamento_sedentario),
    z_score_sf36_fisico = (sf36_fisico = (sf36_fisico - mean(sf36_fisico))/sd(sf36_fisico)),
    z_score_sf36_mental = (sf36_mental = (sf36_mental - mean(sf36_mental))/sd(sf36_mental)),
    e_outlier_mvpa = case_when(z_score_mvpa > 2 ~ "É outlier",
                               TRUE ~  "Não é outlier"),
    e_outlier_comportamento_sedentario = case_when(z_score_comportamento_sedentario > 2 ~ "É outlier",
                                                   TRUE ~  "Não é outlier"),
    e_outlier_sf36_fisico = case_when(z_score_sf36_fisico > 2 ~ "É outlier",
                                      TRUE ~  "Não é outlier"),
    e_outlier_sf36_mental = case_when(z_score_sf36_mental > 2 ~ "É outlier",
                                      TRUE ~  "Não é outlier")
)


# Escrevendo base ajustada --------------------------------------------------------------------

write.csv2(x = df_ajustado,file = "data/df_ajustado.csv")

