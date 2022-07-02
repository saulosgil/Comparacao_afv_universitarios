# Pacotes
library(tidyverse)
library(fmsb)

# Ajustando a base para fazer o grafico -------------------------------------------------------
# Lendo a base
df <- read.csv2("data/df_ajustado.csv")

# Ajustando a base
df_ajustada <-
  df |> select( # Selecionando as variaveis
    area_conhecimento,
    capacidade_funcional,
    aspecto_fisico,
    dor,
    estado_geral_saude,
    vitalidade,
    aspectos_sociais,
    aspectos_emocionais,
    saude_mental
  ) |>
  rename( # Renomeando !!!Importante estar igual ao utilizado no minino maximo do radar!!!
    "Function capacity" = capacidade_funcional,
    "Physical Appearance" = aspecto_fisico,
    Pain = dor,
    "General health status" = estado_geral_saude,
    Vitality = vitalidade,
    "Social Aspect" = aspectos_sociais,
    "Emotional Aspect" = aspectos_emocionais,
    "Mental health" = saude_mental
  )

# Sumarizando as variaveis que serãp plotadas
sf36_score <-
  df_ajustada |>
  group_by(area_conhecimento) |>
  summarise("Function capacity" = mean(`Function capacity`),
            "Physical Appearance" = mean(`Physical Appearance`),
            Pain = mean(Pain),
            "General health status" = mean(`General health status`),
            Vitality =  mean(Vitality),
            "Social Aspect" = mean(`Social Aspect`),
            "Emotional Aspect" = mean(`Emotional Aspect`),
            "Mental health" = mean(`Mental health`)
  ) |>
  select(-area_conhecimento) |>
  as.data.frame()

row.names(sf36_score) <- c("Exatas", "Humanas", "Saúde") # inserindo o nome das linhas

sf36_score

# Definindo a amplitude do escore: Minimo e máximo
max_min <- tibble(
  "Function capacity" = c(100, 0),
  "Physical Appearance" = c(100, 0),
  Pain = c(100, 0),
  "General health status" = c(100, 0),
  Vitality = c(100, 0),
  "Social Aspect" = c(100, 0),
  "Emotional Aspect" = c(100, 0),
  "Mental health" = c(100, 0)
) |>
  as.data.frame()

row.names(max_min) <- c("Max", "Min") # inserindo o nome das linhas

max_min

# Juntando as variaveis sumarizadas com os escores minimo e máximo
df_radar <- rbind(max_min, sf36_score)

df_radar

# Plot the data for student 1
# Reduce plot margin using par()
op <- par(mar = c(2, 2, 2, 2))

radarchart(df_radar,
           axistype = 1, # Cutomizando as labels
           caxislabels = c(0, 25, 50, 75, 100),
           vlabels = colnames(df_radar),
           axislabcol = "grey",
           vlcex = 1,
           pcol = c("#00AFBB", "#E7B800", "#FC4E07"), # Customizando o poligono
           # pfcol = scales::alpha(c("#00AFBB", "#E7B800", "#FC4E07"), 0.1),
           plwd = 2,
           plty = 1,
           cglcol = "grey", # Customizando a grid
           cglwd = 0.8,
           cglty = 1,
           title = "QUALITY OF LIFE\nShort-Form Health Survey (SF-36)" # Titulo
)

# Add an horizontal legend
legend(
  x = 1.3,
  y = 1.2,
  legend = rownames(df_radar[-c(1,2),]),
  horiz = FALSE,
  bty = "n",
  pch = 20,
  col = c("#00AFBB", "#E7B800", "#FC4E07"),
  text.col = "black",
  cex = 1,
  pt.cex = 1.5
)
par(op)
