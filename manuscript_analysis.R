# PLOT AND ANALYSES FOR MANUSCRIPT ------------------------------------------------------------
# Packages ------------------------------------------------------------------------------------
library(tidyverse)
library(patchwork)

# Reading dataset --------------------------------------------------------------------------------

df <-
  read_delim("data/df_ajustado.csv",
             delim = ";",
             escape_double = FALSE,
             locale = locale(encoding = "ISO-8859-1"),
             trim_ws = TRUE)

# Adjusting dataset ---------------------------------------------------------------------------

df <-
  df |>
  mutate(area = case_when(
    curso == "Análise e desenvolvimento de sistemas" ~ "Natural and Applied Sciences",
    curso == "Serviço Social" ~ "Social Sciences",
    curso == "Engenharia Elétrica" ~ "Natural and Applied Sciences",
    curso == "Arquitetura e urbanismo" ~ "Humanities",
    curso == "Gestão Financeira" ~ "Social Sciences",
    curso == "Engenharia da Computação" ~ "Natural and Applied Sciences",
    curso == "Medicina Veterinária" ~ "Natural and Applied Sciences",
    curso == "História (Licenciatura)" ~ "Humanities",
    curso == "Fisioterapia" ~ "Natural and Applied Sciences",
    curso == "Psicologia" ~ "Social Sciences",
    curso == "Direito" ~ "Natural and Applied Sciences",
    curso == "Gestão Comercial" ~ "Humanities",
    curso == "Nutrição" ~ "Natural and Applied Sciences",
    curso == "Pedagogia (2ª graduação)" ~ "Humanities",
    curso == "Gestão Hospitalar" ~ "Social Sciences",
    curso == "Educação Física (Bacharelado)" ~ "Natural and Applied Sciences",
    curso == "Administração" ~ "Social Sciences",
    curso == "Enfermagem" ~ "Natural and Applied Sciences",
    curso == "Engenharia de Produção" ~ "Natural and Applied Sciences",
    curso == "Ciências Contábeis" ~ "Social Sciences",
    curso == "Logística" ~ "Humanities",
    curso == "Ciências Biológicas - Biologia (Bacharelado)" ~ "Natural and Applied Sciences",
    curso == "Processos gerenciais" ~ "Social Sciences",
    curso == "Geografia (Licenciatura)" ~ "Social Sciences",
    curso == "Odontologia" ~ "Natural and Applied Sciences",
    curso == "Engenharia Ambiental" ~ "Natural and Applied Sciences",
    curso == "Engenharia Civil" ~ "Natural and Applied Sciences",
    curso == "Letras (Licenciatura)" ~ "Humanities",
    curso == "Serviços jurídicos, cartoriais e notariais" ~ "Humanities",
    curso == "Farmácia" ~ "Natural and Applied Sciences",
    curso == "Medicina" ~ "Natural and Applied Sciences",
    curso == "Gestão de Recursos Humanos" ~ "Social Sciences",
    curso == "Gestão da Tecnologia da Informação" ~ "Social Sciences",
    curso == "Educação Física (Licenciatura)" ~ "Natural and Applied Sciences",
    curso == "Estética e Cosmética" ~ "Natural and Applied Sciences",
    curso == "Engenharia Mecânica" ~ "Natural and Applied Sciences",
    curso == "Engenharia Química" ~ "Natural and Applied Sciences",
    curso == "Gastronomia" ~ "Humanities",
    curso == "Ciências Biológicas - Biologia (Licenciatura)" ~ "Natural and Applied Sciences",
    curso == "Biomendicina" ~ "Natural and Applied Sciences",
    curso == "Teologia (Bacharelado)" ~ "Humanities",
    curso == "Comunicação Social - Publicidade e Propaganda" ~ "Humanities",
    curso == "Segurança da Informação" ~ "Social Sciences",
    curso == "Design de Interiores" ~ "Humanities",
    curso == "Marketing Digital" ~ "Humanities",
    curso == "Gestão da Qualidade" ~ "Social Sciences",
    curso == "Gestão Pública" ~ "Social Sciences",
    curso == "Engenharia Mecatrônica" ~ "Natural and Applied Sciences",
    curso == "Processos escolares" ~ "Social Sciences",
    curso == "Pedagogia" ~ "Humanities",
    curso == "Marketing" ~ "Humanities"
  ))


# PLOTS FOR PAPER -----------------------------------------------------------------------------
# MVPA ----------------------------------------------------------------------------------------
# Prevalence of physical inactivity
df |>
  select(area,
         mvpa_ativo) |>
  group_by(area, mvpa_ativo) |>
  summarise(n = n())

prevalence_inactivity <-
  tibble::tribble(
    ~area, ~prevalencia,
    "Humanities", 63,
    "Natural and Applied Sciences",  63,
    "Social Sciences", 68
  )

prevalence_plt <-
  prevalence_inactivity |>
  ggplot(mapping = aes(x = area,
                       y = prevalencia,
                       fill = area,
                       alpha = 0.7)) +
  geom_col(colour = "black",
           show.legend = FALSE) +
  # Customizations
  theme(
    # axis.ticks = element_blank(),
    axis.line = element_line(colour = "black"),
    panel.grid = element_line(color = "#b4aea9"),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(linetype = "dashed"),
    panel.background = element_rect(fill = "#ffffff", color = "#ffffff"),
    plot.background = element_rect(fill = "#ffffff", color = "#ffffff"),
    # legend
    legend.title = element_blank(),
    legend.position = "top",
    axis.text = element_text(family = "arial",
                             size = 10,
                             colour = "black")
  ) +
  # Add labels and title
  labs(
    x = "",
    y = "Prevalence of physical inactivity (%)"
  ) +
  coord_flip()

# PLOT!!!!
prevalence_plt

# MVPA as continuos variable
# Basic plot
plt_mvpa <-
  df |>
  ggplot(mapping = aes(x = area,
                       y = mvpa,
                       color = area)) +
  stat_boxplot(varwidth = FALSE,
               outlier.shape = NA,
               show.legend = TRUE) +
  geom_jitter(width = 0.2,
              size = 1.5,
              alpha = 0.7,
              show.legend = TRUE) +
  stat_summary(
    fun = "mean",
    shape = 3,
    size = 1,
    colour = "red") +
# Customizing
# Add labels and title
  labs(
    x = "",
    y = "Moderate to vigorous physical activity (min/wk)"
    ) +
  # Customizations
  theme(
    # axis.ticks = element_blank(),
    axis.line = element_line(colour = "black"),
    panel.grid = element_line(color = "#b4aea9"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(linetype = "dashed"),
    panel.background = element_rect(fill = "#ffffff", color = "#ffffff"),
    plot.background = element_rect(fill = "#ffffff", color = "#ffffff"),
    # legend
    legend.title = element_blank(),
    legend.position = "top",
    axis.text = element_text(family = "arial",
                             size = 10,
                             colour = "black")
  )

# PLOT!!!!
plt_mvpa

# Sedentary behavior --------------------------------------------------------------------------
# Prevalence of elevate sedentary behavior


# Basic plot
plt_sed <-
  df |>
  ggplot(mapping = aes(x = area,
                       y = comportamento_sedentario,
                       color = area)) +
  stat_boxplot(varwidth = FALSE,
               outlier.shape = NA,
               show.legend = TRUE) +
  geom_jitter(width = 0.2,
              size = 1.5,
              show.legend = TRUE) +
  stat_summary(
    fun = "mean",
    shape = 3,
    size = 1,
    colour = "red") +
  # Add labels and title
  labs(
    x = "",
    y = "Sedentary behavior (min/wk)"
  ) +
  # Customizations
  theme(
    # axis.ticks = element_blank(),
    axis.line = element_line(colour = "black"),
    panel.grid = element_line(color = "#b4aea9"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(linetype = "dashed"),
    panel.background = element_rect(fill = "#ffffff", color = "#ffffff"),
    plot.background = element_rect(fill = "#ffffff", color = "#ffffff"),
    # legend
    legend.title = element_blank(),
    legend.position = "top",
    axis.text = element_text(family = "arial",
                             size = 10,
                             colour = "black")
  )









