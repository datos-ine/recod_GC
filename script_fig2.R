### Mortalidad por códigos garbage en Argentina (2010–2023):
### redistribución hacia causas específicas
### Figura 3
### Autora: Tamara Ricardo
# Última modificación: 25-09-2026 12:53

# Cargar paquetes --------------------------------------------------------
pacman::p_load(
  rio,
  janitor,
  tidyverse
)

# Cargar datos -----------------------------------------------------------
datos_gc <- import("clean/arg_recod_defun_gbd23.rds")

# Frecuencias ------------------------------------------------------------
fr_sex_edad <- datos_gc |>
  # Crear grupo de nivel 1
  mutate(
    grupo_nivel1 = str_remove(gbd_paso1, ":.*") |>
      fct_relevel("CMNN", "CE", "ENT", "GC")
  ) |>

  # Crear grupo de causas
  mutate(
    grupo_causa = case_when(
      str_detect(gbd_paso1, "CRD|DM|ECV|NPL") ~ "ENT-OBJ",
      str_detect(gbd_paso1, "CA|SH|TRA$|VI") ~ "CE-OBJ",
      str_detect(gbd_paso1, "ENT") ~ "ENT-OTR",
      str_detect(gbd_paso1, "CE") ~ "CE-OTR",
      .default = str_remove(gbd_paso1, ".*:"),
    ) |>
      fct_relevel(
        "INF",
        "MAT-NEO",
        "NUTR",
        "CE-OBJ",
        "CE-OTR",
        "ENT-OBJ",
        "ENT-OTR"
      )
  ) |>

  # Calcular frecuencias x edad y sexo
  count(grupo_edad, sexo, grupo_nivel1, grupo_causa, wt = n) |>
  mutate(pct = n / sum(n), .by = c(sexo, grupo_nivel1, grupo_causa))


# Figura 2 ---------------------------------------------------------------
fig2 <- fr_sex_edad |>
  ggplot(aes(x = grupo_edad, y = pct, fill = fct_inseq(grupo_causa))) +
  facet_grid(sexo ~ grupo_nivel1) +
  geom_col(position = "dodge", alpha = .9) +

  # --- Escalas ---
  scale_cbpal_fill(palette = "managua", name = NULL, reverse = FALSE) +
  scale_y_continuous(name = NULL, labels = percent) +
  scale_x_discrete(name = NULL) +

  # --- Layout ---
  guides(fill = guide_legend(nrow = 2)) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(angle = 90),
    text = element_text(family = "Times New Roman", size = 12)
  )

## Save as PNG ----
# ggsave(
#   fig2,
#   filename = "figs_tablas/Figura2.png",
#   width = 17,
#   height = 20,
#   units = "cm",
#   dpi = 300
# )

## Save as SVG ----
# ggsave(
#   fig2,
#   filename = "figs_tablas/Figura2.svg",
#   width = 17,
#   height = 20,
#   units = "cm",
#   dpi = 300
# )
