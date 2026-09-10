### Mortalidad por códigos garbage en Argentina (2010-2023):
### Redistribución hacia causas específicas
### Limpieza del dataset:
# Defunciones Generales Mensuales ocurridas y registradas en la
# República Argentina (MSAL-DEIS, 2010-2023)
### Asignación de códigos CIE-10 a grupos de causas nivel 2 y 3 según GBD-2023
### Recategorización y redistribución de códigos garbage (GC)
### Autora: Tamara Ricardo
### Revisor: Juan I. Irassar
# Última modificación: 09-09-2026 14:53

# Cargar paquetes --------------------------------------------------------
pacman::p_load(
  rio,
  janitor,
  tabulapdf,
  tidyverse
)


# Cargar datos defunciones mensuales (2010-2023) -------------------------
defun_raw <- bind_rows(
  # Período 2010-2015
  import("raw/base_def_10_15_men_4dig.csv"),
  # Período 2016-2021
  import("raw/base_def_16_20_men_4dig.csv"),
  # Período 2022-2023
  import("raw/base_def_21_23_men_4dig.csv")
)


# Cargar tabla frecuencias X59-Y34 ---------------------------------------
fr_gc2 <- extract_areas(
  file = "extra/suppl-e00056424_5279.pdf",
  pages = 4
) |>
  # --- Convertir a dataframe ---
  list_rbind() |>

  # --- Weights a numérico ---
  mutate(
    weight = parse_number(
      weight,
      locale = locale(decimal_mark = ",", grouping_mark = ".")
    )
  ) |>

  # --- Recategorizar causas ---
  mutate(
    causa = case_when(
      ## --- CE ---
      target == "Injuries - Road" ~ "CE:TRA",
      target == "Injuries - Other transport injuries" ~ "CE:OTR-TRA",
      target == "Injuries - Falls" ~ "CE:CA",
      target == "Injuries - Suicide" ~ "CE:SH",
      target == "Injuries - Homicide" ~ "CE:VI",
      target == "Injuries - Others" ~ "CE:OTR-CE",

      ## --- ENT ---
      target == "dcnt_cardiovascular" ~ "ENT:ECV",
      target == "dcnt_chronic respiratory" ~ "ENT:CRD",
      target == "dcnt_diabetes" ~ "ENT:DM-CKD",
      target == "dcnt_neoplasms" ~ "ENT:NPL",

      ## --- Otras causas ---
      .default = NA
    )
  ) |>

  # --- Agrupar datos ---
  count(CG, causa, wt = weight, name = "weight") |>

  # --- Eliminar NAs ---
  drop_na() |>

  # --- Pasar a formato wide ---
  pivot_wider(
    names_from = CG,
    values_from = weight,
    names_prefix = "fr"
  )


# Limpiar datos defunciones mensuales (2010-2023) ------------------------
defun <- defun_raw |>
  # --- Estandarizar nombres de columnas ---
  rename(
    anio = anio_def,
    mes = mes_def,
    sexo = sexo_id,
    grupo_edad = grupo_etario,
    cie10_cod = cod_causa_muerte_CIE10
  ) |>

  # --- Filtrar defunciones 2009 ---
  filter(between(anio, 2010, 2023)) |>

  # --- Filtrar datos ausentes región geográfica ---
  filter_out(region == "10.sin especificar.") |>

  # --- Filtrar datos ausentes grupo etario ---
  filter_out(grupo_edad == "08.sin especificar") |>

  # --- Filtrar datos ausentes sexo ---
  filter(between(sexo, 1, 2)) |>

  # --- Estandarizar formato códigos CIE-10 ---
  mutate(cie10_cod = str_to_upper(cie10_cod)) |>

  # --- Añadir separador de 4to dígito código CIE-10 ---
  mutate(
    cie10_cod = if_else(
      str_detect(cie10_cod, "X$"),
      str_replace(cie10_cod, "X$", ".0"),
      paste0(str_sub(cie10_cod, 1, 3), ".", str_sub(cie10_cod, 4))
    )
  ) |>

  # --- Modificar etiquetas sexo ---
  mutate(sexo = if_else(sexo == 1, "Masculino", "Femenino")) |>

  # --- Modificar etiquetas grupo etario ---
  mutate(
    grupo_edad = fct_relabel(
      grupo_edad,
      .fun = ~ c(
        "<20 años",
        "20-39",
        "40-49",
        "50-59",
        "60-69",
        "70-79",
        "≥80 años"
      )
    )
  ) |>

  # --- Crear región DEIS ---
  mutate(
    region_deis = case_when(
      str_detect(region, "6.Cuyo|7.Cuyo") ~ "Cuyo",
      str_detect(region, "NOA") ~ "NOA",
      str_detect(region, "8.Pat|9.Pat") ~ "Patagonia",
      .default = str_remove_all(region, "^\\d+\\.|\\.")
    )
  ) |>

  # --- Crear jurisdicción DEIS ---
  mutate(
    jurisd_deis = case_when(
      jurisdiccion == "6.Prov. Bs.As." ~ "Buenos Aires",
      jurisdiccion == "14.Cordoba." ~ "Córdoba",
      jurisdiccion == "30.Entre Rios." ~ "Entre Ríos",
      jurisdiccion == "90.Tucuman." ~ "Tucumán",
      str_detect(region, "Sur") ~ "Patagonia Sur",
      str_detect(region, "Norte") ~ "Patagonia Norte",
      jurisdiccion == "99.no identificado." ~ str_remove_all(
        region,
        "^\\d+\\.|\\."
      ),
      .default = str_remove_all(jurisdiccion, "[0-9[:punct:]]")
    )
  ) |>

  # --- Seleccionar columnas relevantes ---
  select(
    anio,
    mes,
    region_deis,
    jurisd_deis,
    sexo,
    grupo_edad,
    cie10_cod
  )


# Paso 1: Categorizar causas GBD-2023 ------------------------------------
## GC nivel 1 (GC1) -----
recod_defun <- defun |>
  mutate(
    gbd_paso1 = case_when(
      between(cie10_cod, "A40.0", "A41.9") |
        cie10_cod %in%
          c(
            "A48.0",
            "A48.3",
            "A49.0",
            "A49.1"
          ) |
        between(cie10_cod, "A59.0", "A59.9") |
        between(cie10_cod, "A71.0", "A71.9") |
        cie10_cod %in% c("A74.0", "B07.0") |
        # B07: No tiene decimales
        between(cie10_cod, "B30.0", "B30.9") |
        between(cie10_cod, "B35.0", "B36.9") |
        between(cie10_cod, "B85.0", "B85.4") |
        between(cie10_cod, "B87.0", "B88.9") |
        cie10_cod %in%
          c(
            "B94.0",
            "D50.0",
            "D50.9",
            "D62.0",
            "D63.0",
            "D63.8"
            # D64.0: OTR-ENT
          ) |
        between(cie10_cod, "D64.1", "D65.9") |
        # D68.0 - D68.9: OTR-ENT
        cie10_cod %in% c("D69.9", "E15.0") |
        # E15: No tiene decimales
        # E16.0: OTR-CE
        # E16.1 - E16.9: OTR-ENT
        between(cie10_cod, "E50.0", "E50.9") |
        cie10_cod == "E64.1" |
        between(cie10_cod, "E85.3", "E87.6") |
        cie10_cod == "E87.8" |
        # E87.9: No existe
        between(cie10_cod, "F19.0", "F19.9") |
        between(cie10_cod, "G06.0", "G08.0") |
        between(cie10_cod, "G32.0", "G32.8") |
        between(cie10_cod, "G43.0", "G44.2") |
        cie10_cod %in% c("G44.4", "G44.8", "G47.4") |
        between(cie10_cod, "G47.0", "G47.2") |
        between(cie10_cod, "G47.4", "G47.9") |
        between(cie10_cod, "G50.0", "G60.9") |
        cie10_cod == "G62.0" |
        between(cie10_cod, "G62.2", "G64.0") |
        # G65: No existe
        between(cie10_cod, "G80.0", "G83.9") |
        # G89: No existe
        between(cie10_cod, "G91.0", "G91.2") |
        # G91.4: No existe
        between(cie10_cod, "G91.8", "G92.0") |
        # G92: No tiene decimales
        # G93.0: GC2
        cie10_cod %in% c("G93.1", "G93.2") |
        between(cie10_cod, "G93.4", "G93.6") |
        between(cie10_cod, "G94.0", "G94.8") |
        between(cie10_cod, "G99.0", "H04.9") |
        # H05.0: Otras ENT
        between(cie10_cod, "H05.2", "H69.9") |
        between(cie10_cod, "H71.0", "H95.9") |
        # H96 - H99: No existe
        between(cie10_cod, "I26.0", "I26.9") |
        between(cie10_cod, "I31.2", "I31.4") |
        between(cie10_cod, "I46.0", "I46.9") |
        cie10_cod %in%
          c(
            "I50.0",
            "I50.1",
            # I50.4: No existe
            # I76: No existe
            # I91: No existe
            "I95.0",
            "I95.1",
            "I95.8",
            "I95.9"
          ) |
        between(cie10_cod, "J69.0", "J69.8") |
        # J69.9: No existe
        between(cie10_cod, "J80.0", "J81.0") |
        between(cie10_cod, "J85.0", "J86.9") |
        cie10_cod == "J90.0" |
        between(cie10_cod, "J93.0", "J94.9") |
        between(cie10_cod, "J96.0", "J96.9") |
        between(cie10_cod, "J98.1", "J98.3") |
        between(cie10_cod, "K00.0", "K14.9") |
        # K15 - K19: No existe
        cie10_cod == "K30.0" |
        between(cie10_cod, "K65.0", "K66.1") |
        cie10_cod == "K66.9" |
        # K68: No existe
        between(cie10_cod, "K71.0", "K71.6") |
        between(cie10_cod, "K71.8", "K72.9") |
        cie10_cod == "K75.0" |
        between(cie10_cod, "L20.0", "L30.9") |
        between(cie10_cod, "L40.0", "L50.9") |
        between(cie10_cod, "L52.0", "L54.8") |
        between(cie10_cod, "L56.0", "L56.2") |
        cie10_cod %in% c("L56.4", "L56.5") |
        between(cie10_cod, "L57.0", "L57.9") |
        between(cie10_cod, "L59.0", "L68.9") |
        between(cie10_cod, "L70.0", "L75.9") |
        # L76: No existe
        between(cie10_cod, "L80.0", "L87.9") |
        between(cie10_cod, "L90.0", "L92.9") |
        between(cie10_cod, "L94.0", "L95.9") |
        # L96: No existe
        between(cie10_cod, "L98.5", "L99.8") |
        # M04: No existe
        between(cie10_cod, "M10.0", "M12.0") |
        between(cie10_cod, "M12.2", "M25.9") |
        # M26 - M29: No existe
        # M37 - M39: No existe
        between(cie10_cod, "M43.2", "M49.0") |
        between(cie10_cod, "M49.2", "M63.8") |
        # M64: No existe
        between(cie10_cod, "M65.1", "M70.9") |
        # M71.0: Otras ENT
        between(cie10_cod, "M71.2", "M72.4") |
        cie10_cod %in% c("M72.8", "M72.9") |
        # M73.0: CMNN
        between(cie10_cod, "M73.8", "M79.9") |
        between(cie10_cod, "M83.0", "M86.2") |
        between(cie10_cod, "M86.5", "M86.9") |
        between(cie10_cod, "M87.2", "M87.9") |
        between(cie10_cod, "M89.1", "M89.4") |
        between(cie10_cod, "M90.0", "M99.9") |
        between(cie10_cod, "N17.0", "N17.9") |
        cie10_cod %in%
          c(
            "N19.0",
            # N19: No tiene decimales
            "N32.1",
            "N32.2"
          ) |
        between(cie10_cod, "N32.8", "N33.8") |
        between(cie10_cod, "N35.0", "N35.9") |
        cie10_cod %in% c("N37.0", "N37.8") |
        between(cie10_cod, "N39.3", "N39.8") |
        between(cie10_cod, "N42.0", "N43.4") |
        # N44.1 - N44.8: No existe
        between(cie10_cod, "N46.0", "N48.9") |
        between(cie10_cod, "N50.0", "N51.8") |
        # N52 - N53: No existe
        between(cie10_cod, "N61.0", "N64.9") |
        between(cie10_cod, "N82.0", "N82.9") |
        between(cie10_cod, "N91.0", "N91.5") |
        # N95.0: GC3
        between(cie10_cod, "N95.1", "N95.9") |
        between(cie10_cod, "N97.0", "N97.9") |
        cie10_cod %in% c("R02.0", "R03.1") |
        # R02: No tiene decimales
        # R04.0: GC2
        between(cie10_cod, "R04.1", "R04.9") |
        cie10_cod == "R07.0" |
        # R08: No existe
        between(cie10_cod, "R09.0", "R12.0") |
        between(cie10_cod, "R14.0", "R22.9") |
        # R23.0:  GC2
        between(cie10_cod, "R23.1", "R30.9") |
        between(cie10_cod, "R32.0", "R50.0") |
        # R50.1: No existe
        between(cie10_cod, "R50.8", "R72.0") |
        # R72: No tiene decimales
        between(cie10_cod, "R74.0", "R77.9") |
        # R78.0: Uso de sustancias
        between(cie10_cod, "R78.6", "R94.8") |
        between(cie10_cod, "R96.0", "R99.0") |
        # R99: No tiene decimales
        # U05: No existe
        between(cie10_cod, "U08.0", "U49.9") |
        # U51 - U81: No existe
        # U90 - U99: No existe
        between(cie10_cod, "X40.0", "X44.9") |
        between(cie10_cod, "X46.0", "X46.9") |
        between(cie10_cod, "X49.0", "X49.9") |
        between(cie10_cod, "Y10.0", "Y14.9") |
        between(cie10_cod, "Y16.0", "Y19.9") |
        between(cie10_cod, "Z00.0", "Z99.9") ~ "GC:GC1",
      # Z16: No existe

      # --- Valor por defecto ---
      .default = NA
    )
  )


## GC nivel 2 (GC2) -----
recod_defun <- recod_defun |>
  mutate(
    gbd_paso1 = coalesce(
      gbd_paso1,
      case_when(
        # A14.9: No existe
        # A29: No existe
        between(cie10_cod, "A30.0", "A30.9") |
          # A45: No existe
          # A47: No existe
          # A48.0: GC1
          cie10_cod == "A48.8" |
          # A49.0: GC1
          between(cie10_cod, "A49.3", "A49.9") |
          # A61 - A62: No existe
          # A72 - A73: No existe
          # A76: No existe
          between(cie10_cod, "A97.0", "A97.9") |
          between(cie10_cod, "B08.0", "B09.0") |
          # B11 - B14: No existe
          # B28 - B29: No existe
          # B31 - B32: No existe
          cie10_cod %in% c("B34.0", "B34.1") |
          between(cie10_cod, "B34.3", "B34.9") |
          # B61 - B63: No existe
          between(cie10_cod, "B68.0", "B68.9") |
          between(cie10_cod, "B73.0", "B74.2") |
          between(cie10_cod, "B76.0", "B76.9") |
          between(cie10_cod, "B79.0", "B81.8") |
          cie10_cod %in%
            c(
              # B84: No existe
              "B92.0",
              # B93: No existe
              # B94.0: GC1
              "B94.8",
              "B94.9"
            ) |
          between(cie10_cod, "B95.6", "B97.1") |
          cie10_cod == "B97.3" |
          between(cie10_cod, "B97.7", "B99.0") |
          # B99: No tiene decimales
          cie10_cod == "F07.2" |
          between(cie10_cod, "F17.0", "F17.9") |
          cie10_cod %in%
            c(
              "G44.3",
              "G91.3",
              "G93.0",
              "G93.3",
              "I10.0"
              # I10: No tiene decimales
            ) |
          between(cie10_cod, "I15.0", "I15.9") |
          cie10_cod %in%
            c(
              # I27.0: ECV
              "I27.8",
              "I27.9",
              # I50.0: GC1
              # I50.8: No existe
              "I50.9",
              "I67.4",
              "I70.0",
              "I70.1",
              "I70.8",
              "I70.9"
            ) |
          between(cie10_cod, "I74.0", "I74.9") |
          # I75: No existe
          # J81.0: GC1 (no tiene decimales)
          between(cie10_cod, "K92.0", "K92.2") |
          between(cie10_cod, "N70.0", "N71.9") |
          between(cie10_cod, "N73.0", "N73.9") |
          # N74.0: CMNN
          between(cie10_cod, "N74.2", "N74.8") |
          cie10_cod %in% c("R03.0", "R04.0") |
          between(cie10_cod, "R05.0", "R06.8") |
          # R06.9: No existe
          cie10_cod %in% c("R13.0", "R23.0") |
          # R13: No tiene decimales
          # R58: GC1
          between(cie10_cod, "S00.0", "T99.9") |
          # U50: No existe
          # W47 - W48: No existe
          # W63: No existe
          # W71 - W72: No existe
          # W82: No existe
          # W95 - W98: No existe
          # X07: No existe
          # X55 - X56: No existe
          between(cie10_cod, "X59.0", "X59.9") |
          between(cie10_cod, "Y20.0", "Y34.9") |
          cie10_cod %in%
            c(
              "Y86.0",
              # Y87.0: CE-SH
              "Y87.2",
              # Y89.0: OTR-CE
              "Y89.9"
            ) |
          # Y92 - Y94: No existe
          between(cie10_cod, "Y95.0", "Y98.0") ~ "GC:GC2",
        # Y98: No tiene decimales
        # Y99: No existe
      )
    )
  )


## GC nivel 3 (GC3) -----
recod_defun <- recod_defun |>
  mutate(
    gbd_paso1 = coalesce(
      gbd_paso1,
      case_when(
        # A01.0: CMNN
        between(cie10_cod, "A31.0", "A31.9") |
          between(cie10_cod, "A42.0", "A44.9") |
          cie10_cod %in%
            c(
              "A49.2",
              "A64.0",
              "A99.0",
              # B17.0: CMNN
              "B17.1",
              "B17.8",
              "B17.9",
              "B19.0",
              # B19.2: No existe
              "B19.9"
            ) |
          between(cie10_cod, "B37.0", "B46.9") |
          cie10_cod == "B49.0" |
          # B49: No tiene decimales
          # B55.0: CMNN
          between(cie10_cod, "B55.1", "B55.9") |
          between(cie10_cod, "B58.0", "B58.9") |
          # B59: No existe
          cie10_cod %in% c("B89.0", "B94.2") |
          between(cie10_cod, "C14.0", "C14.8") |
          # C14.9: No existe
          cie10_cod == "C22.9" |
          between(cie10_cod, "C26.0", "C26.9") |
          # C27 - C29: No existe
          # C35 - C36: No existe
          between(cie10_cod, "C39.0", "C39.9") |
          # C42: No existe
          between(cie10_cod, "C46.0", "C46.9") |
          cie10_cod %in%
            c(
              "C55.0",
              # C55: No tiene decimales
              "C57.9",
              # C59: No existe
              "C63.9",
              # C68.0: Neoplasias
              "C68.9"
            ) |
          between(cie10_cod, "C74.0", "C74.9") |
          between(cie10_cod, "C75.9", "C80.9") |
          cie10_cod %in%
            c(
              # C83.0: Neoplasias
              "C83.9",
              "C85.1",
              "C85.9",
              # C87: No existe
              "C94.6"
            ) |
          between(cie10_cod, "C97.0", "D00.0") |
          # D01.0: Neoplasias
          between(cie10_cod, "D01.4", "D01.9") |
          # D02.0: Neoplasias
          cie10_cod %in%
            c(
              "D02.4",
              # D02.9: No existe
              # D07.0: Neoplasias
              "D07.3",
              "D07.6",
              # D08: No existe
              # D09.0: Neoplasias
              "D09.1",
              "D09.7",
              "D09.9",
              # D10.0: Neoplasias
              "D10.9",
              # D13.0: Neoplasias
              "D13.9",
              # D14.0: Neoplasias
              "D14.4"
            ) |
          between(cie10_cod, "D17.0", "D21.9") |
          cie10_cod %in%
            c(
              # D28.0: Neoplasias
              "D28.9",
              # D29.0: Neoplasias
              "D29.9",
              # D30.0: Neoplasias
              "D30.9",
              "D36.0",
              "D36.9",
              "D37.0"
            ) |
          between(cie10_cod, "D37.6", "D37.9") |
          # D38.0: Neoplasias
          cie10_cod %in%
            c(
              "D38.6",
              "D39.0",
              "D39.7",
              "D39.9",
              # D40.0: Neoplasias
              "D40.9",
              # D41.0: Neoplasias
              "D41.9",
              # D44.0: Neoplasias
              "D44.9",
              # D48.0: Neoplasias
              "D48.7",
              "D48.9",
              # D49: No existe
              # D54: No existe
              "D75.9"
              # D79: No existe
            ) |
          between(cie10_cod, "D80.0", "D84.9") |
          # D85: No existe
          # D87 - D88: No existe
          cie10_cod %in% c("D89.8", "D89.9") |
          # D90 - D99: No existe
          cie10_cod %in%
            c(
              "E07.8",
              "E07.9",
              # E08: No existe
              # E17 - E19: No existe
              "E34.0"
            ) |
          between(cie10_cod, "E34.9", "E35.8") |
          # E37 - E39: No existe
          # E47 - E49: No existe
          # E62: No existe
          # E69: No existe
          cie10_cod %in% c("E87.7", "E90.0") |
          # E91 - E99: No existe
          between(cie10_cod, "F04.0", "F07.0") |
          between(cie10_cod, "F07.8", "F09.0") |
          # F09: No tiene decimales
          between(cie10_cod, "F20.0", "F48.9") |
          # F49: No existe
          # F50.0: Mentales
          between(cie10_cod, "F50.8", "F99.0") |
          cie10_cod %in%
            c(
              "G09.0",
              # G09: No tiene decimales
              # G15 - G19: No existe
              # G21.0: CE
              "G21.2"
            ) |
          between(cie10_cod, "G21.4", "G22.0") |
          # G27 - G29: No existe
          # G33 - G34: No existe
          # G38 - G39: No existe
          # G42: No existe
          # G48 - G49: No existe
          # G66 - G69: No existe
          # G74 - G79: No existe
          # G84 - G88: No existe
          cie10_cod %in% c("G93.8", "G93.9") |
          # G94.0: GC1
          between(cie10_cod, "G96.0", "G96.9") |
          cie10_cod %in% c("G98.0", "I00.0") |
          # G98: No tiene decimales
          # I03 - I04: No existe
          # I14: No existe
          # I16 - I19: No existe
          # I29: No existe
          between(cie10_cod, "I44.0", "I45.9") |
          between(cie10_cod, "I49.0", "I49.9") |
          # I51.0: ECV
          between(cie10_cod, "I51.6", "I52.8") |
          # I53 - I59: No existe
          # I90 - I94: No existe
          # I96: No existe
          cie10_cod %in% c("I98.8", "I99.0") |
          # ID05: No existe
          cie10_cod %in%
            c(
              "J02.9",
              "J03.9",
              "J04.3",
              # J06.0: CMNN
              "J06.9"
            ) |
          between(cie10_cod, "J40.0", "J40.9") |
          cie10_cod %in%
            c(
              "J47.0",
              # J48 - J59: No existe
              "J65.0",
              # J71 - J79: No existe
              # J81.9: No existe
              # J83: No existe
              # J85.9: No existe
              # J87 - J89: No existe
              # J90.9: No existe
              # J93.6: No existe
              # J97: No existe
              "J98.0"
            ) |
          between(cie10_cod, "J98.4", "J99.8") |
          between(cie10_cod, "K21.0", "K21.9") |
          cie10_cod %in%
            c(
              "K22.7",
              "K31.9"
              # K32 - K34: No existe
              # K39: No existe
              # K47 - K49: No existe
              # K53 - K54: No existe
            ) |
          between(cie10_cod, "K63.0", "K63.4") |
          cie10_cod %in%
            c(
              "K63.8",
              "K63.9",
              # K69: No existe
              "K70.4",
              "K70.9",
              # K78 - K79: No existe
              # K84: No existe
              # K88 - K89: No existe
              # K92.0: Digestivas
              "K92.9"
              # K93.0: CMNN
              # K96 - K99: No existe
              # L06 - L07: No existe
              # L09: No existe
              # L15 - L19: No existe
              # L31 - L39: No existe
              # L69: No existe
              # L77 - L79: No existe
              # N09: No existe
            ) |
          between(cie10_cod, "N13.0", "N13.5") |
          between(cie10_cod, "N13.7", "N13.9") |
          # N24: No existe
          cie10_cod %in%
            c(
              "N28.8",
              "N28.9",
              # N38: No existe
              "N39.9",
              "N40.0"
              # N40: No tiene decimales
            ) |
          # N54 - N59: No existe
          # N66 - N69: No existe
          # N78 - N79: No existe
          # N84.0: Otras ENT
          between(cie10_cod, "N84.2", "N86.0") |
          between(cie10_cod, "N88.0", "N90.9") |
          between(cie10_cod, "N92.0", "N95.0") |
          between(cie10_cod, "O08.0", "O08.9") |
          # O17 - O19: No existe
          # O27: No existe
          # O37 - O39: No existe
          # O49 - O59: No existe
          # O78 - O79: No existe
          # O93: No existe
          between(cie10_cod, "O94.0", "O95.0") |
          # 095: No tiene decimales
          # P06: No existe
          # P09: No existe
          # P16 - P19: No existe
          # P30 - P34: No existe
          # P40 - P49: No existe
          # P62 - P69: No existe
          # P73: No existe
          between(cie10_cod, "P74.0", "P75.0") |
          # P79: No existe
          between(cie10_cod, "P80.0", "P81.9") |
          # P82: No existe
          # P85 - P89: No existe
          between(cie10_cod, "P92.0", "P92.9") |
          cie10_cod == "P96.9" |
          # P97 - P99: No existe
          # Q08 - Q09: No existe
          between(cie10_cod, "Q10.0", "Q10.3") |
          # Q19: No existe
          # Q29: No existe
          # Q46 - Q49: No existe
          # Q57: No existe
          # Q88: No existe
          cie10_cod == "Q89.9" |
          # Q94: No existe
          between(cie10_cod, "Q99.9", "R01.2") |
          # R07.0: GC1
          between(cie10_cod, "R07.1", "R07.9") |
          cie10_cod == "R31.0" |
          # R31: No tiene decimales
          between(cie10_cod, "X64.0", "X64.9") | # GBD-2023
          between(cie10_cod, "X69.0", "X69.9") ~ "GC:GC3"
      )
    )
  )


## GC nivel 4 (GC4) -----
recod_defun <- recod_defun |>
  mutate(
    gbd_paso1 = coalesce(
      gbd_paso1,
      case_when(
        cie10_cod %in%
          c("B16.9", "B54.0", "B64.0") |
          between(cie10_cod, "B82.0", "B82.9") |
          cie10_cod %in%
            c(
              "B83.9",
              # C69.0: Neoplasias
              "C69.9",
              "C91.1",
              "C91.4",
              "C91.5",
              "C91.7",
              "C91.8",
              "C91.9",
              "C92.7",
              "C92.8",
              "C92.9",
              "C93.2",
              "C93.5",
              "C93.6",
              "C93.7",
              "C93.9"
            ) |
          between(cie10_cod, "E12.0", "E14.9") |
          # G00.0: CMNN
          between(cie10_cod, "G00.9", "G02.1") |
          cie10_cod %in%
            c(
              "G03.9",
              "I37.9",
              "I42.0",
              "I42.9",
              "I51.5"
            ) |
          between(cie10_cod, "I64.0", "I64.9") |
          # I67.0: ECV
          cie10_cod %in%
            c(
              "I67.8",
              "I67.9",
              # I68.0: ECV
              "I68.8",
              "I68.9"
            ) |
          between(cie10_cod, "I69.4", "I69.9") |
          # J07 - J08: No existe
          cie10_cod == "J15.9" |
          between(cie10_cod, "J17.0", "J18.9") |
          # J19: No existe
          cie10_cod %in% c("J22.0", "J64.0") |
          # J23 - J29: No existe
          # J64: No tiene decimales
          # P23.0: CMNN
          between(cie10_cod, "P23.5", "P23.9") |
          between(cie10_cod, "R73.0", "R73.9") |
          cie10_cod %in% c("V87.0", "V87.1") |
          between(cie10_cod, "V87.4", "V88.1") |
          between(cie10_cod, "V88.4", "V89.9") |
          cie10_cod == "V99.0" |
          between(cie10_cod, "X84.0", "X84.9") |
          between(cie10_cod, "Y09.0", "Y09.9") |
          between(cie10_cod, "Y85.0", "Y85.9") ~ "GC:GC4"
      )
    )
  )


## ENT objetivo  -----
recod_defun <- recod_defun |>
  mutate(
    gbd_paso1 = coalesce(
      gbd_paso1,
      case_when(
        # --- Neoplasias (NPL) ---
        between(cie10_cod, "C00.0", "C57.8") |
          # C58.0: CMNN
          # C59: No existe
          between(cie10_cod, "C60.0", "D48.6") ~ "ENT:NPL",
        # C89: No existe
        # D49: No existe

        # --- Cardiovasculares (ECV) ---
        cie10_cod %in%
          c("B33.2", "K75.1") |
          between(cie10_cod, "G45.0", "G46.8") |
          between(cie10_cod, "I01.0", "I02.0") |
          # I02.9: CMNN
          between(cie10_cod, "I05.0", "I11.9") |
          # I12.0 - I13.9: DM-CKD
          between(cie10_cod, "I20.0", "I27.0") |
          # I27.1: Musculoesqueléticas
          cie10_cod == "I27.2" |
          between(cie10_cod, "I28.0", "I41.1") |
          # I41.2: CMNN
          between(cie10_cod, "I41.8", "I67.6") |
          # I67.7: Musculoesqueléticas
          between(cie10_cod, "I68.0", "I83.9") |
          # I84.0 - I85.9: Digestivas
          between(cie10_cod, "I86.0", "I89.9") ~ "ENT:ECV",
        # I98.0 - I98.1: CMNN
        # I98.2: Digestivas
        # I98.4: No existe
        # I99.0: GC3
        # I98.9: CE

        # --- Respiratorias crónicas (CRD) ---
        between(cie10_cod, "D86.0", "D86.2") |
          cie10_cod %in% c("D86.9", "G47.3") |
          between(cie10_cod, "J30.0", "J35.9") |
          # J36.0: CMNN
          between(cie10_cod, "J37.0", "J68.9") |
          # J70.0 - J70.4: CE
          # J70.5: No existe
          between(cie10_cod, "J70.8", "J84.9") |
          # J91.0: CMNN (no tiene decimales)
          cie10_cod %in% c("J92.0", "J92.9") ~ "ENT:CRD",

        # --- Diabetes y renales crónicas (DM-CKD) ---
        between(cie10_cod, "E10.0", "E11.9") |
          between(cie10_cod, "I12.0", "I13.9") |
          between(cie10_cod, "N00.0", "N08.8") |
          between(cie10_cod, "N18.0", "N18.9") |
          cie10_cod %in% c("N15.0", "P70.2") ~ "ENT:DM-CKD"
        # D63.1: No existe
      )
    )
  )


## ENT no objetivo -----
recod_defun <- recod_defun |>
  mutate(
    gbd_paso1 = coalesce(
      gbd_paso1,
      case_when(
        # --- Digestivas ---
        between(cie10_cod, "B18.0", "B18.9") |
          between(cie10_cod, "I84.0", "I85.9") |
          cie10_cod == "I98.2" |
          between(cie10_cod, "K20.0", "K22.9") |
          # K23.0 - K23.1: CMNN
          between(cie10_cod, "K23.8", "K42.9") |
          # K43.0 - K43.9: CE
          between(cie10_cod, "K44.0", "K51.9") |
          # K47 - K49: No existe
          # K52.0: CE
          # K52.1 - K52.3: CMNN
          between(cie10_cod, "K52.8", "K62.6") |
          # K53 - K54: No existe
          # K62.7: CE
          between(cie10_cod, "K62.8", "K66.8") |
          # K67.0 - K67.8: CMNN
          # K68 - K69: No existe
          between(cie10_cod, "K70.0", "K74.9") |
          # K75.1: ECV
          cie10_cod == "K75.2" |
          # K75.3: CMNN
          between(cie10_cod, "K75.4", "K76.2") |
          # K76.3: CMNN
          between(cie10_cod, "K76.4", "K76.9") |
          # K77.0: CMNN
          between(cie10_cod, "K77.8", "K90.9") |
          # K78 - K79: No existe
          # K84: No existe
          # K88 - K89: No existe
          # K91.0 - K91.9: CE
          cie10_cod %in%
            c(
              "K92.8",
              # K93.0 - K93.1: CMNN
              "K93.8",
              "M07.4",
              "M07.5",
              "M09.1",
              "M09.2"
            ) ~ "ENT:DIG",

        # --- Neurológicas ---
        between(cie10_cod, "F00.0", "F02.0") |
          # F02.1: CMNN
          cie10_cod %in% c("F02.2", "F02.3") |
          # F02.4: CMNN
          between(cie10_cod, "F02.8", "F03.9") |
          between(cie10_cod, "G10.0", "G13.8") |
          # G14.0 - G14.6: CMNN
          cie10_cod == "G20.0" |
          # G20: No tiene decimales
          # G21.0 - G21.1: CE
          # G21.3: CMNN
          between(cie10_cod, "G23.0", "G23.9") |
          # G24.0: CE
          between(cie10_cod, "G24.1", "G25.0") |
          # G25.1: CE
          cie10_cod %in%
            c(
              "G25.2",
              "G25.3",
              # G25.4: CE
              "G25.5"
              # G25.6 - G25.7: CE
            ) |
          between(cie10_cod, "G25.8", "G31.1") |
          # G27 - G29: No existe
          # G31.2: Uso de sustancias
          between(cie10_cod, "G31.8", "G41.9") |
          # G33 - G34: No existe
          # G38 - G39: No existe
          # G42: No existe
          # G45.0 - G46.8: ECV
          # G47.3: CRD
          # G48 - G49: No existe
          between(cie10_cod, "G61.0", "G61.9") |
          # G62.1: Uso de sustancias
          # G65 - G69: No existe
          between(cie10_cod, "G70.0", "G71.1") |
          # G71.2: Otras ENT
          between(cie10_cod, "G71.3", "G71.9") |
          # G72.0: CE
          # G72.1: Uso de sustancias
          between(cie10_cod, "G72.2", "G90.9") |
          # G74 - G79: No existe
          # G84 - G89: No existe
          # G93.7: CE
          between(cie10_cod, "G95.0", "G95.9") |
          # G97.0 - G97.9: CE
          between(cie10_cod, "M33.0", "M33.9") |
          cie10_cod == "P94.0" |

          # --- Mentales ---
          between(cie10_cod, "F50.0", "F50.5") ~ "ENT:NEU-MENT",

        # --- Uso de sustancias ---
        between(cie10_cod, "F10.0", "F18.9") |
          between(cie10_cod, "R78.0", "R78.5") |
          between(cie10_cod, "X45.0", "X45.9") |
          between(cie10_cod, "X65.0", "X65.9") |
          between(cie10_cod, "Y15.0", "Y15.9") |
          between(cie10_cod, "Y90.0", "Y91.9") |
          cie10_cod %in%
            c(
              "E24.4",
              "G31.2",
              "G62.1",
              "G72.1",
              "P04.3",
              "P04.4",
              "P96.1",
              "Q86.0"
            ) ~ "ENT:SUST",

        # --- Piel y subcutáneas ---
        between(cie10_cod, "A66.0", "A67.9") |
          between(cie10_cod, "I89.1", "I89.8") |
          between(cie10_cod, "L00.0", "L51.9") |
          # L06 - L07: No existe
          # L09: No existe
          # L15 - L19: No existe
          # L31 - L39: No existe
          # L46 - L49: No existe
          # L55.0 - L55.9: CE
          # L56.3: CE
          # L56.8 - L56.9: CE
          # L58.0 - L58.9: CE
          # L69: No existe
          # L76 - L79: No existe
          between(cie10_cod, "L88.0", "L89.9") |
          # L93.0 - L93.2: Musculoesqueléticas
          # L96: No existe
          between(cie10_cod, "L97.0", "L98.4") |
          between(cie10_cod, "M07.0", "M07.3") |
          cie10_cod %in%
            c(
              "A46.0",
              "B86.0",
              "D86.3",
              "H05.0",
              "H05.1",
              "M09.0",
              "M72.5",
              "M72.6"
            ) ~ "ENT:PIEL",

        # --- Musculoesqueléticas ---
        cie10_cod %in%
          c("I27.1", "I67.7") |
          between(cie10_cod, "L93.0", "L93.2") |
          between(cie10_cod, "M00.0", "M03.0") |
          # M03.1: CMNN
          between(cie10_cod, "M03.2", "M06.9") |
          # M04: No existe
          # M07.0 - M07.3: Piel y subcutáneas
          # M07.4 - M07.5: Digestivas
          between(cie10_cod, "M07.6", "M09.0") |
          # M09.1 - M09.2: Digestivas
          cie10_cod == "M09.8" |
          # M12.1: CMNN
          # M26 - M29: No existe
          between(cie10_cod, "M30.0", "M32.9") |
          # M33.0 - M33.9: Neurológicas
          between(cie10_cod, "M34.0", "M43.1") |
          # M37 - M39: No existe
          # M49.1: CMNN
          # M55 - M59: No existe
          # M64: No existe
          cie10_cod %in%
            c(
              "M65.0",
              "M71.0",
              "M71.1"
              # M73.0 - M73.1: CMNN
              # M74: No existe
            ) |
          between(cie10_cod, "M80.0", "M87.0") |
          # M87.1: CE
          between(cie10_cod, "M88.0", "M89.5") |
          # M89.6: CMNN
          between(cie10_cod, "M89.7", "M89.9") ~ "ENT:MUSC",

        # --- Otras ENT ---
        between(cie10_cod, "D25.0", "D25.9") |
          # D26.0:  Neoplasias
          cie10_cod == "D28.2" |
          between(cie10_cod, "D55.0", "D58.9") |
          # D59.0:  Causas externas
          cie10_cod == "D59.1" |
          # D59.2: CE
          between(cie10_cod, "D59.3", "D59.5") |
          # D59.6: CE
          between(cie10_cod, "D59.8", "D69.4") |
          # D69.5: CE
          between(cie10_cod, "D69.6", "D77.0") |
          # D70: No tiene decimales
          # D78 - D79: No existe
          # D85: No existe
          # D86.0 - D86.2: CRD
          # D86.3: Piel y subcutáneas
          cie10_cod == "D86.8" |
          # D86.9: CRD
          # D87 - D88: No existe
          between(cie10_cod, "D89.0", "D89.2") |
          # D89.3: CMNN
          # D90 - D99: No existe
          # E00.0 - E02.0: CMNN
          cie10_cod %in% c("E03.0", "E03.1") |
          # E03.2: CE
          between(cie10_cod, "E03.3", "E06.3") |
          # E06.4: CE
          between(cie10_cod, "E06.5", "E07.1") |
          # E08 - E09: No existe
          # E10.0 - E11.9: DM-CKD
          # E16.0: CE
          between(cie10_cod, "E16.1", "E23.0") |
          # E17 - E19: No existe
          # E23.1: CE
          between(cie10_cod, "E23.2", "E24.1") |
          # E24.2: CE
          cie10_cod == "E24.3" |
          # E24.4: Uso de sustancias
          between(cie10_cod, "E24.8", "E27.2") |
          # E27.3: CE
          between(cie10_cod, "E27.4", "E34.8") |
          # E36 - E39: No existe
          # E40.0 - E46.0: CMNN
          # E46: No tiene decimales
          # E47 - E49: No existe
          # E51.0 - E61.9: CMNN
          # E62: No existe
          # E63.0 - E64.0: CMNN
          # E64.2 - E64.9: CMNN
          between(cie10_cod, "E65.0", "E66.0") |
          # E66.1: CE
          between(cie10_cod, "E66.2", "E88.2") |
          # E69: No existe
          # E88.3: CE
          between(cie10_cod, "E88.4", "E88.9") |
          # E89.0 - E89.9: CE
          # E91 - E99: No existe
          cie10_cod == "G71.2" |
          # N00.0 - N08.8: DM-CKD
          # N09: No existe
          between(cie10_cod, "N10.0", "N13.6") |
          # N14.0 - N14.4: CE
          # N15.0: DM-CKD
          between(cie10_cod, "N15.1", "N16.8") |
          # N18.0 - N18.9: DM-CKD
          between(cie10_cod, "N20.0", "N30.3") |
          # N24: No existe
          # N30.4: CE
          between(cie10_cod, "N30.8", "N72.0") |
          # N38: No existe
          # N44: No tiene decimales
          # N52 - N59: No existe
          # N65 - N69: No existe
          # N74.0 - N74.1: CMNN
          between(cie10_cod, "N75.0", "N87.9") |
          # N78 - N79: No existe
          # N96.0: CMNN
          # N98.0 - N98.9: CMNN
          # N99.0 - N99.9: CE
          between(cie10_cod, "Q00.0", "Q85.9") |
          # Q08 - Q09: No existe
          # Q19: No existe
          # Q29: No existe
          # Q46 - Q49: No existe
          # Q57 - Q59: No existe
          # Q86.0:  Uso de sustancias
          # Q86.1 - Q86.2: CE
          between(cie10_cod, "Q86.8", "Q99.8") |
          # Q88: No existe
          # Q94: No existe
          cie10_cod %in% c("P72.1", "P96.0") |
          between(cie10_cod, "R95.0", "R95.9") ~ "ENT:OTR-ENT"
      )
    )
  )


## Causas externas (CE) -----
recod_defun <- recod_defun |>
  mutate(
    gbd_paso1 = coalesce(
      gbd_paso1,
      case_when(
        # --- Accidentes de tránsito (TRA) ---
        between(cie10_cod, "V01.0", "V04.9") |
          # V05.0 - V05.9: Otras lesiones transporte
          between(cie10_cod, "V06.0", "V80.9") |
          # V81.0 - V81.9: Otras lesiones transporte
          between(cie10_cod, "V82.0", "V82.9") |
          # V83.0 - V86.9: Otras lesiones transporte
          cie10_cod %in% c("V87.2", "V87.3") ~ "CE:TRA",

        # --- Otras lesiones por transporte ---
        # V00.0 - V00.8: No existe
        between(cie10_cod, "V05.0", "V05.9") |
          between(cie10_cod, "V81.0", "V81.9") |
          between(cie10_cod, "V83.0", "V86.9") |
          between(cie10_cod, "V90.0", "V98.8") |
          cie10_cod %in% c("V88.2", "V88.3") ~ "CE:OTR-TRA",

        # --- Suicidio (SH) ---
        between(cie10_cod, "X60.0", "X63.9") |
          between(cie10_cod, "X66.0", "X68.9") |
          between(cie10_cod, "X70.0", "X83.9") |
          cie10_cod == "Y87.0" ~ "CE:SH",

        # --- Violencia interpersonal (VI) ---
        between(cie10_cod, "X85.0", "Y08.9") |
          cie10_cod == "Y87.1" ~ "CE:VI",

        # --- Accidentes por caídas (CA) ---
        between(cie10_cod, "W00.0", "W19.9") ~ "CE:CA",

        # --- Represión policial y terrorismo ---
        between(cie10_cod, "U00.0", "U03.0") |
          between(cie10_cod, "Y35.0", "Y38.9") |
          cie10_cod %in% c("Y89.0", "Y89.1") |

          # --- Otras lesiones no intencionales ---
          cie10_cod %in%
            c(
              "D52.1",
              "D59.0",
              "D59.2",
              "D59.6",
              "D69.5",
              # D70.1 - D70.2: No existe
              "E03.2",
              "E06.4",
              "E16.0",
              "E23.1",
              "E24.2",
              "E27.3",
              "E66.1",
              "E88.3"
            ) |
          between(cie10_cod, "E89.0", "E89.9") |
          cie10_cod %in%
            c(
              "G21.0",
              "G21.1",
              "G24.0",
              "G25.1",
              "G25.4",
              "G25.6",
              "G25.7",
              "G72.0",
              "G93.7"
            ) |
          between(cie10_cod, "G97.0", "G97.9") |
          cie10_cod %in% c("I95.2", "I95.3") |
          between(cie10_cod, "I97.0", "I97.9") |
          between(cie10_cod, "J70.0", "J70.5") |
          between(cie10_cod, "J95.0", "J95.9") |
          between(cie10_cod, "K43.0", "K43.9") |
          cie10_cod %in% c("K52.0", "K62.7") |
          between(cie10_cod, "K91.0", "K91.9") |
          between(cie10_cod, "L55.0", "L55.9") |
          cie10_cod %in% c("L56.3", "L56.8", "L56.9") |
          between(cie10_cod, "L58.0", "L58.9") |
          cie10_cod == "M87.1" |
          between(cie10_cod, "N14.0", "N14.4") |
          cie10_cod == "N30.4" |
          between(cie10_cod, "N99.0", "N99.9") |
          cie10_cod %in%
            c(
              "P04.0",
              "P04.1",
              "P70.3",
              "P93.0",
              # P93: No tiene decimales
              "P96.2",
              "P96.5",
              "Q86.1",
              "Q86.2",
              "R50.2"
            ) |
          between(cie10_cod, "W20.0", "W46.0") |
          # W46: No tiene decimales
          # W47 - W48: No existe
          between(cie10_cod, "W49.0", "W62.9") |
          # W61 - W63: No existe
          between(cie10_cod, "W64.0", "W70.9") |
          # W71 - W72: No existe
          between(cie10_cod, "W73.0", "W81.9") |
          # W82: No existe
          between(cie10_cod, "W83.0", "W94.9") |
          # W95 - W98: No existe
          between(cie10_cod, "W99.0", "X06.9") |
          # X07: No existe
          between(cie10_cod, "X08.0", "X39.9") |
          # X45.0 - X45.9: Uso de sustancias
          between(cie10_cod, "X47.0", "X48.9") |
          between(cie10_cod, "X50.0", "X54.9") |
          # X55 - X56: No existe
          between(cie10_cod, "X57.0", "X58.9") |
          # X60.0 - X63.9: Suicidio
          # X65.0 - X65.9: Uso de sustancias
          # X66.0 - X68.9: Suicidio
          # X70.0 - X83.9: Suicidio
          # X85.0 - Y08.9: Suicidio
          # Y35.0 - Y38.9: Suicidio
          # Y15.0 - Y15.9: Uso de sustancias
          # Y35.0 - Y35.9: Represión policial
          # Y36.0: Terrorismo
          # Y37 - Y39: No existe
          between(cie10_cod, "Y40.0", "Y84.9") |
          # Y87.0 - Y87.1: Suicidio
          between(cie10_cod, "Y88.0", "Y88.3") ~ "CE:OTR-CE",
        # Y89.0 - Y89.1: Suicidio
        # Y90.0 - Y91.9: Uso de sustancias
      )
    )
  )


## CMNN -----
recod_defun <- recod_defun |>
  mutate(
    gbd_paso1 = coalesce(
      gbd_paso1,
      case_when(
        # --- Infecciosas ---
        between(cie10_cod, "A00.0", "A39.9") |
          # A10 - A14: No existe
          # A29: No existe
          # A45: No existe
          # A46: Otras ENT
          # A47: No existe
          between(cie10_cod, "A48.1", "A65.0") |
          # A61 - A62: No existe
          # A65: No tiene decimales
          # A66.0 - A67.9: Piel y subcutáneas
          between(cie10_cod, "A68.0", "B17.2") |
          # A70: No tiene decimales
          # A72 - A73: No existe
          # A76: No existe
          # A90 - A91: No existe
          # B10 - B14: No existe
          # B18.0 - B18.9: Digestivas
          # B19.1: No existe
          between(cie10_cod, "B20.0", "B83.8") |
          # B28 - B29: No existe
          # B31 - B32: No existe
          # B59: No existe
          # B61 - B63: No existe
          # B84: No existe
          # B86.0: Piel y subcutáneas
          # B89.0: GC3 (no tiene decimales)
          between(cie10_cod, "B90.0", "B97.6") |
          # B91: No tiene decimales
          # B93: No existe
          cie10_cod %in% c("D70.3", "D89.3") |
          cie10_cod %in% c("F02.4", "F07.1") |
          between(cie10_cod, "G00.0", "G05.8") |
          # G10.0 - G13.8: Neurológicas
          cie10_cod %in% c("G14.0", "G21.3") |
          # G14: No tiene decimales
          between(cie10_cod, "H70.0", "H70.9") |
          cie10_cod %in% c("I02.9", "I41.2", "I98.0", "I98.1") |
          between(cie10_cod, "J00.0", "J21.9") |
          # J07 - J08: No existe
          # J19: No existe
          # J23 - J29: No existe
          cie10_cod %in% c("J36.0", "J91.0", "K23.0", "K23.1") |
          between(cie10_cod, "K52.1", "K52.3") |
          between(cie10_cod, "K67.0", "K67.8") |
          cie10_cod %in%
            c(
              "K75.3",
              "K76.3",
              "K77.0",
              "K93.0",
              "K93.1",
              "M03.1",
              "M49.0",
              "M49.1",
              "M73.0",
              "M73.1",
              "M89.6",
              "N74.0",
              "N74.1"
            ) |
          between(cie10_cod, "P23.0", "P23.4") |
          between(cie10_cod, "P35.0", "P35.9") |
          # P36.0 - P36.9: MAT-NEO
          cie10_cod %in%
            c(
              "P37.0",
              "P37.1"
              # P37.2: MAT-NEO
            ) |
          between(cie10_cod, "P37.3", "P37.9") |
          # R19.7: No existe
          # U00 - U03: No existe
          between(cie10_cod, "U04.0", "U89.9") |
          # U05: No existe
          # U07.0: No existe
          # U50 - U81: No existe
          between(cie10_cod, "Z16.0", "Z16.3") ~ "CMNN:INF",

        # --- Maternas y neonatales ----
        cie10_cod == "C58.0" |
          between(cie10_cod, "N96.0", "N98.9") |
          between(cie10_cod, "O00.0", "P03.9") |
          # O09: No existe
          # O17 - O19: No existe
          # O27: No existe
          # O37 - O39: No existe
          # O49 - O59: No existe
          # O76 - O79: No existe
          # O93: No existe
          # P04.0 - P04.1: CE
          cie10_cod == "P04.2" |
          # P04.3 - P04.4: Uso de sustancias
          between(cie10_cod, "P04.5", "P22.9") |
          # P06: No existe
          # P09: No existe
          # P16 - P19: No existe
          # P23.0 - P23.4: Infecciosas
          between(cie10_cod, "P24.0", "P29.9") |
          # P30 - P34: No existe
          # P35.0 - P35.9: Infecciosas
          between(cie10_cod, "P36.0", "P36.9") |
          # P37.0 - P37.1: Infeccionsas
          cie10_cod == "P37.2" |
          # P37.3 - P37.9: Infecciosas
          between(cie10_cod, "P38.0", "P70.1") |
          # P40 - P49: No existe
          # P62 - P69: No existe
          # P70.2: DM-CKD
          # P70.3: CE
          between(cie10_cod, "P70.4", "P72.0") |
          # P72.1: Otras ENT
          between(cie10_cod, "P72.2", "P95.9") |
          # P73: No existe
          # P79: No existe
          # P82: No existe
          # P84 - P89: No existe
          # P93: CE (no tiene decimales)
          # P96.0:  Otras ENT
          cie10_cod %in% c("P96.3", "P96.4", "P96.8") ~ "CMNN:MAT-NEO",
        # P97 - P99: No existe

        # --- Deficiencias nutricionales ---
        between(cie10_cod, "D50.1", "D52.0") |
          # D52.1: CE
          between(cie10_cod, "D52.8", "D53.9") |
          between(cie10_cod, "E00.0", "E02.0") |
          # E03.0 - E03.1: Otras ENT
          # E03.2: CE
          # E03.3 - E06.3: Otras ENT
          between(cie10_cod, "E40.0", "E64.9") |
          # E47 - E49: No existe
          # E62: No existe
          cie10_cod == "M12.1 " ~ "CMNN:NUTR"
      )
    )
  )


# Paso 2a: Reclasificar GC3-GC4 ------------------------------------------
recod_defun <- recod_defun |>
  mutate(
    gbd_paso2a = case_when(
      # --- CMNN: Infecciosas ---
      str_detect(gbd_paso1, "GC3|GC4") &
        (between(cie10_cod, "A31.0", "B94.2") |
          cie10_cod %in%
            c("J02.9", "J03.9", "J04.3", "J06.9", "J22.0")) ~ "CMNN:INF",

      # --- CMNN: Maternas y neonatales ---
      str_detect(gbd_paso1, "GC3|GC4") &
        between(cie10_cod, "O08.0", "P99.9") ~ "CMNN:MAT-NEO",

      # --- ENT: Neoplasias ---
      str_detect(gbd_paso1, "GC3|GC4") &
        between(cie10_cod, "C14.0", "D48.9") ~ "ENT:NPL",

      # --- ENT: Cardiovasculares ---
      str_detect(gbd_paso1, "GC3|GC4") &
        between(cie10_cod, "I00.0", "I99.0") ~ "ENT:ECV",

      # --- ENT: Respiratorias crónicas ---
      str_detect(gbd_paso1, "GC3|GC4") &
        between(cie10_cod, "J40.0", "J98.9") ~ "ENT:CRD",

      # --- ENT: Diabetes y renales crónicas ---
      str_detect(gbd_paso1, "GC3|GC4") &
        (between(cie10_cod, "E12.0", "E14.9") |
          between(cie10_cod, "R73.0", "R73.9")) ~ "ENT:DM-CKD",

      # --- ENT: Digestivas ---
      str_detect(gbd_paso1, "GC3|GC4") &
        between(cie10_cod, "K21.0", "K92.9") ~ "ENT:DIG",

      # --- ENT: Neurológicas y mentales ---
      str_detect(gbd_paso1, "GC3|GC4") &
        between(cie10_cod, "F04.0", "G98.9") ~ "ENT:NEU-MENT",

      # --- ENT: Otras ENT ---
      str_detect(gbd_paso1, "GC3|GC4") &
        (between(cie10_cod, "D75.9", "E07.9") |
          between(cie10_cod, "E34.0", "E87.7") |
          between(cie10_cod, "N13.0", "N95.0") |
          between(cie10_cod, "Q08.0", "R31.9")) ~ "ENT:OTR-ENT",

      # --- CE: Accidentes de tránsito ---
      gbd_paso1 == "GC:GC4" &
        (between(cie10_cod, "V87.0", "V87.9") |
          between(cie10_cod, "V89.0", "V89.9")) ~ "CE:TRA",

      # --- CE: Otras lesiones por transporte ---
      gbd_paso1 == "GC:GC4" &
        (between(cie10_cod, "V88.0", "V88.9") |
          between(cie10_cod, "V99.0", "V99.9")) ~ "CE:OTR-TRA",

      # --- CE: Suicidio ---
      str_detect(gbd_paso1, "GC3|GC4") &
        between(cie10_cod, "X64.0", "X84.9") ~ "CE:SH",

      # --- CE: Violencia interpersonal ---
      gbd_paso1 == "GC:GC4" &
        between(cie10_cod, "Y09.0", "Y09.9") ~ "CE:VI",

      # --- CE: Otras CE ---
      gbd_paso1 == "GC:GC4" &
        between(cie10_cod, "Y85.0", "Y85.9") ~ "CE:OTR-CE",

      # --- Neumonías inespecíficas (NNE) ---
      gbd_paso1 == "GC:GC4" &
        (cie10_cod == "J15.9" |
          between(cie10_cod, "J18.0", "J18.9")) ~ "GC:NNE",

      # --- Valor por defecto ---
      .default = gbd_paso1
    )
  )


# Frecuencias paso 2a ----------------------------------------------------
## Frecuencias NNE x sexo y edad----
recod_defun |>
  filter(gbd_paso2a == "GC:NNE") |>
  tabyl(grupo_edad, sexo) |>
  adorn_percentages(denominator = "col")


## Frecuencias causas definidas x sexo y edad ----
tab_fr <- recod_defun |>
  # Descartar GC
  filter_out(str_detect(gbd_paso2a, "GC")) |>

  # Renombrar columnas
  rename(causa = gbd_paso2a) |>

  # Frecuencias x sexo y edad
  tabyl(causa, sexo, grupo_edad) |>
  adorn_percentages(denominator = "col") |>

  # Convertir a dataframe
  list_rbind(names_to = "grupo_edad")


# Paso 2b: Redistribuir neumonías NE -------------------------------------
set.seed(123)

recod_defun <- recod_defun |>
  # --- Distribuir 50% de GC:NNE entre CMNN ---
  mutate(
    gbd_paso2b = {
      out <- gbd_paso2a

      # Sexo y edad del grupo actual
      sexo_actual <- cur_group()$sexo
      edad_actual <- cur_group()$grupo_edad

      # Seleccionar ~50% de las NEE
      idx <- which(
        gbd_paso2a == "GC:NNE" &
          runif(n()) <= 0.5
      )

      # Redistribución multinomial x edad y sexo
      if (length(idx) > 0) {
        datos <- tab_fr |>
          filter(
            str_detect(causa, "^CMNN:"),
            grupo_edad == edad_actual
          )

        # Probabilidades según el sexo
        prob <- datos[[sexo_actual]]

        # Normalizar para que sumen 1
        prob <- prob / sum(prob)

        # Redistribución multinomial x sexo y edad
        out[idx] <- rep(
          datos$causa,
          rmultinom(
            n = 1,
            size = length(idx),
            prob = prob
          )
        )
      }

      out
    },
    .by = c(sexo, grupo_edad)
  ) |>

  # --- Redistribuir 50% entre ENT objetivo ---
  mutate(
    gbd_paso2b = {
      out <- gbd_paso2b

      # Sexo y edad del grupo actual
      sexo_actual <- cur_group()$sexo
      edad_actual <- cur_group()$grupo_edad

      # Seleccionar NNE
      idx <- which(out == "GC:NNE")

      if (length(idx) > 0) {
        # Obtener frecuencias ENT objetivo x edad y sexo
        datos <- tab_fr |>
          filter(
            str_detect(causa, "CRD|DM|ECV|NPL"),
            grupo_edad == edad_actual
          )

        # Normalizar las probabilidades
        prob <- datos[[sexo_actual]] / sum(datos[[sexo_actual]])

        # Redistribución multinomial
        out[idx] <- rep(
          datos$causa,
          rmultinom(
            n = 1,
            size = length(idx),
            prob = prob
          )
        )
      }

      # Resultado final
      out
    },
    .by = c(sexo, grupo_edad)
  )


# Paso 3: Redistribuir GC2 -----------------------------------------------
## Reclasificar GC2 de ECV -----
recod_defun <- recod_defun |>
  mutate(
    gbd_paso3a = if_else(
      gbd_paso2b == "GC:GC2" &
        (between(cie10_cod, "I10.0", "I27.9") |
          between(cie10_cod, "I70.0", "I74.9")),
      "ENT:ECV",
      gbd_paso2b
    )
  )


## Redistribuir GC2 de  CE -----
set.seed(123)
recod_defun <- recod_defun |>
  # --- GC2: cualquier CE ---
  mutate(
    gbd_paso3b = {
      out <- gbd_paso3a

      # Sexo y edad del grupo actual
      sexo_actual <- cur_group()$sexo
      edad_actual <- cur_group()$grupo_edad

      # Seleccionar GC redistribuibles
      idx <- which(
        gbd_paso3a == "GC:GC2" &
          (between(cie10_cod, "Y24.5", "Y24.7") |
            between(cie10_cod, "Y27.4", "Y27.6") |
            between(cie10_cod, "Y33.0", "Y33.9") |
            between(cie10_cod, "Y95.0", "Y98.0") |
            cie10_cod %in%
              c(
                "Y25.2",
                "Y26.3",
                "Y28.3",
                "Y28.5",
                "Y29.3",
                "Y86.0",
                "Y86.2",
                "Y86.8",
                "Y87.2",
                "Y89.9",
                "G44.3",
                "G91.3"
              ))
      )

      if (length(idx) > 0) {
        # Obtener las CE para esa edad
        datos <- tab_fr |>
          filter(
            str_detect(causa, "CE:"),
            grupo_edad == edad_actual
          )

        # Normalizar las probabilidades según el sexo
        prob <- datos[[sexo_actual]] / sum(datos[[sexo_actual]])

        # Redistribución multinomial
        out[idx] <- rep(
          datos$causa,
          rmultinom(
            n = 1,
            size = length(idx),
            prob = prob
          )
        )
      }
      # Resultado final
      out
    },
    .by = c(sexo, grupo_edad)
  ) |>

  # --- GC2: CE objetivo ---
  mutate(
    gbd_paso3b = {
      out <- gbd_paso3b

      # Sexo y edad del grupo actual
      sexo_actual <- cur_group()$sexo
      edad_actual <- cur_group()$grupo_edad

      # Seleccionar GC redistribuibles
      idx <- which(
        gbd_paso3b == "GC:GC2" &
          between(cie10_cod, "Y31.0", "Y32.9")
      )

      if (length(idx) > 0) {
        # Obtener las CE objetivo de esa edad
        datos <- tab_fr |>
          filter(
            str_detect(causa, "CE:TRA|SH|VI"),
            grupo_edad == edad_actual
          )

        # Normalizar las probabilidades
        prob <- datos[[sexo_actual]] / sum(datos[[sexo_actual]])

        # Redistribución multinomial
        out[idx] <- rep(
          datos$causa,
          rmultinom(
            n = 1,
            size = length(idx),
            prob = prob
          )
        )
      }

      out
    },
    .by = c(sexo, grupo_edad)
  ) |>

  # --- GC2: VI, SH, CA ---
  mutate(
    gbd_paso3b = {
      out <- gbd_paso3b

      # Sexo y edad del grupo actual
      sexo_actual <- cur_group()$sexo
      edad_actual <- cur_group()$grupo_edad

      # Seleccionar GC redistribuibles
      idx <- which(
        gbd_paso3b == "GC:GC2" &
          (between(cie10_cod, "Y29.4", "Y30.9") |
            cie10_cod %in% c("Y29.1", "Y29.2"))
      )

      if (length(idx) > 0) {
        # Obtener las CE objetivo  de esa edad
        datos <- tab_fr |>
          filter(
            str_detect(causa, "CE:CA|SH|VI"),
            grupo_edad == edad_actual
          )

        # Normalizar las probabilidades
        prob <- datos[[sexo_actual]] / sum(datos[[sexo_actual]])

        # Redistribución multinomial
        out[idx] <- rep(
          datos$causa,
          rmultinom(
            n = 1,
            size = length(idx),
            prob = prob
          )
        )
      }

      out
    },
    .by = c(sexo, grupo_edad)
  )


## Redistribuir X59 ----
set.seed(123)
recod_defun <- recod_defun |>
  # --- Redistribuir ~88% entre CE ---
  mutate(
    gbd_paso3c = {
      out <- gbd_paso3b

      idx <- which(
        gbd_paso3b == "GC:GC2" &
          between(cie10_cod, "X59.0", "X59.9")
      )

      if (length(idx) > 0) {
        # Número de códigos X59 que se redistribuyen
        n_ce <- round(length(idx) * sum(fr_gc2$fr_x59, na.rm = TRUE))

        # Seleccionar aleatoriamente las posiciones que se redistribuyen
        idx <- sample(idx, n_ce)

        # Causas externas y sus probabilidades
        datos <- fr_gc2 |> filter(str_detect(causa, "CE:"))

        # Redistribución multinomial
        out[idx] <- rep(
          datos$causa,
          rmultinom(n = 1, size = n_ce, prob = datos$fr_x59)
        )
      }
      out
    }
  ) |>

  # --- Distribuir ~11% entre CMNN y ENT no objetivo ---
  mutate(
    gbd_paso3c = {
      out <- gbd_paso3c

      # Sexo y edad del grupo actual
      sexo_actual <- cur_group()$sexo
      edad_actual <- cur_group()$grupo_edad

      # Seleccionar GC redistribuibles
      idx <- which(
        gbd_paso3c == "GC:GC2" &
          between(cie10_cod, "X59.0", "X59.9")
      )

      if (length(idx) > 0) {
        # Obtener las frecuencias para CMNN y ENT no objetivo
        datos <- tab_fr |>
          filter(
            str_detect(causa, "CMNN|DIG|MUSC|NEU-MENT|OTR-ENT|PIEL|SUST"),
            grupo_edad == edad_actual
          )

        # Normalizar las probabilidades
        prob <- datos[[sexo_actual]] / sum(datos[[sexo_actual]])

        # Redistribución multinomial
        out[idx] <- rep(
          datos$causa,
          rmultinom(
            n = 1,
            size = length(idx),
            prob = prob
          )
        )
      }

      out
    },
    .by = c(sexo, grupo_edad)
  )


## Redistribuir Y34 ----
set.seed(123)
recod_defun <- recod_defun |>
  # --- Redistribuir ~89% entre CE Y ENT ---
  mutate(
    gbd_paso3d = {
      out <- gbd_paso3c

      idx <- which(
        gbd_paso3c == "GC:GC2" &
          between(cie10_cod, "Y34.0", "Y34.9")
      )

      if (length(idx) > 0) {
        # Número de códigos X59 que se redistribuyen
        n_ce <- round(length(idx) * sum(fr_gc2$fr_y34, na.rm = TRUE))

        # Seleccionar aleatoriamente las posiciones que se redistribuyen
        idx <- sample(idx, n_ce)

        # Causas externas y sus probabilidades
        # Redistribución multinomial
        out[idx] <- rep(
          fr_gc2$causa,
          rmultinom(n = 1, size = n_ce, prob = fr_gc2$fr_y34)
        )
      }
      out
    }
  ) |>

  # --- Distribuir ~11% entre CMNN y ENT no objetivo ---
  mutate(
    gbd_paso3d = {
      out <- gbd_paso3d

      # Sexo y edad del grupo actual
      sexo_actual <- cur_group()$sexo
      edad_actual <- cur_group()$grupo_edad

      # Seleccionar GC redistribuibles
      idx <- which(
        gbd_paso3c == "GC:GC2" &
          between(cie10_cod, "Y34.0", "Y34.9")
      )

      if (length(idx) > 0) {
        # Obtener las frecuencias para CMNN y ENT no objetivo
        datos <- tab_fr |>
          filter(
            str_detect(causa, "CMNN|DIG|MUSC|NEU-MENT|OTR-ENT|PIEL|SUST"),
            grupo_edad == edad_actual
          )

        # Normalizar las probabilidades
        prob <- datos[[sexo_actual]] / sum(datos[[sexo_actual]])

        # Redistribución multinomial
        out[idx] <- rep(
          datos$causa,
          rmultinom(
            n = 1,
            size = length(idx),
            prob = prob
          )
        )
      }

      out
    },
    .by = c(sexo, grupo_edad)
  )


## Redistribuir GC2 generales -----
recod_defun <- recod_defun |>
  mutate(
    gbd_paso3 = {
      out <- gbd_paso3d

      # Sexo y edad del grupo actual
      sexo_actual <- cur_group()$sexo
      edad_actual <- cur_group()$grupo_edad

      # Seleccionar GC redistribuibles
      idx <- which(gbd_paso3d == "GC:GC2")

      if (length(idx) > 0) {
        datos <- tab_fr |>
          filter(,
            grupo_edad == edad_actual
          )

        # Normalizar las probabilidades
        prob <- datos[[sexo_actual]] / sum(datos[[sexo_actual]])

        # Redistribución multinomial
        out[idx] <- rep(
          datos$causa,
          rmultinom(
            n = 1,
            size = length(idx),
            prob = prob
          )
        )
      }

      out
    },
    .by = c(sexo, grupo_edad)
  )


# Paso 4: Redistribuir GC1 -----------------------------------------------
## Reclasificar GC1 de CRD -----
recod_defun <- recod_defun |>
  mutate(
    gbd_paso4a = if_else(
      gbd_paso3 == "GC:GC1" & cie10_cod == "J96.1",
      "ENT:CRD",
      gbd_paso3
    )
  )


## Redistribuir GC1 de CE -----
set.seed(123)

recod_defun <- recod_defun |>
  mutate(
    gbd_paso4b = {
      out <- gbd_paso4a

      # Sexo y edad del grupo actual
      sexo_actual <- cur_group()$sexo
      edad_actual <- cur_group()$grupo_edad

      # Seleccionar GC redistribuibles
      idx <- which(
        gbd_paso4a == "GC:GC1" &
          between(cie10_cod, "X40.0", "Y19.9")
      )

      if (length(idx) > 0) {
        # Obtener las CE para esa edad
        datos <- tab_fr |>
          filter(
            str_detect(causa, "SH|VI|OTR-CE"),
            grupo_edad == edad_actual
          )

        # Normalizar las probabilidades según el sexo
        prob <- datos[[sexo_actual]] / sum(datos[[sexo_actual]])

        # Redistribución multinomial
        out[idx] <- rep(
          datos$causa,
          rmultinom(
            n = 1,
            size = length(idx),
            prob = prob
          )
        )
      }
      # Resultado final
      out
    },
    .by = c(sexo, grupo_edad)
  )


## Redistribuir GC1 generales -----
set.seed(123)

recod_defun <- recod_defun |>
  mutate(
    gbd_paso4 = {
      out <- gbd_paso4b

      # Sexo y edad del grupo actual
      sexo_actual <- cur_group()$sexo
      edad_actual <- cur_group()$grupo_edad

      # Seleccionar GC redistribuibles
      idx <- which(gbd_paso4b == "GC:GC1")

      if (length(idx) > 0) {
        # Obtener las CE para esa edad
        datos <- tab_fr |>
          filter(
            grupo_edad == edad_actual
          )

        # Normalizar las probabilidades según el sexo
        prob <- datos[[sexo_actual]] / sum(datos[[sexo_actual]])

        # Redistribución multinomial
        out[idx] <- rep(
          datos$causa,
          rmultinom(
            n = 1,
            size = length(idx),
            prob = prob
          )
        )
      }
      # Resultado final
      out
    },
    .by = c(sexo, grupo_edad)
  )


# Crear dataset para análisis GC -----------------------------------------
datos_gc <- recod_defun |>
  count(
    anio,
    region_deis,
    jurisd_deis,
    sexo,
    grupo_edad,
    cie10_cod,
    gbd_paso1,
    gbd_paso2a,
    gbd_paso2b,
    gbd_paso3,
    gbd_paso4
  ) |>

  # --- Variables caracter a factor ---
  mutate(
    across(.cols = where(is.character), .fns = ~ factor(.x))
  )


## Exportar datos -----
export(datos_gc, "clean/arg_recod_defun_gbd23.rds")


# Crear dataset para análisis EM -----------------------------------------
datos_em <- recod_defun |>
  # Filtrar fechas fuera de rango
  filter_out(anio == 2023 | mes == 0) |>

  # Separar en grupo causa y causa
  separate(gbd_paso4, into = c("grupo_causa", "causa"), sep = ":") |>

  # Agrupar datos
  count(
    anio,
    mes,
    region_deis,
    jurisd_deis,
    sexo,
    grupo_edad,
    grupo_causa,
    causa
  ) |>

  # --- Variables caracter a factor ---
  mutate(
    across(.cols = where(is.character), .fns = ~ factor(.x))
  )


## Exportar datos train -----
datos_em |>
  filter(anio < 2020) |>
  export("../EM_ENT_CE/clean/arg_datos_em_train.rds")


## Exportar datos pandemia -----
datos_em |>
  filter_out(anio < 2020) |>
  export("../EM_ENT_CE/clean/arg_datos_em_test.rds")
