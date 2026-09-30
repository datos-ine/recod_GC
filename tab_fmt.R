# Tema para tablas -------------------------------------------------------
tab_fmt <- function(x) {
  x |>
    bold(part = "header") |>
    font(fontname = "Times New Roman", part = "all") |>
    fontsize(size = 9, part = "all") |>
    line_spacing(space = 1.5, part = "all") |>
    align(align = "left", part = "all") |>
    merge_v(j = 1) |>
    merge_v(j = 2:3, combine = TRUE) |>
    (\(ft) {
      idx <- which(ft$body$dataset[[2]] != dplyr::lag(ft$body$dataset[[2]])) - 1
      idx <- idx[!is.na(idx) & idx > 0]
      hline(ft, i = idx)
    })()
}
