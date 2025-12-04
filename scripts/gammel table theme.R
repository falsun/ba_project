## --- 3. CREATE THE CUSTOM GT TABLE THEME ---
theme_gt_bachelor_project <- function(data) {
  numeric_cols <- data$`_data` %>%
    select(where(is.numeric)) %>%
    names()
  p_val_col <- intersect(numeric_cols, "p.value")
  other_numeric_cols <- setdiff(numeric_cols, p_val_col)

  data %>%
    fmt(
      columns = all_of(p_val_col),
      fns = function(x) {
        gtsummary::style_pvalue(x, digits = 3, decimal.mark = ",")
      }
    ) %>%
    fmt_number(
      columns = all_of(other_numeric_cols),
      decimals = 3,
      dec_mark = ",",
      sep_mark = "."
    ) %>%
    opt_table_font(
      font = "IBM Plex Serif",
      color = "black",
      add = FALSE
    ) %>%
    tab_options(
      table.font.size = "11pt",
      footnotes.marks = "letters"
    ) %>%
    tab_style(
      style = cell_text(weight = "bold"),
      locations = cells_column_labels(columns = everything())
    )
}
