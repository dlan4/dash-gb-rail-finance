
finance_data <- data_loc %>%
  file.path(".") %>%
  list.files(full.names = TRUE) %>%
  map( \(filepath) {
    sheets = list_ods_sheets(filepath)
    sheet_data = read_ods(filepath, sheet = length(sheets), col_names = FALSE)
    # determined by amount of metadata space at top
    sheet_data[-c(1:5), ]
    } ) %>%
  list_rbind

lookup <- function(val, vec1, vec2) {
  ind <- match(val, vec1)
  return(vec2[ind])
}
options(scipen = 40)

finance_data_colnames <- finance_data[1, ] %>% as.character %>%
  str_remove("\\n")
finance_data_yearcodes <- finance_data_colnames %>%
  str_extract("Mar (\\d{4})", group = 1) %>%
  set_names(finance_data_colnames) %>%
  .[!is.na(.)]
# mapping to align franchised and non-franchised data categories, in form new = old
finance_data_replacements <- c(
  "Total income"= "Operating income (a) [note 5]",
  "Total expenditure"= "Operating expenditure (b)",
  "Diesel fuel" = "Diesel fuel [note 1]",
  "Rolling stock" = "Total rolling stock expenditure",
  "Income less expenditure" = "Total income less expenditure (a − b − c)",

  "Net subsidy" = "Net franchise subsidy (d) [note 5]"
)

finance_data_cleaned <- finance_data %>%
  rename_with(~finance_data_colnames) %>%
  filter(!str_detect(Franchise, "^(Train operator|Franchise|Table 72)") &
         !is.na(Franchise)) %>%
  mutate(.keep = "unused", .before = 1,
         operator = str_remove_all(Franchise, "\\s*\\[[^]]+\\]$"),
         data_id = `Income and expenditure categories` ) %>%
  pivot_longer(-c(1:2), names_to = "date", values_to = "value") %>%
  mutate(date = lookup(date, names(finance_data_yearcodes), finance_data_yearcodes ),
         date = ymd( paste0(date, "0331")) ) %>%
  mutate(value = as.numeric(value)) %>% suppressWarnings %>%
  mutate(data_id = coalesce( lookup(data_id, finance_data_replacements, names(finance_data_replacements)),
                             data_id ) ) %>%
  pivot_wider(names_from = "data_id", values_from = "value") %>%
  # un-franchised table doesn't break down other expenditure
  mutate(`Other expenditure (including access charges)` = coalesce(
    `Other expenditure (including access charges)`,
    `Access charges [r]` + `Other operating expenditure [r] [note 1] [note 6, 7]`
  ), .keep = "unused")

col_types <- tibble(
  col = names(finance_data_cleaned)
) %>% mutate(
  type = case_when(col %in% c("operator", "date") ~ "id",
                   TRUE ~ "data")
)
hierarchy <- tribble(~higher, ~lower,
                     "Total income", c("Fare income", "Other operator income"),
                     "Total expenditure", c("Rolling stock", "Diesel fuel", "Staff"),
                     "Rolling stock", c("Rolling stock leasing",
                                           "Rolling stock maintenance",
                                           "Rolling stock other"),
                     "Operating income less expenditure (a − b)", c("Total expenditure",
                                                                    "Total income")
) %>% unnest(cols = lower)
