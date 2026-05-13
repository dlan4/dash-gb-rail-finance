
# Total income and expenditure over time, operator = CSleeper
data = finance_data_cleaned
datacols = c("Total income", "Total expenditure")
idcol = "date"
filters = exprs(operator == "Caledonian Sleeper")

t1 <- \(value) value / `Passenger km`
t2 <- \(value) value * 100 / 90
t3 <- compose(t1, t2, .dir = "forward")
testenv <- new.env(parent = globalenv())
testenv$value <- 20
testenv$`Passenger km` <- 100
# doesn't work, because t3 is defined in the global env and not testenv
# but it's fine, because tidyverse would specifically enclose the function in
# the data mask
do.call(t3, list(value = 20), envir = testenv)
eval(expr(`Passenger km` + value), envir= testenv)
not_null <- compose(`!`, is.null)
not_null(4)

create_data <- function(data, datacols, idcols, filters, transformations) {
  datacols_s <- syms(datacols)
  idcols_s <- syms(idcols)
  data %>%
    filter(!!!filters) %>%
    mutate(across(any_of(datacols), transformations)) %>%
    select(!!!idcols_s, any_of(datacols) ) %>%
    pivot_longer(cols = all_of(datacols), names_to = ".id", values_to = ".value")
}

type = "table"
data = create_data(finance_data_cleaned, datacols, idcol, filters)
# mapping for table
mapping = list(
  x = ".id",
  y = "date",
  value = ".value"
)
# mapping for line
mapping = list(
  x = "date",
  y = ".value",
  group = ".id"
)
attrs = list(

)
create_data(finance_data_cleaned, idcols = "operator", datacols = "Fare income",
            filters = exprs(lubridate::year(date) == 2025))

create_visual <- function(data, type = c("line", "bar", "table"), mapping, attrs) {
  struc <- list(data = data, mapping = mapping, attrs = attrs) %>%
    structure(., class = c(sprintf("visual_%s", type), "visual") )
  visual_engine(struc)
}
print.visual <- function(x, ...) {
  mapping_p <- imap_chr(struc$mapping, \(n,name) paste(name,"=",n) ) %>%
    paste(collapse=", ")
  cat(sep="","<",attr(x,"class")[1],">\n",
      "Mapping: ",mapping_p,"\n")
  print(x$data)
}

as_plotly_formula <- function(x) {
  as.formula( paste0("~`",x,"`") )
}

visual_engine <- function(struc) {
  UseMethod("visual_engine")
}
# mapping requires `x`, `y`, `value`
visual_engine.visual_table <- function(struc) {
  s_mapping <- struc$mapping
  plot_data <- struc$data %>%
    pivot_wider(names_from = s_mapping$x, values_from = s_mapping$value)
  plot_ly(plot_data, type = "table",
          header = list(
            values = names(plot_data),
            line = list(color = "darkgreen")
          ),cells = list(
            values = inject(rbind( !!!as.list(plot_data) ))
          )
  )
}
# mapping requires `x`, `y`,
# optional `group`
visual_engine.visual_line <- function(struc) {
  s_mapping <- struc$mapping %>%
    map(as_plotly_formula)
  names(s_mapping)[names(s_mapping) == "group"] <- "color"
  plot_data <- struc$data
  args <- c( type = "scatter", mode = "lines", list(data = plot_data),
             s_mapping,
             struc$attrs)
  do.call(plotly::plot_ly, args)
}
