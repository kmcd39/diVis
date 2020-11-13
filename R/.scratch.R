
bin.var_format_T <- function (x, ...)  {
  # browser()
  breaks <- get_breaks(x, ...)
  bin_from_breaks(x, breaks, ...)
}

unlist(gui_test)
bin.var_format_T(x, (gui_test))
do.call('bin.var_format_T',
        c(list(x), gui_test))
