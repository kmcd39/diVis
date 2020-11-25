

#' draw_point_hist
#'
#' returns a ggplot plot from prepped data
#' histogram. ~~~ moved to diVis
#' @param prepped_df Probably output from \code{prep_for_point_hist}
#' @param var.name column to get distribution along
#' @param label_interval Used with range of data to determine x-axis label placement.
#' @param ... Passed onto  \code{geom_point}
draw_point_hist <- function (prepped_df, var.name = "x", label_interval = 0.2, ...) {

  vv <- pull(prepped_df, !!var.name)

  mini <- min(vv, na.rm = T)
  maxi <- max(vv, na.rm = T)
  breaks <- seq(mini, maxi, by = ((maxi - mini) * label_interval))


    prepped_df %>%
      ggplot(aes(x = interval_numeric,
                 y = ypos,
                 color = color)) +
      geom_point(na.rm = T, size = 4.5, position = "identity",
                 shape = 16, ...) +
      scale_color_identity() +
      scale_x_continuous(name = rlang::sym(var.name),
                         breaks = breaks,
                         labels = appHelpers::q.format(breaks, digits = 2)) +
      appHelpers::app_base_theme +
      theme(axis.title.y = element_blank(),
                             axis.text.y = element_blank(), axis.ticks.y = element_blank())
}
