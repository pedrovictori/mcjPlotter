#' Load timepoint data
#'
#' @param timepoints A vector with the timepoints involved
#' @param dir The directory from which to load the data
#' @param mode The type of timepoint stat
#' @param suffix The suffix of the filename, such as "_1.csv"
#'
#' @return A dataframe with the timepoint data
#' @export
loadTimePointData = function(timepoints, dir, mode, suffix) {
  all = vector("list", length(timepoints))
  prefix = paste0(dir, "/", mode, "_t")

  for (tp in timepoints) {
    data = readr::read_csv(paste0(prefix, tp, suffix)) %>%
      stats::na.omit()

    all[[tp]] = data
  }

  bind_rows(all, .id = "t")
}


#' Plot timepoint data
#'
#' @param data The data to be plotted
#' @param by The name of the column represented in the cell color
#'
#' @return A ggplot2 object
#' @export
plotCellData = function(data, by) {
  p = data %>% ggplot() +
    ggforce::geom_circle(aes(x0 = i, y0 = j, r = r, fill = {{ by }}),
      color = "black"
    ) +
    facet_wrap(vars(t), labeller = label_both) +
    viridis::scale_fill_viridis(discrete = T) +
    ggpubr::theme_pubr() +
    theme(
      axis.text = element_blank(), axis.ticks = element_blank(),
      axis.title = element_blank(), axis.line = element_blank()
    )
  p
}
