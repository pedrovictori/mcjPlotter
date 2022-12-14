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
    all[[tp]] = read.csv(paste0(prefix, tp, suffix)) %>%
      mutate(across(!c(subpopulation, cell_id, any_of("state")), as.numeric)) %>%
      mutate(tp = tp)
  }

  bind_rows(all, .id = "t") %>% mutate(t = as.numeric(t))
}


#' Plot timepoint data
#'
#' @param data The data to be plotted
#' @param by The name of the column represented in the cell color
#'
#' @return A ggplot2 object
#' @export
plotCellData = function(data, by, r = 10, discr = F, directory = "",
                        filename = "cellPlot",
                        width = 10, height = 10, viridisOption = "D", facet = T) {
  p = data %>% ggplot() +
    ggforce::geom_circle(aes(x0 = i, y0 = j, r = r,col= {{by}}, fill = {{by}}),
                         alpha = 0.8) +
    viridis::scale_fill_viridis(discrete = discr, option = viridisOption) +
    viridis::scale_color_viridis(discrete = discr, option = viridisOption) +
    ggpubr::theme_pubr() +
    theme(
      axis.text = element_blank(), axis.ticks = element_blank(),
      axis.title = element_blank(), axis.line = element_blank()
    )

  if(facet){
    p = p + facet_wrap(vars(tp))
  }

  if (!file.exists(directory)) {
    dir.create(directory)
  }

  ggsave(paste0(directory, filename, ".png"),
         p, width = width, height = height)
  p
}
