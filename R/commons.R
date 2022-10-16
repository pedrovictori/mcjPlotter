#' Load raw microCJ data and bind it into a single data frame
#'
#' @param directory the directory where the data is located
#'
#' @return a data frame with the integrated data
#' @export
loadAndBind = function(directory){
  files = list.files(directory, pattern = ".csv", full.names = FALSE)
  all = vector("list", length(files))
  for(file in files){
    all[[file]] = read_csv(paste0(directory, "/", file))
  }

  data = bind_rows(all, .id = 'filename')
  data
}

#' Plot standard microCJ data
#'
#' @param data microCJ data
#' @param timesteps how many steps in the data
#'
#' @return A ggplot2 object
#' @export
plotCounts = function(data, timesteps = 3000){
  p = data %>%
    ggplot(aes(x = timepoint, y = mean, colour = pop, fill = pop)) +
    geom_line() +
    geom_ribbon(aes(ymin = mean - sterr, ymax = mean + sterr), alpha = 0.2, linetype = 0) +
    xlim(0, timesteps) +
    ggpubr::theme_pubr(legend = "top") +
    labs(
      y = "Cell count", x = "Time step",
      colour = "Subpopulation", fill = "Subpopulation"
    )
  p
}
