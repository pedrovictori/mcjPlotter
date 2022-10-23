#' Process node data
#'
#' @param data the data to be processed
#' @param filename the file name to save processed data.
#'
#' @return the processed data
#' @export
processNodeData = function(data, filename){
  proc = data %>% na.omit() %>%
    select(-c(Apoptosis, DNA_damage, EGFR_stimulus, Growth_Arrest, Necrosis,
              Proliferation, TGFBR_stimulus, FGFR3_stimulus, cell_id)) %>%
    mutate(t = as.numeric(sub(".+by\\_cell\\_t(\\d+).+\\.csv", "\\1", filename)),
           .keep = "unused") %>%
    group_by(subpopulation, t) %>%
    summarise(across(.cols = everything(), mean)) %>%
    pivot_longer(!c(subpopulation,t),
                 names_to = "node", values_to = "status")

  write_csv(proc, filename)
  proc
}

#' Plot node data
#'
#' @param data processed data
#' @param timepoints time points to include?
#' @param filename file name to save the plot
#' @param width width of the plot
#' @param height height of the plot
#' @param title title of the plot
#' @param subtitle subtitle of the plot
#' @return the plot
#' @export
plotNodes = function(data, timepoints = c(), filename, width = 18, height = 10,
                     title = "Node status, average of all cells in each subpopulation",
                     subtitle = "100 replicates per time point"){
  p = data %>% filter(t %in% timepoints) %>%
    ggplot(aes(x=node, y=subpopulation)) +
    geom_tile(aes(fill=status)) +
    scale_fill_paletteer_c(`"ggthemes::Temperature Diverging"`, direction = -1) +
    facet_grid(rows = vars(t)) +
    theme_pubr() +
    theme(axis.text.x = element_text(angle=-45, vjust = 1, hjust = 0)) +
    ggtitle(title,
            subtitle = subtitle)
  p
  ggsave(filename = filename,
         p, width = width, height = height, dpi = 400)
}
