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
