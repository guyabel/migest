#' Calculate Counterstream and Stream Net Migration
#'
#' @param m A \code{matrix} or data frame of origin-destination flows. For \code{matrix} the first and second dimensions correspond to origin and destination respectively. For a data frame the first and second columns correspond to origin and destination respecivly. The third column contains the number of migraitons or migrants.
#'
#' @return A \code{tibble} with columns for orig, destination, corridor, stream, countersteam and net stream in each pair (corridor)
#' @export
net_stream <- function(m){
  if(!is.matrix(m)){
    d <- m %>%
      dplyr::rename(orig = 1, 
                    dest = 2, 
                    stream = 3)
  }
  if(is.matrix(m)){
    d <- as.data.frame.table(x = m, responseName = "stream", stringsAsFactors = FALSE) %>%
      dplyr::rename(orig = 1, 
                    dest = 2) 
  }
  d %>%
    dplyr::as_tibble() %>%
    dplyr::filter(orig != dest) %>%
    dplyr::mutate(corridor = ifelse(orig < dest, paste(orig, dest, sep = " - "), paste(dest, orig, sep = " - "))) %>%
    dplyr::group_by(corridor) %>%
    dplyr::mutate(counterstream = rev(stream),
                  stream_net = stream - counterstream) %>%
    dplyr::ungroup() %>%
    dplyr::select(orig, dest, corridor, dplyr::everything())
}
