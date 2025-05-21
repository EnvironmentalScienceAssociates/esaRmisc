#' Split Egnyte path into parts
#'
#' Split a file path copied from Windows File Explorer into parts for pasting into call to file.path().
#' Needs to be read from clipboard to escape the path separators.
#'
#' @export
#'

split_egnyte_path <- function(){
  out = readClipboard()
  path_pieces = strsplit(out, "\\\\")[[1]]
  pp_dq = sapply(path_pieces, function(x) paste0("\"", x, "\""),
                 simplify = TRUE, USE.NAMES = FALSE)
  cat(pp_dq, sep = ", ")
}
