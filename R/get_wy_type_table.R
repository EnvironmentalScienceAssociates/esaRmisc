#' Water year classifications
#'
#' Get table of Water Year Hydrologic Classification Indices from [CDEC](https://cdec.water.ca.gov/reportapp/javareports?name=WSIHIST).
#'
#' @md
#' @seealso [get_water_year_type], [water_year_type]
#' @export
#'

get_wy_type_table <- function(){
  raw <- data.frame(lines = readLines(con = 'https://cdec.water.ca.gov/reportapp/javareports?name=WSIHIST'))
  start = (which(grepl("------", raw$lines))[1] + 5)
  end = as.numeric(format(Sys.Date(), "%Y")) - 1901

  page <- read.delim(file = 'https://cdec.water.ca.gov/reportapp/javareports?name=WSIHIST',
                     skip = start,
                     nrows = end,
                     sep = "",
                     header = FALSE)

  cn <- c("WaterYear", "OctMar", "AprJul", "WYSum", "Index", "WYT")

  sac <- page[, 1:6]
  colnames(sac) <- cn
  sac$Valley = "SAC"

  sjr <- page[, c(1, 7:11)]
  colnames(sjr) <- cn
  sjr$Valley <- "SJR"

  type_table <- merge(rbind(sac, sjr),
                      data.frame(WYT = c("W", "AN", "BN", "D", "C"),
                                 WaterYearType = c("Wet", "Above Normal", "Below Normal",
                                                   "Dry", "Critical")))

  type_table <- type_table[order(type_table$Valley, type_table$WaterYear),]
  row.names(type_table) <- NULL # removing row names because potentially confusing with order step
  type_table
}
