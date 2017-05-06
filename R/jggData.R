
require(tidyr,quietly=T, warn.conflicts=F)
require(dplyr,quietly=T, warn.conflicts=F)
require(data.table,quietly=T, warn.conflicts=F)
require(stringi, quietly=T, warn.conflicts=F)
require(lubridate, quietly=T, warn.conflicts=F)

#' @title       eq_load_data
#' @description dowload and clean the earthquakes file from NOAA
#'
#' @param filename Path to earthquakes file if already downloaded
#'                 when missing the file is automatically downloaded
#'
#' @return The dataframe generated
#'         Error if file doesn't exist
#'
#' @export
eq_load_data <- function(filename=NULL) {
  dt <- eq_get_data(filename)
  dt <- eq_clean_data(dt)
}

#' @title       eq_get_data
#' @description load the file
#'
#' @param filename Path to earthquakes file if already downloaded
#'                 when missing the file is automatically downloaded
#'
#' @return The dataframe generated
#'         Error if file doesn't exist
#'
#' @export
eq_get_data <- function(filename) {
  if (is.null(filename)) {
    filename = "signif.txt"
    fileUrl = "https://www.ngdc.noaa.gov/nndc/struts/results?type_0=Exact&query_0=$ID&t=101650&s=13&d=189&dfn=signif.txt"
    download.file(fileUrl, destfile=filename, method="curl", quiet=T)
  }
  fread(filename)
}

#' @title       eq_clean_data
#' @description Clean and prepare the dataframe for future uses
#'
#' @param df Data frame
#'
#' @return The dataframe/datatable cleaned
#'
#' @export
eq_clean_data <- function(df) {
  message("loading data ...")

  dt <- df %>%
    select(-starts_with("TOTAL"), -contains("DESCRIPTION"), -contains("HOUSES")) %>%     # Remove some columns
    drop_na(YEAR, MONTH, DAY, EQ_PRIMARY) %>%                                            # Remove NA values
    subset(YEAR > 0) %>%                                                                 # Remove negative years
    mutate(LOCATION_NAME = eq_location_clean(LOCATION_NAME, 2),
           DATE = ymd(sprintf("%04d/%02d/&02d",YEAR, MONTH, DAY)),
           INTENSITY = EQ_PRIMARY
           )                                                    %>%                      # Split location name
    select(-I_D, -YEAR, -MONTH, -DAY, -HOUR, -MINUTE, -SECOND)  %>%                      # Still removing columns
    select(-starts_with("F"), -STATE)                           %>%                      # Still removing columns
    select(-starts_with("EQ_"), -contains("HOUSES"))

  # Set data types
  dt$LATITUDE = as.numeric(dt$LATITUDE)
  dt$LONGITUDE = as.numeric(dt$LONGITUDE)

  return (dt)
}

eq_location_clean <- function(text, pos) {
  x = strsplit(text, ":")
  return (stri_trans_general(x[[1]][pos], id="Title"))
}


# Bueno
# ggplot(dt3, aes(x=DATE, y=0)) + geom_point2(shape=21, aes(size=RITCHER, fill=DEATHS), show.legend=T)
# ggplot(dt3) + geom_point(aes(RITCHER,0), shape=21, fill=dt3$DEATHS, size=as.numeric(cut(dt3$RITCHER, c(0:9), labels=c(1:9))))
