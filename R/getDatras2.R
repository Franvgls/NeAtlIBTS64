#' Function getDatras2 modification of function getDATRAS to give the possibility of not getting the list of files downloaded
#' @param Survey: either the Survey to be downloaded from DATRAS (see details), or a data frame with the HH information with  the DATRAS HH format  and the years and quarter selected in years and quarter 
#' @param years: years to be downloaded and used, had to be available in DATRAS. The time series will be ploted in grey dots, last year in yellow, it depends on the order of years, not the actual chronological year.
#' @param quarter: the quarter of the survey to be ploted
#' @details Surveys available in DATRAS: i.e. SWC-IBTS, ROCKALL, NIGFS, IE-IGFS, SP-PORC, FR-CGFS, EVHOE, SP-NORTH, PT-IBTS and SP-ARSA
#' @return Downloads from DATRAS the HH files from the survey and the years selected.
#' @examples getDatras2("HH","SP-NORTH",c(2014:2016),4,listData=FALSE)
#' @export
getDatras2<-function (record = "HH", survey, years, quarters,listData=FALSE) {
    # check record type
    if (!record %in% c("HH", "HL", "CA")) {
      message("Please specify record type:",
              "\n\t\tHH (haul data)",
              "\n\t\tHL (length-based data)",
              "\n\t\tCA (age-based data)")
      return(FALSE)
    }
    
    # check survey name
    if (!checkSurveyOK(survey)) return(FALSE)
    
    # cross check available years with those requested
    available_years <- getSurveyYearList(survey)
    available_years_req <- intersect(years, available_years)
    if (length(available_years_req) == 0) {
      # all years are unavailable
      message("Supplied years (", paste(years, collapse = ", "), ") are not available.\n  Available options are:\n",
              paste(capture.output(print(available_years)), collapse = "\n"))
      return(FALSE)
    } else if (length(available_years_req) < length(years)) {
      # some years are unavailable
      message("Some supplied years (", paste(setdiff(years, available_years), collapse = ", "),
              ") are not available.")
    }
    
    # get matrix of available data for years and quarters requested
    mat <- sapply(as.character(available_years_req),
                  function(y) getSurveyYearQuarterList(survey, as.integer(y)),
                  simplify = FALSE)
    mat <- sapply(mat, function(x) as.integer(1:4 %in% x)) # hard wire 4 quarters
    row.names(mat) <- 1:4
    
    if (sum(mat[quarters,]) == 0) {
      # all quarters are unavailable
      message("Supplied quarters (", paste(quarters, collapse = ", "), ") are not available.\n  Available options are:\n",
              paste(capture.output(print(mat)), collapse = "\n"))
      return(FALSE)
    } else if (sum(mat[quarters,] == 0) > 0) {
      # some quarters are unavailable
      message("Some supplied quarter and year combinations are not available.")
    }
    
    # work out year and quarter combinations to extract
    amat <- mat[quarters,,drop = FALSE]
    qvec <- quarters[row(amat)[amat == 1]]
    yvec <- available_years_req[col(amat)[amat == 1]]
    
    # report to user which years and quarters are being extracted?
  if (listData) { message("Data being extracted for:\n",
            paste(capture.output(print(cbind.data.frame(survey = survey, year = yvec, quarter = qvec))), collapse = "\n"))}
    
    # create list of web service URLs
    url <-
      sprintf(
        "https://datras.ices.dk/WebServices/DATRASWebService.asmx/get%sdata?survey=%s&year=%i&quarter=%i",
        record, survey, yvec, qvec)
    
    # read XML string and parse to data frame
    out <- lapply(url,
                  function(x) {
                    x <- readDatras(x)
                    parseDatras(x)
                  })
    out <- do.call(rbind, out)
    
    out
}


readDatras <- function(url) {
  # try downloading first:
  # create file name
  tmp <- tempfile()
  on.exit(unlink(tmp))
  
  # download file
  ret <-
      download.file(url, destfile = tmp, quiet = TRUE)
    
  # check return value
  if (ret == 0) {
    # scan lines
    scan(tmp, what = "", sep = "\n", quiet = TRUE)
  } else {
    message("Unable to download file so using slower method url().\n",
            "Try setting an appropriate value via\n\t",
            "options(download.file.method = ...)\n",
            "see ?download.file for more information.")
    # connect to url
    con <- url(url)
    on.exit(close(con))
    
    # scan lines
    scan(con, what = "", sep = "\n", quiet = TRUE)
  }
}



parseDatras <- function(x) {
  # parse using line and column separators
  type <- gsub(" *<ArrayOf(.*?) .*", "\\1", x[2])
  
  # convert any lazy teminated feilds to full feilds
  x <- gsub("^ *<(.*?) />$", "<\\1> NA </\\1>", x)
  starts <- grep(paste0("<", type, ">"), x)
  ends <- grep(paste0("</", type, ">"), x)
  ncol <- unique(ends[1] - starts[1]) - 1
  # drop everything we don't need
  x <- x[-c(1, 2, starts, ends, length(x))]
  
  # exit if no data is being returned
  if (length(x) == 0) return(NULL)
  
  # match content of first <tag>
  names_x <- gsub(" *<(.*?)>.*", "\\1", x[1:ncol])
  
  # delete all <tags>
  x <- gsub(" *<.*?>", "", x)
  # trim white space
  x <- trimws(x)
  
  # convert to data frame
  dim(x) <- c(ncol, length(x)/ncol)
  row.names(x) <- names_x
  x <- as.data.frame(t(x), stringsAsFactors = FALSE)
  
  # return data frame now if empty
  if (nrow(x) == 0) return(x)
  
  # DATRAS uses -9 and "" to indicate NA
  x[x == -9] <- NA
  x[x == ""] <- NA
  
  # simplify all columns except StatRec and AreaCode (so "45e6" does not become 45000000)
  x[!names(x) %in% c("StatRec", "AreaCode", "Ship")] <- simplify(x[!names(x) %in% c("StatRec", "AreaCode", "Ship")])
  
  x
}



