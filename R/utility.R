#' Create Species x Site matrix
#'
#' Creates a Species x Site abundance or occurrence matrix from a dataframe
#'
#' @param data A data.frame containing species and site info
#' @param cols A vector of the form c("SPECIESNAME_COLUMN", "SITENAME_COLUMN") or
#' c("SPECIESNAME_COLUMN", "SITENAME_COLUMN", "ABUNDANCE_COLUMN")
#'
#' @details
#'
#' Creates a Species x Site matrix from data tabulated in a csv
#' (i.e. like DarwinCore download from GBIF). If three column names are
#' specified in the cols argument, then sxsmat() creates an abundance
#' matrix where cell values are taken from the inputed column name
#' "ABUNDANCE_COLUMN". If only two columns are specified sxsmat()
#' creates a presence absence matrix
#'
#' @return An object of class "matrix"
#'
#' @export

sxsmat <- function(data, cols) {
  if(length(cols) == 2 | length(cols) == 3 ) {
    spec <- as.character(data[[cols[1]]])
    site <- as.character(data[[cols[2]]])

    uspec <- sort(unique(spec))
    usite <- sort(unique(site))

    outmat <- matrix(nrow = length(usite), ncol = length(uspec))
    colnames(outmat) <- uspec
    rownames(outmat) <- usite

    for(i in 1:length(data[,1])) {
      curspec <- data[i,cols[1]]
      cursite <- data[i,cols[2]]
      if(length(cols) == 2) {
        outmat[cursite, curspec] <- 1
      } else if(length(cols) == 3) {
        curcount <- data[i,cols[3]]
        outmat[cursite, curspec] <- curcount
      }
    }

    outmat[is.na(outmat)] <- 0
    return(outmat)

  } else {
    stop("Too many or too few columns specified")
  }
}


#' Merge columns from multiple dataframes
#'
#' This function uses a common column (i.e. key in relational databases) to
#' add columns from one data frame to another.
#'
#' @param data1 A data.frame, the data frame to which new columns will be added
#' @param data2 A data.frame, the data frame from which columns will be extracted
#' @param key Character vector with names of the columns which contain the keys
#' @param selection Character vector with names of columns which will be extracted
#' from data2
#' @param selection2 Optional character vector with names of columns from data1
#' that want to be kept, all others will be removed
#' @param output Logical, should a warning message be output if key matches multiple entries
#'
#' @details This function takes two data.frames and copies the entries from one data.frame
#' to another as matched by one or multiple "key" columns found in both data.frames. This
#' function can be used to combine the relevant data from multiple tables from a relational
#' database. NOTE: if a key matches two entries in data2, the data from the first entry
#' is used and a warning message is output.
#'
#' @return A new data.frame object
#'
#' @export

dbmerge <- function(data1, data2, key, selection, selection2 = NULL, output = FALSE) {
  if(is.null(selection2)) { # Checks to see if user wants to cut some columns from data1
    data1[,selection] <- NA # Initialize new columns
  } else { # If yes...
    allcols <- c(key,selection2) # include key columns in desired output
    #print(allcols)
    data1 <- data1[,allcols] # cut data1 down to desired columns
    data1[,selection] <- NA # Initialize new columns
  }

  # Make all factors character strings so text is moved not factor indices
  data1[,] <- lapply(data1, function(x) if(is.factor(x)) as.character(x) else x)
  data2[,] <- lapply(data2, function(x) if(is.factor(x)) as.character(x) else x)

  for(i in 1:length(data1[,1])) { # Loop through each entry in data1
    keyval <- data1[i,key] # Extract the value of the key(s) for that entry
    #cat("keyval =\n")
    #print(keyval, quote = F)
    keyrow <- data2 # Create a copy of data2 for subsetting
    if(length(keyval) > 1) { # If multiple keys
      for(j in 1:length(key)) { # Loop through each key and subset the data
        keyrow <- keyrow[which(keyrow[,key[j]] == keyval[1,j]),]
        # The above line says subset keyrow (data2) finding the rows where
        # the values in the key column line up with the key value
      }
    } else { # If only one key...
      keyrow <- keyrow[which(keyrow[,key] == keyval),]
    }
    #cat("keyrow =\n")
    #print(keyrow, quote = F)
    if(nrow(keyrow) > 1) { # Check to see if key value extracted multiple entries
      if(output == T) {
        print(keyrow)
      }
      warning("Key returned multiple entries, try adding key or use a different key")
    }
    #print(keyrow)

    data1[i,selection] <- keyrow[1,selection]
  }
  data1
}
