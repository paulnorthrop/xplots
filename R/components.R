#' By-module scatter plots of component marks
#'
#' Create a PDF file containing, for each module, a pairs plot that compares
#' module component marks.
#'
#' @param input The directory containing the module mark Excel files.
#' @param output The directory in which the PDF file of plots is to be written.
#'   If this does not exist then it will be created.
#' @param skip An integer scalar. The number of rows to skip before reading
#'   data from the source marks file, that is, the number of rows before the
#'   row that contains the column names.
#' @param filename The name of the output PDF file.
#' @param zeros A logical scalar. Should marks of zero be included in the
#'   plots?
#' @param alt A character scalar that determines how marks for alternative
#'   assessments are plotted
#'
#'   * `"red"`: to highlight these marks against the black used for other
#'     marks,
#'   * `"black"`: to plot in black,
#'   * `"exclude"`: to exclude from the plots.
#'
#' @details A single PDF file is produced. Each page contains a matrix of
#'   scatterplots, produced using [`graphics::pairs`], in which the component
#'   marks for a module are plotted against each other. There is a page for
#'   each spreadsheet of module marks in the `input` directory. The title of a
#'   plot is the module code followed by the module delivery code, e.g.
#'   HPSC0003 A4U.
#'
#'   The relevant pass mark and the first/distinction mark of 70% are indicated
#'   by dotted lines on the plots. For postgraduate modules (A7P) the area
#'   where at least one mark is below the lower end of the condonable range
#'   (40%) is shaded grey and any component mark below 40% is plotted using
#'   a special symbol, a circle with a cross on it.
#'
#'   If an error is thrown when trying to produce a plot for a module then a
#'   warning message is printed and this module is skipped. If a module has
#'   only one assessment component then this module also skipped.
#'
#' **Notes**
#'
#' * The files `HPSC0003.xls` etc, which I know have been downloaded direct
#'   from Portico and advertised as Excel files, seem **not** to be Excel
#'   files, but rather formatted as webpages (`.html` or the equivalent). This
#'   meant that I could not read their contents into R. To solve this, I opened
#'   them in Excel and saved them as Excel Workbook (*.xlsx). These module mark
#'   Excel files, e.g., `HPSC0003.xlsx`, should be placed in the directory
#'   `marks`.
#' * It is assumed that a given `.xlsx` file does not mix students of Levels
#'   4-6 with students of Level 7. The level is inferred by searching for the
#'   Module Delivery entry in the `.xlsx` file. If there are modules taken by
#'   students of Level 6 and Level 7 then their marks should be in separate
#'   files, for example, `HPSC0003.xlsx` and `HPSC0003level7.xlsx`.
#' * The example file `HPSC0007.xlsx` contained marks for an alternative
#'   assessments, separated from the main set of marks. The code looks for this
#'   and merges these marks with the others, under the assumption that there is
#'   the same number of assessments as in the main set of marks and that they
#'   are in a comparable order. The argument `alt` determines how/whether these
#'   marks are plotted.
#' @return A named list containing the component marks for each module is
#'   returned invisibly. The names are the module codes.
#' @examples
#' \dontrun{
#' # The following code will produce a PDF file containing a page of plots for
#' # each spreadsheet of module marks in the input directory
#' components()
#' }
#'
#' @export
components <- function(input = "marks", output = "plots", skip = 9,
                       filename = "components.pdf", zeros = FALSE,
                       alt = c("red", "black", "exclude")) {

  # Check and set alt
  alt <- match.arg(alt)

  # --------------------------- Find mark files ----------------------------- #

  # Find all .xls or .xlsx files in the input directory
  filenames <- list.files(path = input, pattern = utils::glob2rx("*.xls*"))
  # Remove temporary filenames created by an open .xlsx file
  # These start with ~$
  first2 <- substring(filenames, 1, 2)
  filenames <- filenames[first2 != "~$"]
  # If no suitable files have been found then explain this, list all the files
  # in the directory and exit
  if (length(filenames) == 0) {
    cat("No suitable files found \n")
    allFilenames <- list.files(path = input)
    if (length(allFilenames) == 0) {
      cat("There are no files in ''input''. \n")
    } else {
      cat("The files in ''input'' are: \n")
      print(allFilenames)
    }
    stop()
  }

  # ------------------------------- Read data ------------------------------- #

  # Use the readxl package to read data from the Excel files
  # Use read_excel() rather than read_xls() or read_xlsx() so that either .xls
  # or .xlsx files can be used
  paths <- paste0(input, "/", filenames)
  nModules <- length(filenames)
  # Empty lists in which to store the module marks, levels, module delivery
  # codes and row numbers of alternative assessment marks
  componentMarks <- list()
  levels <- list()
  moduleDeliveryCodes <- list()
  altAssRows <- list()
  # For each module
  for (i in 1:nModules) {
    # 1. Read the data, skipping lines 1 to skip
    temp <- readxl::read_excel(path = paths[i], skip = skip)
    # 2. Convert the tibble to a data frame
    temp <- as.data.frame(temp)
    # 3. Find where the post-mark metadata starts
    chop <- which(temp$`#` == "Key to Codes")
    # 4. If there are marks for "Alternative assessment" then include these
    #    whereAltAss stores the row number for any alternative assessment marks
    altAss <- grep("Alternative assessment", temp$`#`)
    if (length(altAss) > 0) {
      rowsToInclude <- c(1:(min(altAss) - 3), (max(altAss) + 2):(chop - 2))
      firstAlt <- min(altAss) - 2
      nAlt <- length((max(altAss) + 2):(chop - 2))
      altAssRows[[i]] <- firstAlt:(firstAlt + nAlt - 1)
    } else {
      rowsToInclude <- 1:(chop - 2)
      altAssRows[[i]] <- NA
    }
    # 5. Include only marks, no metadata
    temp <- temp[rowsToInclude,]
    # 6. Extract only the component marks
    marksOnly <- grep("Mark", names(temp))
    temp <- temp[, marksOnly]
    overallMark <- grep("Overall Mark", names(temp))
    temp <- temp[, -overallMark, drop = FALSE]
    # read_excel() sometimes gets confused and makes numbers characters
    temp <- apply(temp, 2, as.numeric)
    colnames(temp) <- substring(colnames(temp), first = 1, last = 2)
    # 7. Store these marks in the list moduleMarks
    componentMarks[[i]] <- temp
    # 8. Also read the Module Delivery in the premable to determine UGT vs PGT
    temp <- readxl::read_excel(path = paths[i], n_max = skip,
                               .name_repair = "minimal")
    temp <- as.data.frame(temp)
    moduleDeliveryRow <- grep("Module Delivery", temp[, 1])
    moduleDelivery <- substring(temp[moduleDeliveryRow, 2], 1, 3)
    moduleDeliveryCodes[[i]] <- moduleDelivery
    levels[[i]] <- as.numeric(substring(moduleDelivery, 2, 2))
  }
  # Name the elements of the list using the module codes
  names(componentMarks) <- substring(filenames, first = 1, last = 8)

  # --------------------------- Create the plots ---------------------------- #

  # If the output directory does not exist then create it
  if (!dir.exists(output)) {
    dir.create(output)
  }

  # Create the output filename
  pdfname <- paste0(output, "/", filename)
  grDevices::pdf(file = pdfname, width = 8, height = 11)
  # Function for handling errors when plotting
  plotError <- function(e) {
    cat("No plot for", plotTitle, "owing to an error \n")
  }
  # Add a plot for each module to the output PDF file
  for (i in 1:nModules) {
    level <- levels[[i]]
    moduleDeliveryCode <- moduleDeliveryCodes[[i]]
    print(level)
    plotTitle <- paste(names(componentMarks)[i], moduleDeliveryCode)
    if (moduleDeliveryCode == "A7P") {
      tryCatch(
        plotPG(marks = componentMarks[[i]], plotTitle = plotTitle,
               zeros = zeros, altAssRows = altAssRows[[i]], alt = alt),
        error = plotError)
    } else {
      tryCatch(
        plotUG(marks = componentMarks[[i]], plotTitle = plotTitle,
               zeros = zeros, altAssRows = altAssRows[[i]], alt = alt),
        error = plotError)
    }
  }
  # Shut down the graphics device
  grDevices::dev.off()

  return(invisible(componentMarks))
}
