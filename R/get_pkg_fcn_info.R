#' Get a table of info for a package's functions
#'
#' Create a tibble of information related to each
#' function available in a package.
#' @param pkg_location the path to the root
#' directory of a package.
#' @importFrom stringr str_detect str_replace str_replace_all
#' @importFrom stringr str_split_fixed fixed
#' @importFrom dplyr tibble mutate pull inner_join group_by
#' @importFrom purrr map_df
#' @importFrom tidyr nest
#' @export
get_pkg_fcn_info <- function(pkg_location) {

  # Get the working directory
  present_wd <- getwd()

  # Temporarily change the working directory
  # to the package location
  setwd(dir = pkg_location)

  # Get a list of all .R files in the `./R` folder
  r_files <- list.files(path = "./R", full.names = TRUE)

  # Detect those lines from NAMESPACE where a
  # function is exported
  exported_fcn_lines <-
    "./NAMESPACE" %>% readLines() %>%
    stringr::str_detect(pattern = "^export\\(.*")

  exported_fcns <-
    (("./NAMESPACE" %>% readLines())[exported_fcn_lines]) %>%
    stringr::str_replace_all(
      pattern = "(^export|\\(|\\))",
      replacement = "")

  # Get the function reference table
  fcn_info_tbl <-
    seq(r_files) %>%
    purrr::map_df(.f = function(x) {

      # Get lines from each .R file
      file_lines <- r_files[x] %>% readLines()

      # Detect those lines where a function begins
      function_def_lines <-
        file_lines %>%
        stringr::str_detect(pattern = "^[a-zA-Z0-9_\\.].* <- function\\(")

      # Detect those lines where a function ends
      function_end_lines <-
        file_lines %>%
        stringr::str_detect(pattern = "^\\}")

      # Get the beginning and ending line numbers for
      # each function
      line_numbers_start <- which(function_def_lines)
      line_numbers_end <- which(function_end_lines)

      (file_lines[function_def_lines] %>%
          stringr::str_split_fixed(pattern = " ", 2))[, 1] %>%
        dplyr::tibble(
          fcn_name = .,
          r_file = r_files[x]) %>%
        dplyr::mutate(r_file = stringr::str_replace(
          string = r_file,
          pattern = stringr::fixed("./R/"),
          replacement = "")) %>%
        dplyr::mutate(line_start = line_numbers_start) %>%
        dplyr::mutate(line_end = line_numbers_end) %>%
        dplyr::mutate(lines = (line_end - line_start + 1) %>% as.integer()) %>%
        dplyr::mutate(exported = ifelse(
          fcn_name %in% exported_fcns, TRUE, FALSE))
    })

  # Get a vector of all package functions
  all_fcns <- fcn_info_tbl %>% pull(fcn_name)

  fcn_info_tbl <-
    fcn_info_tbl %>%
    dplyr::mutate(
      n_pkg_fcns_called = NA_integer_,
      names_fcns_called = NA_character_)


  fcn_info_tbl <-
    seq(all_fcns) %>%
    purrr::map_df(.f = function(x) {

      tbl_row <- fcn_info_tbl[x, ]

      fcn_lines <-
        readLines(paste0("./R/", fcn_info_tbl[x, 2]))[
          seq((fcn_info_tbl[x, 3] %>% pull()), (fcn_info_tbl[x, 4] %>% dplyr::pull()))]

      # Determine whether any pkg functions are called
      any_pkg_function_called <-
        fcn_lines %>%
        stringr::str_detect(
          pattern = paste(all_fcns, "\\(", sep = "", collapse = "|")) %>%
        any()

      if (any_pkg_function_called) {

        # Determine which lines have calls to pkg fcns
        which_lines_pkg_function_called <-
          fcn_lines %>%
          stringr::str_detect(
            pattern = paste(all_fcns, "\\(", sep = "", collapse = "|")) %>%
          which()

        # Get the called functions
        called_functions <-
          fcn_lines[which_lines_pkg_function_called] %>%
          stringr::str_extract_all(
            pattern = paste(all_fcns, sep = "", collapse = "|")) %>%
          unlist() %>%
          unique()

        # Insert the value for the number of package functions called
        tbl_row <-
          tbl_row %>%
          dplyr::mutate(n_pkg_fcns_called = length(called_functions))

        # Join in the names of the called package functions
        tbl_row <-
          tbl_row %>%
          dplyr::inner_join(
            dplyr::tibble(
              n_pkg_fcns_called = length(called_functions),
              names_fcns_called = called_functions),
            by = "n_pkg_fcns_called")

      } else {

        fcn_info_tbl[x, 7] <- 0
        fcn_info_tbl[x, 8] <- NA_character_

        tbl_row <- tbl_row %>% dplyr::mutate(n_pkg_fcns_called = 0)
        tbl_row <- tbl_row %>% dplyr::mutate(names_fcns_called = NA_character_)
      }

      tbl_row
    })

  # Nest the `names_fcns_called` column
  fcn_info_tbl <-
    fcn_info_tbl %>%
    dplyr::group_by(fcn_name, r_file, line_start, line_end, lines, exported, n_pkg_fcns_called) %>%
    tidyr::nest()

  # Set the working directory back to the previous one
  setwd(dir = present_wd)

  fcn_info_tbl
}
