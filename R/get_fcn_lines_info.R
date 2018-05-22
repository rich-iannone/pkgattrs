#' Get a table with a line-by-line info of package fcns
#'
#' Create a tibble of information related to each
#' line of every function available in a package.
#' @param ... a series of objects pointing to
#' package locations. These can be strings with
#' paths to local package directories, or,
#' invocations of helper functions such as
#' \code{from_github()}.
#' @param .make_clean an option to clean the
#' working directory of any temporary package files
#' downloaded from GitHub.
#' @importFrom purrr map_df
#' @importFrom dplyr pull tibble bind_rows mutate case_when select
#' @importFrom stringr str_detect
#' @export
get_fcn_lines_info <- function(..., .make_clean = TRUE) {

  # Create bindings for global variables
  ln <- type <- subtype <- ln_content <- NULL

  fcn_df <- get_pkg_fcn_info(..., .make_clean = FALSE)

  fcn_lines_tbl <-
    seq(nrow(fcn_df)) %>%
    purrr::map_df(.f = function(x) {

      fcn_df_line <- fcn_df[x, ]

      fcn_name <- fcn_df_line %>% dplyr::pull("fcn_name")
      file <- fcn_df_line %>% dplyr::pull("r_file")
      fcn_start <- fcn_df_line %>% dplyr::pull("ln_start")
      fcn_end <- fcn_df_line %>% dplyr::pull("ln_end")
      r_file_path <- fcn_df_line %>% dplyr::pull("r_file_path")
      pkg_name <- fcn_df_line %>% dplyr::pull("pkg_name")
      pkg_src <- fcn_df_line %>% dplyr::pull("pkg_src")
      pkg_path <- fcn_df_line %>% dplyr::pull("pkg_path")

      # Get the .R file path
      r_file_path <-
        gsub(
          pattern = "^\\.",
          replacement = pkg_path,
          x = r_file_path)

      fcn_lines_tbl <-
        dplyr::tibble(
          pkg_name = pkg_name,
          pkg_src = pkg_src,
          fcn_name = fcn_name,
          ln = fcn_start:fcn_end,
          type = "function",
          ln_content = (r_file_path %>% readLines())[fcn_start:fcn_end])

      if ((fcn_lines_tbl %>% dplyr::pull("ln"))[1] >= 2) {

        line <- (fcn_lines_tbl %>% dplyr::pull("ln"))[1]
        line <- line - 1

        blank_line <- 0
        blank_lines_before_roxygen <- 0
        inside_roxygen_block <- FALSE

        repeat {

          if (
            stringr::str_detect(
              string =
              (r_file_path %>% readLines())[line],
              pattern = "^$")) {

            if (inside_roxygen_block) break
            blank_line <- blank_line + 1

          } else if (

            stringr::str_detect(
              string =
              (r_file_path %>% readLines())[line],
              pattern = "^#'")) {

            inside_roxygen_block <- TRUE
            blank_lines_before_roxygen <- blank_line

          } else if (

            inside_roxygen_block &
            stringr::str_detect(
              string =
              (r_file_path %>% readLines())[line],
              pattern = "^(?!#').*$")) {
            break
          }

          fcn_lines_tbl <-
            dplyr::bind_rows(
              dplyr::tibble(
                pkg_name = pkg_name,
                pkg_src = pkg_src,
                fcn_name = fcn_name,
                ln = line,
                type = "roxygen",
                ln_content = (r_file_path %>% readLines())[line]),
              fcn_lines_tbl)

          line <- line - 1

          if (line == 0) break
        }
      }

      # Provide a `subtype` classification
      fcn_lines_tbl %>%
        dplyr::mutate(subtype = case_when(
          type == "function" &
            stringr::str_detect(string = ln_content, pattern = "(^$|^[ ]*$)") ~ "blank",
          type == "function" &
            stringr::str_detect(string = ln_content, pattern = "[ ]*#.*") ~ "comment",
          type == "function" ~ "code",
          type == "roxygen" ~ "roxygen")) %>%
        dplyr::select(pkg_name, pkg_src, fcn_name, ln, type, subtype, ln_content)
    })

  if (.make_clean) {

    # Remove temporary directory
    if (dir.exists("./temp_pkgattrs")) {

      unlink(
        "./temp_pkgattrs",
        recursive = TRUE, force = TRUE)
    }
  }

  fcn_lines_tbl
}
