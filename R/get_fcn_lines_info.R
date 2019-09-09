#' Get a table with a line-by-line info for each package function
#'
#' Create a tibble of information related to each line of every function
#' available in a package.
#' @param ... A series of objects pointing to package locations. These can be
#'   strings with paths to local package directories, or, invocations of helper
#'   functions such as \code{from_github()}.
#' @param .fn_info_tbl An optional tibble of function information obtained from
#'   the \code{pkgattrs()} function.
#' @param .make_clean An option to clean the working directory of any temporary
#'   package files downloaded from GitHub.
#' @noRd
get_fn_lines_info <- function(...,
                               .fn_info_tbl = NULL,
                               .make_clean = TRUE) {

  if (!is.null(.fn_info_tbl)) {
    fn_df <- .fn_info_tbl
  } else {
    fn_df <- get_pkg_fn_info(..., .make_clean = FALSE)
  }

  fn_lines_tbl <-
    seq(nrow(fn_df)) %>%
    purrr::map_df(.f = function(x) {

      fn_df_line <- fn_df[x, ]

      fn_name <- fn_df_line %>% dplyr::pull("fn_name")
      file <- fn_df_line %>% dplyr::pull("r_file")
      fn_start <- fn_df_line %>% dplyr::pull("ln_start")
      fn_end <- fn_df_line %>% dplyr::pull("ln_end")
      r_file_path <- fn_df_line %>% dplyr::pull("r_file_path")
      pkg_name <- fn_df_line %>% dplyr::pull("pkg_name")
      pkg_src <- fn_df_line %>% dplyr::pull("pkg_src")
      pkg_path <- fn_df_line %>% dplyr::pull("pkg_path")

      # Get the .R file path
      r_file_path <-
        gsub(
          pattern = "^\\.",
          replacement = pkg_path,
          x = r_file_path
        )

      fn_lines_tbl <-
        dplyr::tibble(
          pkg_name = pkg_name,
          pkg_src = pkg_src,
          fn_name = fn_name,
          ln = fn_start:fn_end,
          type = "function",
          ln_content = (r_file_path %>% readLines())[fn_start:fn_end]
        )

      if ((fn_lines_tbl %>% dplyr::pull("ln"))[1] >= 2) {

        line <- (fn_lines_tbl %>% dplyr::pull("ln"))[1]
        line <- line - 1

        blank_line <- 0
        blank_lines_before_roxygen <- 0
        inside_roxygen_block <- FALSE

        repeat {

          if (
            stringr::str_detect(
              string = (r_file_path %>% readLines())[line],
              pattern = "^$")) {

            if (inside_roxygen_block) break
            blank_line <- blank_line + 1

          } else if (
            stringr::str_detect(
              string = (r_file_path %>% readLines())[line],
              pattern = "^#'")) {

            inside_roxygen_block <- TRUE
            blank_lines_before_roxygen <- blank_line

          } else if (inside_roxygen_block &
                     stringr::str_detect(
                       string = (r_file_path %>% readLines())[line],
                       pattern = "^(?!#').*$")) {
            break
          }

          fn_lines_tbl <-
            dplyr::bind_rows(
              dplyr::tibble(
                pkg_name = pkg_name,
                pkg_src = pkg_src,
                fn_name = fn_name,
                ln = line,
                type = "roxygen",
                ln_content = (r_file_path %>% readLines())[line]
              ),
              fn_lines_tbl
            )

          line <- line - 1

          if (line == 0) break
        }
      }

      # Provide a `subtype` classification
      fn_lines_tbl %>%
        dplyr::mutate(subtype = dplyr::case_when(
          type == "function" &
            stringr::str_detect(ln_content, "(^$|^[ ]*$)") ~ "blank",
          type == "function" &
            stringr::str_detect(ln_content, "[ ]*#.*") ~ "comment",
          type == "function" ~ "code",
          type == "roxygen" ~ "roxygen")
        ) %>%
        dplyr::select(
          pkg_name, pkg_src, fn_name, ln,
          type, subtype, ln_content
        )
    })

  if (is.null(.fn_info_tbl) && .make_clean) {

    # Remove temporary directory
    if (dir.exists("./temp_pkgattrs")) {

      unlink(
        "./temp_pkgattrs",
        recursive = TRUE,
        force = TRUE
      )
    }
  }

  fn_lines_tbl
}
