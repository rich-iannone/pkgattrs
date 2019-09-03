#' Write the pkg API to disk
#'
#' Exported functions from a package (along with their arguments and any
#' associated default values) are printed to a file.
#' @param ... Any objects pointing to a package location. This can be a string
#'   with a path to a local package directory, or, an invocation of a helper
#'   function such as [from_github()].
#' @param filename The output filename for the text to be written to disk.
#' @export
write_pkg_api <- function(...,
                          filename = "pkg_api") {

  # Get a table of package function info
  pkg_fcn_info <- pkgattrs(...)

  # Get API function listing
  exported_fcns <-
    pkg_fcn_info %>%
    dplyr::filter(exported == TRUE) %>%
    dplyr::arrange(fcn_name) %>%
    dplyr::pull(fcn_name)

  # Get a vector of package names
  pkg_names <-
    pkg_fcn_info %>%
    dplyr::pull(pkg_name) %>%
    unique()

  fcn_args_tbl <-
    seq(exported_fcns) %>%
    purrr::map_df(.f = function(x) {

      if (!is.null(formals(exported_fcns[x]))) {

        arg_names_tbl <-
          dplyr::tibble(arg_name = formals(exported_fcns[x]) %>% names())

        arg_names <- arg_names_tbl %>% dplyr::pull(arg_name)

        default_values <- vector(mode = "character")

        for (i in seq(arg_names)) {

          if (typeof(formals(exported_fcns[x])[[i]]) != "symbol" &&
              formals(exported_fcns[x])[[i]] %>% is.character()) {

            default_value <-
              paste(
                paste0("\"", formals(exported_fcns[x])[[i]], "\""),
                collapse = ", "
              )

          } else if (typeof(formals(exported_fcns[x])[[i]]) == "NULL") {

            default_value <- "NULL"

          } else {

            default_value <-
              paste(
                as.character(formals(exported_fcns[x])[[i]]),
                collapse = ", "
              )
          }

          default_values <- c(default_values, default_value)
        }

        dflt_vals_str <-
          dplyr::tibble(
            fcn_name = exported_fcns[x],
            arg_names = arg_names,
            default_values = default_values
          ) %>%
          dplyr::mutate(
            args_defaults = dplyr::case_when(
              default_values != "" ~ paste(arg_names, default_values, sep = " = "),
              TRUE ~ arg_names)
          ) %>%
          dplyr::pull(args_defaults) %>%
          paste(collapse = ", ")

        fcn_tbl <-
          dplyr::tibble(
            fcn_args_defaults = paste0(
              exported_fcns[x], "(", dflt_vals_str, ")"
            )
          )

      } else {
        fcn_tbl <-
          dplyr::tibble(fcn_args_defaults = paste0(exported_fcns[x], "()"))
      }

      fcn_tbl
    })

  fcn_args_tbl %>%
    dplyr::pull(fcn_args_defaults) %>%
    cat(file = filename, sep = "\n")
}
