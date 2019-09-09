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

  fn_args_tbl <- pkg_api(...)

  fn_args_tbl %>%
    dplyr::mutate(
      args_defaults = dplyr::case_when(
        default_values != "" ~ paste(arg_names, default_values, sep = " = "),
        TRUE ~ arg_names)
    ) %>%
    dplyr::group_by(fn_name) %>%
    dplyr::summarize(args_defaults = paste(args_defaults, collapse = ", ")) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(fn_args_defaults = paste0(fn_name, "(", args_defaults, ")")) %>%
    dplyr::pull(fn_args_defaults) %>%
    cat(file = filename, sep = "\n")
}
