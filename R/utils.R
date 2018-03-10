#' Determine which list components provide GitHub repos
#' @param pkg_location_list a list of package locations.
#' @importFrom purrr map_lgl
#' @noRd
are_github_paths <- function(pkg_location_list) {

  seq(pkg_location_list) %>%
    purrr::map_lgl(.f = function(x) {
      inherits(pkg_location_list[[x]], "tbl_df") &&
        all(names(pkg_location_list[[x]]) ==
              c("src", "repo", "url", "pkg_path"))}) %>%
    which()
}

#' Determine which list components refer to local paths
#' @param pkg_location_list a list of package locations.
#' @importFrom purrr map_lgl
#' @noRd
are_local_paths <- function(pkg_location_list) {

  seq(pkg_location_list) %>%
    purrr::map_lgl(.f = function(x) {
      inherits(pkg_location_list[[x]], "character") &&
        pkg_location_list[[x]] %>% dir.exists()}) %>%
    which()
}
