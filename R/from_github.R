#' Helper function for pointing to an R package on GitHub
#'
#' This helper function allows for inspection of R
#' packages that are hosted on GitHub.
#' @param repo the GitHub repository address
#' of the following construction: \code{[username]/[repo]}.
#' @importFrom dplyr tibble mutate
#' @export
from_github <- function(repo) {

  # Construct the tibble with the necessary repo information
  dplyr::tibble(src = "GitHub", repo = repo) %>%
    dplyr::mutate(url = paste0(
      "http://github.com/", repo, "/zipball/master")) %>%
    dplyr::mutate(pkg_path = paste0(getwd(), "/temp_pkgattrs/", repo))
}
