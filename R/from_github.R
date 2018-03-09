#' Helper function for pointing to an R package on GitHub
#'
#' This helper function allows for inspection of R
#' packages that are hosted on GitHub.
#' @param pkg_location the GitHub repository address
#' of the following construction: \code{[username]/[repo]}.
#' @export
from_github <- function(repo) {

  # Construct the GitHub URL for the zipball
  url_str <-
    paste0("http://github.com/", repo, "/zipball/master")

  # Construct the temp path for the package .zip file
  temp_path_str <-
    paste0(getwd(), "/temp_pkgattrs/", repo)

  list(
    repo = repo,
    temp_path_str = temp_path_str)
}
