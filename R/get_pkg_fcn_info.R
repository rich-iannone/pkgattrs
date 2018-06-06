#' Get a table of info of package fcns
#'
#' Create a tibble of information related to each
#' function available in a package.
#' @param ... a series of objects pointing to
#' package locations. These can be strings with
#' paths to local package directories, or,
#' invocations of helper functions such as
#' \code{from_github()}.
#' @param .make_clean an option to clean the
#' working directory of any temporary package files
#' downloaded from GitHub.
#' @importFrom stringr str_detect str_replace str_replace_all
#' @importFrom stringr str_split_fixed fixed
#' @importFrom dplyr tibble mutate pull inner_join group_by left_join
#' @importFrom dplyr rename bind_rows bind_cols select everything as_tibble
#' @importFrom purrr map_df flatten_chr
#' @importFrom tidyr nest
#' @importFrom cyclocomp cyclocomp_package_dir
#' @import downloader
#' @export
get_pkg_fcn_info <- function(...,
                             .make_clean = TRUE) {

  # Create bindings for global variables
  pkg_path <- r_file <- ln_start <- ln_end <- fcn_name <- NULL
  pkg_fcns_called <- names_fcns_called <- r_file_path <- NULL
  lines <- exported <- n_pkg_fcns_called <- data <- NULL

  pkg_location_list <- list(...)

  github_paths <-
    are_github_paths(pkg_location_list = pkg_location_list)

  local_paths <-
    are_local_paths(pkg_location_list = pkg_location_list)

  pkg_locations <-
    dplyr::tibble(
      src = NA_character_,
      repo = NA_character_,
      url = NA_character_,
      pkg_path = NA_character_)[-1, ]

  # Add local paths to `pkg_locations`
  if (length(local_paths) > 0) {

    pkg_locations <-
      dplyr::bind_rows(
        pkg_locations,
        dplyr::tibble(
          src = "local",
          repo = NA_character_,
          url = NA_character_,
          pkg_path = pkg_location_list[local_paths] %>% purrr::flatten_chr()))
  }

  # Add GitHub paths to `pkg_locations`
  if (length(github_paths) > 0) {
    pkg_locations <-
      dplyr::bind_rows(
        pkg_locations,
        pkg_location_list[github_paths] %>% dplyr::bind_rows())
  }

  # Get the pkg names
  pkg_locations <-
    pkg_locations %>%
    dplyr::mutate(
      pkg_name = stringr::str_replace(
        string = pkg_path,
        pattern = ".*\\/(.*)",
        replacement = "\\1"))

  # Get the working directory
  present_wd <- getwd()

  # Remove any extant temporary directory
  if (dir.exists(paste0(present_wd, "/temp_pkgattrs"))) {

    unlink(
      paste0(present_wd, "/temp_pkgattrs"),
      recursive = TRUE, force = TRUE)
  }

  # Generate the table of function info for all packages
  fcn_info_tbl_all <-
    seq(nrow(pkg_locations)) %>%
    purrr::map_df(.f = function(x) {

       pkg_src <- pkg_locations$src[x]
      pkg_repo <- pkg_locations$repo[x]
       pkg_url <- pkg_locations$url[x]
      pkg_path <- pkg_locations$pkg_path[x]
      pkg_name <- pkg_locations$pkg_name[x]

      if (pkg_locations$src[x] == "GitHub") {

        if (dir.exists(pkg_path) == FALSE) {

          # Prepare temporary directory
          dir.create(pkg_path, recursive = TRUE)
        }

        # Download the package to the temp location
        downloader::download(
          url = pkg_locations$url[x],
          dest = paste0(pkg_locations$pkg_path[x], "/pkg.zip"))

        # Unzip the package
        utils::unzip(
          zipfile = paste0(pkg_locations$pkg_path[x], "/pkg.zip"),
          exdir = pkg_locations$pkg_path[x])

        pkg_path <-
          list.files(pkg_locations$pkg_path[x], full.names = TRUE)[2]
      }

      # Temporarily change the working directory
      # to the package location
      setwd(dir = pkg_path)

      # Get the cyclomatic complexity of each function
      cc_df <-
        cyclocomp::cyclocomp_package_dir(getwd()) %>%
        dplyr::as_tibble()

      # Get a list of all .R files in the `./R` folder
      r_files <-
        list.files(
          path = "./R",
          pattern = ".*.R",
          full.names = TRUE)

      # Detect those lines from NAMESPACE where a
      # function is exported
      exported_fcn_lines <-
        "./NAMESPACE" %>% readLines() %>%
        stringr::str_detect(pattern = "^export\\(.*") %>%
        which()

      exported_fcns <-
        (("./NAMESPACE" %>% readLines())[exported_fcn_lines]) %>%
        stringr::str_replace_all(
          pattern = "(^export|\\(|\\))",
          replacement = "")

      # Get the function reference table
      fcn_info_tbl <-
        seq(r_files) %>%
        purrr::map_df(.f = function(y) {

          # Get lines from each .R file
          file_lines <- r_files[y] %>% readLines()

          # Detect those lines where a function begins
          function_def_lines <-
            file_lines %>%
            stringr::str_detect(
              pattern = "^[a-zA-Z0-9_\\.].* <- function\\(")

          # Detect those lines where a function ends
          function_end_lines <-
            file_lines %>%
            stringr::str_detect(pattern = "^\\}")

          # Get the beginning and ending line numbers for
          # each function
          line_numbers_start <- which(function_def_lines)
          line_numbers_end <- which(function_end_lines)

          fcn_name <-
            (file_lines[function_def_lines] %>%
               stringr::str_split_fixed(pattern = " ", 2))[, 1]

          fcn_info_tbl <-
            dplyr::tibble(
              pkg_name = pkg_name,
              fcn_name = fcn_name,
                r_file = r_files[y],
               pkg_src = pkg_src,
              pkg_repo = pkg_repo,
              pkg_path = pkg_path) %>%
            dplyr::mutate(
              r_file_path = r_file) %>%
            dplyr::mutate(
              r_file = stringr::str_replace(
                string = r_file,
                pattern = stringr::fixed("./R/"),
                replacement = "")) %>%
            dplyr::mutate(ln_start = line_numbers_start) %>%
            dplyr::mutate(ln_end = line_numbers_end) %>%
            dplyr::mutate(lines = (ln_end - ln_start + 1) %>% as.integer()) %>%
            dplyr::mutate(exported = ifelse(
              fcn_name %in% exported_fcns, TRUE, FALSE)) %>%
            dplyr::select(
              pkg_name, pkg_src, fcn_name, exported, r_file, r_file_path,
              ln_start, ln_end, lines, pkg_repo, pkg_path)

          fcn_info_tbl
        })

      # Get a vector of all package functions
      all_fcns <- fcn_info_tbl %>% dplyr::pull(fcn_name)

      fcn_info_tbl <-
        seq(all_fcns) %>%
        purrr::map_df(.f = function(y) {

          fcn_name <- all_fcns[y]
          tbl_row <- fcn_info_tbl[y, ]

          fcn_lines <-
            readLines(tbl_row %>% dplyr::pull("r_file_path"))[
              seq((tbl_row %>% dplyr::pull("ln_start")),
                  (tbl_row %>% dplyr::pull("ln_end")))]

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

            # Join in the names of the called package functions
            tbl_row <-
              tbl_row %>%
              dplyr::inner_join(
                dplyr::tibble(
                  fcn_name = fcn_name,
                  n_pkg_fcns_called = length(called_functions) %>% as.integer(),
                  names_fcns_called = called_functions),
                by = "fcn_name")

          } else {

            tbl_row <-
              tbl_row %>%
              dplyr::mutate(n_pkg_fcns_called = 0L) %>%
              dplyr::mutate(names_fcns_called = NA_character_)
          }

          tbl_row
        })

      # Nest the `names_fcns_called` column
      fcn_info_tbl <-
        fcn_info_tbl %>%
        dplyr::group_by(
          pkg_name, pkg_src, fcn_name, exported, r_file, r_file_path,
          ln_start, ln_end, lines, pkg_repo, pkg_path, n_pkg_fcns_called) %>%
        tidyr::nest() %>%
        dplyr::rename(pkg_fcns_called = data)

      # Join the cyclocomp data to the table
      fcn_info_tbl <-
        fcn_info_tbl %>%
        dplyr::left_join(cc_df, by = c("fcn_name" = "name")) %>%
        dplyr::select(
          pkg_name, pkg_src, fcn_name, exported, r_file, r_file_path,
          ln_start, ln_end, lines, cyclocomp, everything())

      # Set the working directory back to the previous one
      setwd(dir = present_wd)

      if (.make_clean) {

        # Remove temporary directory
        if (dir.exists("./temp_pkgattrs")) {

          unlink(
            "./temp_pkgattrs",
            recursive = TRUE, force = TRUE)
        }
      }

      fcn_info_tbl
    })

  chr_list_col <-
    1:nrow(fcn_info_tbl_all) %>%
    purrr::map_df(.f = function(x) {

      if (all(is.na(fcn_info_tbl_all[[x, 13]]))) {

        dplyr::tibble(
          names_fcns_called = list(
            names_fcns_called = vector(mode = "character")))

      } else {

        dplyr::tibble(
          names_fcns_called = list(
            names_fcns_called = fcn_info_tbl_all[[x, 13]] %>%
              dplyr::pull(names_fcns_called)))
      }
    })

  fcn_info_tbl_all %>%
    dplyr::bind_cols(chr_list_col) %>%
    dplyr::select(-pkg_fcns_called) %>%
    dplyr::rename(pkg_fcns_called = names_fcns_called)
}
