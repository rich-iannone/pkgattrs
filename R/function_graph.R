#' Create a graph that describes network of package functions
#'
#' Create a DiagrammeR graph that contains nodes of the type \code{function} and
#' edges that have the relationship \code{called_in}.
#' @param pkgattrs_tbl A tibble object that contains package function
#'   information. This is created by the \code{pkgattrs()} function.
#' @param pkg_name An optional package name for filtering the tibble provided to
#'   \code{pkgattrs_tbl}, which is useful if that tibble describes multiple
#'   packages.
#' @export
function_graph_all <- function(pkgattrs_tbl,
                               pkg_name = NULL) {

  complete_graph <-
    produce_complete_graph(
      pkgattrs_tbl = pkgattrs_tbl,
      pkg_name = pkg_name
    )

  complete_graph %>% DiagrammeR::render_graph(layout = "nicely")
}

#' Show all package functions that are called from a given function
#'
#' Create a DiagrammeR graph that contains nodes of the type \code{function} and
#' edges that have the relationship \code{called_in}.
#' @param pkgattrs_tbl A tibble object that contains package function
#'   information. This is created by the \code{pkgattrs()} function.
#' @param pkg_name An optional package name for filtering the tibble provided to
#'   \code{pkgattrs_tbl}, which is useful if that tibble describes multiple
#'   packages.
#' @param target_fcn the name of the function that is to be examined for its
#'   calls of package functions.
#' @export
function_graph_single <- function(pkgattrs_tbl,
                                  target_fcn,
                                  pkg_name = NULL) {

  complete_graph <-
    produce_complete_graph(
      pkgattrs_tbl = pkgattrs_tbl,
      pkg_name = pkg_name
    )

  partial_graph <-
    suppressMessages(
      complete_graph %>%
        DiagrammeR::select_nodes(label == target_fcn) %>%
        DiagrammeR::trav_in(add_to_selection = TRUE) %>%
        DiagrammeR::transform_to_subgraph_ws()
    )

  partial_graph %>% DiagrammeR::render_graph(layout = "nicely")
}


produce_complete_graph <- function(pkgattrs_tbl,
                                   pkg_name) {

  if (!is.null(pkg_name)) {

    pkgattrs_tbl <-
      pkgattrs_tbl %>%
      dplyr::filter(pkg_name == pkg_name)
  }

  edge_tbl <-
    pkgattrs_tbl %>%
    tidyr::unnest(pkg_fcns_called) %>%
    dplyr::select(fcn_name, pkg_fcns_called) %>%
    dplyr::rename(from = pkg_fcns_called) %>%
    dplyr::rename(to = fcn_name) %>%
    dplyr::mutate(color = "gray90") %>%
    dplyr::mutate(arrowhead = "dot")

  node_tbl <-
    pkgattrs_tbl %>%
    dplyr::mutate(tooltip = paste0("file: ", r_file)) %>%
    dplyr::select(fcn_name, exported, n_pkg_fcns_called, tooltip) %>%
    dplyr::mutate(fillcolor = ifelse(exported, "green", "lightblue")) %>%
    dplyr::mutate(fontcolor = "gray35") %>%
    dplyr::mutate(color = "gray90")

  DiagrammeR::create_graph() %>%
    DiagrammeR::add_nodes_from_table(
      table = node_tbl,
      label_col = fcn_name,
      set_type = "function"
    ) %>%
    DiagrammeR::add_edges_from_table(
      table = edge_tbl,
      from_col = from,
      to_col = to,
      from_to_map = label,
      set_rel = "called_in"
    ) %>%
    DiagrammeR::rescale_node_attrs(
      node_attr_from = n_pkg_fcns_called,
      to_lower_bound = 0.15,
      to_upper_bound = 1.00,
      node_attr_to = width
    )
}
