#' Access files in the current app
#'
#' NOTE: If you manually change your package name in the DESCRIPTION,
#' don't forget to change it here too, and in the config file.
#' For a safer name change mechanism, use the `golem::set_golem_name()` function.
#'
#' @param ... character vectors, specifying subdirectory and file(s)
#' within your package. The default, none, returns the root of the app.
#'
#' @noRd
app_sys <- function(...) {
  system.file(..., package = "lisdashboard")
}


#' Read App Config
#'
#' @param value Value to retrieve from the config file.
#' @param config GOLEM_CONFIG_ACTIVE value. If unset, R_CONFIG_ACTIVE.
#' If unset, "default".
#' @param use_parent Logical, scan the parent directory for config file.
#' @param file Location of the config file
#'
#' @noRd
get_golem_config <- function(
  value,
  config = Sys.getenv(
    "GOLEM_CONFIG_ACTIVE",
    Sys.getenv(
      "R_CONFIG_ACTIVE",
      "default"
    )
  ),
  use_parent = TRUE,
  # Modify this if your config file is somewhere else
  file = app_sys("golem-config.yml")
) {
  config::get(
    value = value,
    config = config,
    file = file,
    use_parent = use_parent
  )
}


# 为一些字符串规定好特定的对应颜色
color_vec <- c(
  "男" = "#92C5DE",
  "女" = "#E78AC3",
  "+" = "#FF7F00",
  "结果无效" = "#B3B3B3",
  "-" = "#B2DF8A",
  "±" = "#CAB2D6",
  "阴性" = "#B3B3B3",
  "正常" = "#B2DF8A",
  "偏高" = "#FF7F00",
  "偏低" = "#92C5DE"
)


color_default <- c(
  "#66C2A5", "#FC8D62", "#8DA0CB", "#E78AC3",
  "#A6D854", "#FFD92F", "#E5C494", "#B3B3B3",
  "pink",    "#1F78B4", "#33A02C", "#FB9A99",
  "#CAB2D6", "#FDBF6F", "#B15928", "#A6CEE3",
  "#B2DF8A", "#6A3D9A", "#FF7F00", "#FFFF99",
  "#E31A1C", "#F4A582", "#92C5DE", "#D1E5F0",
  "#4575B4", "#66A61E", "#A6761D", "#E6AB02"
)
