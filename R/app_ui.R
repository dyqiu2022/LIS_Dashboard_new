#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @import bslib
#' @noRd

# 在 app_ui.R 中定义
my_theme <- bslib::bs_theme(
  bootswatch = "flatly",
  bg = "#fff",
  fg = "rgb(31, 45, 58)",
  # 1. 基础字号直接在这里设为 0.85rem，bslib 会自动缩放其他元素
  base_font_size = "0.85rem",

  # 2. 告诉 bslib 去加载 Noto Sans SC (Google Font)
  # 虽然不写 collection，但 bslib 会自动把它排在字体列表第一位
  base_font = bslib::font_google("Noto Sans SC", local = TRUE),
  heading_font = bslib::font_google("Noto Sans SC")
) %>%
  bslib::bs_add_variables(
    # 3. 重点：在这里把你的长串备选字体清单写进 Bootstrap 变量
    "font-family-base" = "'Noto Sans SC', 'Lato', -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, 'Helvetica Neue', Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol'",

    # 4. 其他精细化调整保持不变
    "h5-font-size"    = "1rem",
    "h4-font-size"    = "1.1rem",
    "input-font-size" = "0.85rem",
    "btn-padding-y"   = "0.25rem",
    "btn-padding-x"   = "0.5rem"
  )

app_ui <- function(request) {
  tagList(
    golem_add_external_resources(),
    page_navbar(
      title = "数据看板",
      theme = my_theme,
      navbar_options = navbar_options(
        collapsible = TRUE
      ),
      # 数据上传页面
      mod_data_upload_ui("origin"),
      # 聚类工具页面
      nav_panel(title = "聚类工具"),
      # 分析工具页面
      nav_panel(
        full_screen = FALSE,
        title = "分析工具",
        layout_sidebar(
          fillable = TRUE,
          sidebar = sidebar(
            bg = "#f8f9fa",
            open = "desktop",
            width = "350px",
            mod_analysis_tools_sidebar_ui("origin")
          ),
          navset_card_tab(
            # 定量变量离散化UI
            mod_quantitative_discretization_ui("origin"),
            # 探索性定性分析UI
            mod_qualitative_analysis_ui("origin"),
            # 探索性定量分析UI
            mod_quantitative_analysis_ui("origin"),
            nav_panel(title = "参考区间与人群异质性评估"),
            nav_panel(title = "批间差异分析"),
            mod_show_filtered_data_ui("origin")
          )
        )
      )
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  # 上传文件大小限制
  options(shiny.maxRequestSize = 1000 * 1024^2)


  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "lisdashboard"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
