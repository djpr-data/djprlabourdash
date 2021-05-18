page_groups <- function(...) {
  djpr_tab_panel(
    title = "Groups",
    "Compare groups",
    br(),
    htmlOutput("groups_footnote"),
    br()
  )
}
