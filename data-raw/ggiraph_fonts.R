library(gfonts)

dir.create("fonts")

setup_font("roboto", output_dir = "fonts")

# use_font("roboto", "fonts/css/roboto.css")

library(systemfonts)
if (!font_family_exists("Open Sans")) {
  register_font(
    name = "Open Sans",
    plain = list("fonts/fonts/open-sans-v18-latin-regular.woff", 0),
    bold = list("fonts/fonts/open-sans-v18-latin-700.woff", 0),
    italic = list("fonts/fonts/open-sans-v18-latin-italic.woff", 0),
    bolditalic = list("fonts/fonts/open-sans-v18-latin-700italic.woff", 0)
  )
}
