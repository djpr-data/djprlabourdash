pkgload::load_all(".")
shinyOptions(cache = cachem::cache_disk(file.path(dirname(tempdir()), "djpr-jobs-cache")))
app()
