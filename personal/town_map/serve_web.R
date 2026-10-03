# Serves the static site (web/) at http://127.0.0.1:8787 for local testing.
# Run from the project folder: Rscript serve_web.R  (Ctrl+C to stop)
# The pages must be served over http, not opened as files: browsers block
# loading the data files and JS modules from file:// URLs.
httpuv::runStaticServer("web", host = "127.0.0.1", port = 8787, browse = FALSE)
