options(repos = c(CRAN = 'https://cloud.r-project.org'))
if (!requireNamespace('reticulate', quietly = TRUE)) install.packages('reticulate')
tryCatch({
  reticulate::install_miniconda()
}, error = function(e) cat('Miniconda install error:', e, '\n'))
reticulate::py_install('kaleido')
cat('Kaleido setup complete.\n')