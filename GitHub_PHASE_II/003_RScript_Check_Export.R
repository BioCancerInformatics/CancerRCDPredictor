library(plotly)
tryCatch({
  p <- plot_ly(x = 1:5, y = 1:5)
  save_image(p, 'test_kaleido.png')
  cat('KALEIDO_SUCCESS')
}, error = function(e) {
  cat('KALEIDO_FAIL:', e)
})