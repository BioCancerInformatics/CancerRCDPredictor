setwd('D:/Pré-artigo 5-optosis model/Machine learning CancerRCDShiny_prediction/Enrique version/PHASE_II')

if (!requireNamespace('webshot2', quietly = TRUE)) install.packages('webshot2', repos='http://cran.us.r-project.org')
if (!requireNamespace('magick', quietly = TRUE)) install.packages('magick', repos='http://cran.us.r-project.org')

# Use Landscape Dimensions
webshot2::webshot('sankey_lineage_annotated_default_landscape.html', 'sankey_lineage_annotated_default.pdf', delay = 15, vwidth = 3200, vheight = 1800, cliprect="viewport")
webshot2::webshot('sankey_lineage_annotated_default_landscape.html', 'sankey_lineage_annotated_default.png', delay = 15, vwidth = 3200, vheight = 1800, zoom = 2, cliprect="viewport")

img <- magick::image_read('sankey_lineage_annotated_default.png')
bg <- magick::image_blank(magick::image_info(img)[['width']], magick::image_info(img)[['height']], color = 'white')
img <- magick::image_composite(bg, img, operator = 'over')
img <- magick::image_flatten(img)

magick::image_write(img, path = 'sankey_lineage_annotated_default.jpg', format = 'jpeg', quality = 100)
magick::image_write(img, path = 'sankey_lineage_annotated_default.tiff', format = 'tiff', compression = 'lzw', density = '600x600')

cat('IMAGES GENERATED SUCCESSFULLY\n')
