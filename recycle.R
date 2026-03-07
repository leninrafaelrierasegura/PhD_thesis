```{r, eval = TRUE}
library(reticulate)
#use_python("/home/rierasl/miniconda3/envs/phdenv/bin/python", required = TRUE)
py_config()
library(plotly)

p <- plot_ly(z = ~volcano, type = "surface")
# Save static images in figures folder
plotly::save_image(p, "plot3d.png", width = 1400, height = 1000)
plotly::save_image(p, "plot3d.pdf")
plotly::save_image(p, "plot3d.svg")
```


```{r, eval = TRUE}
library(plotly)

# Create plots
p1 <- plot_ly(z = ~volcano, type = "surface") %>% layout(title = "Plot 1")
p2 <- plot_ly(z = ~volcano + 10, type = "surface") %>% layout(title = "Plot 2")

# Save each separately in high resolution
plotly::save_image(p1, "p1.png", width = 1400, height = 1000, scale = 2)
plotly::save_image(p2, "p2.png", width = 1400, height = 1000, scale = 2)
```

```{python}
from PIL import Image

img1 = Image.open("p1.png")
img2 = Image.open("p2.png")

combined = Image.new('RGB', (img1.width + img2.width, img1.height))
combined.paste(img1, (0,0))
combined.paste(img2, (img1.width,0))
combined.save("combined.png")
```

```{python}
from PIL import Image

img1 = Image.open("p1.png")
img2 = Image.open("p2.png")

combined = Image.new('RGB', (img1.width + img2.width, max(img1.height, img2.height)))
combined.paste(img1, (0, 0))
combined.paste(img2, (img1.width, 0))
combined.save("combined_side_by_side.pdf", "PDF", resolution=300.0)
```


```{r}
library(plotly)

# Your 3D plots
p1 <- plot_ly(z = ~volcano, type = "surface") %>% layout(title = "Plot 1")
p2 <- plot_ly(z = ~volcano + 10, type = "surface") %>% layout(title = "Plot 2")

# Save each plot as PDF
plotly::save_image(p1, "p1.pdf", width = 1400, height = 1000, scale = 2)
plotly::save_image(p2, "p2.pdf", width = 1400, height = 1000, scale = 2)
```


```{python}
from PyPDF2 import PdfReader, PdfWriter
from PyPDF2 import Transformation

# Load PDFs
pdf1 = PdfReader("p1.pdf")
pdf2 = PdfReader("p2.pdf")

writer = PdfWriter()

page1 = pdf1.pages[0]
page2 = pdf2.pages[0]

# Get dimensions
w1 = float(page1.mediabox.width)
h1 = float(page1.mediabox.height)
w2 = float(page2.mediabox.width)
h2 = float(page2.mediabox.height)

# Enlarge page1 to fit both PDFs side by side
page1.mediabox.upper_right = (w1 + w2, max(h1, h2))

# Translate page2 and merge into page1
page2.add_transformation(Transformation().translate(tx=w1, ty=0))
page1.merge_page(page2)

# Add the combined page to writer
writer.add_page(page1)

# Save
with open("combined_side_by_side.pdf", "wb") as f:
  writer.write(f)
```






library(plotly)
library(htmlwidgets)
library(webshot2)  # lightweight, no sudo needed
library(rsvg)

# Your Plotly plot

p2 <- plot_ly(z = ~volcano + 10, type = "surface") %>% layout(title = "Plot 2")

# Save as self-contained HTML
htmlwidgets::saveWidget(p2, "temp_plot.html", selfcontained = TRUE)

# Take SVG screenshot with webshot2
webshot2::webshot("temp_plot.html", "temp_plot.svg", vwidth = 800, vheight = 600)



















