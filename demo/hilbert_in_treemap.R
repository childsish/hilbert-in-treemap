library(dplyr)
library(HilbertInTreemap)

lengths = c(
  248956422, 242193529, 198295559, 190214555, 181538259, 170805979, 159345973,
  145138636, 138394717, 133797422, 135086622, 133275309, 114364328, 107043718,
  101991189, 90338345, 83257441, 80373285, 58617616, 64444167, 46709983, 50818468
)

lengths = lengths / 10000
ratio = get.best.ratio(lengths)
height = sqrt(sum(lengths) / ratio)
width = height * ratio

rows = squarify(lengths, width, height)
rectangles = squarify.coordinates(rows, width, height) %>%
  mutate(x1 = x,
         y1 = y,
         x2 = x + width,
         y2 = y + height,
         x1 = x1 %/% 2 * 2,
         y1 = y1 %/% 2 * 2,
         x2 = x2 %/% 2 * 2,
         y2 = y2 %/% 2 * 2,
         y1 = max(y2) - y1,
         y2 = max(y2) - y2,
         x = x1,
         y = y2,
         width = x2 - x1,
         height = y1 - y2)

par(mar = c(0,0,0,0), xaxs = "i", yaxs = "i")
plot(1, type="n", axes=F, xlab="", ylab="", xlim=c(0, max(rectangles$x + rectangles$width)), ylim=c(0, max(rectangles$y + rectangles$height)))
rect(rectangles$x1, rectangles$y1, rectangles$x2, rectangles$y2, border='red')

for (i in 1:nrow(rectangles)) {
  rectangle = rectangles[i,]
  points = pseudo.hilbert(rectangle$width,
                          rectangle$height)
   points = points %>%
     mutate(x = x + rectangle$x + 0.5,
            y = y + rectangle$y + 0.5)
  lines(points)
}
