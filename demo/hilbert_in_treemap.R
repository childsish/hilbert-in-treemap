library(dplyr)
library(HilbertInTreemap)

lengths = c(
  248956422, 242193529, 198295559, 190214555, 181538259, 170805979, 159345973,
  145138636, 138394717, 133797422, 135086622, 133275309, 114364328, 107043718,
  101991189, 90338345, 83257441, 80373285, 58617616, 64444167, 46709983, 50818468
) / 10000

rectangles = adjust.till.fit(lengths)

par(mar = c(0,0,0,0), xaxs = "i", yaxs = "i")
plot(1, type="n", axes=F, xlab="", ylab="", xlim=c(0, max(rectangles$x + rectangles$width)), ylim=c(0, max(rectangles$y + rectangles$height)))
rect(rectangles$x1, rectangles$y1, rectangles$x2, rectangles$y2, border='red')

for (i in 1:nrow(rectangles)) {
  rectangle = rectangles[i,]
  points = pseudo.hilbert(rectangle$width, rectangle$height) %>%
     mutate(x = x + rectangle$x + 0.5,
            y = y + rectangle$y + 0.5)
  lines(points[1:floor(lengths[i]),])
}
