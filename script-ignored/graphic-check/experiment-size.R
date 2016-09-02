# experiment for plot size

library(gogamer)
library(ggplot2)


for (stonesize in seq(1, 10))
{
  for (savesize in 1:10)
  {
    g <- plotat(mimiaka, at = 127, stonesize = stonesize)
    ggsave(sprintf("script-ignored/output/ggoban%d-%d.pdf", stonesize, savesize),
           g, width = savesize, height = savesize)
  }
}
