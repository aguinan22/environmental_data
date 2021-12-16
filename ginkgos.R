require(here)
ginkgos <- read.csv(here("data","ginkgo_data_2021.csv"))
head(ginkgos)

boxplot(notch_depth ~ seeds_present, data = ginkgos)



plot(ginkgos$max_depth, ginkgos$max_width, xlab = "Max Width", ylab = "Max Depth", col = ginkgos$notch)

ginkgos$notch <- (c(ginkgos$notch_depth != 0))

ginkgos$notch[ginkgos$notch==TRUE]="red"
ginkgos$notch[ginkgos$notch==FALSE]="blue"

ginkgos$seeds_present[ginkgos$seeds_present==TRUE]="red"
ginkgos$seeds_present[ginkgos$seeds_present==FALSE]="blue"