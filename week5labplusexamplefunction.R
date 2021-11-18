require(palmerpenguins)
require(here)


#initial png foolishness ----

pngslave = function(image_file)
  {
  require(here)
  png(
    here("images",image_file),
    width = 1200, height = 1000
  )

}

pngslave("nightmarehistogram.png")

dev.off()






#more goofiness with histograms ----

dat_vec <- penguins$body_mass_g

hist(dat_vec, col = "steelblue", main = "blueboy")

histoslave = function(dat_vec, my_title, x_label)
{
  hist(dat_vec, col = "steelblue", main = my_title, xlab = x_label)
}

sample(x = 1:100, size = 1000, replace = TRUE)

histoslave(dat_vec = sample(x = 1:100, size = 1000, replace = TRUE), my_title = "Suffer More", x_label = "woohoo")



#i do my actual lab now please ----


