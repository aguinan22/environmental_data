require(here)
dat_catrate <- data.frame(read.csv(here("data","catrate.csv")))
dat_delomys <- data.frame(read.csv(here("data","delomys.csv")))
dat_rope <- data.frame(read.csv(here("data","rope.csv")))

head(dat_catrate)
head(dat_delomys)
head(dat_rope)

plot(dat_delomys$body_length, dat_delomys$body_mass, main = "Rat size tomfoolery - Abigail Guinan", xlab = "Body mass", ylab = "Body length")

points(250,100, col = "red")
