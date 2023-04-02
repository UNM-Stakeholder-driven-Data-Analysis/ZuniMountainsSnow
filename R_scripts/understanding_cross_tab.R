# very simple example to see waht cross tabulate does
#it lets you see how may cells of one layer correspond to how many cells of a 
# particular value in another layer
r <- rast(nc=2, nr=2)
values(r) <- c(0, 4, 4, 4)
set.names(r, "r")
s <- rast(nc=2, nr=2)
values(s) <- c(17, 3, 3, 10)
set.names(s, "s")
x <- c(r, s)
plot(x)
crossed <- crosstab(x, long=TRUE)
crossed

#this is the sort of operation we want to do to sum the error values for a particular 
#class of pixels
crossed %>% 
  mutate(intermediateTotal=s*Freq) %>% 
  group_by(r) %>% 
  summarize(total=sum(intermediateTotal))
