library(ggplot2)
library(GGally)
  
par(mfrow = c(6, 3))

c <- ggplot(liver.data, aes(Age, Gender, colour=Result))
c <- c + geom_point()
print(c)

c <- ggplot(liver.data, aes(Age, TotBrubin, colour=Result))
c <- c + geom_point()
print(c)

c <- ggplot(liver.data, aes(Age, DirBrubin, colour=Result))
c <- c + geom_point()
print(c)

c <- ggplot(liver.data, aes(Age, AlkPhosph, colour=Result))
c <- c + geom_point()
print(c)

c <- ggplot(liver.data, aes(Age, AlamAmino, colour=Result))
c <- c + geom_point()
print(c)

c <- ggplot(liver.data, aes(Age, AspAmino, colour=Result))
c <- c + geom_point()
print(c)

c <- ggplot(liver.data, aes(Age, TotProt, colour=Result))
c <- c + geom_point()
print(c)

c <- ggplot(liver.data, aes(Age, Albumin, colour=Result))
c <- c + geom_point()
print(c)

c <- ggplot(liver.data, aes(Age, AlbGlobRat, colour=Result))
c <- c + geom_point()
print(c)

c <- ggplot(liver.data, aes(Gender, Age, colour=Result))
c <- c + geom_point()
print(c)

c <- ggplot(liver.data, aes(Gender, TotBrubin, colour=Result))
c <- c + geom_point()
print(c)

c <- ggplot(liver.data, aes(Gender, DirBrubin, colour=Result))
c <- c + geom_point()
print(c)

c <- ggplot(liver.data, aes(Gender, AlkPhosph, colour=Result))
c <- c + geom_point()
print(c)

c <- ggplot(liver.data, aes(Gender, AlamAmino, colour=Result))
c <- c + geom_point()
print(c)

c <- ggplot(liver.data, aes(Gender, AspAmino, colour=Result))
c <- c + geom_point()
print(c)

c <- ggplot(liver.data, aes(Gender, TotProt, colour=Result))
c <- c + geom_point()
print(c)

c <- ggplot(liver.data, aes(Gender, Albumin, colour=Result))
c <- c + geom_point()
print(c)

c <- ggplot(liver.data, aes(Gender, AlbGlobRat, colour=Result))
c <- c + geom_point()
print(c)



print(ggpairs(liver.data, columns = 1:10, mapping = aes(colour = Result)))





