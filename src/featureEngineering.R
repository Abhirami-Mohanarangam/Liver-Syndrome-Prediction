library(ggplot2)
library(crayon)

# dataForComputation <- liver.data
# 
# dataForComputation$Gender <- as.numeric(liver.data$Gender)
# dataForComputation$Result <- as.numeric(liver.data$Result)
# dataForComputation$AspAlamAmino <- liver.data$AspAmino - liver.data$AlamAmino
# print(dataForComputation)
# 
# correlation = cor(dataForComputation)
# print(correlation)

# heatmap(correlation)

by.gender.result <- with(liver.data, group_by(liver.data, Gender, Result))
summary.by.gender.result <- summarize(by.gender.result, mean_age = mean(Age))

print(ggplot(liver.data, aes(DirBrubin, TotBrubin, colour=Result)) + geom_point() + with(liver.data, facet_grid(rows = vars(Result), cols = vars(Gender), )))
print(paste("Correlation between Direct_Bilirubin and Total_Bilirubin: ", cor(liver.data$DirBrubin, liver.data$TotBrubin)))
    # Good Correlation

print(ggplot(liver.data, aes(AspAmino, AlamAmino, colour=Result)) + geom_point() + with(liver.data, facet_grid(rows = vars(Result), cols = vars(Gender), )))
print(paste("Correlation between Aspartate_Aminotransferase and Alamine_Aminotransferase: ", cor(liver.data$AspAmino, liver.data$AlamAmino)))
    # Good Correlation

print(ggplot(liver.data, aes(AlkPhosph, AlamAmino, colour=Result)) + geom_point() + with(liver.data, facet_grid(rows = vars(Result), cols = vars(Gender), )))
print(paste("Correlation between Alkaline_Phosphotase and Alamine_Aminotransferase: ", cor(liver.data$AlkPhosph, liver.data$AlamAmino)))
    # No significant correlation

print(ggplot(liver.data, aes(AlkPhosph, AspAmino, colour=Result)) + geom_point() + with(liver.data, facet_grid(rows = vars(Result), cols = vars(Gender), )))
print(paste("Correlation between Alkaline_Phosphotase and Aspartate_Aminotransferase: ", cor(liver.data$AlkPhosph, liver.data$AspAmino)))
    # No significant correlation

print(ggplot(liver.data, aes(TotProt, Albumin, colour=Result)) + geom_point() + with(liver.data, facet_grid(rows = vars(Result), cols = vars(Gender), )))
print(paste("Correlation between Total_Proteins and Albumin: ", cor(liver.data$TotProt, liver.data$Albumin)))
    # Good Correlation

print(ggplot(liver.data, aes(TotProt, AlbGlobRat, colour=Result)) + geom_point() + with(liver.data, facet_grid(rows = vars(Result), cols = vars(Gender), )))
print(paste("Correlation between Total_Proteins and Albumin_and_Globulin_Ratio: ", cor(liver.data$TotProt, liver.data$AlbGlobRat)))
    # No significant correlation

print(ggplot(liver.data, aes(Albumin, AlbGlobRat, colour=Result)) + geom_point() + with(liver.data, facet_grid(rows = vars(Result), cols = vars(Gender), )))
print(paste("Correlation between Albumin and Albumin_and_Globulin_Ratio: ", cor(liver.data$Albumin, liver.data$AlbGlobRat)))
    # Good Correlation

print(cat("We can choose one feature between", red(bold("Direct_Bilirubin")), "and", green(bold("Total_Bilirubin"))))
print(cat("We can choose one feature between", red(bold("Aspartate_Aminotransferase")), "and", green(bold("Alamine_Aminotransferase"))))
print(cat("We also choose", green(bold("Total_Proteins")), ",", green(bold("Albumin")), "and", green(bold("Albumin_and_Globulin_Ratio"))))


