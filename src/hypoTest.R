correlation_testing <- function()
{
  par(mfrow = c(1, 2))
  cat("Correlation between Alamine aminotransferase and Aspartate aminotransferase:\n")
  
  cor_1 = cor.test(liver_patient$Alamine_Aminotransferase, liver_patient$Aspartate_Aminotransferase)
  
  cat(blue(bold("Null hypothesis, H0:")), "Correlation of two atributes is 0.\n")
  cat(blue(bold("Alternating hypothesis, H1:")), "True correlation is non zero.\n\n")
  
  cf = 0.95 # confidence interval
  
  # printing result of the testing  
  if(cor_1$p.value <= 1 - cf){
    cat(bold("Alamine aminotransferase and Aspartate aminotransferase are", underline("correlated"), ".\n"))
    cat("Correlation: ", green(bold(cor(liver_patient$Alamine_Aminotransferase, liver_patient$Aspartate_Aminotransferase))), "\n\n")
  }else{
    cat(bold("Alamine aminotransferase and Aspartate aminotransferase are not correlated.\n\n"))
  }
  
  # plottings
  plot(liver_patient$Alamine_Aminotransferase, liver_patient$Aspartate_Aminotransferase, pch = 19, col = "blue",
       main = "Alamine  vs Aspartate ", xlab = "Alamine aminotransferase", ylab = "Aspartate aminotransferase"
  )
  # print(ggplot(liver.data, aes(DirBrubin, TotBrubin, colour=Result)) + geom_point() + with(liver.data, facet_grid(rows = vars(Result), cols = vars(Gender))) + abline(lm(liver_patient$Aspartate_Aminotransferase ~ liver_patient$Alamine_Aminotransferase, data = liver_patient), col = "red"))
  abline(lm(liver_patient$Aspartate_Aminotransferase ~ liver_patient$Alamine_Aminotransferase, data = liver_patient), col = "red")
  
  
  cat("Correlation between Total bilirubin and Direct bilirubin:\n")
  
  cor_2 = cor.test(liver_patient$Total_Bilirubin, liver_patient$Direct_Bilirubin)
  
  cat(blue(bold("Null hypothesis, H0:")), "Correlation of two atributes is 0.\n")
  cat(blue(bold("Alternating hypothesis, H1:")), "True correlation is non zero.\n\n")
  
  cf = 0.95 # confidence interval
  
  # printing result of the testing  
  if(cor_2$p.value <= 1 - cf){
    cat(bold("Total bilirubin and Direct bilirubin are", underline("correlated"), ".\n"))
    cat("Correlation: ", green(bold(cor(liver_patient$Total_Bilirubin, liver_patient$Direct_Bilirubin))), "\n\n")
  }else{
    cat(bold("Total bilirubin and Direct bilirubin are not correlated.\n\n"))
  }
  
  # plottings
  plot(liver_patient$Total_Bilirubin, liver_patient$Direct_Bilirubin, pch = 19, col = "blue",
       main = "Total bilirubin vs direct bilirubin", xlab = "Total bilirubin", ylab = "Direct bilirubin"
  )
  abline(lm(liver_patient$Direct_Bilirubin ~ liver_patient$Total_Bilirubin, data = liver_patient), col = "red")
}

clustering <- function()
{
  par(mfrow = c(1, 2))
  d = liver_patient[, c(6, 7)]
  kmeans.result <- kmeans(d, centers = 1)
  
  centers <- kmeans.result$centers[kmeans.result$cluster, ]
  distances <- sqrt(rowSums((d - centers)^2))
  outliers <- order(distances, decreasing = T)[1:2]
  
  #print(outliers)
  #print(d[outliers,])
  
  plot(liver_patient$Alamine_Aminotransferase, liver_patient$Aspartate_Aminotransferase, pch = 19, main = "Aspartate aminotransferase vs Albumin", xlab = "Aspartate aminotransferase", ylab = "Albumin")
  #points(kmeans.result$centers[,c("Aspartate_Aminotransferase", "Albumin")], col = c("blue", "red", "green"), pch = 15, cex = 2)
  points(d[outliers, c("Alamine_Aminotransferase", "Aspartate_Aminotransferase")], pch = 19, col = "green")
  legend("topright", legend=c("Outliers"), col=c("green"), pch = 19, cex = 1)
  
  
  d = liver_patient[, c(3, 4)]
  kmeans.result <- kmeans(d, centers = 3)
  
  centers <- kmeans.result$centers[kmeans.result$cluster, ]
  distances <- sqrt(rowSums((d - centers)^2))
  outliers <- order(distances, decreasing = T)[1]
  
  #print(outliers)
  #print(d[outliers,])
  
  plot(liver_patient$Total_Bilirubin, liver_patient$Direct_Bilirubin, pch = 19, main = "Total Bilirubin vs Direct Bilirubin", xlab = "Total Bilirubin", ylab = "Direct Bilirubin")
  #points(kmeans.result$centers[,c("Total_Bilirubin", "Direct_Bilirubin")], col = c("blue", "red", "green"), pch = 15, cex = 2)
  points(d[outliers, c("Total_Bilirubin", "Direct_Bilirubin")], pch = 19, col = "green")
  legend("topright", legend=c("Outliers"), col=c("green"), pch = 19, cex = 1)
  
  
}




correlation_testing()

clustering()


