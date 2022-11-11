
# Nomor 4: Uwuwuuw --------------------------------------------------------

#fungsi jarak
jarak = function(m1, m2, p) {
  apply(abs(m1-m2)^p, 1, sum)^(1/p)
}

#read data and matrix
df4 = scan(text="
159    176    168    171    174    172    180    176    164    170    169    183    155    159    177    181    179    163    165    167
81    76    60    55    68    73    84    91    75    87    57 56    59    78    88    86    83    53    63    82")

mt4 = matrix(c(df4, rep(1, 20)), ncol=3, byrow=F)
mt4

#k_medoid func
#medoid sy ubh utk memudahkan perhitungan eak
kMedoid = function(data, medoid=c(140,40,190,80),
                   distance=c("Euclidian", "Manhattan"),
                   iterations=5){
  
  pK = ifelse(substr(distance,1,3)=="Euc", 2, 1)
  
  for (i in 1:iterations){
    
    medoid = matrix(rep(medoid, 20), ncol=4, byrow=T)
    dis1 = jarak(data[,1:2], medoid[,1:2], p=pK)
    dis2 = jarak(data[,1:2], medoid[,3:4], p=pK)
    
    data[,3] = ifelse(dis1>dis2, 2, 1)
    
    #medoid: med1tb, med2tb
    medoid = c(
      apply(data[data[,3]==1, 1:2], 2, median),
      apply(data[data[,3]==2, 1:2], 2, median)
    )
  }
  cat("K-Medoid Clustering \nDistance yang digunakan:",
      distance, "\nIterasi:", iterations,
      "\nBuat 2 cluster dengan medoid masing-masing cluster:",
      "\n  Medoid CLuster 1:", medoid[1:2],
      "\n  Medoid CLuster 2:", medoid[3:4],
      "\nData hasil clustering:\n")
  print(data) #langsung print
  
  war = c("green", "blue")
  pl = #bikin plot
    plot(data[,1], data[,2], pch=20, col=war[data[,3]],
         xlab="Tinggi Badan", ylab="Berat Badan",
         main=paste(distance, iterations, "Iterasi"))
  legend("topleft", title="Keterangan",
         legend = c("Cluster 1", "Cluster 2", "Medioid"),
         col = c(war, "tomato"), cex=0.5,
         y.intersp = 0.7, pch=c(20,20,4), pt.cex = 0.8)
  points(medoid[c(1,3)], medoid[c(2,4)], pch=4,
         col=war)
  
  print(pl)
  #return(data) #kembaliin data if u want eak
}

kMedoid(mt4, distance = "Manhattan", iter=1)
kMedoid(mt4, distance = "Manhattan", iter=2)
kMedoid(mt4, distance = "Manhattan", iter=5)

kMedoid(mt4, medoid = c(150,80, 195,80),
        distance = "Manhattan", iter=5)