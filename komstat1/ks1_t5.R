dfNilai = data.frame(
  No = 1:5,
  NIM = 10000+c(1436, 1616, 1734, 2230, 1832),
  NAMA = c("ADI", "siti", "ika", "LEO", "Ihram"),
  MID = c(75,55,75,75,75),
  UAS = c(32,47,49,21,59),
  TUGAS1 = c(rep(0,3), 58, 0),
  TUGAS2 = c(rep(0,3), 50, 0),
  TUGAS3 = c(66,70,62,45,67)
)

dfNilai

nA = with(dfNilai, 0.4*UAS+0.3*MID+0.2*(TUGAS1+TUGAS2+TUGAS3)/3+10)
nA = round(nA, 2)
nA

fKonversi = function(df){
  df$NAkhir = with(df, 0.4*UAS+0.3*MID+0.2*(TUGAS1+TUGAS2+TUGAS3)/3+10)
  df$NAkhir = round(df$NAkhir,2)
  df$NHuruf = NULL
  
  for (i in 1:nrow(df)){
    df[i, "NHuruf"] = 
      with(df[i,], 
           if (NAkhir >= 80) "A" else
             if (NAkhir >= 65) "B" else
               if (NAkhir >= 50) "C" else "D")
  }
  
    return(df)
}

fKonversi(dfNilai) #ISI DENGAN NAMA TABEL


volume_bola <- function(){
  #Volume bola
  print("===Mencari Volume Bola===")
  radius = as.double(readline ("Masukkan jari-jari : "))
  
  volume <- function(){
    v = (4/3)*pi*(radius)^3
    return(v)
  }
  
  cat("Dengan jari-jari", radius, "volume bola adalah", volume())
}

volume_bola()

plot(TUGAS3~NIM, dfNilai)
text(locator(2), "uwu, dang em",)

