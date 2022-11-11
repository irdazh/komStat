passcheck = function(password){
  
  # ascii code thingy
  kap = 65:90
  non = 97:122
  ang = 48:57
  sim = c(33,35,36,38,94,95)
  val = c(kap, non, ang, sim)
  
  # read unsafe pw
  unsafe = read.delim("data/no2uts.txt",header = F)
  
  pw = password
  x = utf8ToInt(pw)
  
  #syarat rendah
  r1 = sum(x %in% val) >= 8
  
  #syarat sedang
  s2 = sum(x %in% val) >= 10
  s3 = sum(x %in% ang) >= 1
  s4 = sum(x %in% kap) >= 1
  s5 = sum(x %in% non) >= 1
  s6 = sum(x %in% sim) >= 1
  
  #syarat tinggi
  t2 = sum(x %in% val) >= 12
  t3 = sum(pw == unsafe) == 0
  
  k = if (s2+s3+s4+s5+s6==5){
    if (t2&t3) "tinggi"
    else "sedang"
  } else if (r1) "rendah" else "tidak_valid"
  
  return(k)
  
}

#Jalankan syntax di bawah setelah fungsi selesai dibuat, kemudian lampirkan file OUTPUT_NIU.csv.
list_password = c('KOMSTAT', 'PenjagaJanji', '1Januari1980', 'jessica123', 'tikusbertopi45', 'FireFox!123', 'cleop4tr4', 'citrusjuice', 'muhammadtahta', 'Alex44T0x',
                  'andoconnect', 'GLORYGLORYMANUNITED!!!', '44+66=100', 'Lorem Ipsum', 'emyu_10hag', 'kursiGoyangkakekku', 'RubYMonsTer9', 'asal_ikut', 'symph0ny@museum', '109690101010',
                  'rubikCubeSpeed', 'MOMGETtheCAMERA!!!!1!', 'KLouisss5', 'iLovePDF.jpg', 'rumble_rac3R', 'dan1p2k3', 'koMstAt_LanJuT%', 'lesparedes', 'HeadsetG@m1ng', 'LCar-9', '1,1,1,1',
                  'Y0zjhF!j&0)', 'SeeeeN0#o13', 'thelegendofR', 'Gambar3Dimen$i', 'kriptonium_', 'Helsinki-rac3r', 'PunyaPacr', 'passsword', 'carla18cardy', 'Boiling_Wat@r', 'trustw0rThy')
result = c()
for (i in list_password){
  result = c(result, passcheck(i)) 
}
df = data.frame('password' = list_password, 'result' = result)
df

write.csv(df, 'OUTPUT_474059.csv')

table(df$result)


#####SAMPAH--------------------------------------------

# #Valid:
# - capital: 65-90
# - noncap: 97-122
# - angka: 48-57
# - simbol: 33,35,36,38,94,95

# ascii code thingy
kap = 65:90
non = 97:122
ang = 48:57
sim = c(33,35,36,38,94,95)
val = c(kap, non, ang, sim)

# read unsafe pw
unsafe = read.delim("data/no2uts.txt",header = F)

# check whether unsafe or yeah
sum("welcome" == unsafe) == 0

# function

passcheck = function(pw="abc 5 dasar!"){
  x = utf8ToInt(pw)
  
  #syarat rendah
  r1 = sum(x %in% val) >= 8
  
  #syarat sedang
  s2 = sum(x %in% val) >= 10
  s3 = sum(x %in% ang) >= 1
  s4 = sum(x %in% kap) >= 1
  s5 = sum(x %in% non) >= 1
  s6 = sum(x %in% sim) >= 1
  
  #syarat tinggi
  t2 = sum(x %in% val) >= 12
  t3 = sum(pw == unsafe) == 0
  
  k = if (s2+s3+s4+s5+s6==5){
    if (t2&t3) "tinggi"
    else "sedang"
  } else if (r1) "rendah" else "tidak_valid"
  
  return(k)
}

passcheck()

file.edit("data/no2uts_test.R")

