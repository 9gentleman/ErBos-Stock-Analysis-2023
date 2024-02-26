# Burada dosya uzantı yolu verilmiştir Aşşağıda ki kısma 
# C:/Users/Dell yerine kendi kullanıcı adınızı yazınız
# Bu işlemi de bu excel dosyalarınızı masaüstüne attıktan sonra gerçekleştirin
library(readxl)
ERBOS_Geçmiş_Verileri <- read_excel("C:/Users/Dell/Desktop/Home/academy/Yüksek lisans/İstatistiğe Giriş/İstatistiğe Giriş Proje/ERBOS-Geçmiş-Verileri-noktalı.xlsx")
library(readxl)
BTC_verileri <- read_excel("C:/Users/Dell/Desktop/Home/academy/Yüksek lisans/İstatistiğe Giriş/İstatistiğe Giriş Proje/BTC-verileri.xlsx")

install.packages("e1071")
install.packages("moments")
install.packages("car")

library(moments)
library(car)

Erbos_Acılıs<-ERBOS_Geçmiş_Verileri$Açılış
Erbos_kapanıs<-ERBOS_Geçmiş_Verileri$Şimdi
BTC_kapanıs<-BTC_verileri$Şimdi

data_BTC_Kapanıs=data.frame(BTC_kapanıs)
data_Erbos_Acılıs=data.frame(Erbos_Acılıs)
data_Erbos_Kapanıs=data.frame(Erbos_kapanıs)

erbos_acilis <- data_Erbos_Acılıs$Erbos_Acılıs
erbos_kapanis <- data_Erbos_Kapanıs$Erbos_kapanıs
BTC_kapanis <- data_BTC_Kapanıs$BTC_kapanıs


# A ŞIKKI --------------------------------------
# Tablolar
erbos_acilis_tablo<-table(Erbos_Acılıs)
erbos_kapanis_tablo<-table(Erbos_kapanıs)

# Ortalama Hesaplama
erbos_acilis_mean<-mean(erbos_acilis) 
erbos_kapanis_mean<-mean(erbos_kapanis) 

# Medyan Hesaplama
erbos_acilis_median<-median(erbos_acilis) 
erbos_kapanis_median<-median(erbos_kapanis) 

# Mod Hesaplama
erbos_acilis_mod<-names(erbos_acilis_tablo)[which(erbos_acilis_tablo==max(erbos_acilis_tablo))] #Açılış için mod hesabı
erbos_kapanis_mod<-names(erbos_kapanis_tablo)[which(erbos_kapanis_tablo==max(erbos_kapanis_tablo))] #Kapanış için mod hesabı

# Çeyreklikler Hesaplama
erbo_acilis_quantile <- quantile(erbos_acilis,probs = c(0.10,0.25,0.75,0.90)) #Açılış için 10,25,75,90 çeyreklikler hesabı
erbos_kapanis_quantile <- quantile(erbos_kapanis,probs = c(0.10,0.25,0.75,0.90))#Kapanış için 10,25,75,90 çeyreklikler hesabı
# --------------------------------------
# C ŞIKKI --------------------------------------
# Histogram Grafiği
hist(erbos_acilis,main = paste("Histogram of ERBOS-AC"),xlim = c(130,200),ylim = c(0,20)) #Açılış için Histogram Graph
hist(erbos_kapanis,main = paste("Histogram of ERBOS_KA"),xlim = c(130,200),ylim = c(0,20))#Kapanış için Histogram Graph

# Birikimli Sıklık Grafiği
birikimli_sıklık_acilis<-cumsum(erbos_acilis_tablo)
birikimli_sıklık_kapanis<-cumsum(erbos_kapanis_tablo)
plot(names(birikimli_sıklık_acilis), birikimli_sıklık_acilis, type = "l", col = "blue", xlab = "Değerler", ylab = "Birikimli Sıklık", main = "Birikimli Sıklık Grafiği") # birikimli sıklık grafiğini açılış için çizdiririz
plot(names(birikimli_sıklık_kapanis), birikimli_sıklık_kapanis, type = "l", col = "blue", xlab = "Değerler", ylab = "Birikimli Sıklık", main = "Birikimli Sıklık Grafiği") # birikimli sıklık grafiğini kapanış için çizdiririz

# Trend Grafiği
tarih<-seq(1,62,1)
erbos_acilis_trend_graph<-plot(tarih,erbos_acilis,type = "o",main = "Erbos'un Açılışının Değişimi",xlab = "Tarihler (27.09.23 : 03.07.2023)",ylab ="ERBOS Açılış değerleri" ) #trend grafiği açılış
erbos_kapanis_trend_graph<-plot(tarih,erbos_kapanis,type="o",main = "Erbos'un Kapanışının Değişimi",xlab ="Tarihler (27.09.23 : 03.07.2023)",ylab ="ERBOS Kapanış değerleri" ) #trend grafiği kapanış
# --------------------------------------
# F Şıkkı --------------------------------------
# Üstel büyüme denklemi ile tahmin bulma 
P0 <- 146      # Başlangıç değeri
SonDeger <- 187 # Son değer
t <- 62         # r hesaplamak için t
t_1 <- 365 # tahmin edilen değer için t

# Yıllık büyüme oranını hesapla
r <- log(SonDeger / P0) / t

# Tahmin edilen değeri hesapla
tahmin_edilen_deger <- P0 * exp(r * t_1)

# Sonuçları yazdır
cat("Yıllık Büyüme Oranı (r):", r, "\n")
cat("365 gün yani 1 yıl sonra olan Erbos Tahmini.\nTahmin Edilen Değer:", tahmin_edilen_deger, "\n")
#----------------------------------------
# B ŞIKKI --------------------------------------
# Varyans Hesaplama
erbos_acilis_varyans <- var(erbos_acilis)
erbos_kapanis_varyans <- var(erbos_kapanis) 

# Standart Sapma Hesaplama
erbos_acilis_sd <- sd(erbos_acilis)  
erbos_kapanis_sd <- sd(erbos_kapanis)

# standart hatalar
erbos_acilis_standartHata <- sd(erbos_acilis)/sqrt(length(erbos_acilis))
erbos_kapanis_standartHata <- sd(erbos_kapanis)/sqrt(length(erbos_kapanis))

# skewness hesaplama
erbos_acilis_skewness <- skewness(erbos_acilis)
erbos_kapanis_skewness <- skewness(erbos_kapanis)

# kurtosis
erbos_acilis_kurtosis <- kurtosis(erbos_acilis)
erbos_kapanis_kurtosis <- kurtosis(erbos_kapanis)

# Bowley'in asimetri ölçütünü hesaplama
Q1_erbos_acilis<-149.80 
Q2_erbos_acilis<-154.55
Q3_erbos_acilis<-150.50
bow_erbos_acilis<-(Q3_erbos_acilis+Q1_erbos_acilis-2*Q2_erbos_acilis)/(Q3_erbos_acilis-Q1_erbos_acilis)

Q1_erbos_kapanis<-149.50 
Q2_erbos_kapanis<-154.35
Q3_erbos_kapanis<-160.05
bow_erbos_kapanis<-(Q3_erbos_kapanis+Q1_erbos_kapanis-2*Q2_erbos_kapanis)/(Q3_erbos_kapanis-Q1_erbos_kapanis)

# Pearson'ın asimetri ölçütünü hesaplama
Pear_ebos_acilis <- (erbos_acilis_mean- as.integer(erbos_acilis_mod))/erbos_acilis_sd
Pear_ebos_kapanis <- (erbos_kapanis_mean - 140.4433)/erbos_kapanis_sd
# --------------------------------------
# D ŞIKKI --------------------------------------
# Kutu Grafiği çizimi ve aykırı değer hesaplama
erbos_acilis_boxPlot<-boxplot(erbos_acilis,col = "bisque")
cat("Açılış için AYKIRI DEĞER : ",erbos_acilis_boxPlot$out)

erbos_kapanis_boxPlot<-boxplot(erbos_kapanis,col = "bisque")
cat("Kapanış için AYKIRI DEĞERLER : ",erbos_kapanis_boxPlot$out)
# --------------------------------------
# E ŞIKKI --------------------------------------
# Değişim Katsayısını Hesaplama ERBOS 
erbos_acilis_degisimKatsayısı<-(erbos_acilis_sd/erbos_acilis_mean)*100
erbos_kapanis_degisimKatsayısı<-(erbos_kapanis_sd/erbos_kapanis_mean)*100

# BTC için Değişim katsayısı
BTC_kapanis_degisimKatsayısı<-(sd(BTC_kapanis)/mean(BTC_kapanis))*100

#BTC Kapanış ve ERBOS kapanış kıyaslaması
cat("BTC Kapanış değişim katsayısı : ",BTC_kapanis_degisimKatsayısı,"\nERBOS kapanış değişim katsayısı :",erbos_kapanis_degisimKatsayısı,
    "\n BTC'nin katsayısı daha fazla olduğu için \n  BTC EBOS'a göre daha HETEROJENDİR!","\nAradaki fark :",BTC_kapanis_degisimKatsayısı- erbos_kapanis_degisimKatsayısı)
# --------------------------------------