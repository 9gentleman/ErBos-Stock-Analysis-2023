install.packages("e1071")
install.packages("moments")
install.packages("car")

library(moments)
library(car)
library(readxl)

Erbos_opening<-ERBOS_GeC'miE_Verileri$AC'D1lD1E
Erbos_closing<-ERBOS_GeC'miE_Verileri$Eimdi
BTC_kapanD1s<-BTC_verileri$Eimdi

data_BTC_KapanD1s=data.frame(BTC_kapanD1s)
data_Erbos_opening=data.frame(Erbos_opening)
data_Erbos_closing=data.frame(Erbos_closing)

erbos_acilis <- data_Erbos_opening$Erbos_opening
erbos_kapanis <- data_Erbos_closing$Erbos_closing
BTC_kapanis <- data_BTC_KapanD1s$BTC_kapanD1s


# Table
erbos_acilis_tablo<-table(Erbos_opening)
erbos_kapanis_tablo<-table(Erbos_closing)

# Mean
erbos_acilis_mean<-mean(erbos_acilis) 
erbos_kapanis_mean<-mean(erbos_kapanis) 

# Median
erbos_acilis_median<-median(erbos_acilis) 
erbos_kapanis_median<-median(erbos_kapanis) 

# Mod
erbos_acilis_mod<-names(erbos_acilis_tablo)[which(erbos_acilis_tablo==max(erbos_acilis_tablo))] #AC'D1lD1E iC'in mod hesabD1
erbos_kapanis_mod<-names(erbos_kapanis_tablo)[which(erbos_kapanis_tablo==max(erbos_kapanis_tablo))] #KapanD1E iC'in mod hesabD1

# Quantiles
erbo_acilis_quantile <- quantile(erbos_acilis,probs = c(0.10,0.25,0.75,0.90)) #AC'D1lD1E iC'in 10,25,75,90 C'eyreklikler hesabD1
erbos_kapanis_quantile <- quantile(erbos_kapanis,probs = c(0.10,0.25,0.75,0.90))#KapanD1E iC'in 10,25,75,90 C'eyreklikler hesabD1


# Histogram Graph
hist(erbos_acilis,main = paste("Histogram of ERBOS-AC"),xlim = c(130,200),ylim = c(0,20)) #AC'D1lD1E iC'in Histogram Graph
hist(erbos_kapanis,main = paste("Histogram of ERBOS_KA"),xlim = c(130,200),ylim = c(0,20))#KapanD1E iC'in Histogram Graph

# Cumalitve Frequences Graph
birikimli_sD1klD1k_acilis<-cumsum(erbos_acilis_tablo)
birikimli_sD1klD1k_kapanis<-cumsum(erbos_kapanis_tablo)
plot(names(birikimli_sD1klD1k_acilis), birikimli_sD1klD1k_acilis, type = "l", col = "blue", xlab = "DeDerler", ylab = "Birikimli SD1klD1k", main = "Birikimli SD1klD1k GrafiDi") # birikimli sD1klD1k grafiDini aC'D1lD1E iC'in C'izdiririz
plot(names(birikimli_sD1klD1k_kapanis), birikimli_sD1klD1k_kapanis, type = "l", col = "blue", xlab = "DeDerler", ylab = "Birikimli SD1klD1k", main = "Birikimli SD1klD1k GrafiDi") # birikimli sD1klD1k grafiDini kapanD1E iC'in C'izdiririz

# Trend Graph
tarih<-seq(1,62,1)
erbos_acilis_trend_graph<-plot(tarih,erbos_acilis,type = "o",main = "Erbos'un AC'D1lD1ED1nD1n DeDiEimi",xlab = "Tarihler (27.09.23 : 03.07.2023)",ylab ="ERBOS AC'D1lD1E deDerleri" ) #trend grafiDi aC'D1lD1E
erbos_kapanis_trend_graph<-plot(tarih,erbos_kapanis,type="o",main = "Erbos'un KapanD1ED1nD1n DeDiEimi",xlab ="Tarihler (27.09.23 : 03.07.2023)",ylab ="ERBOS KapanD1E deDerleri" ) #trend grafiDi kapanD1E



# The improvments prediction
P0 <- 146      # BaElangD1C' deDeri
SonDeger <- 187 # Son deDer
t <- 62         # r hesaplamak iC'in t
t_1 <- 365 # tahmin edilen deDer iC'in t

# Annual growth rate calculation
r <- log(SonDeger / P0) / t

# calculate the predictioning value
tahmin_edilen_deger <- P0 * exp(r * t_1)


cat("YD1llD1k BC<yC<me OranD1 (r):", r, "\n")
cat("365 gC<n yani 1 yD1l sonra olan Erbos Tahmini.\nTahmin Edilen DeDer:", tahmin_edilen_deger, "\n")


# Variance
erbos_acilis_varyans <- var(erbos_acilis)
erbos_kapanis_varyans <- var(erbos_kapanis) 

# Standart Deviation
erbos_acilis_sd <- sd(erbos_acilis)  
erbos_kapanis_sd <- sd(erbos_kapanis)

# Standart Error
erbos_acilis_standartHata <- sd(erbos_acilis)/sqrt(length(erbos_acilis))
erbos_kapanis_standartHata <- sd(erbos_kapanis)/sqrt(length(erbos_kapanis))

# Skewness
erbos_acilis_skewness <- skewness(erbos_acilis)
erbos_kapanis_skewness <- skewness(erbos_kapanis)

# Kurtosis
erbos_acilis_kurtosis <- kurtosis(erbos_acilis)
erbos_kapanis_kurtosis <- kurtosis(erbos_kapanis)

# Bowley's Coefficient
Q1_erbos_acilis<-149.80 
Q2_erbos_acilis<-154.55
Q3_erbos_acilis<-150.50
bow_erbos_acilis<-(Q3_erbos_acilis+Q1_erbos_acilis-2*Q2_erbos_acilis)/(Q3_erbos_acilis-Q1_erbos_acilis)

Q1_erbos_kapanis<-149.50 
Q2_erbos_kapanis<-154.35
Q3_erbos_kapanis<-160.05
bow_erbos_kapanis<-(Q3_erbos_kapanis+Q1_erbos_kapanis-2*Q2_erbos_kapanis)/(Q3_erbos_kapanis-Q1_erbos_kapanis)

# Pearson Coefficient
Pear_ebos_acilis <- (erbos_acilis_mean- as.integer(erbos_acilis_mod))/erbos_acilis_sd
Pear_ebos_kapanis <- (erbos_kapanis_mean - 140.4433)/erbos_kapanis_sd


# Box-Plot Grahp and Outlier detection
erbos_acilis_boxPlot<-boxplot(erbos_acilis,col = "bisque")
cat("AC'D1lD1E iC'in AYKIRI DEDER : ",erbos_acilis_boxPlot$out)

erbos_kapanis_boxPlot<-boxplot(erbos_kapanis,col = "bisque")
cat("KapanD1E iC'in AYKIRI DEDERLER : ",erbos_kapanis_boxPlot$out)


# Coefficient of Variation
erbos_acilis_degisimKatsayD1sD1<-(erbos_acilis_sd/erbos_acilis_mean)*100
erbos_kapanis_degisimKatsayD1sD1<-(erbos_kapanis_sd/erbos_kapanis_mean)*100

# Coefficient of Variation for BTC
BTC_kapanis_degisimKatsayD1sD1<-(sd(BTC_kapanis)/mean(BTC_kapanis))*100

#Coefficient of Variation for ERBOS
cat("BTC KapanD1E deDiEim katsayD1sD1 : ",BTC_kapanis_degisimKatsayD1sD1,"\nERBOS kapanD1E deDiEim katsayD1sD1 :",erbos_kapanis_degisimKatsayD1sD1,
    "\n BTC'nin katsayD1sD1 daha fazla olduDu iC'in \n  BTC EBOS'a gC6re daha HETEROJENDD0R!","\nAradaki fark :",BTC_kapanis_degisimKatsayD1sD1- erbos_kapanis_degisimKatsayD1sD1)
