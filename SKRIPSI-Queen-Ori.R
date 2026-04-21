data <- read.csv("D:/SKRIPSI/REGRESI/Data/Data Fix/DataFix7P.csv")
data

summary(data)

var(data$X3_TPAK, na.rm = TRUE)
sd(data$X3_TPAK, na.rm = TRUE)


# Cek struktur data sebelum konversi
str(data)

#Cek Missing Value
colSums(is.na(data))

##========================##NORMALISASI##========================##
# Fungsi normalisasi Min-Max
min_max_norm <- function(x) {
  return ((x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE)))
}

# Hanya normalisasi untuk kolom ke-4 hingga ke-12 (asumsi semua numerik)
data[, 4:14] <- lapply(data[, 4:14], function(col) {
  if (is.numeric(col)) {
    return(min_max_norm(col))
  } else {
    return(col)  # Jika ada non-numerik, tetap biarkan
  }
})
##========================####========================##
##========================####========================##

# Baca shapefile
library(sf)
prov <- st_read("D:/SKRIPSI/REGRESI/File Ori Peta Indonesia/batas_provinsi_desember_2019_dukcapil.shp")
prov

#Cek penamaan provinsi
setdiff(unique(prov$PROVINSI), unique(data$PROVINSI))

#Merge Data
library(dplyr)
merged_data <- left_join(prov, data, by = c("PROVINSI" = "PROVINSI"))
merged_data

#========================##Diagnostik Regresi##========================
#=================#Model regresi pada data asli#=================#
model_ols <- lm(Y_log ~ X1_PAD + X2_IPM + X3_TPAK + X4_AKSESLISTRIK +
                  X5_TELEPONSELULER + X11_JP + X7_IMK + X6_WISATAWAN , data = merged_data)

# Tampilkan ringkasan hasil model
print(summary(model_ols))

#=================#Uji normalitas residual#=================#lolos uji asumsi setelah transformasi log
shapiro.test(residuals(model_ols))
# data:  residuals(model_ols)
# W = 0.98128, p-value = 0.8123

#cek normalisasi y
shapiro.test(merged_data$Y_PDRB)
# W = 0.62881, p-value = 4.688e-08

shapiro.test(merged_data$Y_log)
# W = 0.94605, p-value = 0.09346

# data:  residuals(model_ols)
# W = 0.93291, p-value = 0.03805
# Karena p-value = 0.03805 < 0.05 maka tolak H0 yang berarti distribusi tidak normal
hist(merged_data$Y_PDRB) #skew
hist(merged_data$Y_log)

# Mengatur layout grafik menjadi 3 baris dan 3 kolom
par(mfrow = c(3, 3))

# --- Plot Histogram Anda ---

# 1. Histogram untuk X1_PAD
hist(merged_data$X1_PAD, 
     main = "Histogram X1_PAD", 
     xlab = "Nilai X1_PAD", 
     col = "lightblue")

# 2. Histogram untuk X2_IPM
hist(merged_data$X2_IPM, 
     main = "Histogram X2_IPM", 
     xlab = "Nilai X2_IPM", 
     col = "salmon")

# 3. Histogram untuk variabel ke-3 (GANTI NAMA VARIABELNYA)
hist(merged_data$X3_TPAK, 
     main = "Histogram X3_TPAK", 
     xlab = "Nilai X3_TPAK", 
     col = "lightgreen")

# 4. Histogram untuk variabel ke-4 (GANTI NAMA VARIABELNYA)
hist(merged_data$X4_AKSESLISTRIK, 
     main = "Histogram X4_AKSESLISTRIK", 
     xlab = "Nilai X4_AKSESLISTRIK", 
     col = "gold")

# 5. Histogram untuk variabel ke-5 (GANTI NAMA VARIABELNYA)
hist(merged_data$X5_TELEPONSELULER, 
     main = "Histogram X5_TELEPONSELULER", 
     xlab = "Nilai X5_TELEPONSELULER", 
     col = "orchid")

# 6. Histogram untuk variabel ke-6 (GANTI NAMA VARIABELNYA)
hist(merged_data$X11_JP, 
     main = "Histogram X11_JP", 
     xlab = "Nilai X11_JP", 
     col = "skyblue")

# 7. Histogram untuk variabel ke-7 (GANTI NAMA VARIABELNYA)
hist(merged_data$X7_IMK, 
     main = "Histogram X7_IMK", 
     xlab = "Nilai X7_IMK", 
     col = "tomato")


# (Sangat disarankan) Mengembalikan pengaturan layout ke default
par(mfrow = c(1, 1))


# Buat kolom baru Y_log agar data asli tetap ada
merged_data$Y_log <- log(merged_data$Y_PDRB)

# regresi linier dengan Y_log sebagai respon
model_ols_x6x_log <- lm(Y_log ~ X1_PAD + X2_IPM + X3_TPAK + X4_AKSESLISTRIK +
                      X5_TELEPONSELULER + X7_IMK + X11_JP, data = merged_data)

shapiro.test(residuals(model_ols_x6x_log))
# data:  residuals(model_ols_log)
# W = 0.97671, p-value = 0.6666
# p-value > 0,05 = data berdistribusi normal

#=================#Uji Heteroskedastisitas#=================#lolos uji asumsi
library(lmtest)
# Uji Breusch-Pagan untuk ols
bptest(model_ols)
# data:  model_ols
# BP = 7.1421, df = 7, p-value = 0.4142
#Karena p-value = 0.4142 > 0.05, maka kita gagal menolak H₀.
#Homoskedastisitas — varians residual adalah konstan
#tidak ada heterogenitas spasial

bptest(model_ols_log)
# data:  model_ols_log
# BP = 4.8079, df = 7, p-value = 0.6834
#Karena p-value = 0.6834 > 0.05, maka kita gagal menolak H₀.
#Homoskedastisitas — varians residual adalah konstan
#tidak ada heterogenitas spasial

#===============# Cek pake Glesjer #===============#
# Glejser test
# 1. Hitung residual dan absolute residual
res  <- resid(model_ols_x6x)
abs_residu <- abs(res)

# 3. Buat data frame untuk regresi Glejser
glejser_data <- data.frame(
  abs_residu = abs_residu,
  X1_PAD = merged_data$X1_PAD,
  X2_IPM = merged_data$X2_IPM,
  X3_TPAK = merged_data$X3_TPAK,
  X4_AKSESLISTRIK = merged_data$X4_AKSESLISTRIK,
  X5_TELEPONSELULER = merged_data$X5_TELEPONSELULER,
  X7_IMK = merged_data$X7_IMK,
  X11_JP = merged_data$X11_JP
)

# 4. Regresi Glejser: Nilai absolut residu terhadap variabel independen
glejser_model <- lm(abs_residu ~ X1_PAD + X2_IPM + X3_TPAK + X4_AKSESLISTRIK + 
                      X5_TELEPONSELULER + X7_IMK + X11_JP, data = glejser_data)

# 5. Tampilkan hasil regresi Glejser
summary(glejser_model)

# 6. (Opsional) Uji signifikansi keseluruhan
anova(glejser_model)

# Call:
#   lm(formula = abs_residu ~ X1_PAD + X2_IPM + X3_TPAK + X4_AKSESLISTRIK + 
#        X5_TELEPONSELULER + X7_IMK + X11_JP, data = glejser_data)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -0.42330 -0.17335 -0.02112  0.14797  0.48323 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)  
# (Intercept)         0.3801     0.3553   1.070   0.2945  
# X1_PAD             -0.4134     0.3877  -1.066   0.2961  
# X2_IPM             -0.1911     0.3953  -0.483   0.6328  
# X3_TPAK            -0.2688     0.2250  -1.195   0.2430  
# X4_AKSESLISTRIK     0.5793     0.7519   0.770   0.4480  
# X5_TELEPONSELULER  -0.3191     0.7915  -0.403   0.6901  
# X7_IMK             -1.1639     0.6115  -1.903   0.0681 .
# X11_JP              1.3522     0.6307   2.144   0.0415 *
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.2706 on 26 degrees of freedom
# Multiple R-squared:  0.2778,	Adjusted R-squared:  0.08332 
# F-statistic: 1.428 on 7 and 26 DF,  p-value: 0.2365

# P-value = 0.2365 > 0.05 ⇒ gagal tolak𝐻0​
# ⇒ tidak ada bukti heteroskedastisitas (homoskedastisitas).

#=================#Menghitung VIF#=================# 2 variabel >10
library(car)
vif(model_ols)

# X1_PAD              X2_IPM           X3_TPAK   X4_AKSESLISTRIK X5_TELEPONSELULER 
# 2.758023          2.527718          1.505396          8.066445          7.582594 
# X6_WISATAWAN        X7_IMK 
# 15.733480         13.391282 

vif(model_ols_log)
# X1_PAD              X2_IPM           X3_TPAK   X4_AKSESLISTRIK X5_TELEPONSELULER 
# 2.758023          2.527718          1.505396          8.066445          7.582594 
# X6_WISATAWAN         X7_IMK 
# 15.733480         13.391282

#=================#uji asumsi autokorelasi#=================#lolos uji asumsi
library(lmtest)
#uji Durbin Watson
dwtest(model_ols)
# data:  model_ols
# DW = 2.264, p-value = 0.7844
# alternative hypothesis: true autocorrelation is greater than 0
#Nilai DW = 2.264, artinya:
#Tidak ada indikasi kuat adanya autokorelasi residual dalam model.
#p-value = 0.7844 > 0.05, artinya:
#Tidak cukup bukti untuk menolak hipotesis nol (H₀), berarti tidak ada autokorelasi.


dwtest(model_ols_log)
# data:  model_ols_log
# DW = 1.8978, p-value = 0.381
# alternative hypothesis: true autocorrelation is greater than 0

#=================#Seleksi variabel#=================#
# Seleksi variabel dengan stepAIC (library(MASS))
library(MASS)
step_model_ols <- stepAIC(model_ols, 
                              direction = "both", 
                              scope = list(lower = ~X2_IPM, upper = ~X1_PAD + X2_IPM + X3_TPAK + X4_AKSESLISTRIK + X5_TELEPONSELULER + X6_WISATAWAN + X7_IMK), 
                              trace = FALSE)
summary(step_model_ols)

# Seleksi variabel dengan menghapus Variabel VIF tinggi satu persatu
model_ols_x6x <- lm(Y_log ~ X1_PAD + X2_IPM + X3_TPAK + X4_AKSESLISTRIK +
                          X5_TELEPONSELULER + X7_IMK + X11_JP, data = merged_data)
summary(model_ols_x6x)

vif(model_ols)
vif(model_ols_x6x)
# X1_PAD            X2_IPM           X3_TPAK 
# 2.442069          2.592037         1.596621 
# X4_AKSESLISTRIK X5_TELEPONSELULER     X7_IMK 
# 8.127528          7.659166          9.204698 
# X11_JP 
# 9.997929 
vif(model_ols_x6x_log)
shapiro.test(residuals(model_ols_x6x))
bptest(model_ols_x6x)
dwtest(model_ols_x6x)

#========================##Pembuatan Bobot Spasial##========================
library(spdep)

# perbaiki geometri yang tidak valid
merged_data <- st_make_valid(merged_data) 

# Buat daftar tetangga (neighbors) menggunakan contiguity Queen menggunakan GEODA
# Baca file .gal modifikasi
gal_file_ori <- "D:/SKRIPSI/REGRESI/File Ori Peta Indonesia/queen-ori.gal"
nb_queen_ori <- read.gal(gal_file_ori, region.id = merged_data$OBJECTID)

# Cek konektivitas baru
summary(nb_queen_ori)  # Pastikan tidak ada wilayah dengan 0 tetangga

# Buat bobot spasial (jika ingin digunakan untuk regresi spasial)
weights_QO <- nb2listw(nb_queen_ori, style = "W", zero.policy = TRUE)

# Konversi daftar tetangga menjadi matriks bobot
matrix <- nb2mat(nb_queen_ori, style = "W", zero.policy = TRUE)

# Tampilkan matriksnya
print(matrix)

#Eksport matriks
# Pastikan W_matrix adalah matriks biasa (bukan list)
W_df <- as.data.frame(matrix)
# Tambahkan nama baris/kolom
rownames(W_df) <- colnames(W_df) <- merged_data$PROVINSI 
# Simpan ke file CSV
write.csv(W_df, "D:/SKRIPSI/REGRESI/Output/matrix_queen_ori.csv", row.names = TRUE)

#========================##Autokorelasi Spasial##========================
# Lakukan uji Moran's I PDRB ADHK
moran_result <- moran.test(merged_data$Y_PDRB, weights_QO, zero.policy = TRUE)
# Tampilkan hasil uji Moran's I
print(moran_result)

# Cek morans prediktor
# moran_result <- moran.test(merged_data$X8_PMA, listw_knn, zero.policy = TRUE)
# moran_result

## Moran scatterplot
library(spdep)
# Keterangan Indeks
moran.plot(merged_data$Y_PDRB, weights_QO, zero.policy = TRUE,
           labels = rownames(merged_data),
           xlab = "Produk Domestik Regional Bruto",
           ylab = "Spasial Lag PDRB")

# Dapatkan data dari moran.plot tanpa plotting otomatis
mp <- moran.plot(merged_data$Y_PDRB, weights_QO, plot = FALSE)

# Buat plot manual dengan semua titik sebagai lingkaran merah
plot(mp$x, mp$wx, 
     pch = 19,  # Lingkaran solid
     col = "red",  # Warna merah
     xlab = "Produk Domestik Regional Bruto", 
     ylab = "Spasial Lag PDRB")

# Tambahkan garis regresi
abline(lm(mp$wx ~ mp$x), col = "black")

# Tambahkan garis rata-rata untuk sumbu x dan y
abline(h = mean(mp$wx), lty = 2, col = "black")
abline(v = mean(mp$x), lty = 2, col = "black")

library(ggplot2)

# Dapatkan data dari moran.plot
mp <- moran.plot(merged_data$Y_PDRB, weights_QO, plot = FALSE)

# Buat plot dengan ggplot2
ggplot(mp, aes(x = x, y = wx)) + 
  geom_point(color = "red", size = 3) +  # Semua titik merah dan ukuran seragam
  geom_smooth(formula = y ~ x, method = "lm", se = FALSE, color = "black") +  # Garis regresi
  geom_hline(yintercept = mean(mp$wx), linetype = "dashed", color = "black") +  # Garis rata-rata y
  geom_vline(xintercept = mean(mp$x), linetype = "dashed", color = "black") +  # Garis rata-rata x
  theme_minimal() + 
  labs(x = "PDRB ADHK", y = "Spatially lagged PDRB ADHK", title = "Moran Scatterplot")

# 1. Jalankan moran.plot dengan return_df = TRUE dan tanpa plot
mp_df <- moran.plot(
  merged_data$Y_PDRB,
  listw = weights_QO,
  labels = FALSE,
  plot   = FALSE,
  return_df = TRUE
)

# Lihat struktur mp_df
head(mp_df)
# >      x             wx            res      hat variable
# > 1  ...           ...           ...      ...  "provinsi1"

# 2. Tambahkan nama provinsi and kuadran
mp_df$Provinsi <- merged_data$PROVINSI

mp_df$Quadrant <- with(mp_df, ifelse(x >= 0 & wx >= 0, "High–High",
                                     ifelse(x <  0 & wx >= 0, "Low–High",
                                            ifelse(x <  0 & wx <  0, "Low–Low", "High–Low"))))

# 3. Cetak daftar provinsi berdasarkan kuadran
list_by_quad <- split(mp_df$Provinsi, mp_df$Quadrant)
print(list_by_quad)

# Keterangan Nama Provinsi
# moran.plot(merged_data$Y_PDRB, weights, zero.policy = TRUE,
#            labels = merged_data$PROVINSI,
#            xlab = "PDRB",
#            ylab = "Spatially Lagged PDRB",
#            main = "Moran Scatterplot")


##========================##Uji Lagrange Multiplier##========================##
# Model OLS akhir sebagai dasar uji LM
library(spdep)
# Model akhir dari diagnostik regresi
model_ols_x6x <- lm(Y_log ~ X1_PAD + X2_IPM + X4_AKSESLISTRIK + X5_TELEPONSELULER + X7_IMK + X11_JP, data = merged_data)

# Uji Lagrange Multiplier untuk memilih model spasial
lm_test <- lm.RStests(model_ols_x6x, listw = weights_QO, test = "all")

# Tampilkan hasil uji LM
print(lm_test) #SLM signifikan

##========================##IMPLEMENTASI MODEL##========================##
library(spatialreg)
##========================## OLS ##========================##
# Model akhir dari diagnostik regresi
model_ols_x6x <- lm(Y_log ~ X1_PAD + X2_IPM + X3_TPAK + X4_AKSESLISTRIK + X5_TELEPONSELULER + X7_IMK + X11_JP, data = merged_data)

#LR Test: -, 
AIC(model_ols_x6x)
#AIC: 68.98161, 
#R²: 0.8036, 
#Adjusted R²: 0.7508, 

# Y_PDRB
# RMSE: 629.919,7, 
# MAPE: 99,98891 %

# Y_log
# RMSE: 0,5121135
# MAPE: 3,592776 %
##========================## SAR ##========================##

# Model SAR sebelum ada variabel JP (LR Test: 0.04682, AIC: 75.7, R²: 0.7607487, Adjusted R²: 0.7956255, RMSE: 0.5650957, MAPE: 4.038712 %)
sar_model_ori <- lagsarlm(Y_log ~ X1_PAD + X2_IPM + X3_TPAK + X4_AKSESLISTRIK + X5_TELEPONSELULER + X7_IMK, data = merged_data, listw = weights_QO, zero.policy = TRUE)
summary(sar_model_ori)

# wilayah yang tidak punya tetangga 6
#                   Estimate Std. Error z value  Pr(>|z|)
# (Intercept)       10.39122    0.77839 13.3496 < 2.2e-16
# X1_PAD             2.42494    0.73354  3.3058  0.000947
# X2_IPM             1.38878    0.82690  1.6795  0.093057 x
# X3_TPAK           -0.25948    0.46094 -0.5629  0.573470 x
# X4_AKSESLISTRIK   -0.62696    1.57280 -0.3986  0.690170 x
# X5_TELEPONSELULER  0.76048    1.64250  0.4630  0.643363 x
# X7_IMK             2.11928    0.64930  3.2639  0.001099

# Rho: 0.044542, LR test value: 3.9518, p-value: 0.04682
# Asymptotic standard error: 0.022032
# z-value: 2.0217, p-value: 0.043207
# Wald statistic: 4.0873, p-value: 0.043207
# 
# Log likelihood: -28.85014 for lag model
# ML residual variance (sigma squared): 0.31933, (sigma: 0.5651)
# Number of observations: 34 
# Number of parameters estimated: 9 
# AIC: 75.7, (AIC for lm: 77.652)
# LM test for residual autocorrelation
# test value: 0.4134, p-value: 0.52025

# Model SAR FIX nambah variabel Jumlah Penduduk
# (LR Test: 0.13781, AIC: 68.779, R²: 0.8159, Adjusted R²: 0.8534, RMSE: 0.4957, MAPE: 3.4664 %)
sar_model_ori22_log <- lagsarlm(model_ols_x6x, data = merged_data, listw = weights_QO, zero.policy = TRUE)
summary(sar_model_ori22_log)

#                    Estimate Std. Error z value  Pr(>|z|)
# (Intercept)       10.028582   0.691815 14.4961 < 2.2e-16
# X1_PAD             1.447492   0.712071  2.0328  0.042074
# X2_IPM             1.952476   0.745649  2.6185  0.008832
# X3_TPAK            0.061948   0.416950  0.1486  0.881890 x
# X4_AKSESLISTRIK   -1.153212   1.387811 -0.8310  0.405998 x
# X5_TELEPONSELULER  1.336855   1.450518  0.9216  0.356716 x
# X7_IMK            -0.991511   1.127090 -0.8797  0.379017 x
# X11_JP             3.798053   1.186975  3.1998  0.001375
# 
# Rho: 0.029657, LR test value: 2.2023, p-value: 0.13781
# Asymptotic standard error: 0.019899
# z-value: 1.4903, p-value: 0.13613
# Wald statistic: 2.2211, p-value: 0.13613
# 
# Log likelihood: -24.38967 for lag model
# ML residual variance (sigma squared): 0.24573, (sigma: 0.49572)
# Number of observations: 34 
# Number of parameters estimated: 10 
# AIC: 68.779, (AIC for lm: 68.982)
# LM test for residual autocorrelation
# test value: 1.8219, p-value: 0.17709

##========================## SEM ##========================##

# Model SEM sebelum ada variabel JP (LR Test: 0.92205, AIC: 79.643, R2: 0.7313355, adjstd R2: 0.7648638, RMSE: 0.59882, MAPE: 4.191391 %)
sem_model_ori <- errorsarlm(Y_log ~ X1_PAD + X2_IPM + X3_TPAK + X4_AKSESLISTRIK + X5_TELEPONSELULER + X7_IMK,
                        data = merged_data, listw = weights_QO, zero.policy = TRUE)
summary(sem_model_ori)

# wilayah yang tidak punya tetangga 6
#                   Estimate Std. Error z value  Pr(>|z|)
# (Intercept)       10.86064    0.75550 14.3754 < 2.2e-16
# X1_PAD             2.49842    0.77288  3.2326 0.0012267
# X2_IPM             1.66049    0.85914  1.9327 0.0532690 x
# X3_TPAK           -0.41484    0.48078 -0.8629 0.3882192 x
# X4_AKSESLISTRIK   -1.08168    1.64372 -0.6581 0.5104960 x
# X5_TELEPONSELULER  1.10640    1.74770  0.6331 0.5266912 x
# X7_IMK             2.33380    0.66614  3.5034 0.0004593

# Lambda: -0.045319, LR test value: 0.0095762, p-value: 0.92205
# Asymptotic standard error: 0.19131
# z-value: -0.23689, p-value: 0.81275
# Wald statistic: 0.056115, p-value: 0.81275
# 
# Log likelihood: -30.82127 for error model
# ML residual variance (sigma squared): 0.35859, (sigma: 0.59882)
# Number of observations: 34 
# Number of parameters estimated: 9 
# AIC: 79.643, (AIC for lm: 77.652)

# Model SEM FIX nambah variabel Jumlah Penduduk
# (LR Test: 0.046232, AIC: 67.008, R2: 0.8253, adjstd R2: 0.8631, RMSE: 0.4516, MAPE: 3.2726 %)
sem_model_ori22_log <- errorsarlm(model_ols_x6x, data = merged_data, listw = weights_QO, zero.policy = TRUE)
summary(sem_model_ori22_log)

#                   Estimate Std. Error z value  Pr(>|z|)
# (Intercept)        9.07171    0.55570 16.3247 < 2.2e-16
# X1_PAD             0.93082    0.72476  1.2843 0.1990319 x
# X2_IPM             1.91781    0.66268  2.8940 0.0038036
# X3_TPAK            0.58308    0.41277  1.4126 0.1577670 x
# X4_AKSESLISTRIK   -3.06446    1.11683 -2.7439 0.0060716
# X5_TELEPONSELULER  4.52802    1.36777  3.3105 0.0009312
# X7_IMK            -1.71972    0.95455 -1.8016 0.0716080 x
# X11_JP             5.33480    1.12977  4.7220 2.335e-06
# 
# Lambda: -0.59487, LR test value: 3.9731, p-value: 0.046232
# Asymptotic standard error: 0.14683
# z-value: -4.0514, p-value: 5.0911e-05
# Wald statistic: 16.414, p-value: 5.0911e-05
# 
# Log likelihood: -23.50424 for error model
# ML residual variance (sigma squared): 0.20398, (sigma: 0.45164)
# Number of observations: 34 
# Number of parameters estimated: 10 
# AIC: 67.008, (AIC for lm: 68.982)

##========================## SARMA ##========================##

# Model SARMA sebelum ada variabel JP (LR Test: 0.059244, AIC: 76, R²: 0.7724194, Adjusted R²: 0.8078312, RMSE: 0.5335874, MAPE: 3.780441 %)
sarma_model_ori <- sacsarlm(Y_log ~ X1_PAD + X2_IPM  + X3_TPAK + X4_AKSESLISTRIK + X5_TELEPONSELULER + X7_IMK, data = merged_data, listw = weights_QO, zero.policy = TRUE)
summary(sarma_model_ori)

# wilayah yang tidak punya tetangga 6
#                   Estimate Std. Error z value  Pr(>|z|)
# (Intercept)        9.29780    0.71194 13.0597 < 2.2e-16
# X1_PAD             2.92118    0.66184  4.4137 1.016e-05 
# X2_IPM             0.91009    0.78093  1.1654 0.2438581 x
# X3_TPAK            0.12707    0.46723  0.2720 0.7856478 x
# X4_AKSESLISTRIK   -1.50351    1.37059 -1.0970 0.2726486 x
# X5_TELEPONSELULER  2.91316    1.60239  1.8180 0.0690636 x
# X7_IMK             1.96204    0.55195  3.5547 0.0003784

# Rho: 0.053888
# Asymptotic standard error: 0.020232
# z-value: 2.6635, p-value: 0.0077332
# Lambda: -0.42224
# Asymptotic standard error: 0.17389
# z-value: -2.4282, p-value: 0.015175
# 
# LR test value: 5.6522, p-value: 0.059244
# 
# Log likelihood: -27.99998 for sac model
# ML residual variance (sigma squared): 0.28472, (sigma: 0.53359)
# Number of observations: 34 
# Number of parameters estimated: 10 
# AIC: 76, (AIC for lm: 77.652)

# Model SARMA FIX nambah variabel Jumlah Penduduk 
# (LR Test: 0.007954, AIC: 63.313, R²: 0.8522, Adjusted R²: 0.8913, RMSE: 0.4038, MAPE: 2.7918 %)
sarma_model_ori22_log <- sacsarlm(model_ols_x6x, data = merged_data, listw = weights_QO, zero.policy = TRUE)
summary(sarma_model_ori22_log)

# Type: sac 
# Coefficients: (asymptotic standard errors) 
#                   Estimate Std. Error z value  Pr(>|z|)
# (Intercept)        8.42892    0.53069 15.8829 < 2.2e-16
# X1_PAD             1.20175    0.67405  1.7829  0.074604 x
# X2_IPM             1.46983    0.60911  2.4131  0.015819
# X3_TPAK            0.81135    0.37752  2.1491  0.031624
# X4_AKSESLISTRIK   -2.90795    0.98722 -2.9456  0.003223
# X5_TELEPONSELULER  4.77431    1.22398  3.9007 9.593e-05
# X7_IMK            -1.48812    0.85262 -1.7454  0.080923 x
# X11_JP             4.81445    1.05259  4.5739 4.787e-06
# Rho: 0.03971
# Asymptotic standard error: 0.015602
# z-value: 2.5451, p-value: 0.010923
# Lambda: -0.69121
# Asymptotic standard error: 0.1267
# z-value: -5.4553, p-value: 4.8885e-08
# 
# LR test value: 9.6682, p-value: 0.007954
# 
# Log likelihood: -20.65672 for sac model
# ML residual variance (sigma squared): 0.1631, (sigma: 0.40386)
# Number of observations: 34 
# Number of parameters estimated: 11 
# AIC: 63.313, (AIC for lm: 68.982)

# karena ga signifikan 2 jadi coba di hapus
model_ols_x6x_x1x_x7x <- lm(Y_log ~ X2_IPM + X3_TPAK + X4_AKSESLISTRIK + X5_TELEPONSELULER + X11_JP, data = merged_data)
sarma_model_ori22_log_x6x_x1x_x7x <- sacsarlm(model_ols_x6x_x1x_x7x, data = merged_data, listw = weights_QO, zero.policy = TRUE)
summary(sarma_model_ori22_log_x6x_x1x_x7x)

# (LR Test: 0.071216, AIC: 68.47, R²: 0.8066, Adjusted R²: 0.8436, 
# RMSE: 0.4038, MAPE: 2.7918 %)
# 
# Type: sac 
# Coefficients: (asymptotic standard errors) 
# Estimate Std. Error z value  Pr(>|z|)
# (Intercept)        9.19775    0.62423 14.7344 < 2.2e-16
# X2_IPM             2.30754    0.60761  3.7977  0.000146
# X3_TPAK            0.10455    0.39418  0.2652  0.790819
# X4_AKSESLISTRIK   -3.40321    1.17729 -2.8907  0.003844
# X5_TELEPONSELULER  4.36932    1.43804  3.0384  0.002378
# X11_JP             3.97822    0.44159  9.0089 < 2.2e-16
# 
# Rho: 0.035681
# Asymptotic standard error: 0.018517
# z-value: 1.927, p-value: 0.053985
# Lambda: -0.5127
# Asymptotic standard error: 0.1615
# z-value: -3.1746, p-value: 0.0015006
# 
# LR test value: 5.2841, p-value: 0.071216
# 
# Log likelihood: -25.23484 for sac model
# ML residual variance (sigma squared): 0.23433, (sigma: 0.48407)
# Number of observations: 34 
# Number of parameters estimated: 9 
# AIC: 68.47, (AIC for lm: 69.754)
##========================##EVALUASI MODEL##========================##
# R2
library(spatialreg)

NagelkerkeR2 <- function(model) {
  l_full <- logLik(model)
  l_null <- logLik(lm(model$y ~ 1))
  
  R2 <- 1 - exp((2 / length(model$y)) * (l_null - l_full))
  R2max <- 1 - exp((2 / length(model$y)) * l_null)
  adjR2 <- R2 / R2max
  
  result <- data.frame(
    R2 = as.numeric(R2),
    R2max = as.numeric(R2max),
    adjR2 = as.numeric(adjR2)
  )
  return(result)
}

NagelkerkeR2(model_ols_x6x)
#          R2     R2max     adjR2
#   0.7607487 0.9561644 0.7956255

NagelkerkeR2(sem_model_ori22_log)
#          R2     R2max     adjR2
#   0.7313355 0.9561644 0.7648638

NagelkerkeR2(sarma_model_ori22_log)
#          R2     R2max    adjR2
#   0.7724194 0.9561644 0.8078312

NagelkerkeR2(sarma_model_ori22_log_x6x_x1x_x7x)
#          R2     R2max     adjR2
# 1 0.8065827 0.9561644 0.8435608

# Untuk model SAR (R² SAR: 0.9829)
res_sar <- residuals(sar_model)
y_sar <- merged_data$Y_PDRB
sst_sar <- sum((y_sar - mean(y_sar))^2)
ssr_sar <- sum(res_sar^2)
r2_sar <- 1 - (ssr_sar / sst_sar)
cat("Pseudo-R² SAR:", round(r2_sar, 4), "\n")

# Untuk model SARMA (R² SARMA: 0.9856)
res_sarma <- residuals(sarma_model)
y_sarma <- merged_data$Y_PDRB
sst_sarma <- sum((y_sarma - mean(y_sarma))^2)
ssr_sarma <- sum(res_sarma^2)
r2_sarma <- 1 - (ssr_sarma / sst_sarma)
cat("Pseudo-R² SARMA:", round(r2_sarma, 4), "\n")


# Jumlah observasi dan jumlah variabel independen
n <- nrow(merged_data)
k <- 6  # jumlah X, sesuaikan jika beda

# Adjusted R² SAR
adj_r2_sar <- 1 - ((1 - r2_sar) * (n - 1) / (n - k - 1))
cat("Adjusted R² SAR:", round(adj_r2_sar, 4), "\n")

# Adjusted R² SARMA
adj_r2_sarma <- 1 - ((1 - r2_sarma) * (n - 1) / (n - k - 1))
cat("Adjusted R² SARMA:", round(adj_r2_sarma, 4), "\n")

#=================#RMSE & MAPE#=================#

# Prediksi dari model
pred <- sarma_model_ori22_log_x6x_x1x_x7x$fitted.values  # atau: fitted

# Nilai aktual
actual <- merged_data$Y_log  # atau Y_PDRB kalau pakai versi sebelum log

# RMSE (Root Mean Squared Error)
rmse <- sqrt(mean((actual - pred)^2))
cat("RMSE:", rmse, "\n")

# MAPE (Mean Absolute Percentage Error)
mape <- mean(abs((actual - pred) / actual)) * 100
cat("MAPE:", mape, "%\n")

#=================# Uji Asumsi Model terbaik / SARMA #=================#
# sarma_model_ori22_log <- sacsarlm(model_ols_x6x, data = merged_data, listw = weights_QO, zero.policy = TRUE)
# summary(sarma_model_ori22_log)
# Asumsi paket sudah diinstal
library(spdep)   # Untuk sacsarlm dan moran.test
library(lmtest)  # Untuk bptest

# 1. Uji normalitas residual menggunakan Shapiro-Wilk
residuals_sarma <- residuals(sarma_model_ori22_log)
shapiro_test <- shapiro.test(residuals_sarma)
print(shapiro_test)
# Interpretasi: Jika p-value > 0,05, residual berdistribusi normal (lihat Tabel 4.20 di skripsi).

# 2. Uji heteroskedastisitas
# Catatan: bptest() tidak langsung kompatibel dengan sacsarlm karena struktur model spasial.
# Alternatif: Uji visual heteroskedastisitas dengan plot residual vs fitted values
fitted_sarma <- fitted(sarma_model_ori22_log)  # Abaikan peringatan, ini hanya informasi
plot(fitted_sarma, residuals_sarma, main="Residuals vs Fitted Values (SARMA)",
     xlab="Fitted Values", ylab="Residuals")
abline(h=0, col="red")
# Interpretasi: Jika tidak ada pola sistematis (misalnya, corong), tidak ada indikasi heteroskedastisitas.

# Alternatif manual untuk uji Breusch-Pagan (karena bptest tidak cocok untuk SARMA)
resid_sq <- residuals_sarma^2
bp_lm <- lm(resid_sq ~ fitted_sarma)
bp_test_manual <- lmtest::bptest(bp_lm)
print(bp_test_manual)
# Interpretasi: Jika p-value > 0,05, tidak ada heteroskedastisitas (lihat Tabel 4.21 untuk pendekatan serupa pada OLS).

# Catatan: Untuk uji heteroskedastisitas yang lebih tepat pada model spasial, gunakan uji Lagrange Multiplier (LM) untuk heteroskedastisitas spasial
# Contoh: lm.LMtests() dari spdep, tetapi ini biasanya dilakukan pada model OLS awal
ols_model <- lm(model_ols_x6x, data = merged_data)
lm.LMtests(ols_model, listw = weights_QO, test="LMerr")

# 3. Uji autokorelasi spasial pada residual menggunakan Moran's I
moran_test_resid <- moran.test(residuals_sarma, listw = weights_QO, zero.policy = TRUE)
print(moran_test_resid)
# Interpretasi: Jika p-value > 0,05, tidak ada autokorelasi spasial pada residual (lihat Tabel 4.26).
#=================#Uji Heteroskedastisitas#=================#lolos uji asumsi
library(lmtest)
library(spatialreg)
# Uji Breusch-Pagan
bp_sar <- spatialreg::bptest.sarlm(sar_model) # masih error
print(bp_sar)

bptest(sar_model)
bptest(sarma_model)

#===================================================================#
# tetangga untuk masing-masing provinsi
library(sf)
library(spdep)
library(tmap)  # opsional kalau mau tampil lebih rapi

# 1. Ambil titik tengah (centroid) dari masing-masing provinsi
centroids <- st_centroid(merged_data)

# 2. Ekstrak koordinatnya menjadi matriks (longitude, latitude)
coords <- st_coordinates(centroids)

# 3. Plot peta dasar
plot(st_geometry(merged_data), border = "black")

# 4. Tambahkan garis antar tetangga
plot(nb_queen_ori, coords, add = TRUE, col = "red", lwd = 1.5)

# 5. Tambahkan titik centroid
points(coords, pch = 20, col = "blue", cex = 1)

# 6. (Opsional) Tambahkan label nama provinsi
text(coords, labels = merged_data$OBJECTID, cex = 0.6, pos = 4)

#===================================================================#
# 1. Buat Data Prediksi (log) dan Kembalikan ke Skala Asli
# Prediksi dalam skala log
merged_data$pred_log <- predict(sarma_model_ori22_log)

# Kembalikan ke skala PDRB asli (anti-log / eksponensial)
merged_data$pred_PDRB <- exp(merged_data$pred_log)

# Jika data aktual masih berupa log
# merged_data$actual_PDRB <- exp(merged_data$Y_log)

# 2. Buat Data Frame Ringkas
pred_df_sarma <- data.frame(
  Provinsi = merged_data$PROVINSI,
  Index = 1:nrow(merged_data),
  Aktual = merged_data$Y_PDRB,
  Prediksi = merged_data$pred_PDRB
)

pred_df_sarma
# csv
write.csv(pred_df_sarma, "pred_df_sarma.csv")
write.csv(data, "data-normalisasi.csv")
write.csv(merged_data$Y_log, "Y-Log.csv")
# 3. Visualisasi dengan Plot (Bar Chart atau Scatterplot)
library(ggplot2)

ggplot(pred_df, aes(x = Index)) +
  geom_point(aes(y = Aktual, color = "Aktual"), size = 3) +
  geom_point(aes(y = Prediksi.fit, color = "Prediksi SAR"), size = 3) +
  scale_color_manual(values = c("Aktual" = "blue", "Prediksi SAR" = "grey")) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    legend.box = "horizontal"
  )

# prediksi sarma
library(Matrix)

# 1. Ekstrak matriks W (sparse) dari listw Anda
W_mat <- as(as_dgRMatrix_listw(weights_QO), "CsparseMatrix")

# 2. Ambil koefisien dan rho
rho <- sarma_model_ori22_log$rho
beta <- sarma_model_ori22_log$coefficients

# 3. Buat matriks desain X (termasuk intercept)
X <- model.matrix( 
  formula(sarma_model_ori22_log), 
  data = merged_data 
)

# 4. Hitung (I - rho W)^(-1)
n <- nrow(W_mat)
I   <- Diagonal(n)
M   <- solve(I - rho * W_mat)   # (I - ρW)^{-1}

# 5. Prediksi log‐Y
pred_log <- as.numeric( M %*% (X %*% beta) )

# 6. Simpan ke data
merged_data$pred_log <- pred_log
merged_data$pred_PDRB <- exp(pred_log)


#==============================# Vektor u #================================#

# Residual untuk SEM
residual_sem <- residuals(sem_model_ori22_log)
print(residual_sem)
write.csv(residual_sem, "Vektor-u-SEM.csv")
# Residual untuk SARMA
residual_sarma <- residuals(sarma_model_ori22_log)
print(residual_sarma)
write.csv(residual_sarma, "Vektor-u-SARMA.csv")

#==============================# Prediksi #================================#

# Prediksi nilai Y_log dari model
prediksi_log <- predict(model_ols_x6x)

# Kembalikan ke skala asli dari log (jika Y_log = log(Y))
prediksi_Y <- exp(prediksi_log)

# Buat data frame hasil prediksi vs aktual
hasil_prediksi <- data.frame(
  Aktual_Y = exp(merged_data$Y_log),
  Prediksi_Y = exp(predict(model_ols_x6x))
)

install.packages("writexl")  # jika belum terpasang
library(writexl)

# Simpan ke file Excel
write_xlsx(hasil_prediksi, "hasil_prediksi_ols.xlsx")

plot(model_ols, which = 1)  # Residual vs Fitted

# cek residual
# Asumsikan model Anda sudah ada: model_sar, model_sem, model_sarma, model_ols
res_sar <- residuals(sar_model_ori22_log)
res_sem <- residuals(sem_model_ori22_log)
res_sarma <- residuals(sarma_model_ori22_log)
res_ols <- residuals(model_ols_x6x)

# Cek residual untuk Jawa Barat (titik 9)
print(res_sar[9])
print(res_sem[9])
print(res_sarma[9])
print(res_ols[9])
