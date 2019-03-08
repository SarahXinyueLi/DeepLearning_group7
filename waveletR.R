library(readr)
RawData <- read_csv("C:/Users/basja/Desktop/Columbia MSFE/Deep Learning/Project/Data/RawDataLaggedExternal.csv")
RawDataTraining <- data.frame(RawData)
log_path <- "C:/Users/basja/Desktop/Columbia MSFE/Deep Learning/Project/Data/"


library(wmtsa)
'
#To test the function
newData <- wavShrink(RawData$`High Price`, wavelet="haar",
          shrink.fun="soft", thresh.fun = "minimax", n.level = 2)[1:length(RawData$`High Price`)]
'

#Denoise the different features using wavelet thresholding
waveletDenoise <- function(input, waveletName = "haar", shrink = "soft", threshold = "minimax", levels = 5){
  return(wavShrink(input, wavelet=waveletName,
                   shrink.fun=shrink, thresh.fun = threshold, n.level = levels)[1:length(input)]
)
}
denoised2level1 <- data.frame(lapply(RawDataTraining, waveletDenoise))
denoised2level2 <- data.frame(lapply(denoised2level1, waveletDenoise))

#Change to 5 levels in the wavelet decomposition
denoised5level1 <- data.frame(lapply(RawDataTraining, waveletDenoise, levels = 5))
denoised5level2 <- data.frame(lapply(denoised5level1, waveletDenoise, levels = 5))


#To show how wavelet thresholding changes the inputs:
plot(denoised2level1$High.Price)
plot(denoised2level2$High.Price)
plot(RawData$`High Price`)

plot(denoised2level1$ATR)
plot(denoised2level2$ATR)
plot(RawData$ATR)

plot(denoised5level1$ATR)
plot(denoised5level2$ATR)
plot(RawData$ATR)


#Write new datasets to CSV file
write.csv(denoised2level1, paste(log_path,"denoised2level1laggedExternal.csv"))
write.csv(denoised2level2, paste(log_path,"denoised2level2laggedExternal.csv"))
write.csv(denoised5level1, paste(log_path,"denoised5level1laggedExternal.csv"))
write.csv(denoised5level2, paste(log_path,"denoised5level2laggedExternal.csv"))



#DOESN'T WORK YET!!
#Only transform wavelets, no denoising:
library(wavelets)

waveletDecomp <- function(input, waveletName = "haar", levels = 2){
  return(dwt(input, n.levels = levels))
}
newData2 <- lapply(RawDataTraining, waveletDecomp)

val <- dwt(RawDataTraining$Open.Price)
