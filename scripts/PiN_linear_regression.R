library(car)
library(dplyr)
library(ggplot2)
library(psych)
library(moments)

set.seed(42) 

setwd('/path/to/the/directory')
PiN_both_data <- read.csv("PiN_both.tsv", sep = "\t")
PiN_either_data <- read.csv("PiN_either.tsv", sep = "\t")
PiN_all_data <- read.csv("PiN.tsv", sep = "\t")

glimpse(PiN_both_data)

# Step 1: Data explorations

hist_a_var <- function(PiN_data, xAxis ){

  PiN_data$Noise_ID <- as.factor(PiN_data$Noise_ID)
  levels(PiN_data$Noise_ID) <- c('babble, SNR 5', 'babble, SNR 0', 'babble, SNR -5')
  PiN_plot_data  <- unique(PiN_data[c("Noise_ID", "Short.Utterance", xAxis)])

  
  print(ggplot2::ggplot(data = PiN_plot_data, aes_string(x=xAxis, fill="Noise_ID", color="Noise_ID") )  +
    geom_histogram(alpha = 0.5, aes(y = ..density..), position = 'identity') +
    facet_wrap(~Noise_ID))
  
  print(nrow(PiN_plot_data))
  return(PiN_data)  
}


summary(lm( PhER ~ Noise_ID, data=PiN_both_data)) 
summary(lm( PhER ~ Noise_ID, data=PiN_either_data))
summary(lm( PhER ~ Noise_ID, data=PiN_all_data))

PiN_both_data <- hist_a_var(PiN_both_data, "PhER")
PiN_either_data <- hist_a_var(PiN_either_data, "PhER")
PiN_all_data <- hist_a_var(PiN_all_data, "PhER")


#View(PiN_all_data)

mean(PiN_all_data$PhER); sd(PiN_all_data$PhER)

############################################## 
### Step 2 [optional] : Define functions for Histogram (eg: x-axis: diff-STOI) ###

hist_diff_STOI <- function(PiN_diff_data ){
  
  print(count(PiN_diff_data))
  ggplot2::ggplot(data = PiN_diff_data, aes(x=diff.STOI, fill=factor(Noise_ID, levels=c("babble, SNR -5", "babble, SNR 0", "babble, SNR 5")), color=factor(Noise_ID, levels=c("babble, SNR -5", "babble, SNR 0", "babble, SNR 5"))))  +
    geom_histogram(binwidth=0.05, show.legend = FALSE) + facet_wrap(~Noise_ID)
    #geom_histogram(alpha = 0.5, aes(y = ..density..)) 
  
}

hist_diff_phLen <- function(PiN_diff_data ){
  
  print(count(PiN_diff_data))
  ggplot2::ggplot(data = PiN_diff_data, aes(x=diff.phLen, fill=factor(Noise_ID, levels=c("babble, SNR -5", "babble, SNR 0", "babble, SNR 5")), color=factor(Noise_ID, levels=c("babble, SNR -5", "babble, SNR 0", "babble, SNR 5"))) )  +
    geom_histogram(binwidth=5, show.legend = FALSE) + facet_wrap(~Noise_ID)
  #geom_histogram(alpha = 0.5, aes(y = ..density..)) 
  
}


hist_diff_ppl <- function(PiN_diff_data ){
  print(count(PiN_diff_data))
  ggplot2::ggplot(data = PiN_diff_data, aes(x=diff.ppl, fill=factor(Noise_ID, levels=c("babble, SNR -5", "babble, SNR 0", "babble, SNR 5")), color=factor(Noise_ID, levels=c("babble, SNR -5", "babble, SNR 0", "babble, SNR 5"))) )  +
    geom_histogram(binwidth=50, show.legend = FALSE) +
    facet_wrap(~Noise_ID) +
    #ggtitle("Perplexity difference among paraphrase pairs") +
    xlab("scaled(diff.ppl)") + ylab("#paraphrase pairs") +
    theme(text=element_text(size=15, face="bold"),
          axis.text.x=element_text(size=10, face="bold"),
          axis.text.y=element_text(size=10, face="bold"),
          #axis.text = element_text(size = 15), 
          #axis.title = element_text(size = 10),
          #plot.subtitle = element_text(size = 10)
    )
  
  
  # theme(legend.position = "none", 
  #       text=element_text(size=20, face="bold", family="Times New Roman"), 
  #       axis.text.x=element_text(size=10, face="bold"),
  #       axis.text.y=element_text(size=10, face="bold"))
  # #geom_histogram(alpha = 0.5, aes(y = ..density..)) 
  
}


############################################## 
### Step 3 : Prepare pairwise_diff_dataset' with diff.SI as 'Sent-Int-Gain'

PiN_all_diff_data <- PiN_all_data %>% arrange(PhER) %>% group_by( Noise_ID, paraphrase_pair_id) %>% summarise( diff.SI = diff(PhER), diff.phLen = diff(ph_len), diff.ppl = diff(ppl), diff.STOI = diff(STOI), diff.mean_nChars = diff(mean_nChars), diff.mean_wordRank = diff(mean_wordRank), diff.mean_nPhonemes = diff(mean_nPhonemes) )
#View(PiN_all_diff_data)

hist_diff_STOI(PiN_all_diff_data)
hist_diff_phLen(PiN_all_diff_data)
hist_diff_ppl(PiN_all_diff_data)

PiN_both_diff_data <- PiN_both_data %>% arrange(PhER) %>% group_by( Noise_ID, paraphrase_pair_id) %>% summarise( diff.SI = diff(PhER), diff.phLen = diff(ph_len), diff.ppl = diff(ppl), diff.STOI = diff(STOI), diff.mean_nChars = diff(mean_nChars), diff.mean_wordRank = diff(mean_wordRank), diff.mean_nPhonemes = diff(mean_nPhonemes) )

hist_diff_STOI(PiN_both_diff_data)
hist_diff_phLen(PiN_both_diff_data)
hist_diff_ppl(PiN_both_diff_data)

PiN_either_diff_data <- PiN_either_data %>% arrange(PhER) %>% group_by( Noise_ID, paraphrase_pair_id) %>% summarise( diff.SI = diff(PhER), diff.phLen = diff(ph_len), diff.ppl = diff(ppl), diff.STOI = diff(STOI), diff.mean_nChars = diff(mean_nChars), diff.mean_wordRank = diff(mean_wordRank), diff.mean_nPhonemes = diff(mean_nPhonemes) )

hist_diff_STOI(PiN_either_diff_data)
hist_diff_phLen(PiN_either_diff_data)
hist_diff_ppl(PiN_either_diff_data)


############################################## Step 3 Note : with increase in SNR, better intelligble paraphrases have better STOI compared to its pair.

### Step  4 : modeling 'Sent-Int' with  linguistic and acoustic features   (Expected output is commented after each command)

model_SI <- function(PiN_data, LE){
  
  new_feats <- c("ph_len", 'ppl' , 'STOI') 
  model_data <- unique(PiN_data[PiN_data$Noise_ID == LE,][append( new_feats , 'PhER')])
  
  model_all_feats <- lm( PhER ~  scale(ph_len) + scale(ppl) + scale(STOI) , data = model_data)
  
  #summary(step(model_all_feats))
  summary(model_all_feats)
  
  # print( car::vif(model_all_feats))
  # print("Correlation co-effecient : ")
  # print(cor.test(model_data$PhER, model_data$STOI))
  # print(cor.test(model_data$PhER, model_data$ppl))
  # print(cor.test(model_data$PhER, model_data$ph_len))
  
  # print(cohen.d( model_data))
  # print(cohen.d( PhER ~  ppl , data = model_data))
  # print(cohen.d( PhER ~  STOI , data = model_data))
}


model_SI( PiN_all_data, 'babble, SNR 5')

# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)    0.967506   0.003018 320.531  < 2e-16 ***
#   scale(ph_len) -0.014969   0.003038  -4.927 1.39e-06 ***
#   scale(ppl)    -0.006600   0.003032  -2.177   0.0303 *  
#   scale(STOI)   -0.001516   0.003031  -0.500   0.6175    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1


model_SI( PiN_all_data, 'babble, SNR 0')

# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)    0.938428   0.004083 229.863  < 2e-16 ***
# scale(ph_len) -0.006742   0.004106  -1.642  0.10164    
# scale(ppl)    -0.017688   0.004106  -4.307 2.25e-05 ***
# scale(STOI)    0.015044   0.004091   3.677  0.00028 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

model_SI( PiN_all_data, 'babble, SNR -5')

# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)    0.70590    0.01032  68.416  < 2e-16 ***
#   scale(ph_len) -0.03078    0.01043  -2.951  0.00342 ** 
#   scale(ppl)    -0.04890    0.01035  -4.726 3.54e-06 ***
#   scale(STOI)    0.07008    0.01044   6.713 9.75e-11 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1


### Step  5 : modeling 'Sent-Int-Gain' with  linguistic and acoustic features  (Expected output is commented after each command)

model_diff_SI <- function(PiN_data, LE){
  
  model_data <- PiN_data[PiN_data$Noise_ID == LE,]
  lm_model_all_feats <- lm( diff.SI ~  scale(diff.phLen) + scale(diff.ppl) + scale(diff.STOI) , data = model_data)
  
  #print(step(lm_model_all_feats))
  print(summary(lm_model_all_feats))
}


model_diff_SI( PiN_both_diff_data, 'babble, SNR 5')

# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)        0.0296474  0.0037165   7.977 2.54e-12 ***
# scale(diff.phLen)  0.0009698  0.0042387   0.229    0.819    
# scale(diff.ppl)    0.0022826  0.0040633   0.562    0.576    
# scale(diff.STOI)  -0.0015470  0.0039141  -0.395    0.694    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
  
model_diff_SI( PiN_both_diff_data, 'babble, SNR 0')

# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)        0.061843   0.005769  10.720   <2e-16 ***
# scale(diff.phLen) -0.008067   0.006368  -1.267    0.208    
# scale(diff.ppl)   -0.004942   0.006360  -0.777    0.439    
# scale(diff.STOI)   0.014341   0.005808   2.469    0.015 *  
#  ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

model_diff_SI( PiN_both_diff_data, 'babble, SNR -5')

# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)        0.19643    0.01311  14.980  < 2e-16 ***
# scale(diff.phLen) -0.01140    0.01369  -0.833    0.407    
# scale(diff.ppl)   -0.01614    0.01356  -1.190    0.237    
# scale(diff.STOI)   0.06568    0.01370   4.793 5.67e-06 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
 

model_diff_SI( PiN_either_diff_data, 'babble, SNR 5')

# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)        0.0340598  0.0032918  10.347   <2e-16 ***
# scale(diff.phLen) -0.0075481  0.0034408  -2.194   0.0295 *  
# scale(diff.ppl)    0.0020830  0.0033810   0.616   0.5386    
# scale(diff.STOI)   0.0001621  0.0034002   0.048   0.9620    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

model_diff_SI( PiN_either_diff_data, 'babble, SNR 0')

# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)        0.058447   0.004157  14.061   <2e-16 ***
# scale(diff.phLen) -0.005741   0.004328  -1.326   0.1862    
# scale(diff.ppl)   -0.005000   0.004328  -1.155   0.2493    
# scale(diff.STOI)   0.008920   0.004170   2.139   0.0336 *  
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

model_diff_SI( PiN_either_diff_data, 'babble, SNR -5')

# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)        0.19707    0.01045  18.859  < 2e-16 ***
# scale(diff.phLen) -0.02982    0.01109  -2.688  0.00781 ** 
# scale(diff.ppl)   -0.01383    0.01075  -1.287  0.19970    
# scale(diff.STOI)   0.04387    0.01083   4.049 7.45e-05 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1


model_diff_SI( PiN_all_diff_data, 'babble, SNR 5') 

# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)        0.0350222  0.0027649  12.667  < 2e-16 ***
# scale(diff.phLen) -0.0097176  0.0028156  -3.451 0.000639 ***
# scale(diff.ppl)    0.0008017  0.0028182   0.284 0.776250    
# scale(diff.STOI)   0.0027304  0.0028360   0.963 0.336454    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

model_diff_SI( PiN_all_diff_data, 'babble, SNR 0')

# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)        0.059144   0.003585  16.499   <2e-16 ***
# scale(diff.phLen) -0.006892   0.003692  -1.866   0.0630 .  
# scale(diff.ppl)   -0.005795   0.003693  -1.569   0.1176    
# scale(diff.STOI)   0.006957   0.003592   1.937   0.0537 .  
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

model_diff_SI( PiN_all_diff_data, 'babble, SNR -5')

# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)        0.198578   0.008429  23.559  < 2e-16 ***
# scale(diff.phLen) -0.031764   0.008689  -3.656 0.000303 ***
# scale(diff.ppl)   -0.013233   0.008491  -1.558 0.120187    
# scale(diff.STOI)   0.036704   0.008644   4.246 2.91e-05 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1


