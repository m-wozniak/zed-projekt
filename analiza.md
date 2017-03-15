# Analiza połowu śledzi






Zaawansowana Eksploracja Danych - projekt zaliczeniowy, semestr ziomowy 2016/17, Politechnika Poznańska.
2017-03-15 08:43:27

### Streszczenie

Przedmiotem poniższej analizy było zbadanie zjawiska zmniejszania się śledzi na przestrzeni ostatnich kilkudziesięciu lat. Badane parametry obejmowały warunki środowiskowe takie jak temperatura wody czy też dostępność pożywienia, a także zależne te od człowieka - natężenie połowów lub wielkość połowów. 

Zbiór wymagał wstępnego przetwarzania - w jego ramach zostały uzupełnione wartości puste oraz wyeliminowane wartości odstające. Przedstawione zostały także podstawowe charakterystyki poszczególnych zmiennych. Została przeprowadzona także analiza korelacji, w wyniku której można było zauważyć zależności pomiędzy wielkością śledzia, dostępnością planktonu a czynnikami środowiskowymi w postaci zmiany temperatury wody oraz oscylacji północnoatlantyckiej. Przedstawione zostało jak zmieniał się rozmiar śledzi na przestrzeni czasu i w którym momencie nastąpił stoponiowy jego spadek. Zbudowany został także model regresyjny pozwalający na przewidywanie wielkości śledzia na podstawie pozostałych parametrów.  

### Wykorzystane biblioteki


```r
library('caret')
library('plotly')
library('pander')
library('dplyr')
library('ggplot2')
library('gridExtra')
library('corrplot')
library('GGally')
```

### Zbiór danych
Zbiór zawiera dane komercyjnych połowów śledzi z ostatnich 60 lat. Z każdego połowu wybierano próbkę od 50 do 100 sztuk trzyletnich śledzi i badano ich długość. Zbudowano w ten sposób zbiór ponad 50 tysięcy pomiarów.

#### Opis atrybutów

|Atrybut      |   Opis                                                                       |
|------------:|:----------------------------------------------------------------------------:|
|length       |  długość złowionego śledzia [cm]                                             |
|cfin1        |  dostępność planktonu [zagęszczenie Calanus finmarchicus gat. 1]             |
|cfin2        |  dostępność planktonu [zagęszczenie Calanus finmarchicus gat. 2]             |
|chel1        |  dostępność planktonu [zagęszczenie Calanus helgolandicus gat. 1]            |
|chel2        |  dostępność planktonu [zagęszczenie Calanus helgolandicus gat. 2]            |
|lcop1        |  dostępność planktonu [zagęszczenie widłonogów gat. 1]                       |
|lcop2        |  dostępność planktonu [zagęszczenie widłonogów gat. 2]                       |
|fbar         |  natężenie połowów w regionie [ułamek pozostawionego narybku]                |
|recr         | roczny narybek [liczba śledzi]                                               |
|cumf         | łączne roczne natężenie połowów w regionie [ułamek pozostawionego narybku];  |
|totaln       |  łączna liczba ryb złowionych w ramach połowu [liczba śledzi];               |
|sst          | temperatura przy powierzchni wody [°C];                                      |
|sal          | poziom zasolenia wody [Knudsen ppt];                                         |
|xmonth       | miesiąc połowu [numer miesiąca];                                             |
|nao          | oscylacja północnoatlantycka [mb].                                           |



#### Wczytanie zbioru danych i podstawowe charakterystyki



```r
data <- read.csv('sledzie.csv',na.strings = '?')
summary(data)
```


-------------------------------------------------------------------------
      X          length         cfin1           cfin2          chel1     
------------- ------------ --------------- --------------- --------------
  Min.  : 0   Min.  :19.0  Min.  : 0.0000  Min.  : 0.0000  Min.  : 0.000 

1st Qu.:13145 1st Qu.:24.0 1st Qu.: 0.0000 1st Qu.: 0.2778 1st Qu.: 2.469

Median :26291 Median :25.5 Median : 0.1111 Median : 0.7012 Median : 5.750

 Mean :26291   Mean :25.3   Mean : 0.4458   Mean : 2.0248   Mean :10.006 

3rd Qu.:39436 3rd Qu.:26.5 3rd Qu.: 0.3333 3rd Qu.: 1.7936 3rd Qu.:11.500

Max.  :52581  Max.  :32.5  Max.  :37.6667  Max.  :19.3958  Max.  :75.000 

     NA            NA        NA's :1581      NA's :1536      NA's :1555  
-------------------------------------------------------------------------

 
-----------------------------------------------------------------------------
    chel2           lcop1           lcop2           fbar           recr      
-------------- ---------------- -------------- -------------- ---------------
Min.  : 5.238   Min.  : 0.3074  Min.  : 7.849  Min.  :0.0680  Min.  : 140515 

1st Qu.:13.427 1st Qu.: 2.5479  1st Qu.:17.808 1st Qu.:0.2270 1st Qu.: 360061

Median :21.673 Median : 7.0000  Median :24.859 Median :0.3320 Median : 421391

 Mean :21.221   Mean : 12.8108   Mean :28.419   Mean :0.3304   Mean : 520367 

3rd Qu.:27.193 3rd Qu.: 21.2315 3rd Qu.:37.232 3rd Qu.:0.4560 3rd Qu.: 724151

Max.  :57.706  Max.  :115.5833  Max.  :68.736  Max.  :0.8490  Max.  :1565890 

  NA's :1556      NA's :1653      NA's :1591         NA             NA       
-----------------------------------------------------------------------------

 
--------------------------------------------------------------------------
     cumf           totaln           sst           sal          xmonth    
--------------- --------------- ------------- ------------- --------------
Min.  :0.06833  Min.  : 144137  Min.  :12.77  Min.  :35.40  Min.  : 1.000 

1st Qu.:0.14809 1st Qu.: 306068 1st Qu.:13.60 1st Qu.:35.51 1st Qu.: 5.000

Median :0.23191 Median : 539558 Median :13.86 Median :35.51 Median : 8.000

 Mean :0.22981   Mean : 514973   Mean :13.87   Mean :35.51   Mean : 7.258 

3rd Qu.:0.29803 3rd Qu.: 730351 3rd Qu.:14.16 3rd Qu.:35.52 3rd Qu.: 9.000

Max.  :0.39801  Max.  :1015595  Max.  :14.73  Max.  :35.61  Max.  :12.000 

      NA              NA         NA's :1584        NA             NA      
--------------------------------------------------------------------------

 
----------------
      nao       
----------------
Min.  :-4.89000 

1st Qu.:-1.89000

Median : 0.20000

 Mean :-0.09236 

3rd Qu.: 1.63000

Max.  : 5.08000 

       NA       
----------------



### Wstępne przetwarzanie 

#### Analiza wartości pustych
W tej części zostanie dokonane uzupełnienie wartości pustych. Jak można zauważyć, wartości puste występują tylko w siedmiu kolumnach a w ich ramach rozkład jest niemal równomierny.


```r
sapply(data,function(y) sum (length(which(is.na(y)))))
```

```
##      X length  cfin1  cfin2  chel1  chel2  lcop1  lcop2   fbar   recr 
##      0      0   1581   1536   1555   1556   1653   1591      0      0 
##   cumf totaln    sst    sal xmonth    nao 
##      0      0   1584      0      0      0
```

Zbiór zawiera najprawdopodobniej zdenormalizowane dane z pojedynczych połowów oraz corocznych statystyk. Powoduje to, że większość danych się powtarza w obrębie sąsiednich obserwacji. Poniżej przedstawiony został przykładowy wycinek zbioru. Jak można zauważyć, wartości puste mogą zostać uzupełnione przy wykorzystaniu podobnych obserwacji. 

 

```r
data[1:10,3:8]
```

```
##      cfin1   cfin2   chel1    chel2   lcop1    lcop2
## 1  0.02778 0.27785 2.46875       NA 2.54787 26.35881
## 2  0.02778 0.27785 2.46875 21.43548 2.54787 26.35881
## 3  0.02778 0.27785 2.46875 21.43548 2.54787 26.35881
## 4  0.02778 0.27785 2.46875 21.43548 2.54787 26.35881
## 5  0.02778 0.27785 2.46875 21.43548 2.54787 26.35881
## 6  0.02778 0.27785 2.46875 21.43548 2.54787       NA
## 7  0.02778 0.27785 2.46875 21.43548 2.54787 26.35881
## 8  0.02778 0.27785 2.46875 21.43548 2.54787 26.35881
## 9  0.02778 0.27785 2.46875 21.43548 2.54787 26.35881
## 10 0.02778 0.27785 2.46875 21.43548 2.54787 26.35881
```

Ustalenie grup podobnych obserwacji:


```r
removed.na<-data[complete.cases(data),]
unique.groups <- unique(removed.na[,3:14])
head(unique.groups)
```

```
##       cfin1   cfin2   chel1    chel2    lcop1    lcop2  fbar   recr
## 2   0.02778 0.27785 2.46875 21.43548  2.54787 26.35881 0.356 482831
## 222 0.36032 5.36402 4.32674 27.16006  5.08099 36.68770 0.434 441827
## 291 2.14333 4.45882 6.38667 26.17187  9.01000 32.19090 0.327 783337
## 782 0.18940 0.85684 0.60308  9.43208  0.82803 10.72889 0.571 465638
## 865 1.02508 3.66319 6.42127 25.51806 10.92857 37.39201 0.485 724151
## 976 2.14333 0.29600 6.38667 21.67333  9.01000 24.85867 0.158 392084
##          cumf   totaln      sst      sal
## 2   0.3059879 267380.8 14.30693 35.51234
## 222 0.3726272 191976.2 14.47960 35.50777
## 291 0.3096315 492519.0 13.98133 35.61240
## 782 0.3500081 383913.5 13.86200 35.51779
## 865 0.3838187 457143.9 13.71160 35.51169
## 976 0.1100757 766077.6 14.06933 35.51526
```

```r
nrow(unique.groups)
```

```
## [1] 99
```

Poniższa funkcja jako argumenty przyjmuje pojedynczą obserwację oraz zbiór unikalnych obserwacji, a jej rezultatem jest wiersz z uzupełnionymi wartościami pustymi. Poszukiwana jest taka obserwacja ze zbioru, która posiada najmniejszą odległość Hamminga pomiędzy nią a źródłową obserwacją z wartościami pustymi - to ona stanowi podstawę dla uzupełnienia danych. 


```r
imputeNA <- function (r,groups,columns) {
  
  if (sum(is.na(r))>0) {
    row<-r[columns]
    nns<-apply(groups,1,function(x,y) {
      sum(y[!is.na(y)]!=x[!is.na(y)])
    },y = row)
    
    nn <- unique.groups[names(sort(nns)[1]),]
    
    row[is.na(row)]<-unlist(nn[is.na(row)])
    r[columns]<-row
  }
  r
}
```

Zastosowanie stworzonej funkcji:


```r
data<-as.data.frame(t(apply(data,1,imputeNA,groups=unique.groups,columns=3:14)))
sapply(data,function(y) sum (length(which(is.na(y)))))
```

```
##      X length  cfin1  cfin2  chel1  chel2  lcop1  lcop2   fbar   recr 
##      0      0      0      0      0      0      0      0      0      0 
##   cumf totaln    sst    sal xmonth    nao 
##      0      0      0      0      0      0
```

Jak widać, z całego zbioru zostały usunięte wartości puste. Osiągnięcie podobnego efektu możliwe byłoby również poprzez zastosowanie np. funkcji `kNN` z pakietu VIM, jednak wiązałoby się to ze znacznym nakładem czasowym (dla każdej obserwacji z wartościami pustymi obliczana by była odległość pomiędzy nią a pozostałymi). Niedogodność ta została ograniczona w proponowanym rozwiązaniu poprzez wykorzystanie grup podobnych obserwacji.

#### Poprawa wartości zmiennej `length`

Można zauważyć, że w zdecydowanej większości przypadków zmienna `lenght` przyjmuje wartości będące wielokrotnością liczby `0.5`. Tak nie dzieje jednak we wszystkich przypadkach: 


```r
data %>% filter (length%%0.5 != 0) %>% summarise(n())
```

```
##   n()
## 1  72
```

Stanowi to około 0.1% obserwacji. Można więc założyć, że jest to związane z błędem podczas zbierania danych, co w procesie ich czyszczenia warto naprawić.


```r
data<-data %>% mutate( length = round(length*2)/2)
data %>% filter (length%%0.5 != 0) %>% summarise(n())
```

```
##   n()
## 1   0
```


#### Usuwanie wartości odstających
Na poniższym zestawieniu histogramów można zauważyć, że zwłaszcza zmienne `cfin1` i `lcop1` mogą zawierać wartości odstające.

![](analiza_files/figure-html/unnamed-chunk-12-1.png)<!-- -->

W opracowaniu użyta została odległość Cooka jako miara wpływu, jaki ma obserwacja na model regresji. Jeśli pominięcie danej obserwacji powoduje stosunkowo dużą zmianę współczynników, jest ona potencjalnie odstająca. 


```r
data.scaled<-data %>% mutate_each(funs(scale(.) %>% as.vector),  vars=c(-1,-15))

mod <- lm(length ~ ., data=data.scaled[c(-1,-15)])
cooksd <- cooks.distance(mod)

influential <- as.numeric(names(cooksd)[(cooksd > 4*mean(cooksd))]) 

data<-data[-influential,]
length(influential)
```

```
## [1] 1261
```

Poniższy wykres przedstawia zestawienie histogramów po usunięciu 1261 obserwacji.

![](analiza_files/figure-html/unnamed-chunk-14-1.png)<!-- -->

Zgodnie z przypuszeniami zakres został ograniczony najbardziej dla zmiennych `cfin1` i `lcop` oznaczających dostępność planktonu określnego rodzaju, w mniejszym stopniu zmienną `cumf`, czyli roczne natężenie połowów.

### Analiza danych

#### Korelacja zmiennych

Na poniższym wykresie została przedstawiona macierz korelacji:


```r
c <- cor(data %>% select(-X))
corrplot(c, type="lower", method="color",diag=FALSE)
```

![](analiza_files/figure-html/unnamed-chunk-15-1.png)<!-- -->

Wykres prezentujący korelację pomiędzy występowaniem różnego rodzaju planktonu. Można zauważyć bardzo duże skorelowanie dostępności planktonu *Calanus finmarchicus* oraz *Calanus finmarchicus* (gatunek 1 i 2), a także ujemną korelację łącznego rocznego natężenia połowów

![](analiza_files/figure-html/unnamed-chunk-16-1.png)<!-- -->


![](analiza_files/figure-html/unnamed-chunk-17-1.png)<!-- -->

W kontekście długości złowionych śledzi największą (ujemną) korelację widzimy dla temperatury przy powierzchni wody.

![](analiza_files/figure-html/unnamed-chunk-18-1.png)<!-- -->

#### Analiza zmiany długości śledzi w czasie

Żeby zaprezentować zmianę rozmiaru złowionych śledzi w czasie w odniesieniu do czynników, które mogły mieć na to wpływ, należy dokonać normalizacji zmiennych. Poniższa funkcja realizuje normalizację min-max:


```r
normalize <- function (df,cols) {
  
z<-as.data.frame(df[,cols])

  z<- apply(z, 2, function(x) {
    ranx <- range(x)
    (x - ranx[1]) / diff(ranx)
  })
  
df[,cols]<-z
df
}

data.normalized<-normalize(data,c(2:14,16))
```

Dodatkowo, uśredniona została ilość pożywienia, którą dysponowały śledzie w danym czasie:


```r
data.normalized <- data.normalized %>% mutate( avg.food = rowMeans(.[,3:8]))
```

Na poniższym wykresie zaprezentowano jak zmieniały się czynniki wpływające na wielkość śledzi. Widać wyraźnie zależność pomiędzy ilością pożywienia a długością ryb. Dodatkowo można zauważyć jak wpłyneła zmiana temperatury wody oraz występowanie zjawiska oscylacji północnoatlantyckiej, która może być bezpośrednią przyczyną zmiany ilości planktonu, a tym samym efektywności połowów. Widoczny jest też dokładny moment zmiany trendu.

![](analiza_files/figure-html/unnamed-chunk-21-1.png)<!-- -->

#### Regresja

W celu przewidywania długości śledzia na podstawie pozostałych zmiennych stworzony został model regresji. Wybrany algorytm to Stochastic Gradient Boosting z pakietu `caret`. 


```r
inTraining <- createDataPartition(data$length, p = .75, list = FALSE)
training <- data[ inTraining,-1]
testing  <- data[-inTraining,-1]


caretGrid <- expand.grid(interaction.depth=c(1, 2, 3), n.trees = (1:20)*5,
                         shrinkage=c(0.01, 0.001),
                         n.minobsinnode=10)
metric <- "RMSE"
trainControl <- trainControl(method="cv", number=10, verboseIter = TRUE)

gbm.caret <- train(length ~ ., data=training, distribution="gaussian", method="gbm",
                   trControl=trainControl, verbose=FALSE, 
                   tuneGrid=caretGrid, metric=metric, bag.fraction=0.75)                    
```

Wykres przedstawiający błąd średniokwadratowy dla strojonych parametrów modelu. 
  
![](analiza_files/figure-html/unnamed-chunk-24-1.png)<!-- -->

Trafność regresji dla danych testowych w postaci miar RMSE i R^2:


```r
predict.ts<- predict(gbm.caret,testing)
predict.sm <- data.frame(obs=testing$length, pred=predict.ts)
defaultSummary(predict.sm)
```

```
##      RMSE  Rsquared 
## 1.2754090 0.4467941
```

Wykres ważności atrybutów.

![](analiza_files/figure-html/unnamed-chunk-26-1.png)<!-- -->

Wcześniejsze przypuszczenia się potwierdziły - temperatura wody jest najbardziej istotną zmienną w procesie predykcji wielkości śledzia. Istotny okazał się także roczny narybek oraz pokarm - parametry `recr` oraz `lcop1`, `lcop2`, `cfin2` i `chel2`. 



