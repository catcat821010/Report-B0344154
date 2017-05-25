長庚大學 大數據分析方法 作業六
================

作業說明 （繳交時請直接刪除這個章節）
-------------------------------------

作業目的：期末專題暖身

依下列指示，完成期末分析專題作業規劃：

-   訂出分析問題，並將R Markdown的一級標題(第一行的title:)中的"長庚大學 大數據分析方法 作業六"取代為期末專題的分析問題，並在分析議題背景前加上組員姓名 (`10pt`)
-   分析議題背景 (`10pt`) 與動機 (`10pt`)
-   資料說明 (`10pt`) 與 載入 (`10pt`)
-   資料處理與清洗 (`10pt`) 說明 (`10pt`)
-   對資料們進行探索式資料分析，圖文並茂佳!(`20pt`)
-   期末專題分析規劃與假設 (`10pt`)

分析議題背景
------------

背景介紹背景介紹

分析動機
--------

分析動機分析動機

使用資料
--------

說明使用資料們

載入使用資料們

``` r
library(rvest)
```

    ## Loading required package: xml2

``` r
library(stringr)
library(data.table)
#Sys.setlocale(category = "LC_ALL", locale = "kor")
#Sys.setlocale(category = "LC_ALL", locale = "cht")


totalData<-NULL

GaonDownloadChart<-"http://gaonchart.co.kr/main/section/chart/online.gaon?nationGbn=T&serviceGbn=S1020"
DLCContent<-read_html(GaonDownloadChart)
DLCAlbum <- str_trim(DLCContent %>% html_nodes("td") %>% html_text())
for(i in 0:99){
tempData<-data.table(DLCAlbum[4+8*i])

totalData<-rbind(totalData,tempData)

}

totalData
```

    ##                                                                                                                          V1
    ##   1:                                                                                       SIGNAL\r\n\t\t\t\t\tTWICE|SIGNAL
    ##   2:                                                                     맞지?\r\n\t\t\t\t\t언니쓰|언니들의 슬램덩크 시즌 2
    ##   3:                                                                         I LUV IT\r\n\t\t\t\t\t싸이 (PSY)|PSY 8th 4X2=8
    ##   4:                                                                         New Face\r\n\t\t\t\t\t싸이 (PSY)|PSY 8th 4X2=8
    ##   5:                                              처음부터 너와 나\r\n\t\t\t\t\t볼빨간 사춘기|군주 - 가면의 주인 OST Part.2
    ##   6:                                                                               문득\r\n\t\t\t\t\t로이킴|開花期 (개화기)
    ##   7:                                                                       Shape of You\r\n\t\t\t\t\tEd Sheeran|÷ (Deluxe)
    ##   8:                                                                      혼자\r\n\t\t\t\t\t어반 자카파 (Urban Zakapa)|혼자
    ##   9:                                                    오늘 취하면 (Feat.창모) (Prod. SUGA)\r\n\t\t\t\t\t수란 (SURAN)|WINE
    ##  10:                                                               도원경 (桃源境)\r\n\t\t\t\t\t빅스 (VIXX)|桃源境 (도원경)
    ##  11:                                                               팔레트 (Feat. G-DRAGON)\r\n\t\t\t\t\t아이유 (IU)|Palette
    ##  12:                                                                         이기주의보\r\n\t\t\t\t\t로이킴|開花期 (개화기)
    ##  13:                                                                REALLY REALLY\r\n\t\t\t\t\t위너(WINNER)|FATE NUMBER FOR
    ##  14:                                                        미치고 싶다\r\n\t\t\t\t\t한동근|한동근 1ST ALBUM ‘Your Diary’
    ##  15:                                                                      그러나, 밤\r\n\t\t\t\t\t이수 (M.C The Max)|inhale
    ##  16:                                                                                   TOMBOY\r\n\t\t\t\t\t혁오 (hyukoh)|23
    ##  17:                                                       마지막 장면 (Feat. 이성경)\r\n\t\t\t\t\t싸이 (PSY)|PSY 8th 4X2=8
    ##  18:                                                         BLUE MOON (Prod. GroovyRoom)\r\n\t\t\t\t\t효린, 창모|BLUE MOON
    ##  19:                                                                LOVE (Feat. 태양)\r\n\t\t\t\t\t싸이 (PSY)|PSY 8th 4X2=8
    ##  20:                                                               사랑이 잘 (With 오혁)\r\n\t\t\t\t\t아이유 (IU)|사랑이 잘
    ##  21:                                                          BOMB (Feat. B.I, BOBBY)\r\n\t\t\t\t\t싸이 (PSY)|PSY 8th 4X2=8
    ##  22:                                                        팩트폭행 (Feat. G-DRAGON)\r\n\t\t\t\t\t싸이 (PSY)|PSY 8th 4X2=8
    ##  23:                                                                 아프지 마요\r\n\t\t\t\t\t젝스키스|THE 20TH ANNIVERSARY
    ##  24:                                                                                 밤편지\r\n\t\t\t\t\t아이유 (IU)|밤편지
    ##  25:                                             나야 나 (PICK ME)\r\n\t\t\t\t\tPRODUCE 101|PRODUCE 101 - 나야 나 (PICK ME)
    ##  26:                                                   첫눈처럼 너에게 가겠다\r\n\t\t\t\t\t에일리 (Ailee)|도깨비 OST Part 9
    ##  27:                                                                    Marry Me\r\n\t\t\t\t\t구윤회|마크툽 프로젝트 Vol.03
    ##  28:                                                                      오빠야\r\n\t\t\t\t\t신현희와김루트|신현희와김루트
    ##  29:                                                                   KNOCK KNOCK\r\n\t\t\t\t\tTWICE|TWICEcoaster _ LANE 2
    ##  30:                                                                      너였다면\r\n\t\t\t\t\t정승환|또 오해영 OST Part 5
    ##  31:                                                                               이 지금\r\n\t\t\t\t\t아이유 (IU)|Palette
    ##  32:                                              얼굴 찌푸리지 말아요\r\n\t\t\t\t\t하이라이트 (Highlight)|CAN YOU FEEL IT?
    ##  33:                                                                      아름다워\r\n\t\t\t\t\t창모 (CHANGMO)|돈 벌 시간 2
    ##  34:                                            좋다고 말해\r\n\t\t\t\t\t볼빨간 사춘기|Full Album RED PLANET (Hidden Track)
    ##  35:                                                                                  Be There\r\n\t\t\t\t\tCHEEZE|Be There
    ##  36:                                                                             이런 엔딩\r\n\t\t\t\t\t아이유 (IU)|Palette
    ##  37:                                                                  그대 내게 말하길\r\n\t\t\t\t\t임창정|그대 내게 말하길
    ##  38:                                                                     She`s a Baby\r\n\t\t\t\t\t지코 (ZICO)|She`s a Baby
    ##  39:                                                                                      ONLY 너\r\n\t\t\t\t\tTWICE|SIGNAL
    ##  40:                                                   오랜 날 오랜 밤\r\n\t\t\t\t\t악동뮤지션 (AKMU)|사춘기 하 (思春記 下)
    ##  41:                                                                                  하루에 세번\r\n\t\t\t\t\tTWICE|SIGNAL
    ##  42:                                                                        예뻐서 그래\r\n\t\t\t\t\t로이킴|開花期 (개화기)
    ##  43:                                                                      봄날\r\n\t\t\t\t\t방탄소년단|YOU NEVER WALK ALONE
    ##  44:                                                           우주를 줄게\r\n\t\t\t\t\t볼빨간 사춘기|Full Album RED PLANET
    ##  45:                                                                               불장난\r\n\t\t\t\t\tBLACKPINK|SQUARE TWO
    ##  46:                                                                                  잼잼\r\n\t\t\t\t\t아이유 (IU)|Palette
    ##  47:                                              지금, 우리\r\n\t\t\t\t\t러블리즈|Lovelyz 2nd Album Repackage `지금, 우리`
    ##  48:                                                                 랄랄라 송\r\n\t\t\t\t\t언니쓰|언니들의 슬램덩크 시즌 2
    ##  49:                                                      나만 안되는 연애\r\n\t\t\t\t\t볼빨간 사춘기|Full Album RED PLANET
    ##  50:                                       All I Wanna Do (K) (Feat. Hoody, Loco)\r\n\t\t\t\t\t박재범|EVERYTHING YOU WANTED
    ##  51:                                                                              SOMEONE LIKE ME\r\n\t\t\t\t\tTWICE|SIGNAL
    ##  52:                                                                Beautiful\r\n\t\t\t\t\t크러쉬 (CRUSH)|도깨비 OST Part 4
    ##  53:                                                              Fine\r\n\t\t\t\t\t태연 (TAEYEON)|My Voice - The 1st Album
    ##  54:                                                                            TT\r\n\t\t\t\t\tTWICE|TWICEcoaster _ LANE 1
    ##  55:                                                                                HOLD ME TIGHT\r\n\t\t\t\t\tTWICE|SIGNAL
    ##  56:                                                   Rookie\r\n\t\t\t\t\t레드벨벳(Red Velvet)|Rookie - The 4th Mini Album
    ##  57:                                                                                 EYE EYE EYES\r\n\t\t\t\t\tTWICE|SIGNAL
    ##  58:                                                    부담이 돼 (Feat. 휘인 of 마마무)\r\n\t\t\t\t\t정키 (Jung Key)|EMPTY
    ##  59:                                                            Closer (Feat. Halsey)\r\n\t\t\t\t\tThe Chainsmokers|Collage
    ##  60:                                                          마에스트로 (Maestro)\r\n\t\t\t\t\t창모 (CHANGMO)|돈 벌 시간 2
    ##  61:                                                                                  낮보다는 밤\r\n\t\t\t\t\tEXID|Eclipse
    ##  62:                                                                                   에라 모르겠다\r\n\t\t\t\t\t빅뱅|MADE
    ##  63:                              Something Just Like This\r\n\t\t\t\t\tThe Chainsmokers, Coldplay|Something Just Like This
    ##  64:                                               그대라는 사치\r\n\t\t\t\t\t한동근|The 3rd Digital Single `그대라는 사치`
    ##  65:                                                                               Dejavu\r\n\t\t\t\t\t비와이(BewhY)|Dejavu
    ##  66:                                                      Love Yourself\r\n\t\t\t\t\tJustin Bieber|Purpose (Deluxe Edition)
    ##  67:                                        이 소설의 끝을 다시 써보려 해\r\n\t\t\t\t\t한동근|이 소설의 끝을 다시 써보려 해
    ##  68:                                                                      YESTERDAY\r\n\t\t\t\t\t블락비 (Block B)|YESTERDAY
    ##  69:                                                         오토리버스 (Feat. TABLO)\r\n\t\t\t\t\t싸이 (PSY)|PSY 8th 4X2=8
    ##  70:                                                                              이름에게\r\n\t\t\t\t\t아이유 (IU)|Palette
    ##  71:                                                                                   CHEER UP\r\n\t\t\t\t\tTWICE|PAGE TWO
    ##  72:                                                                  어디에도\r\n\t\t\t\t\t엠씨더맥스 (M.C The Max)|pathos
    ##  73:                                                         Decalcomanie (데칼코마니)\r\n\t\t\t\t\t마마무 (Mamamoo)|MEMORY
    ##  74:                     우리집을 못 찾겠군요 (Feat. 볼빨간사춘기)\r\n\t\t\t\t\t매드클라운 (Mad Clown)|우리집을 못 찾겠군요
    ##  75:                                                                          너란 봄 (Feat. 하림)\r\n\t\t\t\t\t정은지|공간
    ##  76:                                                                               MOVIE\r\n\t\t\t\t\t비투비 (BTOB)|Feel`eM
    ##  77:                                                                 Stay With Me\r\n\t\t\t\t\t찬열, 펀치|도깨비 OST Part 1
    ##  78:                                                                          소나기\r\n\t\t\t\t\t아이오아이 (I.O.I)|소나기
    ##  79:                                                          널 사랑하지 않아\r\n\t\t\t\t\t어반 자카파 (Urban Zakapa)|스틸
    ##  80:                                                               You(=I)\r\n\t\t\t\t\t볼빨간 사춘기|Full Album RED PLANET
    ##  81:                                             돌아오지마 (Feat. 용준형 of 비스트)\r\n\t\t\t\t\t헤이즈 (Heize)|돌아오지마
    ##  82:                                                                               내가 저지른 사랑\r\n\t\t\t\t\t임창정|I`M
    ##  83:                                                            D (half moon) (Feat. 개코)\r\n\t\t\t\t\tDEAN|130 mood  TRBL
    ##  84:                                                                     We Are Young\r\n\t\t\t\t\t싸이 (PSY)|PSY 8th 4X2=8
    ##  85:                                FINGERTIP\r\n\t\t\t\t\t여자친구 (GFRIEND)|여자친구 The 4th Mini Album `THE AWAKENING...
    ##  86:                 러시안 룰렛 (Russian Roulette)\r\n\t\t\t\t\t레드벨벳(Red Velvet)|Russian Roulette - The 3rd Mini Album
    ##  87:                                                                           기댈곳\r\n\t\t\t\t\t싸이 (PSY)|PSY 8th 4X2=8
    ##  88:                                 Make Me Love You\r\n\t\t\t\t\t태연 (TAEYEON)|My Voice - The 1st Album Deluxe Editio...
    ##  89:                                                                                              숨\r\n\t\t\t\t\t박효신|숨
    ##  90: See You Again (폴 워커 추모 엔딩곡)\r\n\t\t\t\t\tCharlie Puth, Wiz Khalifa|분노의 질주_ 더 세븐 (Fast & Furious 7) OST
    ##  91:                                                                             Black Out\r\n\t\t\t\t\t아이유 (IU)|Palette
    ##  92:                                                                     Black Out\r\n\t\t\t\t\t빅스 (VIXX)|桃源境 (도원경)
    ##  93:                                                                       이쁘다니까\r\n\t\t\t\t\t에디킴|도깨비 OST Part 5
    ##  94:                                                                                저 별\r\n\t\t\t\t\t헤이즈 (Heize)|저 별
    ##  95:                                                                                  노래\r\n\t\t\t\t\t자이언티(Zion.T)|OO
    ##  96:                                     바래다줄게 (Take You Home)\r\n\t\t\t\t\t백현 (BAEKHYUN)|바래다줄게 (Take You Home)
    ##  97:                                                                I Miss You\r\n\t\t\t\t\t소유 (씨스타)|도깨비 OST Part 7
    ##  98:                                                                          넘어와 (Feat. 백예린)\r\n\t\t\t\t\tDEAN|limbo
    ##  99:                                                                그대로일까 (Feat. 헤이즈)\r\n\t\t\t\t\t용준형|WONDER IF
    ## 100:                                                         마음아 열려라\r\n\t\t\t\t\t마마무 (Mamamoo)|맨투맨 OST Part. 5
    ##                                                                                                                          V1

資料處理與清洗
--------------

說明處理資料的步驟

處理資料

``` r
#這是R Code Chunk
```

探索式資料分析
--------------

圖文並茂圖文並茂

``` r
#這是R Code Chunk
```

期末專題分析規劃
----------------

期末專題要做XXOOO交叉分析 測試測試測試
