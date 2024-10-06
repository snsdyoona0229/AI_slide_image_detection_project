# 介紹
近年來隨著AI人工智慧的發展，AI也被應用在各個領域之中，在病理診斷部分，要診斷腫瘤細胞是需要經驗豐富且專業的病理醫師進行仔細的檢查，這是個勞力密集且耗時的工作，希望藉由深度學習演算法來輔助醫師。
所以我們開發了一些AI模型主要以ResUNet為主幹，有的模型是訓練20x玻片影像，有的模型是20x、10x玻片影像一起訓練的，訓練出的模型在根據醫師需求可以選取適當的神經網路模型去輔助標註。
透過本次結果，希望透過AI能輔助到醫師，減輕醫師的負擔，讓醫師更有餘力做其它更深入的研究，也縮短在檢查標記的過程所消耗的時間並且提早為病人接受治療。
# 環境
此系統的主程式是使用R語言進行撰寫與分析，程式環境版本為R 4.1.3，並且對R語言使用python與C++程式進行優化速度，硬體部分NVIDIA GeForce RTX 3080Ti，128GB RAM，作業系統環境：Windows 10 專業版64位元。
1.介面呈現
具備加載多種類型WSI的能力
---通過OpenSlide判斷
(.svs.tif.tiff.vsm.vmu.scn.mrxs.svslide.bif.ndpi)
---通過Hamamatsu SDK（.ndpi）判斷
---通過Philips SDK（.iSyntax）判斷
2.	可以上傳多張波片圖像進行AI輔助標註或者選取過去上傳多張波片圖像進行AI輔助標註
3.	有多個標註圖層可以供使用者標註
	標註點的標註與編輯
可以根據標註點的大小、顏色、新增、刪除標註點等等
	多邊形的標註與編輯
例如:多邊形的顏色、新增繪製或刪除多邊形標註等等
另外呈現也可以內部帶有空洞的多邊形呈現，能更精準標註出癌細胞位置
4.	AI 條件功能設定圖
	根據模型的不同，可選擇不同放大倍率玻片影像進行輔助標註
	在AI輔助標註時，使用者設定圖像處理數值，例如有圖像膨脹或是圖像輸出閥值功能
	可以設定玻片影像要輔助標註時，跑圖像圖塊大小，選擇範圍128~4096像素大小
	在ResUNet網路模型輔助標註時你可以選擇Overlap-tile，Overlap-tile帶來的好處有許多：不需要對圖像進行縮放從而避免圖像細節損失、能夠為邊界區域提供上下文信息、在數據量較少時充當數據擴充的手段。
	可設定CPU或GPU在訓練時批量處理資料，增加訓練速度。
	通過選擇欄位設定，設定訓練完後結果呈現圖層位置。
![image](https://github.com/snsdyoona0229/AI_slide_image_detection_project/blob/main/img/01.png)
![image](https://github.com/snsdyoona0229/AI_slide_image_detection_project/blob/main/img/02.png)
## ResUNet
ResNet由深度殘差網路(Deep residual network)和UNet所發想的語意分割(Semantic Segmentation)模型如圖。
它的架構充分展現「殘差」、「UNet」的優點。
神經網路模型堆疊越深除了常見問題如有梯度消失（vanishing gradient）或梯度爆炸（exploding gradient)，可以採用類似Batch Normalization或Dropout等很多手段解決外可能會遇到的就是神經網路模型退化問題（Degradation problem），神經網路模型堆疊越深，整個神經網路複雜度會增加造成網路不是這麼好訓練，訓練過程也會隨著網路模型越深，抓取特徵過程會丟失越多特徵，錯誤率也上升。
UNet網路模型如圖加入ResNet網路模型目的是假設直接mapping input x的結果為H(x)，則ResNet希望模型能學習到的是殘差F(x)= H(x)-x，因此，只有F(x)會通過卷積層(Convolution layer)。這使得當殘差為0時，此時卷積層(Convolution layer)就像做了identity mapping。此論文稱這條identity的路徑為short cut，short cut的設計使得Network的性能得以提升，並且不需要增加額外的參數

