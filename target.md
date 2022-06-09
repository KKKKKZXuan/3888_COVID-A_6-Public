# target

1. 找到四个相关国家的ICU数据  

    a. 由于只有少数国家在发布了ICU病例有关的具体数值，因此以四个国家作为代表  
    b. 数据在每次Knite时通过url链接的形式更新（获取最新数据）  
    c. 通过对数据的提取，并指定日期，清理多余数据  

2. 整合数据  

    a. 通过数据中包含的Human Development Index(HDI)数据进行发达/发展中国家/地区的分类  
    b. 在选定的的四个国家之间整合发达/发展中国家/地区  

3. 分析数据  

    a. 通过逻辑回归对比四个国家/地区的新增病例，新增死亡，新增ICU病例以及ICU的占用率  
    b. 通过逻辑回归对比发达/发展中国家/地区的新增病例，新增死亡，新增ICU病例以及ICU的占用率  
    c. 通过相关性对比四个国家/地区的新增病例，新增死亡，新增ICU病例以及ICU的占用率  
    d. 通过相关性对比发达/发展中国家/地区的新增病例，新增死亡，新增ICU病例以及ICU的占用率  

4. 预测数据  

    a. 通过ACF/PACF的预测对发达/发展中国家/地区的新增ICU病例进行分析  
    b. 通过ARIMA自动预测未来发达/发展中国家/地区的新增ICU病例趋势  

5. 结论  

    a. ICU病例的趋势在发达/发展中国家/地区之间并没有明显区别  
    b. ICU占用率过高可能表明已经超负荷运行，因此该ICU数据其实远超记录值  
    c. 由于所研究国家/地区数据较少，数据可能不太具有代表性

---

1. Find ICU data for four relevant countries  

    a. Since only a few countries are publishing specific numbers on ICU cases, four countries are represented  
    b. The data is updated in the form of a url link every time Knite (get the latest data)  
    c. Clean up redundant data by extracting data and specifying a date  

2. Integrate data  

    a. Classification of developed/developing countries by means of Human Development Index (HDI) data included in the data  
    b. Integrate developed/developing countries/regions among the four selected countries  

3. Analyze the data  

    a. Logistic regression comparing new cases, new deaths, new ICU cases, and ICU occupancy rates in four countries/regions  
    b. Logistic regression comparing new cases, new deaths, new ICU cases, and ICU occupancy rates in developed/developing countries/regions  
    c. Correlation comparison of new cases, new deaths, new ICU cases, and ICU occupancy rates across the four countries  
    d. Correlation comparison of new cases, new deaths, new ICU cases, and ICU occupancy rates in developed/developing countries/regions  

4. Forecast data  

    a. Analysis of new ICU cases in developed/developing countries/regions through ACF/PACF projections  
    b. Automatic prediction of future trend of new ICU cases in developed/developing countries/regions through ARIMA  

5. Conclusion  

    a. Trends in ICU cases do not differ significantly between developed/developing countries/regions  
    b. High ICU occupancy may indicate overloaded operation, so the ICU data is actually far beyond the recorded value  
    c. Data may not be representative due to the small number of countries studied  
