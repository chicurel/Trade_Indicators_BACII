# Trade_Indicators_BACII
Code to calculate several trade indicators from the CEPII (BACI) database and estimate a gravity model.
This code works to calculate some trade indicators using the BACI, CEPII database and to estimate a gravity model using Gravity database from CEPII and Database of Political Institutions (DPI).

## Steps for Indicators
1. Download BACII database (one file for each year) and save it in 2.Data_code/CEPII_BACI
2. Create the same folders
3. Go to 2.Data_code/code and run the code: Code_open_clean_exp & Code_open_clean_imp
4. Then run all codes in imports and exports
5. Run CDI3, Substitutability Indicator
6. Run merge_all_indicators to get the full database with all the indicators

Full database should result in 2.Data_code/Output/Data_with_indicators 

Code should work for any country and/or any region


## Steps for Gravity Model
1. Download Gravity database and save it in 2.Data_code/Gravity
2. Download Database of Political Institutions (DPI) and save it in 2.Data_code/DPI2020
3. Run 2.Data_code/Gravity
   
