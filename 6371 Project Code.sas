data train;
set train;
logsaleprice = log(saleprice);
run;

proc print data = train;
run;

proc print data = test; 
run;

proc print data = trainNeighborhood;
run;

data test;
set test;
SalePrice = .;
logsaleprice = .;
;

data loghouse;
set train test;
logsalprice = log(SalePrice);
logarea = log(GrLivArea);
loglotarea = log(LotArea);
run;

data trainNeighborhood; 
set train; 
where Neighborhood contains "Edwards"
	or Neighborhood contains "NAmes"
	or Neighborhood contains "BrkSide"; 
run; 

proc sgplot data = trainNeighborhood; 
scatter x = GrLivArea y = SalePrice / group = Neighborhood
markerattrs = (symbol=CircleFilled size=7); 
title 'Neighborhood Sale Price vs Square Footage';
run;

data trainNeighborhood;
set trainneighborhood;
logsaleprice = log(saleprice);
run;

data trainNeighborhood2;
set trainNeighborhood;
where Id ~= 524 AND Id ~= 643 AND Id ~= 725 AND Id ~= 1299 AND Id ~= 1424; 
run; 

data trainNeighborhood;
set trainneighborhood;
logGrLivArea = log(GrLivArea);
run;

proc sgplot data = trainNeighborhood; 
scatter x = logGrLivArea y = logsaleprice / group = Neighborhood
markerattrs = (symbol=CircleFilled size=7); 
title 'Neighborhood Sale Price vs Square Footage';
run;

 
/*Model 1 with outliers*/
proc glm data = trainNeighborhood2 plots= all; 
where Neighborhood; 
class Neighborhood; 
model SalePrice = Neighborhood|GrLivArea / solution clparm;
run;

proc glmselect data = trainNeighborhood2 plots= all; 
where Neighborhood; 
class Neighborhood; 
model logsaleprice = Neighborhood|logGrLivArea @2/ selection = Stepwise(stop = cv) 
cvmethod = random(5) stats = adjrsq;;;
run;

proc glm data = trainNeighborhood2 plots= all; 
where Neighborhood; 
class Neighborhood; 
model logsaleprice = Neighborhood|logGrLivArea / solution clparm;
run;

data test;
set test;
SalePrice = .;
logsaleprice = .;
;

data combine;
set train test;
logsaleprice = log(SalePrice);
logarea = log(GrLivArea);
loglotarea = log(LotArea);
run;

proc print data = combine(obs=5);
run;



data combine;
set combine;
if _n_ = 524 then delete;
if _n_ = 643 then delete;
if _n_ = 725 then delete; 
if _n_ = 1299 then delete; 
if _n_ = 1424 then delete; 
run; 

proc sgscatter data = train2;
matrix logsaleprice verallqual overallcond yearbuilt yearremodadd bsmtfinsf1 bsmtfinsf2
bsmtunfsf grlivarea fullbath halfbath bedroomabvgr totrmsabvgrd fireplaces garagecars garagearea 
wooddecksf openporchsf enclosedporch screenporch poolarea yrsold neighborhood mszoning lotshape lotconfig condition1 condition2 bldgtype housestyle
roofstyle bsmtfintype1 heatingqc centralair electrical kitchenqual garagetype garagefinish saletype;
run;

proc glm data = train2 plots= all; 
class Neighborhood mszoning lotshape lotconfig condition1 condition2 bldgtype housestyle
roofstyle bsmtfintype1 heatingqc centralair electrical kitchenqual garagetype garagefinish saletype; 
model logsaleprice = overallqual overallcond yearbuilt yearremodadd bsmtfinsf1 bsmtfinsf2
bsmtunfsf grlivarea fullbath halfbath bedroomabvgr totrmsabvgrd fireplaces garagecars garagearea 
wooddecksf openporchsf enclosedporch screenporch poolarea yrsold neighborhood mszoning lotshape lotconfig condition1 condition2 bldgtype housestyle
roofstyle bsmtfintype1 heatingqc centralair electrical kitchenqual garagetype garagefinish saletype;
run;

proc glmselect data = train2 plots= all; 
class Neighborhood mszoning lotshape lotconfig condition1 condition2 bldgtype housestyle
roofstyle bsmtfintype1 heatingqc centralair electrical kitchenqual garagetype garagefinish saletype; 
model logsaleprice = overallqual overallcond yearbuilt yearremodadd bsmtfinsf1 bsmtfinsf2
bsmtunfsf grlivarea fullbath halfbath bedroomabvgr totrmsabvgrd fireplaces garagecars garagearea 
wooddecksf openporchsf enclosedporch screenporch poolarea yrsold neighborhood mszoning lotshape lotconfig condition1 condition2 bldgtype housestyle
roofstyle bsmtfintype1 heatingqc centralair electrical kitchenqual garagetype garagefinish saletype
/ selection= Forward(stop=CV) cvmethod=random(5) stat=adjrsq;
run;

proc glmselect data = train2; 
class Neighborhood mszoning lotshape lotconfig condition1 condition2 bldgtype housestyle
roofstyle bsmtfintype1 heatingqc centralair electrical kitchenqual garagetype garagefinish saletype; 
model logsaleprice = overallqual overallcond yearbuilt yearremodadd bsmtfinsf1 bsmtfinsf2
bsmtunfsf grlivarea fullbath halfbath bedroomabvgr totrmsabvgrd fireplaces garagecars garagearea 
wooddecksf openporchsf enclosedporch screenporch poolarea yrsold neighborhood mszoning lotshape lotconfig condition1 condition2 bldgtype housestyle
roofstyle bsmtfintype1 heatingqc centralair electrical kitchenqual garagetype garagefinish saletype
/ selection= Stepwise(stop=CV) cvmethod=random(5) stat=adjrsq;
run;

proc glmselect data = train2; 
class Neighborhood mszoning lotshape lotconfig condition1 condition2 bldgtype housestyle
roofstyle bsmtfintype1 heatingqc centralair electrical kitchenqual garagetype garagefinish saletype; 
model logsaleprice = overallqual overallcond yearbuilt yearremodadd bsmtfinsf1 bsmtfinsf2
bsmtunfsf grlivarea fullbath halfbath bedroomabvgr totrmsabvgrd fireplaces garagecars garagearea 
wooddecksf openporchsf enclosedporch screenporch poolarea yrsold neighborhood mszoning lotshape lotconfig condition1 condition2 bldgtype housestyle
roofstyle bsmtfintype1 heatingqc centralair electrical kitchenqual garagetype garagefinish saletype
/ selection= Backward(stop=CV) cvmethod=random(5) stat=adjrsq;
run;

proc glm data = train2 plots= all; 
class Neighborhood mszoning lotshape lotconfig condition1 condition2 bldgtype housestyle
roofstyle bsmtfintype1 heatingqc centralair electrical kitchenqual garagetype garagefinish saletype; 
model logsaleprice = overallqual overallcond yearbuilt yearremodadd bsmtfinsf1 bsmtfinsf2
bsmtunfsf grlivarea fullbath halfbath bedroomabvgr totrmsabvgrd fireplaces garagecars garagearea 
wooddecksf openporchsf enclosedporch screenporch poolarea yrsold neighborhood mszoning lotshape lotconfig condition1 condition2 bldgtype housestyle
roofstyle bsmtfintype1 heatingqc centralair electrical kitchenqual garagetype garagefinish saletype;
run;

proc glm data = train2 plots = all; 
class neighborhood MSZoning BldgType CentralAir KitchenQual;
model logsaleprice = OverallQual OverallCond GrLivArea Neighborhood BsmtFinSF1 YearBuilt 
GarageArea BsmtUnfSF BsmtFinSF2 MSZoning Fireplaces YearRemodAdd BldgType GarageCars 
CentralAir ScreenPorch WoodDeckSF OpenPorchSF EnclosedPorch KitchenQual HalfBath FullBath 
YrSold BedroomAbvGr;
run;


proc glm data = train2 plots= all; 
class Neighborhood Mszoning LotShape LotConfig Condition1 BldgType BsmtFinType1 HeatingQC CentralAir Electrical 
KitchenQual GarageType GarageFinish SaleType;
model logsaleprice = OverallQual OverallCond YearBuilt YearRemodAdd BsmtFinSF1 BsmtFinSF2 BsmtUnfSF 
GrLivArea FullBath HalfBath BedroomAbvGr TotRmsAbvGrd Fireplaces GarageCars GarageArea 
WoodDeckSF OpenPorchSF EnclosedPorch ScreenPorch PoolArea YrSold Neighborhood MSZoning 
LotShape LotConfig Condition1 BldgType BsmtFinType1 HeatingQC CentralAir Electrical 
KitchenQual GarageType GarageFinish SaleType; 
run;

proc glmselect data = train2 plots= all; 
class Neighborhood Mszoning LotShape LotConfig Condition1 BldgType BsmtFinType1 HeatingQC CentralAir Electrical 
KitchenQual GarageType GarageFinish SaleType HouseStyle RoofMatl Functional SaleCondition
Exterior1st Heating LandSlope GarageQual Foundation LotFrontage GarageCond ExterCond Street;
model logsaleprice = GrLivArea Neighborhood GarageCars OverallCond HouseStyle YearBuilt
 RoofMatl BsmtFinSF1 MSZoning Functional Condition1 SaleCondition KitchenQual LotArea 
Condition1 Exterior1st ScreenPorch Heating LandSlope WoodDeckSF TotalBsmtSF LotConfig 
CentralAir GarageQual BsmtFullBath Fireplaces YearRemodAdd GarageArea 
Foundation LotFrontage KitchenAbvGr GarageCond SaleType ExterCond Street HalfBath
/ selection= Forward(stop=CV) cvmethod=random(5) stat=adjrsq;
run;

proc glmselect data = combine; 
class Neighborhood mszoning lotshape lotconfig condition1 condition2 bldgtype housestyle
roofstyle bsmtfintype1 heatingqc centralair electrical kitchenqual garagetype garagefinish saletype; 
model logsaleprice = overallqual overallcond yearbuilt yearremodadd bsmtfinsf1 bsmtfinsf2
bsmtunfsf grlivarea fullbath halfbath bedroomabvgr totrmsabvgrd fireplaces garagecars garagearea 
wooddecksf openporchsf enclosedporch screenporch poolarea yrsold neighborhood mszoning lotshape lotconfig condition1 condition2 bldgtype housestyle
roofstyle bsmtfintype1 heatingqc centralair electrical kitchenqual garagetype garagefinish saletype
/ selection= Forward(stop=CV) cvmethod=random(5) stat=adjrsq;
output out = results p = Predict;
run;


data results2;
set results;
logprice = Predict;
if Predict >0 then logprice = Predict;
if Predict <= 0 then logprice = 9.21034;
keep id SalePrice logprice;
where id > 1460;

proc print data=results2(obs=5);
run;

data results3;
set results2;
if exp(logprice) > 0 then SalePrice = exp(logprice);
if exp(logprice)<= 0 then SalePrice = 10000;
keep id SalePrice;
where id > 1460;
;

proc print data = results3(obs=5);
run;

PROC EXPORT DATA= WORK.RESULTS3 
            OUTFILE= "/home/u62637052/export/backward.csv" 
            DBMS=CSV REPLACE;
     PUTNAMES=YES;
RUN;

proc glmselect data = combine; 
class Neighborhood mszoning lotshape lotconfig condition1 condition2 bldgtype housestyle
roofstyle bsmtfintype1 heatingqc centralair electrical kitchenqual garagetype garagefinish saletype; 
model logsaleprice = overallqual overallcond yearbuilt yearremodadd bsmtfinsf1 bsmtfinsf2
bsmtunfsf grlivarea fullbath halfbath bedroomabvgr totrmsabvgrd fireplaces garagecars garagearea 
wooddecksf openporchsf enclosedporch screenporch poolarea yrsold neighborhood mszoning lotshape lotconfig condition1 condition2 bldgtype housestyle
roofstyle bsmtfintype1 heatingqc centralair electrical kitchenqual garagetype garagefinish saletype
/ selection= Stepwise(stop=CV) cvmethod=random(5) stat=adjrsq;
output out = results p = Predict;
run;

proc glmselect data = combine; 
class Neighborhood mszoning lotshape lotconfig condition1 condition2 bldgtype housestyle
roofstyle bsmtfintype1 heatingqc centralair electrical kitchenqual garagetype garagefinish saletype; 
model logsaleprice = overallqual overallcond yearbuilt yearremodadd bsmtfinsf1 bsmtfinsf2
bsmtunfsf grlivarea fullbath halfbath bedroomabvgr totrmsabvgrd fireplaces garagecars garagearea 
wooddecksf openporchsf enclosedporch screenporch poolarea yrsold neighborhood mszoning lotshape lotconfig condition1 condition2 bldgtype housestyle
roofstyle bsmtfintype1 heatingqc centralair electrical kitchenqual garagetype garagefinish saletype
/ selection= Backward(stop=CV) cvmethod=random(5) stat=adjrsq;
output out = results p = Predict;
run;

proc glmselect data = combine; 
class Neighborhood mszoning lotshape lotconfig condition1 condition2 bldgtype housestyle
roofstyle bsmtfintype1 heatingqc centralair electrical kitchenqual garagetype garagefinish saletype; 
model logsaleprice = overallqual overallcond yearbuilt yearremodadd bsmtfinsf1 bsmtfinsf2
bsmtunfsf grlivarea fullbath halfbath bedroomabvgr totrmsabvgrd fireplaces garagecars garagearea 
wooddecksf openporchsf enclosedporch screenporch poolarea yrsold neighborhood mszoning lotshape lotconfig condition1 condition2 bldgtype housestyle
roofstyle bsmtfintype1 heatingqc centralair electrical kitchenqual garagetype garagefinish saletype
/ selection= Backward(stop=CV) cvmethod=random(5) stat=adjrsq;
output out = results p = Predict;
run;
