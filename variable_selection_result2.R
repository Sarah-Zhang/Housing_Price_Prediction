# BIC removal 
+  KitchenQual.Q        1   0.12072 28.879 -5421.7
+  RoofMatlMembran      1   0.11213 28.887 -5421.3
+  HeatingGasW          1   0.10425 28.895 -5420.9
+  OpenPorchSF          1   0.07363 28.926 -5419.3
+  NeighborhoodVeenker  1   0.06962 28.930 -5419.1
+  ExterCond.L          1   0.05504 28.944 -5418.4
+  StreetPave           1   0.05226 28.947 -5418.2
+  MSZoningFV           1   0.04903 28.950 -5418.1
+  MasVnrArea           1   0.04798 28.951 -5418.0
+  SaleTypeCon          1   0.03298 28.966 -5417.3
+  GarageArea           1   0.00380 28.996 -5415.8
+  BsmtExposure.L       1   0.00151 28.998 -5415.7
+  HeatingQC.Q          1   0.00019 28.999 -5415.6


#model_new
[WINNER] NeighborhoodClearCr 4.555e-02  2.542e-02   1.792 0.073407 . [WINNER]
RoofMatlWdShngl     1.387e-01  7.446e-02   1.863 0.062738 . 
PoolArea            4.486e-03  1.835e-03   2.444 0.014658 * 
  
#model_new1 
[WINNER] RoofMatlWdShngl     1.494e-01  7.429e-02   2.011 0.044556 * [WINNER]
PoolArea            4.597e-03  1.836e-03   2.504 0.012408 * 
  
#model_new2
[WINNER] PoolArea            4.630e-03  1.838e-03   2.519 0.011894 * [WINNER]



# BIC Removal after outlier removal
- NeighborhoodClearCr  1    0.0319 13.096 -6000.4
- RoofMatlWdShngl      1    0.0345 13.098 -6000.1
- PoolArea             1    0.0593 13.123 -5997.5


