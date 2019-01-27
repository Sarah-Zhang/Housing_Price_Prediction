# test parameters with NO stars
model0:
SaleTypeCon          1.043e-01  1.042e-01   1.001 0.316815
OpenPorchSF          1.248e-04  6.394e-05   1.952 0.051141 .
GarageArea           1.240e-05  3.254e-05   0.381 0.703219
HeatingQC.L          6.294e-02  3.562e-02   1.767 0.077422 .  
*** HeatingQC.Q         -3.879e-03  2.165e-02  -0.179 0.857786  ***  
BsmtExposure.L       3.486e-03  1.148e-02   0.304 0.761539 
ExterCond.L          5.663e-02  3.568e-02   1.587 0.112717
MasVnrArea           4.705e-05  2.556e-05   1.840 0.065912 .  
NeighborhoodVeenker  7.273e-02  4.550e-02   1.599 0.110111
StreetPave           9.996e-02  6.142e-02   1.628 0.103829
MSZoningFV           6.853e-02  3.867e-02   1.772 0.076610 .

model1:
SaleTypeCon         1.041e-01  1.041e-01   1.000 0.317478
OpenPorchSF         1.246e-04  6.390e-05   1.950 0.051432 .
GarageArea          1.245e-05  3.252e-05   0.383 0.701869 
*** BsmtExposure.L      3.478e-03  1.148e-02   0.303 0.761986 ***
ExterCond.L         5.688e-02  3.564e-02   1.596 0.110724
MasVnrArea          4.700e-05  2.555e-05   1.839 0.066113 .
NeighborhoodVeenker 7.278e-02  4.548e-02   1.600 0.109742
StreetPave          9.970e-02  6.138e-02   1.624 0.104529
MSZoningFV          6.853e-02  3.866e-02   1.773 0.076508 .

model2:
SaleTypeCon         1.051e-01  1.040e-01   1.011 0.312288
OpenPorchSF         1.240e-04  6.385e-05   1.942 0.052310 . 
*** GarageArea          1.278e-05  3.250e-05   0.393 0.694228 ***
ExterCond.L         5.641e-02  3.559e-02   1.585 0.113223  
MasVnrArea          4.720e-05  2.554e-05   1.848 0.064781 . 
NeighborhoodVeenker 7.344e-02  4.541e-02   1.617 0.106075    
StreetPave          9.876e-02  6.128e-02   1.612 0.107272 
MSZoningFV          6.848e-02  3.865e-02   1.772 0.076605 . 

model3:
*** SaleTypeCon         1.053e-01  1.040e-01   1.012 0.311567 *** 
ExterCond.L         5.652e-02  3.558e-02   1.588 0.112419
MasVnrArea          4.759e-05  2.551e-05   1.866 0.062305 .
NeighborhoodVeenker 7.370e-02  4.539e-02   1.624 0.104697  
StreetPave          9.654e-02  6.100e-02   1.583 0.113730
MSZoningFV          6.774e-02  3.859e-02   1.755 0.079391 . 

model4:
OpenPorchSF         1.236e-04  6.369e-05   1.940 0.052534 . 
ExterCond.L         5.617e-02  3.558e-02   1.579 0.114662 
MasVnrArea          4.732e-05  2.551e-05   1.855 0.063799 .
NeighborhoodVeenker 8.285e-02  4.449e-02   1.862 0.062781 . 
*** StreetPave          9.624e-02  6.100e-02   1.578 0.114877 ***
MSZoningFV          6.924e-02  3.856e-02   1.796 0.072772 . 

model5:
OpenPorchSF         1.215e-04  6.371e-05   1.907 0.056766 . 
*** ExterCond.L         5.475e-02  3.559e-02   1.538 0.124152 *** 
MasVnrArea          4.796e-05  2.552e-05   1.879 0.060396 .
NeighborhoodVeenker 8.360e-02  4.451e-02   1.878 0.060533 .
MSZoningFV          6.989e-02  3.858e-02   1.812 0.070261 . 

model6:
MasVnrArea          4.794e-05  2.553e-05   1.878 0.060628 . 
NeighborhoodVeenker 8.653e-02  4.449e-02   1.945 0.051985 . 
*** MSZoningFV          6.892e-02  3.859e-02   1.786 0.074353 . ***
  
model7:
*** MasVnrArea          4.602e-05  2.553e-05   1.803 0.071666 . ***
NeighborhoodVeenker 8.726e-02  4.452e-02   1.960 0.050194 . 

model8:
OpenPorchSF         1.226e-04  6.345e-05   1.932 0.053573 .
*** NeighborhoodVeenker 8.320e-02  4.450e-02   1.870 0.061733 . ***
  
model9:
*** OpenPorchSF         1.216e-04  6.350e-05   1.915 0.055690 . ***

# test parameters with small stars
model10:
KitchenQual.Q       3.457e-02  1.503e-02   2.300 0.021584 * 
[WINNER] HeatingGasW         7.993e-02  3.675e-02   2.175 0.029813 *  [WINNER]
RoofMatlMembran     3.467e-01  1.473e-01   2.354 0.018698 * 
  
model11:
KitchenQual.Q       3.627e-02  1.503e-02   2.413 0.015935 *
[WINNER] RoofMatlMembran     3.427e-01  1.474e-01   2.324 0.020266 *  [WINNER]
  
model12:
[WINNER] KitchenQual.Q       3.665e-02  1.505e-02   2.435 0.015028 * [WINNER]
