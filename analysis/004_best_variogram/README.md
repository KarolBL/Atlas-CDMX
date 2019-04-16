# 004_best_variogram

- Get the RMSE metrics to select the winner variogram model.
- Obtain the variogram comparison:
	- Facette plot
	- Best variogram model

# Tested models

- metric:          vgmST("metric",                  joint, stAni)
- separable:       vgmST("separable",  space, time, sill)
- productSum:      vgmST("productSum", space, time, k)
- sumMetric:       vgmST("sumMetric",  space, time, joint, stAni)
- simpleSumMetric: vgmST("simpleSumMetric", space, time, joint, nugget, stAni)

# Inputs:

 --file: Previous module RData file containing ```vgm_initial``` and ```fittedSTVariograms```.
 --out: RData with the best vgmST model fior kriging.
 --initial: Initial vgm guesses used for fitting the data.
 --rmse: Root Mean Square Error table for every tested covariance model.
 --winner: Which one was the winner vgm combination under the tested covariance structure.   
 --best: Which one was the best vgmST of all.
 --pdf: vgmST facette plot.

