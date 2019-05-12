# 003_mortality_variogram

## Idea

Get the empirical variogram and apply different variogram models, to find the
one that outperforms the others according to the data.

## Sample spatio-temporal variogram

Use the available data to build it.

## Propose initial guesses

Let's get the mean of each row(time)/column(space) for the initial guess:  
- ```nugget```: mean(gamma[1:3])
- ```sill```: mean(gamma[-(0:4)+length(gamma)])
- ```range```: 1/3 estación más lejana

## Variogrmas to try

- metric:          vgmST("metric",                  joint, stAni)
- separable:       vgmST("separable",  space, time, sill)
- productSum:      vgmST("productSum", space, time, k)
- sumMetric:       vgmST("sumMetric",  space, time, joint, stAni)
- simpleSumMetric: vgmST("simpleSumMetric", space, time, joint, nugget, stAni)


