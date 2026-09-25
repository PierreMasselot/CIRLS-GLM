# A constrained iteratively-reweighted least-squares framework for generalised linear models

[![DOI](https://zenodo.org/badge/944473173.svg)](https://doi.org/10.5281/zenodo.18746187)

-----

> [!WARNING] 
> This work is undergoing peer-reviewing and is expected to change in the future.

This repository stores the code and data to reproduce all results from the arXiv preprint

> Masselot, P., Nenon, D., Vanoli, J., Chalabi, Z., Gasparrini, A., 2026. A constrained iteratively-reweighted least-squares framework for generalised linear models. [doi:10.48550/arXiv.2509.18406](https://doi.org/10.48550/arXiv.2509.18406)

This manuscript introduces a full framework for generalised linear models (GLM) subject to linear constraints on the coefficients. We propose an efficient algorithm to fit the model which is a constrained version of the classical iteratively-reweighted least-squares (CIRLS). We also propose an inference procedure for estimated constrained coefficients, as well as formulas for degrees of freedom.

This repository performs the full simulation study and the case study in the manuscript. 

The methods proposed in this manuscript are implemented in the R package [`cirls`](https://cran.r-project.org/web/packages/cirls/index.html). Although stable and already available on CRAN, the package is still under development. This repo works with recent updates from the [`cirls` GitHub repo](https://github.com/PierreMasselot/cirls). The exact version of the package used to produce the last results is found in the `packageVersion.txt` file. 
