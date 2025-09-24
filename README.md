# Estimation and inference in generalised linear models with constrained iteratively-reweighted least squares

A full simulation study and two reproducible real-world case studies.

-----

> [!WARNING] 
> The manuscript is currently being peer-reviewed. This means the content in this repo might be subject to future changes.

This repository stores the code and data to reproduce all results from the arXiv preprint

> Masselot, P., Nenon, D., Vanoli, J., Chalabi, Z., Gasparrini, A., 2025. Estimation and inference in generalised linear models with constrained iteratively-reweighted least squares. [doi:10.48550/arXiv.2509.18406](https://doi.org/10.48550/arXiv.2509.18406)

This manuscript introduces a simple framework for generalised linear models (GLM) subject to linear constraints on the coefficients. We propose a simple algorithm to fit the model which is a constrained version of the classical iteratively-reweighted least-squares (CIRLS). We also propose an inference procedure for estimated constrained coefficients, as well as formulas for degrees of freedom.

This repository performs the full simulation study and the two case studies in the manuscript. 

The methods proposed in this manuscript are implemented in the R package [`cirls`](https://cran.r-project.org/web/packages/cirls/index.html). Although stable and already available on CRAN, the package is still under development. More specifically, the documentation needs to be more fully developed while some convenience functions are still missing. This repo works with the latest CRAN release (version 0.4.0), but more updated versions can be installed from the [`cirls` GitHub repo](https://github.com/PierreMasselot/cirls).
