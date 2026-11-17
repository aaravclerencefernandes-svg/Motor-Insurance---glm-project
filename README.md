# Motor Insurance Claim Risk Modelling

An actuarial data science project investigating the factors associated
with motor insurance claim occurrence and evaluating statistical and
machine-learning approaches to risk segmentation.

## Project overview

This project analyses **58,592 motor insurance policies**, including
**3,748 policies with at least one claim** (an observed claim incidence
of approximately **6.40%**).

The objective is to investigate how policyholder, vehicle, policy and
regional characteristics relate to claim occurrence, and to compare
interpretable statistical models with more flexible predictive methods.

The response variable is binary:

-   `1` --- at least one claim recorded
-   `0` --- no claim recorded

The analysis focuses on claim occurrence. It does not model claim counts
or claim severity and should not be interpreted as a complete insurance
pricing model.

## Methods and techniques

The project combines actuarial statistical modelling with exploratory
machine learning:

-   **Logistic regression / GLMs** to estimate associations between
    explanatory variables and claim occurrence.
-   **Spline modelling** to investigate nonlinear relationships,
    particularly between subscription length and claim occurrence.
-   **Interaction modelling** to examine whether the relationship
    between vehicle age and subscription length varies jointly.
-   **Classification trees** to identify segments with differing
    observed claim rates.
-   **Risk scoring and segmentation** to group policies into risk bands
    and calculate observed risk relativities.
-   **LASSO regression** for penalised variable selection.
-   **Random forests** with permutation-based variable importance.
-   **Gradient boosting** to assess whether a more flexible model
    improves predictive discrimination.
-   **Model evaluation** using ROC-AUC, calibration, lift,
    likelihood-ratio testing and repeated stratified cross-validation.

## Key results

  Measure                                                                 Result
  ---------------------------------- -------------------------------------------
  Policies analysed                                                       58,592
  Policies with at least one claim                                         3,748
  Overall observed claim incidence                                         6.40%
  Train/test split                                                     70% / 30%
  Cross-validation                     Repeated stratified 5-fold CV (3 repeats)
  Best reported out-of-fold AUC                       0.6555 (gradient boosting)
  Interaction GLM out-of-fold AUC                                         0.6271

Additional findings:

-   Subscription length and vehicle age were among the most influential
    predictors across the fitted models.
-   A spline specification provided a better fit than a simple linear
    specification for subscription length.
-   Adding a vehicle-age-by-subscription-length interaction improved the
    cross-validated AUC of the spline GLM from approximately **0.6119 to
    0.6271**.
-   The four risk bands had observed claim rates ranging from
    approximately **3.70% to 10.64%**.
-   Regional characteristics added statistically significant information
    beyond vehicle characteristics, although overall discrimination
    remained modest.
-   Vehicle safety and engine effects were difficult to isolate because
    vehicle specifications were highly correlated and the dataset
    contained only 11 distinct vehicle model/specification combinations.

**Interpretation:** The reported AUC values indicate moderate
discriminatory ability, not classification accuracy. The results are
exploratory and do not establish causal relationships.

## Tools and R packages

The analysis was conducted in **R 4.5.3** on Windows 11.

Main packages used include:

-   `pROC`
-   `rpart`
-   `glmnet`
-   `ranger`
-   `lme4`
-   `gbm`

The supplied analysis script records the modelling workflow and random
seed (`061024`). Check the script for the complete package requirements
and any additional dependencies.

## Repository structure

A suggested structure for publishing the project is:

``` text
motor-insurance-claim-risk-modelling/
├── README.md
├── R/
│   └── motor_insurance_analysis.R
├── figures/
│   ├── fig01_corr_engine_size.png
│   └── ...
├── outputs/
│   ├── model_tables/
│   └── model_summaries/
└── report/
    └── project_report.pdf
```

Adjust the filenames and folders to match the files actually uploaded to
the repository. The README does not assume that the dataset or generated
figures are included.

## Reproducing the analysis

1.  Install R (the original analysis used R 4.5.3).
2.  Obtain the dataset from its authorised source. The underlying
    policy-level CSV data is not included in this repository.
3.  Install the packages required by the R script.
4.  Update any input and output file paths in the script for your local
    environment.
5.  Run the script to reproduce the data preparation, exploratory
    analysis, model fitting and figures.

For reproducibility, retain the random seed and record package versions
when rerunning the analysis.

## Limitations

-   The outcome is claim occurrence only; claim frequency and claim
    severity are not modelled.
-   Only 11 distinct vehicle model/specification combinations are
    represented, limiting vehicle-level inference.
-   Vehicle characteristics exhibit substantial multicollinearity.
-   The validation uses a random split and repeated cross-validation
    within the same dataset; it is not external or out-of-time
    validation.
-   Predictive discrimination is moderate, and further calibration,
    stability and fairness assessment would be needed before operational
    use.
-   The results should not be used directly to set commercial premiums.

## Future work

Potential extensions include:

-   Expanding the dataset to include a wider range of vehicle models and
    specifications.
-   Incorporating mileage, driving history and prior claims experience.
-   Modelling claim counts and claim costs using frequency-severity
    methods.
-   Testing the models on a later policy period or an independent
    portfolio.
-   Further assessing calibration, model stability, fairness and
    operational interpretability.

## Disclaimer

This repository is an academic actuarial modelling project. The analysis
is for educational and research purposes and does not constitute
insurance pricing advice. No underlying policy-level data should be
published unless its owner has explicitly authorised public release.
