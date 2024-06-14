This repository holds materials (code and data) relating to the editorial
"Mortality disparities across US States: excess deaths during the COVID-19 pandemic and preexisting mortality shortfalls".

1. We take the new Woolf at al excess estimates by state,
2. accumulate excess over phases 1-4 of the pandemic
3. distribute excess over age groups using an indirect method
4. age standardize excess rates
5. construct a best practice mortality standard
6. treat the difference between baseline mortality and best practice mortality by age as a second form of excess (baseline mortality shortfall)
7. plot and compare a variety of metrics, such as age-standardized excess rates, relative rates, p-scores, and so on
some graphs include color coding for governors' political party, others not.

The main code is in scripts
`00_setup.R`
`01_excess_age_estimates.R`
`02_age_standardization_excess_shortfall.R`
`03_figures.R`

Each script sources the prior step. If you just want to produce the graphs, open step 3, the necessary data objects will be produced. 
We suggest you step through the code rather than running it blindly all at once. 
On the first run, some installations may run. Data processing steps are lightly annotated, tips welcome.
