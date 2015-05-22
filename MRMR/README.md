Scripts for running the "more robust Mendelian randomization" scheme.

in the directory scripts/, running

>R --vanilla < 1_get_all_rho.R
>R --vanilla < 2_rank_all.R

will reproduce the analysis. The output is in all_ranked, with the column labeled "ratio" the relative likelihood of a causal versus a non-causal model for the phenotypes in P1 and P2. The four columns starting with AIC show the AICs for the four models.
