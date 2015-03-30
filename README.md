# cRomppass

An R package to score an affinity purification mass spectrometry
(AP-MS) proteomics experiment for interactor confidence.

For any given affinity purification mass spectrometry experiment
(AP-MS), a large number of non-specific background proteins will
contaminate the results unless they are screened out. cRomppass
implements the CompPASS algorithm to score a set of bait-prey
interactions from several AP-MS experiments against each other or a
separate set of prior experiments.

Install this easily in your R console using the devtools package:

    library("devtools")
    install_github("dnusinow/cRomppass")

