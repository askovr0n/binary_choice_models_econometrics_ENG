# Project 9 ENG: Binary choice models

- The aim of this paper is mainly focused on the properties and the diagnosis of the model estimating the probability of default, rather than the correct estimation of the results on the test set
- The data for the model was obtained from a competition called ["Give Me Some Credit!"](https://www.kaggle.com/datasets/brycecf/give-me-some-credit-dataset) from Kaggle.com
- Three models were performed in order to get estimations: **logit, probit and OLS for binary dependent variable (Linear Probability Model) using White's robust matrix**
- The **AIC and BIC information criterion** verified that the probit model appeared to be the most optimal and was therefore used for the following analyses
- A **"general-to-specific"** procedure was used to eliminate irrelevant variables
- The correct functional form of the model was checked using **Linktest, Hosmer-Lemeshow test and Osius-Rojek test**
- The research hypotheses were verified using the **Likelihood Ratio Test**. **Marginal effects** were also determined for the model and some R^2 statistics were interpreted
- The disadvantage of the model is certainly that it has an incorrect functional form, which will certainly affect the correct interpretation of the parameters (nevertheless, an attempt was made to repair this model, which was unsuccessful)

Marginal Effects     |  Distribution of defaults
:-------------------------:|:-------------------------:
![](https://github.com/askovr0n/Portfolio/blob/main/images/Project_9/marginal_effects.png)  |  ![](https://github.com/askovr0n/Portfolio/blob/main/images/Project_9/dist_defaults.png)
