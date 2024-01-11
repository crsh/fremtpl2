
# Vorhersage von Schadenshäufigkeiten und -summen im French Motor TPL Insurance Claims-Datensatz

Nach einer Datenexploration und inhaltlich begründeten Prädiktorauswahl
(`results/exploration.html`) versuche ich mit Hilfe von Generalized
Additive Models (GAM) die Schadenshäufigkeiten und -summen
vorherzusagen. Hierfür nutze ein GAM mit Poisson-Verteilung für die
Schadenshäufigkeiten und ein GAM mit Lognormal-Verteilung für die
Schadenssummen. Die Modelle werden in R mit Bayesischen Methoden
geschätzt (`brms`) und durch Leave-one-out Cross-Validation sowie einen
Hold-out Cross-Validation verglichen (`loo`). Hierzu wird das
Make-ähnlichen Pipeline tools `targets` genutzt.

## Softwarevoraussetzungen

Die benötigte Software wird in der Datei `DESCRIPTION` aufgelistet.

## Übersicht über die Berechnungen

``` mermaid
graph LR
  style Legend fill:#FFFFFF00,stroke:#000000;
  style Graph fill:#FFFFFF00,stroke:#000000;
  subgraph Legend
    direction LR
    x7420bd9270f8d27d([""Up to date""]):::uptodate --- xa8565c104d8f0705([""Dispatched""]):::dispatched
    xa8565c104d8f0705([""Dispatched""]):::dispatched --- x0a52b03877696646([""Outdated""]):::outdated
    x0a52b03877696646([""Outdated""]):::outdated --- xbf4603d6c2c2ad6b([""Stem""]):::none
    xbf4603d6c2c2ad6b([""Stem""]):::none --- x70a5fa6bea6f298d[""Pattern""]:::none
    x70a5fa6bea6f298d[""Pattern""]:::none --- x5bffbffeae195fc9{{""Object""}}:::none
  end
  subgraph Graph
    direction LR
    xe4ff46ac843db806(["mtpl2<br>3.02 seconds"]):::uptodate --> x5933a35c9d0c6709(["mtpl2_train<br>0.033 seconds"]):::uptodate
    x7a958efee91e4e79(["baseline_claimnb_loo<br>1.305 minutes"]):::uptodate --> x8750d234f7b1499e["biv_claimnb_loo<br>5.026 minutes<br>7 branches"]:::outdated
    x9ff6aee0d1adbf87["biv_claimnb_fits<br>19.755 minutes<br>7 branches"]:::dispatched --> x8750d234f7b1499e["biv_claimnb_loo<br>5.026 minutes<br>7 branches"]:::outdated
    xe4ff46ac843db806(["mtpl2<br>3.02 seconds"]):::uptodate --> x22fca3134c52eb9f(["mtpl2_test<br>0.123 seconds"]):::uptodate
    x5933a35c9d0c6709(["mtpl2_train<br>0.033 seconds"]):::uptodate --> x22fca3134c52eb9f(["mtpl2_test<br>0.123 seconds"]):::uptodate
    x9ff6aee0d1adbf87["biv_claimnb_fits<br>19.755 minutes<br>7 branches"]:::dispatched --> xe153f82b0cc673b3["biv_claimnb_ho_elpd<br>14.805 seconds<br>7 branches"]:::outdated
    x22fca3134c52eb9f(["mtpl2_test<br>0.123 seconds"]):::uptodate --> xe153f82b0cc673b3["biv_claimnb_ho_elpd<br>14.805 seconds<br>7 branches"]:::outdated
    x1d61bc0c70fd3877["biv_claimnb_formulae<br>0.002 seconds<br>8 branches"]:::uptodate --> x9ff6aee0d1adbf87["biv_claimnb_fits<br>19.755 minutes<br>7 branches"]:::dispatched
    x5933a35c9d0c6709(["mtpl2_train<br>0.033 seconds"]):::uptodate --> x9ff6aee0d1adbf87["biv_claimnb_fits<br>19.755 minutes<br>7 branches"]:::dispatched
    x5933a35c9d0c6709(["mtpl2_train<br>0.033 seconds"]):::uptodate --> xf7f75ee0ec299ab5(["mtpl2_amount_train<br>0.038 seconds"]):::outdated
    xe153f82b0cc673b3["biv_claimnb_ho_elpd<br>14.805 seconds<br>7 branches"]:::outdated --> x1e2bb56d20db271c(["biv_claimnb_ho_compare<br>0.011 seconds"]):::outdated
    x56679c96a8ed6ef1(["predictors<br>3.607 seconds"]):::uptodate --> x1d61bc0c70fd3877["biv_claimnb_formulae<br>0.002 seconds<br>8 branches"]:::uptodate
    x7a958efee91e4e79(["baseline_claimnb_loo<br>1.305 minutes"]):::uptodate --> xe02c5f77857c3839(["biv_claimnb_loo_compare<br>0.019 seconds"]):::outdated
    x8750d234f7b1499e["biv_claimnb_loo<br>5.026 minutes<br>7 branches"]:::outdated --> xe02c5f77857c3839(["biv_claimnb_loo_compare<br>0.019 seconds"]):::outdated
    x5165672d6580782a(["baseline_claimnb_fit<br>51.236 seconds"]):::uptodate --> x7a958efee91e4e79(["baseline_claimnb_loo<br>1.305 minutes"]):::uptodate
    x5933a35c9d0c6709(["mtpl2_train<br>0.033 seconds"]):::uptodate --> x5165672d6580782a(["baseline_claimnb_fit<br>51.236 seconds"]):::uptodate
    x22fca3134c52eb9f(["mtpl2_test<br>0.123 seconds"]):::uptodate --> xdc9dce57825c4cec(["mtpl2_amount_test<br>1.793 seconds"]):::uptodate
    xb409a192cad3d5fc(["exploration<br>49.214 seconds"]):::uptodate --> xb409a192cad3d5fc(["exploration<br>49.214 seconds"]):::uptodate
    xd51e323eb90213dd{{"project_packages"}}:::uptodate --> xd51e323eb90213dd{{"project_packages"}}:::uptodate
  end
  classDef uptodate stroke:#000000,color:#ffffff,fill:#354823;
  classDef dispatched stroke:#000000,color:#000000,fill:#DC863B;
  classDef outdated stroke:#000000,color:#000000,fill:#78B7C5;
  classDef none stroke:#000000,color:#000000,fill:#94a4ac;
  linkStyle 0 stroke-width:0px;
  linkStyle 1 stroke-width:0px;
  linkStyle 2 stroke-width:0px;
  linkStyle 3 stroke-width:0px;
  linkStyle 4 stroke-width:0px;
  linkStyle 22 stroke-width:0px;
  linkStyle 23 stroke-width:0px;
```
