
# Übersicht über die Berechnungen

``` r
cat("```mermaid\n")
```

``` mermaid

```r
cat(target_diagram, sep = "\n")
```

graph LR style Legend fill:#FFFFFF00,stroke:#000000; style Graph
fill:#FFFFFF00,stroke:#000000; subgraph Legend direction LR
x7420bd9270f8d27d(\[““Up to date””\]):::uptodate —
xa8565c104d8f0705(\[““Dispatched””\]):::dispatched
xa8565c104d8f0705(\[““Dispatched””\]):::dispatched —
x70a5fa6bea6f298d\[““Pattern””\]:::none
x70a5fa6bea6f298d\[““Pattern””\]:::none —
xbf4603d6c2c2ad6b(\[““Stem””\]):::none
xbf4603d6c2c2ad6b(\[““Stem””\]):::none —
x5bffbffeae195fc9{{““Object”“}}:::none end subgraph Graph direction LR
xf5f210ccb195b497(\[”main_effects<br>3.487 seconds”\]):::uptodate –\>
x1d61bc0c70fd3877\[”biv_claimnb_formulae<br>0.004 seconds<br>7
branches”\]:::uptodate xe4ff46ac843db806(\[”mtpl2<br>3.02
seconds”\]):::uptodate –\> x5933a35c9d0c6709(\[”mtpl2_train<br>0.065
seconds”\]):::uptodate xe4ff46ac843db806(\[”mtpl2<br>3.02
seconds”\]):::uptodate –\> x22fca3134c52eb9f(\[”mtpl2_test<br>0.11
seconds”\]):::uptodate x5933a35c9d0c6709(\[”mtpl2_train<br>0.065
seconds”\]):::uptodate –\> x22fca3134c52eb9f(\[”mtpl2_test<br>0.11
seconds”\]):::uptodate xb409a192cad3d5fc(\[”exploration<br>58.823
seconds”\]):::uptodate –\> xb409a192cad3d5fc(\[”exploration<br>58.823
seconds”\]):::uptodate x6e52cb0f1668cc22(\[”readme<br>4.063
seconds”\]):::dispatched –\> x6e52cb0f1668cc22(\[”readme<br>4.063
seconds”\]):::dispatched
xd51e323eb90213dd{{”project_packages”}}:::uptodate –\>
xd51e323eb90213dd{{“project_packages”}}:::uptodate end classDef uptodate
stroke:#000000,color:#ffffff,fill:#354823; classDef dispatched
stroke:#000000,color:#000000,fill:#DC863B; classDef none
stroke:#000000,color:#000000,fill:#94a4ac; linkStyle 0 stroke-width:0px;
linkStyle 1 stroke-width:0px; linkStyle 2 stroke-width:0px; linkStyle 3
stroke-width:0px; linkStyle 8 stroke-width:0px; linkStyle 9
stroke-width:0px; linkStyle 10 stroke-width:0px;

``` r
cat("\n```")
```

\`\`\`
