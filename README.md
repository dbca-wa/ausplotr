# AusplotR

AusplotR converts the raw storage format (SQLite .db) of [AusPlot](http://www.ausplots.org/)'s 
field collection software to CSV.

An alternative to inspect the SQLite files is to use the Mozilla Firefox plugin
[SQLite Manager](https://addons.mozilla.org/en-US/firefox/addon/sqlite-manager/)
and run the queries:

```
-- species records
SELECT * FROM speciesRecord AS sr
LEFT JOIN transectPoints AS tp ON sr.transectPointId = tp.id
LEFT JOIN transects as tx ON tp.transectId = tx.id
LEFT JOIN plots as pl ON tx.plotId = pl.id;

-- vouchered vegetation
SELECT * FROM voucheredVeg AS vv LEFT JOIN plots AS pl ON vv.plotId = pl.id;

-- transects
SELECT * FROM transects AS tx LEFT JOIN plots AS pl ON tx.plotId = pl.id;
```
AusplotR runs the same queries behind the scenes, but selects the fields 
explicitly and ignores duplicate columns from joins.

### License
[![license](https://i.creativecommons.org/l/by-sa/4.0/88x31.png)](http://creativecommons.org/licenses/by-sa/4.0/)

AusplotR is not affiliated with [AusPlot](http://www.ausplots.org/).

AusplotR was inspired by the [RShiny Gallery SuperZIP example](http://shiny.rstudio.com/gallery/superzip-example.html)
