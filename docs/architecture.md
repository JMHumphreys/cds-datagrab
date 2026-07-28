# Architecture

`inventory -> date plan -> request manifest -> download -> raw normalization -> daily extraction -> template standardization -> daily validation -> weekly aggregation -> estimate reconciliation -> future analogs -> final manifest`.

ERA5 uses `2m_temperature` with `daily_minimum` over 6-hourly samples. Values are converted from kelvin to Celsius. The protected study-area raster is the sole target geometry. Estimated (`_est`) files are excluded from observed coverage and climatological donors.
