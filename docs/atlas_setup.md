# Atlas setup

Load `r/4.5`, `udunits`, `gdal`, `proj`, and `geos`; run `scripts/check_dependencies.R --mode plan`. The current template is a validated WGS84 Albers raster with kilometer units and approximately 25-km cells; do not repair or rescale it.

Set `CDS_DATAGRAB_ROOT=/project/disease_ecology/cds-datagrab`; the local default is `runtime/cds-datagrab`. Use `hpc/submit_era5_mintemp.sh` for official submission so SLURM logs are placed beneath the dedicated root. The first real CDS execution should be the three-day smoke download, inspected with `scripts/inspect_smoke_download.R`, before any historical update.

Example smoke submission:

```bash
CDS_DATAGRAB_ROOT=/project/disease_ecology/cds-datagrab PROFILE=smoke \
CONFIG=config/era5_mintemp_smoke.yml MODE=full DRY_RUN=false \
OBSERVED_END=2026-07-03 bash hpc/submit_era5_mintemp.sh
```

Credentials are read by R through `ecmwfr_PAT`; the token is never printed. The root marker `.cds-datagrab-root` identifies the storage root and is required for destructive operations.
### Northern-edge recovery

The corrected workflow uses `template_bbox_union`, a one-degree margin, and outward alignment to the ERA5 0.25-degree grid. The former GPKG-only request can leave a fixed northern gap even when values are otherwise plausible. Incomplete daily files are invalid, do not contribute to date planning or climatology, and are quarantined under the active dataset root before the corrected dates are requested. The new request area changes the deterministic raw filename hash; the previous raw file remains available for audit.
