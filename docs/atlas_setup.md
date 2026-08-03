# Atlas setup

Load `r/4.5`, `udunits`, `gdal`, `proj`, and `geos`; run `scripts/check_dependencies.R --mode plan`. The current template is a validated WGS84 Albers raster with kilometer units and approximately 25-km cells; do not repair or rescale it.

The Git checkout is source code, not an installed R package. Use an external, compute-visible library shared by the deployment and worker, for example:

```bash
git pull --ff-only
module load r/4.5 udunits gdal proj geos
export CDS_DATAGRAB_R_LIB=/project/disease_ecology/cds-datagrab-r-library/4.5
bash hpc/install_cdsdatagrab_atlas.sh /project/disease_ecology/cds-datagrab
R_LIBS_USER="$CDS_DATAGRAB_R_LIB" Rscript hpc/preflight_cdsdatagrab.R
```

The installer records the source commit in `.cds-datagrab-installed-commit`. The worker validates that marker against the checkout before pipeline initialization; rerun the installer after every source update. Do not install into the checkout or a repository `.r_lib` directory.

Keep the repository checkout (for example `/project/disease_ecology/cds-datagrab`) separate from generated data. Use `/project/disease_ecology/cds-datagrab-smoke-output` for smoke and `/project/disease_ecology/cds-datagrab-output` for production; product separation is below the profile layer. The first real CDS execution should be the three-day smoke download, inspected with `scripts/inspect_smoke_download.R`, before any historical update.

Example smoke submission:

```bash
CDS_DATAGRAB_ROOT=/project/disease_ecology/cds-datagrab-smoke-output PROFILE=smoke \
CONFIG=config/era5_mintemp_smoke.yml MODE=full DRY_RUN=false \
OBSERVED_END=2026-07-03 bash hpc/submit_era5_mintemp.sh
```

For the soil-moisture smoke run, submit the variable wrapper explicitly through bash:

```bash
CDS_DATAGRAB_ROOT=/project/disease_ecology/cds-datagrab-smoke-output \
CONFIG=config/era5_soilmoist_smoke.yml MODE=full DRY_RUN=false \
OBSERVED_END=2026-07-03 bash hpc/submit_era5_soilmoist.sh
```

If Atlas reports a local mode-only change to the mintemp wrapper, confirm it with
`git diff --summary` before restoring it. Only for a mode-only difference, use:

```bash
git restore hpc/submit_era5_mintemp.sh
git pull
```

Credentials are read by R through `ecmwfr_PAT`; the token is never printed. The root marker `.cds-datagrab-root` identifies the storage root and is required for destructive operations.
### Northern-edge recovery

The corrected workflow uses `template_bbox_union`, a one-degree margin, and outward alignment to the ERA5 0.25-degree grid. The former GPKG-only request can leave a fixed northern gap even when values are otherwise plausible. Incomplete daily files are invalid, do not contribute to date planning or climatology, and are quarantined under the active dataset root before the corrected dates are requested. The new request area changes the deterministic raw filename hash; the previous raw file remains available for audit.

The Atlas download stage must be considered successful only when the canonical file exists beneath `$CDS_DATAGRAB_ROOT/data/<profile>/<dataset>/raw`, passes signature and NetCDF metadata validation, and is recorded in the download manifest. A missing or unusable target is a `failed_stage=download` error and leaves a durable run manifest; no processing stage follows. Pipeline logs are under `$CDS_DATAGRAB_ROOT/logs/pipeline`, while SLURM logs are under `$CDS_DATAGRAB_ROOT/logs/slurm`.
