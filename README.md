# FOCUS

_(not much here yet...)_

## Validating submission file

Submissions to the COVID-19 Forecast Hub must comply to the entry format for the forecast file:

https://github.com/reichlab/covid19-forecast-hub/blob/master/data-processed/README.md#forecast-file-format

The organizers have provided code to validate entry formats. We have included some of those methods in this repo, such that entry format can be checked as follows:

```
python3 misc/validation/validate_single_forecast_file.py scratch/fable-submission-mockup-allmetrics-forecasts/2021-01-04-sigsci-ts.csv
```

Note that the following Python modules should be installed:

```
pandas
requests
pymmwr
click
urllib3
selenium
webdriver-manager
pyyaml
PyGithub
git+https://github.com/reichlab/zoltpy/
https://github.com/hannanabdul55/pykwalify/archive/master.zip
```

You can install these dependencies by running the following:

```
pip3 install -r misc/validation/requirements.txt
```

In order to run a very similar check using R, we have developed a wrapper to the [`validate_quantile_csv_file()` function from the `zoltpy` module](https://github.com/reichlab/zoltpy/blob/master/zoltpy/covid19.py#L75-L93), which drives the `validate_single_forecast_file.py` script above. To use the wrapper function in R:

```
validate_forecast("scratch/fable-submission-mockup-allmetrics-forecasts/2021-01-04-sigsci-ts.csv")
validate_forecast("scratch/fable-submission-mockup-allmetrics-forecasts/2021-01-05-sigsci-ts.csv")
```

