# sasctl (development version)

* Fixed `register_model` wrongly identifying which version of Viya it is interacting with, which lead to use wrong upload format.

# sasctl 0.7.5

* Added `scr_batch` argument to `format_data_json`, allowing to have all data in a single json payload for Viya 2024.7 SCR batch scoring feature.
* Fixed some issues related to the PMML payload when uploading models to SAS Model Manager in some SAS Viya versions
* Fixed `openBrowser` using a function instead of the argument.
* Improved `codegen` for Tidy Models workflows guessing the predictors from the pre processing pipeline instead of the model workflow recipe.

# sasctl 0.7.4

* Added `verify_ssl` argument to `session` to be used in all api calls.
* Added `openBrowser` argument to `session` to open the browser automatically when `auth_code = TRUE`.
* Fixed a bug that would not open the browser automatically when authenticating through authentication code.

# sasctl 0.7.3

* Fixed `publish_model` exact argument
* Added SAS log to error message when `publish_model` fails.

# sasctl 0.7.2

* Fixed when registering models with variables missing label/measurement metadata
* `delete_*` functions now return `invisible()`, to avoid polluting the terminal.
* Improved documentation

# sasctl 0.7.1

* Fixed misleading error messages in `register_model()`
* Small tweaks to the documentation

# sasctl 0.7.0

* Fixed correct release retrieval when using `session` to connect to Viya 2020.x
* Added `codegen` function in experimental state. Works for simple `lm`, `glm` models and [tidymodels](https://www.tidymodels.org/) `workflow` with regression or classification model mode`
* *[Breaking]* All diagnostics json generator functions requires the `targetName` and `targetPredicted` arguments to be named, now it avoids variable positioning errors.
* Diagnostics now returns invisible to avoid polluting the terminal. You can still assign to a variable to see the results.
* Added xgboost tidymodels vignette

# sasctl 0.6.4

* Added a `NEWS.md` file to track changes to the package.
* Fixed the "or()" statement being invalid in Viya 2023.3 when calling API with filters
* Fixed ZIP model upload would fail with "No model file(s)" statement on SAS Viya 2023.3 or higher
* Creating a session it brings additional server release information

# sasctl 0.6.3

Small bug fixes a quality of life improvements. 

* Added `refresh_session` function 
* Improved `session(... auth_code = TRUE`) to work on Jupyter Notebook and remove require to add `client_secret` parameter
* added `base64enc` and `dplyr` dependencies, for `refresh_token` and to make sure `furrr` doesn't fail when called through `masScore(...)`

# sasctl 0.6.2

* Initial public release
