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
