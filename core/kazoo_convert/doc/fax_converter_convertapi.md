# Kazoo converter using ConvertAPI service

This module will use the module `kz_fax_converter` for image and pdf document conversion. For OpenOffice documents and OpenOffice compatables documents this can be configured to use the convertapi service or just use the existing conversion commands provided by `kz_fax_converter`. This behavior is configured by the setting `try_openoffice` with `for_msoffice_files_also` which uses the libreoffice binary on the local system for conversion of msoffice files or `for_openoffice_files_only` which only uses libreoffice for openoffice file type conversions. 

## Possible configuration parameters 

* `api_url` - convertapi api_url. Default value "https://v2.convertapi.com"
* `pdf_version` - which pdf version files will be produced by convertapi service. The default value `1.7`.
* `resolution` - which document resolution is used for converted files. The default value is `200`.
* `secret` - convertapi secret. There is no default for this parameter.
* `timeout` - document conversion timeout. The default value `60`.
* `try_openoffice` - Which files types will use the openoffice converter. Default value `for_msoffice_files_also`.

