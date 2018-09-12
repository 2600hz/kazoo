# Kazoo converter using ConvertAPI service

THis module is use `kz_fax_converter` for image and pdf document convertsion. For OpenOffice documents and OpenOffice compatables documents may be calls
`kz_fax_converter` too. This is configured by `try_openoffice` config param with `for_msoffice_files_also` or `for_openoffice_files_only`

## Possible config params

* `api_url` - convertapi api_url. Default value "https://v2.convertapi.com"
* `pdf_version` - which pdf version files will be produced by convertapi service. Default value "1.7"
* `resolution` - whith document resolution is used for converted files. Default value 200
* `secret` - convertapi secret. Default value is not exist
* `timeout` - document conversion timeout. Default value 60
* `try_openoffice` - for which files types may be called openoffice. Default value "for_msoffice_files_also"

