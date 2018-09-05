# Kazoo File Format Converter Library

The Kazoo converter provides a core library for converting file formats.

## Modules

The converters used to execute file conversions are modular, modules can be enabled via configuration. This core library is intended to be extended to include multiple types of conversions and formats and be easily extendable by supporting selection of which modules to use via the `system_config/kazoo_convert` document. Currently only fax conversions are done via the converter. But there are many other types of file conversions going on in Kazoo. Stay tuned...

#### Fax Converter

The fax converter module by default use the module `fax_converter`. For a description of how the default fax converter `fax_converter` works, and information about the system commands used in fax file conversions, see [the fax converter documentation.](fax_converter.md)

###Configuration

The `v2/system_configs/kazoo_convert` configuration parameters are used to enable features and define commands to use for conversion operations.

Key | Description | Type | Default | Required | Support Level
--- | ----------- | ---- | ------- | -------- | -------------
`fax.convert_command_timeout` | The timeout value for file conversion | `integer()` | `120000` | `false` |
`fax.convert_image_command` | The command to resample a tiff file to a fax compatible format or convert a supported image/* format to a tiff | `string()` | [see fax_converter doc](fax_converter.md) | `false` |
`fax.convert_openoffice_command` | The command to convert open office documents to pdf | `string()` | [see fax_converter doc](fax_converter.md) | `false` |
`fax.convert_pdf_command` | The command to convert pdf documents to tiff | `string()` | [see fax_converter doc](fax_converter.md) | `false` |
`fax.convert_tiff_command` | The command to convert a tiff file to PDF | `string()` | [see fax_converter doc](fax_converter.md) | `false` |
`fax.resample_image_command` | The command to convert an image file to a faxable tiff format | `string()` | [see fax_converter doc](fax_converter.md) | `false` |
`fax.large_tiff_command` | The command to convert an oversized tiff file to a fax compatible format | `string()` | [see fax_converter doc](fax_converter.md) | `false` |
`fax.small_tiff_command` | The command to convert an undersized tiff file to a fax compatible format | `string()` | [see fax_converter doc](fax_converter.md) | `false` |
`fax.enable_openoffice` | Enables the conversion of openoffice compatible documents | `boolean()` | `true` | `false` |
`fax_converter` | Module to use for fax related file conversions | `string()` | `fax_converter` | `false` |
`fax.serialize_openoffice` | Serializes openoffice compatible document conversions | `boolean()` | `true` | `false` |
`fax.validate_pdf_command` | Verifies a PDF file is valid | `string()` | [see fax_converter doc](fax_converter.md) | `false` |
`fax.validate_tiff_command` | Verifies a TIFF file is valid | `string()` | [see fax_converter doc](fax_converter.md) | `false` |
`file_cache_path` | The working directory to use when converting files | `string()` | `/tmp/` | `false` |
`attachment_format` | Format to use for receipt email messages and api responses | `string()` | `pdf` | `false` |

