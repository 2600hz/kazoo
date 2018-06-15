# Kazoo File Format Converter Library

The Kazoo convert provides a core library for converting file formats. This app moves functionality previously scattered throughout other applications into a single core application.

This app is intended to:
1. Provides a core consistant interface for file format conversions.
1. Provide the capability of generating custom modules for conversion and enabling them via config.
1. Returns a standardized result of either the file path of the conversion output file or the file content, or standardized errors for failures to convert.

## Modules

The kazoo converter command uses modules for the converter based on the type of conversions required. This is intended to be extended to include multipe types of conversions and formats and be easily extendable by supporting selection of which modules to use via the `system_config/kazoo_convert` document.

#### Fax Converter

The fax converter module by default uses the module `fax_converter`.

For a description of how the fax_converter works, see [the fax converter documetation.](fax_converter.md)

#### Schema

Configuration parameters for conversions

Key | Description | Type | Default | Required | Support Level
--- | ----------- | ---- | ------- | -------- | -------------
`convert_command_timeout` | The timeout value for file conversion | `integer()` | `120000` | `false` |
`convert_image_command` | The command to resample a tiff file to a fax compatible format or convert a supported image/* format to a tiff | `string()` | [see fax_converter doc](fax_converter.md) | `false` |
`convert_openoffice_command` | The command to convert open office documents to pdf | `string()` | [see fax_converter doc](fax_converter.md) | `false` |
`convert_pdf_command` | The command to convert pdf documents to tiff | `string()` | [see fax_converter doc](fax_converter.md) | `false` |
`convert_tiff_command` | The command to convert a tiff file to PDF | `string()` | [see fax_converter doc](fax_converter.md) | `false` |
`enable_openoffice` | Enables the conversion of openoffice compatible documents | `boolean()` | `true` | `false` |
`fax_converter` | Module to use for fax related file conversions | `string()` | `fax_converter` | `false` |
`file_cache_path` | The working directory to use when converting files | `string()` | `/tmp/` | `false` |
`serialize_openoffice` | Serializes openoffice compatible document conversions | `boolean()` | `true` | `false` |
`validate_pdf_command` | Verifies a PDF file is valid | `string()` | [see fax_converter doc](fax_converter.md) | `false` |
`validate_tiff_command` | Verifies a TIFF file is valid | `string()` | [see fax_converter doc](fax_converter.md) | `false` |

### Sup Commands

```
sup kazoo_convert_maintenance convert_fax_file {path/to/file} {to file type}
```

This command converts a file specified in the `path/to/file` and allows conversions to the formats `pdf` and `tiff`.

```
sup kazoo_convert_maintenance convert_fax_file {path/to/file} {to file type} {work directory}
```

For batch type converters, an additional argument `work_directory` can be added to specify the directory where the output file will go.

```
sup kazoo_convert_maintenance versions_in_use
```

Used to audit the system and ensure all the converters required for the conversion operations are installed. If installed, this command attempts to display their versions.

```
sup kazoo_convert_maintenance read_metadata /path/to/a/file
```

Used to read the metadata of a file in the file system, if the file type is tiff, it will read the page count as well.

