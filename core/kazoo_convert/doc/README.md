# Kazoo File Format Converter Library

The Kazoo converter provides a core library for converting file formats.

## Modules

The converters used to execute file conversions are modular, modules can be enabled via configuration. This core library is intended to be extended to include multipe types of conversions and formats and be easily extendable by supporting selection of which modules to use via the `system_config/kazoo_convert` document. Currently only fax conversions are done via the converter. But there are many other types of file conversions going on in Kazoo. Stay tuned...

#### Fax Converter

The fax converter module by default use the module `fax_converter`. For a description of how the default fax converter `fax_converter` works, and information about the system commands used in fax file conversions, see [the fax converter documentation.](fax_converter.md)

###Configuration

The `v2/system_configs/kazoo_convert` configuration parameters are used to enable features and define commands to use for conversion operations.

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

Fax conversion commands can be tested via sup, this is useful when debugging issues where a custom conversion command is not working properly.

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

