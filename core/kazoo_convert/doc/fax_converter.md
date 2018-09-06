# Kazoo Fax Converter

The fax converter writes files to a configurable working directory and converts them using system commands. The commands used to convert configurations are user configurable with defaults being set to recommended commands. The module is enabled as the default fax file converter.


### Fax Compatible Documents

Fax machines negotiate image parameters via the `T.30` protocol (sometimes overlaid via T.38 for supposedly better transmissions on VoIP networks). If a receiving fax machine is unable to handle the requested image, the call will fail with an error like `Far end cannot receive at the resolution of the image`. This ultimately means the fax is undeliverable, even if retries are enabled, it will not be able to deliver the document to the far end.

Ultimately, faxes are just an antiquated method to exchange tiff files, which are then printed (or otherwise handled) on the receiving side. On transmission information is exchanged like the acceptable dimensions of the tiff file and file resolution. On receipt, most fax machines will shrink a larger format document to fit on one page, or print multiple pages, but some will simply reject larger images. Generally this negotiation is based on the page sizes that are supported, we are talking about actual paper here now... yeah.

### Tiff File Format

In order to ensure maximum compatibility, and a higher chance of a successful delivery of a fax, by default, the `fax_converter` will format all files received into tiff files following a standards compliant fax compatible format. All documents are converted using `CCITT Group 4 compression`, with dimensions of `1728x2200` at a resolution of `200x200` PPI. This is done on every conversion to tiff format. This behavior is configurable via the converter commands configurations.

Because a user submitted tiff file could be in any format, the converter will check tiff files and read their metadata when conversion from tiff to tiff is requested. The converter will check the resolution and dimensions values, as well as the compression method and do a conversion conditionally on the state of the file. This conversion will guarantee a normalized fax compatible tiff is returned. This means if the resolution is in legal size (8.5 x 14 in.) or has any oversized dimension, it will be resized to fit on an `letter` (8.5 x 11 in.) page using the `Large Tiff Command`. If a document is smaller than an `letter` document, its dimensions will be preserved will be centered on an `letter` page using the `Small Tiff Command`. If the PPI is larger than 200x200, it will be resampled. If the compression is not group3 or group4, it will be resampled using the `Tiff Resample Command`.

## Default Convert Commands

The converter uses system commands to do the conversions and writes and converts files in the configured `tmp_dir`, every command the converter uses is customizable via the `v2/system_configs/kazoo_convert` Api.

## Environment variables provided to all commands

Three environment variables are provided to every command to ensure ordering of arguments can be provided in any order.

| Variable | Description |
| --- | --- |
| `$FROM` | The source filename for the conversion |
| `$TO` | The destination filename for the conversion |
| `$WORKDIR` | The working directory for the conversion |

The `$TO` and `$FROM` environment variables are generally used in most converter commands, but some commands which are intended to operate in batch modes require a work directory instead of a destination file name. If a converter command uses batch mode, the destination directory for the conversion is provided by `work_dir`.

## Migrating To Fax Converter Commands

All of the functionality for conversions was extracted from the `fax` and `teletype` apps, however the conversion commands executed did not survive the journey. Unlike the convert commands in the `fax` app, the `fax_converter` module uses the `exit status` returned by the system, to determine if a convert command was successful or failed. This means if you have customized your converter commands from the system defaults in `fax`, when you migrate to use the kz_convert, you should ensure the exit status returned is `0` when the convert command you use is successful.

So for example, if your command to convert PDF to TIFF formats was:

```bash
/usr/bin/gs -q \
     -r200x200 \
     -g1728x2200 \
     -dNOPAUSE \
     -dBATCH \
     -dSAFER \
     -sDEVICE=tiffg3 \
     -sOutputFile=~s -- ~s \
      > /dev/null 2>&1 \
      && echo -n success
```

the equivalent `fax_converter` command would be:

```bash
/usr/bin/gs -q \
     -r200x200 \
     -g1728x2200 \
     -dNOPAUSE \
     -dBATCH \
     -dSAFER \
     -sDEVICE=tiffg4 \
     -sOutputFile=$TO -- $FROM
```

Which also means, if the converter you are using for a specific purpose is a `jerk` and always returns `exit_status: 0`, you need to handle this in your convert command. Something like this could be appended to the end of the your custom command to handle this case. This example searches for matches to the patterns `parser error`  and `error`in the output and emits exit_status 1 (error) if those matches are found, otherwise emits exit_status 0 (ok).

```bash
|egrep 'parser error|Error' && exit 1 || exit 0"
```

Most converters are nice about exit status, but if you customize your commands, you should definitely test your command in failure cases to ensure you don't end up sending bad faxes or weird notification emails.


### Tiff Resample Command

The configuration parameter for this command is `fax.resample_image_command`. This command is invoked when a conversion from any `image/*` to `image/tiff` is requested.

This is most commonly used to resample a tiff with unknown resolution or a resolution that is too large for faxing. When this command is used, the image is processed again after the resolution is updated to ensure it is the correct height and width for `letter` format.

The default command is:

```bash
convert $FROM \
    -resample 200x200 \
    -page +0+0 \
    -compress group4 $TO
```

### Tiff Convert Command

The configuration parameter for this command is `fax.convert_image_command`. This command is invoked when a conversion from any `image/*` to `image/tiff` is requested if the format of the image is unsuitable for faxing but the resolution is correct.

This is most commonly used to resample an otherwise validly formatted tiff to ensure it is using the standard format for faxing.

The default command is:

```bash
convert $FROM \
    -resample 200x200 \
    -units PixelsPerInch \
    -size 1728x2200 \
    -compress group4 $TO
```

### Large Tiff Command

The configuration parameter for this command is `fax.large_tiff_command`. This command is invoked when a conversion from `image/*` to `image/tiff` is requested.

This is used when a tiff is larger than 1728x2200 to resize it to fit on the page.

The default command is:

```bash
convert $FROM \
    -resample 200x200 \
    -units PixelsPerInch \
    -resize 1728\!x2200 \
    -compress group4 $TO
```

### Small Tiff Command

The configuration parameter for this command is `fax.small_tiff_command`. This command is invoked when a conversion from `image/*` to `image/tiff` is requested.

Convert command to handle case where the tiff is smaller than 1728x2200 ensure it is in the standard format for faxing.

The default command is:

```bash
convert $FROM \
    -gravity center \
    -resample 200x200 \
    -units PixelsPerInch \
    -extent 1728x2200 \
    -compress group4 $TO
```

#### Requirements

These command requires the system support the `convert` command, this is installed via the package `ImageMagick` in CentOS7 and Debian8.

## Tiff to PDF

The configuration parameter for this command is `fax.convert_tiff_command`. This command is invoked when a conversion from `image/tiff` to `application/pdf` is requested.

The default command is:

```bash
tiff2pdf -o $FROM $TO
```

#### Requirements

This command requires `tiff2pdf` be installed, this is installed via the package `libtifftools` in CentOS7 and `libtiff-tools` in Debian8.

## Pdf to Tiff

The configuration parameter for this command is `fax.convert_pdf_command`. This command is invoked when conversion from `application/pdf` to `image/tiff` is requested.

The default command is:

```bash
/usr/bin/gs \
    -q \
    -r200x200 \
    -g1728x2200 \
    -dNOPAUSE \
    -dBATCH \
    -dSAFER \
    -sDEVICE=tiffg4 \
    -sOutputFile=$TO \
    -- $FROM
```

#### Requirements

This command requires `ghostscript` be installed, this is installed via the package `ghostscript` in CentOS7 and Debian8.


## OpenOffice compatible to PDF

The configuration for this command is `fax.convert_openoffice_command`. This command is invoked when conversion from any openoffice compatible format is requested. For this feature to be used, `fax.enable_openoffice` must be set in the `kazoo_convert` configuration. If openoffice compatible format conversions are enabled, by default openoffice conversions are serialized, this can be changed by setting `fax.serialize_openoffice` to false.

MIME types that will use this converter include:

 - `application/vnd.openxmlformats-officedocument.*`
 - `application/vnd.oasis.opendocument.*`
 - `application/msword`
 - `application/vnd.ms-excel`
 - `application/vnd.ms-powerpoint`

The default use of `unoconv` has been deprecated in fax_converter as libreoffice can be invoked directly using the `--convert-to pdf` argument without requiring a constantly running openoffice server. `Libreoffice`, provides useful error output if a command fails but does not provide an error exit status in the event of a conversion failure, so success must be determined from the command output.

```bash
libreoffice \
    --headless \
    --convert-to pdf $FROM \
    --outdir $WORKDIR \
    2>&1 \
    |egrep 'parser error|Error' && exit 1 || exit 0
```

### Requirements

This command requires `libreoffice` via package `libreoffice-core` in CentOS7 and `libreoffice-common` in Debian8.

## Default Validate Commands

### Environment variables provided to all validate commands
Three environment variables are provided to every command to ensure ordering of arguments can be provided in any order.

| Variable | Description |
| --- | --- |
| `$FROM` | The source filename to validate |
| `$TO` | The destination filename for the conversion if a converter is used to validate the command |
| `$WORKDIR` | The working directory for the conversion if a conversion is used to validate the command |
| `$FILE` | Another name for the FROM value, used for clarity when only the target file is needed for validation |

### Validate Tiff Command

The configuration parameter for this command is `fax.verify_tiff_command`. This command is invoked after a files is converted to tiff.

The default command is:

```bash
tiffinfo $FILE
```

#### Requirements

This command requires `tiffinfo` be installed, this is installed via the package `libtifftools` in CentOS7 and `libtiff-tools` in Debian8.

### Validate PDF Command

The configuration parameter for this command is `fax.verify_tiff_command`. This command is invoked when a conversion from `image/tiff` to `application/pdf` is requested.

The default command is:

```bash
gs -dNOPAUSE -dBATCH -sDEVICE=nullpage $FILE
```

#### Requirements

This command requires `ghostscript` be installed, this is installed via the package `ghostscript` in CentOS7 and Debian8.

### Sup Commands

Fax conversion commands can be tested via sup, this is useful when debugging issues where a custom conversion command is not working properly.


#### Test file converters

Converts a file specified in the `path/to/file` and allows conversions to the formats `pdf` and `tiff`.

```
sup kazoo_convert_maintenance convert_fax_file {path/to/file} {to_file_type}
```

Work directory can be added for batch type conversions.

```
sup kazoo_convert_maintenance convert_fax_file {path/to/file} {to_file_type} {work_directory}
```

A destination filename can also be added.

```
sup kazoo_convert_maintenance convert_fax_file {path/to/file} {to file type} {work_directory} {destination_filename}
```

#### Audit System Commands

ensure all the converters required for the conversion operations are installed. If installed, this command attempts to display their versions.

```
sup kazoo_convert_maintenance versions_in_use
```

#### Read File Metadata

Used to read the metadata of a file in the file system, if the file type is tiff, it will read the page count as well.

```
sup kazoo_convert_maintenance read_metadata {/path/to/a/file}
```

Used to read the compression, image size, and resolution of a tiff file in the file system.

```
sup kazoo_convert_maintenance read_tiff_info {/path/to/a/file}
```
