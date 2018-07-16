# Kazoo Fax Converter

This module provide a consistant core interface for fax file format conversions. The module is enabled as the default fax converter, but this can be easily extended by adding other modules to the converter.

## Environment variables provided to all commands
Three enviornment variables are provided to every command to ensure ordering of arguments can be provided in any order.

| Variable | Description |
| --- | --- |
| `$From` | The source filename for the conversion |
| `$TO` | The destination filename for the conversion |
| `$WORKDIR` | The working directory for the conversion |

The `$TO` and `$FROM` environment variables are generally used in most commands, but some commands which are intended to operate in batch modes require a work dir instead of a destination file name.

## Fax Converter Commands

All of the functionality for conversions was extracted from the `fax` and `teletype` apps, however the conversion commands executed did not survive the journey. Unlike the convert commands in the `fax` app, the `fax_converter` module the `exit status` is used to determine if a convert command was successful or failed. This means if you have customized your converter commands from the system defaults in `fax`, when you migrate to use the kz_convert, you should ensure the exit status returned is `0` when the convert command you use is successful.


```bash
/usr/bin/gs -q \
     -r204x98 \
     -g1728x1078 \
     -dNOPAUSE \
     -dBATCH \
     -dSAFER \
     -sDEVICE=tiffg3 \
     -sOutputFile=~s -- ~s \
      > /dev/null 2>&1 \
      && echo -n success
```

The equivalent `fax_converter` command would be:

```bash
/usr/bin/gs -q \
     -r204x98 \
     -g1728x1078 \
     -dNOPAUSE \
     -dBATCH \
     -dSAFER \
     -sDEVICE=tiffg3 \
     -sOutputFile=$TO -- $FROM
```

Which also means, if the converter you are using for a specific purpose is a jerk and always returns exit_status `0`, you need to handle this in your convert command. Something like this could be appended to the end of the your custom command to handle this case. This example searches for matches to the patterns `parser error`  and `error`in the output and emits exit_staus 1 (error) if those matches are found, othewise emits exit_status 0 (ok).

```bash
|egrep 'parser error|Error' && exit 1 || exit 0"
```

Most converters are nice about exit status, but you should definitely test your command in failure cases to ensure you don't end up sending bad faxes or notification emails.

## Default Convert Commands

### Tiff Resample Command

The configuration parameter for this command is `convert_image_command`. This command is invoked when a conversion from `image/*` to `image/tiff` is requested.

This is most commonly used to resample a tiff to ensure it is in the standard format for faxing.

The default command is:

```bash
convert $FROM \
    -resample 204x98 \
    -units PixelsPerInch \
    -compress group4 \
    -size 1728x1078 $TO
```

#### Requirements

This command requires `convert` be installed, this is installed via the package `ImageMagick` in centos and debian.

## Tiff to PDF

The configuration parameter for this command is `convert_tiff_command`. This command is invoked when a conversion from `image/tiff` to `application/pdf` is requested.

The default command is:

```bash
tiff2pdf -o $FROM $TO
```

#### Requirements

This command requires `tiff2pdf` be installed, this is installed via the package `libtifftools` in centos and `libtiff-tools` in debian.

## Pdf to Tiff

The configuration parameter for this command is `convert_pdf_command`. This command is invoked when conversion from `application/pdf` to `image/tiff` is requested.

### Default command

```bash
/usr/bin/gs \
    -q \
    -r204x98 \
    -g1728x1078 \
    -dNOPAUSE \
    -dBATCH \
    -dSAFER \
    -sDEVICE=tiffg4 \
    -sOutputFile=$TO \
    -- $FROM
```

#### Requirements

This command requires `ghostscript` be installed, this is installed via the package `ghostscript` in centos and debian.


## Openoffice compatible to PDF

The configuration for this command is `convert_openoffice_command`. This command is invoked when conversion from any openoffice compatible format is requested. For this feature to be used, `enable_openoffice` must be set in the `kazoo_convert` configuration. If openoffice compatible format conversions are enabled, by default openoffice conversions are serialized, this can be changed by setting `serialize_openoffice` to false. This is strongly reccomended to be enabled if using the unoconv converter for openoffice conversions, as it explicitly requires one conversion at a time. The default use of `unoconv` has been depricated as libreoffice can be invoked directly using the `--convert-to pdf` argument. `libreoffice` unlike `unoconv` provides useful output if a command fails, neither provides a correct exit status so it must be determined from the command output.

Supported mimetype include:

 - `application/vnd.openxmlformats-officedocument.*`
 - `application/vnd.oasis.opendocument.*`
 - `application/msword`
 - `application/vnd.ms-excel`
 - `application/vnd.ms-powerpoint`

The command is passed a from filename and a directory

```bash
libreoffice \
    --headless \
    --convert-to pdf $FROM \
    --outdir $WORKDIR \
    2>&1 \
    |egrep 'parser error|Error' && exit 1 || exit 0
```

### Requirements

This command requires `libreoffice` via package `libreoffice-core` in centos and `libreoffice-common` in debian.

## Default Validate Commands

### Validate Tiff Command

The configuration parameter for this command is `verify_tiff_command`. This command is invoked when a conversion from `image/tiff` to `application/pdf` is requested.

The default command is:

```bash
tiff2pdf -o $FROM $TO
```

#### Requirements

This command requires `tiff2pdf` be installed, this is installed via the package `libtifftools` in centos and `libtiff-tools` in debian.

### Validate PDF Command

The configuration parameter for this command is `verify_tiff_command`. This command is invoked when a conversion from `image/tiff` to `application/pdf` is requested.

The default command is:

```bash
gs -dNOPAUSE -dBATCH -sDEVICE=nullpage $FROM
```

#### Requirements

This command requires `ghostscript` be installed, this is installed via the package `ghostscript` in centos and debian.
