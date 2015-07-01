# eradius

This fork of eradius is a radical deviation from the original
Jungerl code. It contains a generic RADIUS client, support for 
several authentication mechanisms and dynamic configuration
(it implements the `config_change/3` application callback).

## Building eradius

We use the [tetrapak](github.com/travelping/tetrapak) build tool to build eradius.
Tetrapak is required to build the dictionaries.

