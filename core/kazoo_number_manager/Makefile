CWD = $(shell pwd -P)
ROOT = $(realpath $(CWD)/../..)
PROJECT = kazoo_number_manager

# ISO3166-alpha2 to dialcode (ITU) module generator.
ITU = src/knm_iso3166a2_itu.erl
ITU_SRC = src/knm_iso3166a2_itu.erl.src
ITU_FILE = dialcodes.json
ITU_URL = https://raw.githubusercontent.com/mledoze/countries/c212082894e48e8be123dad5dea7d9e6abc0c1b9/countries.json
# official list https://www.itu.int/pub/T-SP-E.164D
# but there are newer ones… https://www.itu.int/pub/T-SP-OB
#
# maintained & good enough https://raw.githubusercontent.com/mledoze/countries/master/countries.json
#
# http://country.io/phone.json
#
# http://www.andrewpatton.com/countrylist.csv
# https://raw.githubusercontent.com/OpenBookPrices/country-data/master/data/countries.csv
# https://raw.githubusercontent.com/datasets/country-codes/master/data/country-codes.csv
# https://www.worlddata.info/downloads/countrycodes.csv
# https://commondatastorage.googleapis.com/ckannet-storage/2011-11-25T132653/iso_3166_2_countries.csv
#
# https://countrycode.org/
# https://en.wikipedia.org/wiki/List_of_country_calling_codes

COMPILE_MOAR = $(ITU)
SOURCES = $(ITU) $(wildcard src/*.erl) $(wildcard src/*/*.erl)

CLEAN_MOAR = clean-generated

all: compile

compile:

include $(ROOT)/make/kz.mk

clean-generated:
	$(if $(wildcard $(ITU)), @rm $(ITU))

$(ITU_FILE):
	wget -qO $@ $(ITU_URL)

$(ITU_SRC): $(ITU_FILE)

$(ITU): $(ITU_SRC)
	@cat $(ITU_SRC) >$(ITU)
	@python -c 'from __future__ import print_function; import json; [print("to_itu(<<\"", l["cca2"], "\">>) -> <<\"+", l["callingCode"][0], "\">>;", sep="") for l in sorted(json.load(open("dialcodes.json")), key=lambda l: l["cca2"]) if len(l["callingCode"]) > 0]' >>$(ITU)
	@echo 'to_itu(_) -> <<>>.' >>$(ITU)
