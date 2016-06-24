ROOT = ../..
PROJECT = kazoo_number_manager

CLEAN_MOAR = clean-generated

# ISO3166-alpha2 to dialcode (ITU) module generator.

ITU = src/knm_iso3166a2_itu.erl
ITU_SRC = src/knm_iso3166a2_itu.erl.src
ITU_FILE = dialcodes.json
ITU_URL = https://raw.githubusercontent.com/mledoze/countries/master/countries.json
# official list https://www.itu.int/pub/T-SP-E.164D
# but there are newer ones… https://www.itu.int/pub/T-SP-OB
#
# maintained & god enough https://raw.githubusercontent.com/mledoze/countries/master/countries.json
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

all: compile | $(ITU)

include $(ROOT)/make/kz.mk


clean-generated:
	$(if $(wildcard $(ITU)), rm $(ITU))

$(ITU_FILE):
	wget -qO $@ $(ITU_URL)

$(ITU): $(ITU_FILE) $(ITU_SRC)
	@cat $(ITU_SRC) >$(ITU)
	@python -c 'from __future__ import print_function; import json; [print("to_itu(<<\"", l["cca2"], "\">>) -> <<\"+", l["callingCode"][0], "\">>;", sep="") for l in sorted(json.load(open("dialcodes.json")), key=lambda l: l["cca2"]) if len(l["callingCode"]) > 0]' >>$(ITU)
	@echo 'to_itu(_) -> <<>>.' >>$(ITU)
	@mkdir -p ebin/
	ERL_LIBS=$(ELIBS) erlc -v $(ERLC_OPTS) $(PA) -o ebin/ $(ITU)
