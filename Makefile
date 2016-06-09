export ERLANG_MK ?= $(CURDIR)/erlang.mk

PROJECT = ego
PROJECT_DESCRIPTION = Erlang (data-based) geo-services
PROJECT_VERSION = 0.1.0

# Core targets

all:: $(ERLANG_MK)
$(ERLANG_MK):
	curl https://erlang.mk/erlang.mk | make -f -

include $(ERLANG_MK)

# Custom targets

.PHONY: data
.SECONDARY:

data:   priv/geoip priv/geonames/allCountries.txt

priv/:
	mkdir -p $@

priv/geoip.gz: SRC = http://geolite.maxmind.com/download/geoip/database/GeoLite2-City.mmdb.gz
priv/geoip.gz: priv/
	curl $(SRC) -o $@

priv/geoip: | priv/geoip.gz
	gunzip $|

priv/geonames:
	mkdir -p $@

priv/geonames/%.zip: SRC = http://download.geonames.org/export/zip
priv/geonames/%.zip: | priv/geonames
	curl $(SRC)/$*.zip -o $@

priv/geonames/%.txt: | priv/geonames/%.zip
	unzip -d $(@D) $| $*.txt
