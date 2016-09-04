ELM_OUTPUT = elm.js
BROWSER_TARGET = example.html

SOURCE_DIR = src

###

ELM_SOURCE = $(SOURCE_DIR)/$(ELM_MAIN).elm
ELM_BUILD = $(BUILD_DIR)/$(ELM_OUTPUT)
STATIC_SOURCES = $(STATIC:%=$(SOURCE_DIR)/%)

###

.PHONY: all open clean clean-build clean-all static elm new

new: clean-build all

$(ELM_BUILD): elm

demo:
	elm make --yes Demo.elm  --output site/index.js


clean:
	rm -rf elm-stuff

clean-build:
	rm -rf $(BUILD_DIR)
	rm -rf elm-stuff/build-artifacts

clean-all: clean-build clean
