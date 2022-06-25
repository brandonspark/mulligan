shell = /bin/sh
SOURCES = $(shell find src -iname '*.sml' -o -iname '*.mlb' -o -iname '*.sig' -o -iname '*.fun')
TEST_SOURCES = $(shell find src -iname '*.sml' -o -iname '*.mlb' -o -iname '*.sig' -o -iname '*.fun') $(shell find test -iname '*.sml' -o -iname '*.mlb' -o -iname '*.sig' -o -iname '*.fun')

mulligan: $(SOURCES)
	mlton -mlb-path-var 'COMPAT mlton' -const 'Exn.keepHistory true' -default-ann 'allowOrPats true' -output mulligan src/top/sources.mlb

test: $(TEST_SOURCES)
	mlton -mlb-path-var 'COMPAT mlton' -const 'Exn.keepHistory true' -default-ann 'allowOrPats true' -output mulligan_test test/sources.mlb
	./mulligan_test
	@rm -f mulligan_test


.PHONY: clean test
clean:
	rm -f mulligan
	rm -f mulligan_test
