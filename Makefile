SOURCES = $(wildcard *.sml)

smldebug: $(SOURCES)
	mlton -mlb-path-var 'COMPAT mlton' -default-ann 'allowOrPats true' -output mulligan src/top/sources.mlb

.PHONY: clean
clean:
	rm -f demo smlfmt
