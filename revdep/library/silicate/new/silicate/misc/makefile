FILE=geospatialsci-2018
TEMPLATE=template
PREFLAGS=--variable geometry="margin=2.54cm" \
		 --variable linestretch=1.6 \
		 --variable mainfont="Times New Roman"
POSTFLAGS=

all: pdf

pdf: $(FILE).md
	pandoc -s $(PREFLAGS) $(FILE).md -o $(FILE).pdf $(POSTFAGS)

open: $(FILE).pdf
	xdg-open $(FILE).pdf &
