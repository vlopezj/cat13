all: evgeny.pdf

evgeny.pdf: evgeny.tex
	# how many compilation runs is enough? three is enough
	xelatex evgeny.tex
	xelatex evgeny.tex
	xelatex evgeny.tex

clean:
	rm evgeny.pdf
