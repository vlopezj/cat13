all: victor.pdf evgeny.pdf

victor.pdf: victor.tex
	xelatex victor.tex </dev/null

evgeny.pdf: evgeny.tex
	# how many compilation runs is enough? three is enough
	xelatex evgeny.tex
	xelatex evgeny.tex
	xelatex evgeny.tex

clean:
	rm evgeny.pdf
	rm victor.pdf
