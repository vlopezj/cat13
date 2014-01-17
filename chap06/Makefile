all : evgeny.pdf victor.pdf

evgeny.pdf : evgeny.tex
	xelatex evgeny.tex
	xelatex evgeny.tex

victor.pdf : victor.tex
	xelatex --shell-escape victor.tex </dev/null
	
clean:
	rm victor.pdf
