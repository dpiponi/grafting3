monads.pdf: monads.tex
	pdflatex monads.tex

monads.tex: monads.lhs
	lhs2TeX monads.lhs > monads.tex
