tutorialSlides.html: tutorialSlides.md
	pandoc tutorialSlides.md -t slidy -s -o tutorialSlides.html

tutorial.pdf: tutorial.tex
	pdflatex tutorial.tex

tutorial.tex: tutorial.md Makefile
	pandoc tutorial.md -t latex -o tutorial.tex -s --bibliography tutorial.bib
