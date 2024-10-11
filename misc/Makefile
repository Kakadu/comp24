ENGINE ?= xelatex
all:

LinScan1.pdf: implLinScan1.tex

%.pdf: misc_fonts.tex

define WRAP
INPUT_$(1)=$(wildcard $(1)*.tex)
all: $$(INPUT_$(1):%.tex=%.pdf)
$$(INPUT_$(1):%.tex=%.pdf): $$(INPUT_$(1)) #preamble.tex
	/usr/bin/time -f "%U" $$(ENGINE) -interaction=nonstopmode -shell-escape $$<
	@echo FINISHED $$(ENGINE) -fmt=$$(<:.tex=.fmt) $$<
endef
LECTURES=cfg1

$(foreach i,$(LECTURES),$(eval $(call WRAP,$(i)) ) )

clean:
	@$(RM) -r _minted*
	@$(RM) *.aux *.log *.nav *.out *.snm *.toc *.vrb *.synctex.gz
	$(RM) *.pdf *.fmt


