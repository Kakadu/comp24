.PHONY: 
.SUFFIXES: .fmt .pdf
ENGINE ?= xelatex

TALK = demo
OTHER_TEX=../preamble.tex

all:

define ADDMISC
$1.pdf: ../misc/$2.pdf
../misc/$2.pdf:
	$$(MAKE) -C ../misc $2.pdf	
endef

$(eval $(call ADDMISC,demo,cfg1))

define WRAP
INPUT_$(1)=$(wildcard $(1)*.tex)
all: $$(INPUT_$(1):%.tex=%.pdf)

ifeq ("$$(ENGINE)","xelatex")
$$(INPUT_$(1):%.tex=%.pdf): $$(INPUT_$(1)) $$(OTHER_TEX)
	/usr/bin/time -f "%U" latexmk -$$(ENGINE) -synctex=1 -interaction=nonstopmode -file-line-error -shell-escape $$<
	@echo FINISHED $$(ENGINE) -fmt=$$(<:.tex=.fmt) $$<
else
$$(INPUT_$(1):%.tex=%.fmt): $$(INPUT_$(1)) $$(OTHER_TEX)
	$$(ENGINE) -synctex=1 -ini -jobname="$$(@:.fmt=)" "&$$(ENGINE)" mylatexformat.ltx $$<
$$(INPUT_$(1):%.tex=%.pdf): $$(INPUT_$(1))  $$(INPUT_$(1):%.tex=%.fmt) 
	/usr/bin/time -f "%U" $$(ENGINE) -synctex=1 -fmt=$$(<:.tex=.fmt) $$<
endif
endef

$(foreach i,$(TALK),$(eval $(call WRAP,$(i)) ) )

celan: clean
clean:
	@$(RM) -r _minted*
	@$(RM) *.aux *.bbl *.blg *.dvi *.log *.nav *.out *.snm *.toc *.vrb *.synctex.gz *.xdv *.run.xml *fls *.fdb_latexmk *.bcf
	$(RM) *.pdf *.fmt

DEBS += texlive-font-utils texlive-fonts-extra
DEBS += texlive-plain-generic
DEBS += lmodern # nicer fonts

debs:
	sudo apt install -y $(DEBS)

