.PHONY: 
.SUFFIXES: .fmt .pdf
ENGINE ?= lualatex

all: abstract-machines.pdf #010intro.pdf 020closures.pdf 030cfg.pdf

define ADDMISC
$1.pdf: ../misc/$2.pdf
../misc/$2.pdf:
	$$(MAKE) -C ../misc $2.pdf	
endef

$(eval $(call ADDMISC,demo,cfg1))

OTHER_TEX=../preamble.tex

define WRAP
INPUT_$(1)=$(wildcard $(1)*.tex)
all: $$(INPUT_$(1):%.tex=%.pdf)

ifeq ("$$(ENGINE)","lualatex")
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

LECTURES  = 
LECTURES += abstract-machines

$(foreach i,$(LECTURES),$(eval $(call WRAP,$(i)) ) )

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
