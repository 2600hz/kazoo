ORG_DOCS = $(wildcard *.org)
TRAINING_DOCS = $(filter-out template.org, $(ORG_DOCS))
PDFS = $(TRAINING_DOCS:.org=.pdf)
# ORG_EL = $(shell find ~/.emacs.d -type f -name "org.el")
# ORG_DIR = $(shell dirname $(ORG_EL))
# ORG_MODE_ELS = $(ORG_EL) $(ORG_DIR)/org-element.el
# ORG_MODE_ELS += $(wildcard $(ORG_DIR)/ox-*.el)
# $(foreach org_el,$(ORG_MODE_ELS),-l $(org_el))

docs: $(PDFS)
	@rm *.tex

%.pdf: %.org
	@emacs -l ~/.emacs --visit=$< --batch -f org-beamer-export-to-pdf --kill 2>/dev/null
	@echo "created $@"
