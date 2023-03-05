pkg := raco pkg
racoinstall := $(pkg) install --skip-installed
racouninstall := $(pkg) remove

pkg_dirs := $(shell find . -maxdepth 2 -type f -name info.rkt | xargs -I{} dirname {} | xargs -I{} basename {})
pkg_names := $(shell find . -maxdepth 2 -type f -name info.rkt | xargs -I{} dirname {} | xargs -I{} basename {} | xargs -I{} printf "corpix-%s " {})
pkg_files := $(shell find $(pkg_dirs) -type f -name '*.rkt')
pkg_catalogs := $(shell raco pkg config catalogs | grep '^https:')

catalog: makefile $(pkg_files)
	mkdir -p .$@
	racket -l- pkg/dirs-catalog --link .$@ .
	raco pkg config --set catalogs file://$(shell pwd)/.$@ $(pkg_catalogs)

.PHONY: install
install: catalog
	$(racoinstall) $(pkg_names)

.PHONY: uninstall
uninstall: catalog
	$(racouninstall) $(pkg_names)

.PHONY: reinstall
reinstall: uninstall install
