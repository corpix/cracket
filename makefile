pkg := raco pkg
install := $(pkg) install --skip-installed
uninstall := $(pkg) remove

.PHONY: install
install:
	$(install) -n hex      file://$(PWD)/hex
	$(install) -n json     file://$(PWD)/json
	$(install) -n http     file://$(PWD)/http
	$(install) -n telegram file://$(PWD)/telegram

.PHONY: uninstall
uninstall:
	-$(uninstall) hex
	-$(uninstall) json
	-$(uninstall) http
	-$(uninstall) telegram

.PHONY: reinstall
reinstall: uninstall install
