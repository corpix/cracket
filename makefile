pkg := raco pkg
install := $(pkg) install --skip-installed
uninstall := $(pkg) remove

.PHONY: install
install:
	$(install) -n corpix-hex      file://$(PWD)/hex
	$(install) -n corpix-json     file://$(PWD)/json
	$(install) -n corpix-url      file://$(PWD)/url
	$(install) -n corpix-http     file://$(PWD)/http
	$(install) -n corpix-telegram file://$(PWD)/telegram

.PHONY: uninstall
uninstall:
	-$(uninstall) corpix-hex
	-$(uninstall) corpix-json
	-$(uninstall) corpix-url
	-$(uninstall) corpix-http
	-$(uninstall) corpix-telegram

.PHONY: reinstall
reinstall: uninstall install
