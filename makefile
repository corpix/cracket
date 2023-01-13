pkg := raco pkg
racoinstall := $(pkg) install --skip-installed
racouninstall := $(pkg) remove

.PHONY: install
install:
	$(racoinstall) -n corpix-bytes     file://$(PWD)/bytes
	$(racoinstall) -n corpix-hex       file://$(PWD)/hex
	$(racoinstall) -n corpix-json      file://$(PWD)/json
	$(racoinstall) -n corpix-url       file://$(PWD)/url
	$(racoinstall) -n corpix-multipart file://$(PWD)/multipart
	$(racoinstall) -n corpix-http      file://$(PWD)/http
	$(racoinstall) -n corpix-telegram  file://$(PWD)/telegram

.PHONY: uninstall
uninstall:
	-$(racouninstall) corpix-bytes
	-$(racouninstall) corpix-hex
	-$(racouninstall) corpix-json
	-$(racouninstall) corpix-url
	-$(racouninstall) corpix-multipart
	-$(racouninstall) corpix-http
	-$(racouninstall) corpix-telegram

.PHONY: reinstall
reinstall: uninstall install
