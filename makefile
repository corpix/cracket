pkg := raco pkg
racoinstall := $(pkg) install --skip-installed
racouninstall := $(pkg) remove

.PHONY: install
install:
	$(racoinstall) -n corpix-bytes      file://$(PWD)/bytes
	$(racoinstall) -n corpix-hex        file://$(PWD)/hex
	$(racoinstall) -n corpix-json       file://$(PWD)/json
	$(racoinstall) -n corpix-prometheus file://$(PWD)/prometheus
	$(racoinstall) -n corpix-url        file://$(PWD)/url
	$(racoinstall) -n corpix-multipart  file://$(PWD)/multipart
	$(racoinstall) -n corpix-http       file://$(PWD)/http
	$(racoinstall) -n corpix-telegram   file://$(PWD)/telegram
	$(racoinstall) -n corpix-task       file://$(PWD)/task
	$(racoinstall) -n corpix-bnf        file://$(PWD)/bnf
	$(racoinstall) -n corpix-time       file://$(PWD)/time

	$(racoinstall) -n corpix-taskd      file://$(PWD)/taskd
	$(racoinstall) -n corpix-firewalld  file://$(PWD)/firewalld

.PHONY: uninstall
uninstall:
	-$(racouninstall) corpix-bytes
	-$(racouninstall) corpix-hex
	-$(racouninstall) corpix-json
	-$(racouninstall) corpix-prometheus
	-$(racouninstall) corpix-url
	-$(racouninstall) corpix-multipart
	-$(racouninstall) corpix-http
	-$(racouninstall) corpix-telegram
	-$(racouninstall) corpix-task
	-$(racouninstall) corpix-bnf
	-$(racouninstall) corpix-time

	-$(racouninstall) corpix-taskd
	-$(racouninstall) corpix-firewalld

.PHONY: reinstall
reinstall: uninstall install
