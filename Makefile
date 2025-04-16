.PHONY: submit.zip

submit.zip:
	zip submit.zip -r * \
		-x \*\*.[os] -x \*\*~ -x \*\*zip \
		-x \*\*Zone.Identifier -x \*\*compiled\*\*
