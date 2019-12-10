.PHONY: ci

ci: .github/workflows/haskell.yml .github/workflows/hlint.yml

.github/workflows:
	mkdir -p $@

.github/workflows/hlint.yml: hlint-ci.dhall .github/workflows
	dhall-to-yaml --file $< --output $@

.github/workflows/haskell.yml: haskell-ci.dhall .github/workflows
	dhall-to-yaml --file $< --output $@
