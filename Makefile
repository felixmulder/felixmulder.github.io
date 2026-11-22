ZOLA ?= .tools/bin/zola
LYCHEE ?= .tools/bin/lychee

.PHONY: build setup serve check

build:
	$(ZOLA) build

setup:
	sh scripts/setup-tools.sh

serve:
	$(ZOLA) serve --port 8000 --extra-watch-path slides

check: build
	$(ZOLA) check --skip-external-links
	$(LYCHEE) --offline --no-progress --no-ignore --include-fragments \
		--root-dir "$(CURDIR)/public" --index-files index.html \
		--exclude '#/' \
		--remap '^https?://felixmulder\.com/(.*) file://$(CURDIR)/public/$$1' public
	@set -e; while IFS= read -r path; do \
		test -f "public/$$path" || { echo "Missing legacy page: /$$path" >&2; exit 1; }; \
	done < checks/legacy-pages.txt
	@test -f public/CNAME && test -f public/.nojekyll
	@grep -q '<h1 class="index-title">Felix Mulder</h1>' public/index.html
	@grep -q 'id="resume"' public/resume/index.html
