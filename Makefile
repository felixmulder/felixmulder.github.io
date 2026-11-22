ZOLA ?= .tools/bin/zola
PANDOC ?= .tools/bin/pandoc
LYCHEE ?= .tools/bin/lychee

SLIDE_SOURCES := $(filter-out content/talks/_index.md,$(wildcard content/talks/*.md))
SLIDE_OUTPUTS := $(patsubst content/talks/%.md,generated/slides/%.html,$(SLIDE_SOURCES))

.PHONY: build setup slides serve check

build: slides
	$(ZOLA) build

setup:
	sh scripts/setup-tools.sh

slides: $(SLIDE_OUTPUTS)

generated/slides/%.html: content/talks/%.md scripts/render-slides.sh
	@sh scripts/render-slides.sh "$<" "$@" "$(PANDOC)"

serve: slides
	@set -e; \
	while sleep 1; do $(MAKE) --silent --no-print-directory slides || :; done & \
	watcher=$$!; \
	trap 'kill "$$watcher" 2>/dev/null || true' EXIT; \
	trap 'exit 130' INT TERM; \
	$(ZOLA) serve --port 8000 --extra-watch-path generated

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
