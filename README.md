# Felix Mulder
[![CI](https://github.com/felixmulder/felixmulder.github.io/actions/workflows/publish.yml/badge.svg)](https://github.com/felixmulder/felixmulder.github.io/actions/workflows/publish.yml)

Personal website hosted at felixmulder.com

## Development

The site uses Zola for pages, Pandoc for Markdown slide decks, and lychee for
local link checking. Make and POSIX shell commands coordinate the build.

Install the pinned binaries into this checkout (macOS ARM64 or Linux x86-64):

```sh
make setup
```

Versions and upstream archive SHA-256 digests live in `tools.env`. The installer
uses curl, tar, unzip, and a SHA-256 utility. No system installs or compiler
toolchains are needed.

Start the preview with live reload at <http://127.0.0.1:8000>:

```sh
make serve
```

The preview watches content, templates, static assets, and slide sources. Stop it
with Ctrl-C. To build the production site and validate it:

```sh
make check
```

This builds `public/`, runs Zola's internal-link checks, and uses lychee to check
local HTML/CSS links and anchors. It also verifies that all pre-migration page
URLs and the GitHub Pages files still exist. External sites are not checked
because their availability is outside this repository's control. `make build`
builds the production site without running the checks.

## Content

- `content/_index.md` and `content/resume.md`: homepage and résumé metadata;
  their HTML layouts live in `templates/index.html` and `templates/resume.html`.
- `content/writing/`: articles with TOML front matter.
- `content/talks/`: talk metadata and original HTML or Markdown slide sources.
- `templates/`: Tera layouts.
- `static/`: CSS, JavaScript, images, fonts, PDFs, and GitHub Pages files.

Pages use clean directory URLs. Each migrated page declares an alias for its
original `.html` URL. Zola's redirects preserve heading fragments.
Explicit heading IDs retain the article anchors that differ between Pandoc and
Zola. Mermaid diagrams use `<pre class="mermaid">` blocks; the display equation
uses a raw HTML block so Markdown typography does not alter the TeX source.

Make prepares slide bodies in ignored `generated/slides/` before starting Zola.
The `slide_format` field uses the explicit form `slide_format = "html"` or
`slide_format = "markdown"`. The generated filename matches the source stem and
its `slides_body` field. Original HTML decks retain their markup; Markdown decks
are converted to Reveal.js with Pandoc. The two PDF talks keep their PDF links.

## Manual validation before merging

Keep the live site open beside <http://127.0.0.1:8000> and review:

- Homepage: title, typography, navigation, and narrow-window layout.
- Résumé: every role, dates, Orbit/Minions links, and layout at desktop and phone widths.
- Writing: all seven articles, newest-first ordering, summaries, and dates.
- Articles: code blocks and horizontal scrolling, lists, footnotes and back links,
  and links between articles. Check the four diagrams across *Monorepos done right*
  and *Interviewing as a Software Engineer*, and the equation in the latter.
- Talks: all ten entries, venue/date metadata, videos, all eight HTML slide decks,
  and the two PDF downloads. Exercise keyboard navigation, nested slides,
  fragments, images, code highlighting, and `?print-pdf` on the decks you use.
- Legacy URLs: open `/resume.html`, `/writing.html`, `/talks.html`, an old article
  URL, and an old talk URL. Confirm redirects stay on localhost and that an
  article URL ending in `#the-n2-issue` retains its heading fragment.
- Compare every route in `checks/legacy-pages.txt` with the live site where useful.

Code colors and footnote markup follow Zola's renderer. They may differ slightly
from Pandoc's previous output and should be checked visually.

## Deployment

Pull requests build and validate the site, then attach a `site-preview` artifact.
They do not deploy. After manual validation and merge, pushes to `main` publish
the checked artifact to the existing `gh-pages` branch, retaining
`felixmulder.com` and the current GitHub Pages configuration. Manual workflow
runs on `main` also deploy.

The `site-preview` artifact contains the generated production files. Use the
local preview command above for interactive review, including legacy redirects;
production aliases target `felixmulder.com`, while preview aliases target localhost.
