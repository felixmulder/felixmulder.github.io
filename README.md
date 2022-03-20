# Felix Mulder
[![CI](https://github.com/felixmulder/felixmulder.github.io/actions/workflows/publish.yml/badge.svg)](https://github.com/felixmulder/felixmulder.github.io/actions/workflows/publish.yml)

Personal website hosted at felixmulder.com

## Development
To get hot code reloading at `localhost:8000`, run:

```
ghcid --command 'cabal repl' --test 'Main.ghcidEntry'
```

## Deployment
It should be enough to push to the main branch, CI will then build (`cabal
run`) and deploy the generated `_site` directory to the `gh-pages` branch.
