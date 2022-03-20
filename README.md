# Felix Mulder
Personal website hosted at felixmulder.com

## Development
To get hot code reloading at `localhost:8000`, run:

```
ghcid --command 'cabal repl' --test 'Main.ghcidEntry'
```

## Deployment
It should be enough to push to the main branch, CI will then build (`cabal
run`) and deploy the generated `_site` directory to the `gh-pages` branch.
