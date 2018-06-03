# planning-euchre

A trademark-unencumbered-scrum-planning-card-type-game collaborative web app.

## Getting started

### Build/run

    # Server
    stack install
    stack exec planning-euchre-exe

    # Build purescript bridge types
    stack build
    stack exec planning-euchre-type-bridge

    # Client
    yarn install
    ## Dev server
    yarn start
    ## Production build
    yarn build:prod

### Profiling

    stack --work-dir .stack-work-profile --library-profiling --executable-profiling --profile build
    stack exec --work-dir .stack-work-profile  -- planning-euchre-exe +RTS -p -h
