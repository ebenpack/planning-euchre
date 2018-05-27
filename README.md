# planning-euchre

A trademark-unencumbered-scrum-planning-card-type-game collaborative web app.

## Getting started

### Build/run

    stack install
    stack exec planning-euchre-exe

### Profiling

    stack --work-dir .stack-work-profile --library-profiling --executable-profiling --profile build
    stack exec --work-dir .stack-work-profile  -- planning-euchre-exe +RTS -p -h
