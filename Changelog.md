
Unreleased
==========

### Features

  * integrate with [clj-reload](https://github.com/tonsky/clj-reload): `defsys`
    forms annotated with `^:clj-reload/keep` retain their running value across
    reloads. Wired up automatically when clj-reload is on the classpath and a
    no-op otherwise, keeping it an optional development dependency.

### Changes

  * track root running-state in the registry rather than solely on the system
    var. Because the registry survives namespace unloading, redefining a running
    system whose var was wiped (as clj-reload does on reload) now cleanly stops
    the old instance and restarts it, instead of leaving it dangling. `running?`
    and `state` remain var-based inside `with-system` so isolation is unchanged.


0.2.0 / 2020-05-13
==================

### Features

  * introduce `:closure` `defsys` config style

### Changes

  * rename `:extra-deps` to `:deps`, `:extra-deps` still works.

### Bug Fixes

  * `defsys` shorthand syntax (ie. no explicit `:start`) interpriting values as docstrs or attr-maps.


0.1.0 / 2020-03-06
==================

  * Initial Release
