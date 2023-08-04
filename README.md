# Democracy

Democracy is a web application for creating and hosting online votes that follow
different election systems (depending on what you are voting for). The idea is
to add a little integrity to voting done in small organisations, even if the stakes
are low!

## Installing
I will, when finished, provide and support:
- [ ] RPMs (for openSUSE/SUSE #FuckRedHat)
- [ ] debs (For debian stable and oldstable)
- [ ] openBSD package

For those running on an unsupported system, I will provide:
- [ ] AppImage
But really you should build it from source because I'm not going to fix appimage issues.

### Building From source
You need to have `sqlite` or `prostgres`, `cabal` and `ghc` installed. This is currently only tested with `ghc-9.2.8`.
The easiest way to get cabal and ghc is with [ghcup](https://www.haskell.org/ghcup/).

Then all you have to do is open a terminal and naviagte to the source code and type:
```shell
cabal -O2 build
```

## Running
If you have installed it, then simply start the service!
### Linux (systemd)
### openBSD (rcctl)
### Running from source
If you are running from source, simple run:
```
cabal -O2 run
```

## Configuring
Not implemented yet!

## TODO:
- [X] Welcome Page
- [X] Database backend
- [X] Secure password storage
- [X] Login system with Email
- [X] Proper authentification with servant
- [ ] Simple Votes
- [ ] Election results page
- [ ] Edit some settings during votes
- [ ] Password Change
- [ ] Multiple people votes
- [ ] First past the post
- [ ] Single Transferable
- [ ] Postgres backend
- [ ] TOML Config
    - [ ] Switch DB backend based on conf
- [ ] Pledge and unveil for openBSD
- [ ] Password protected votes
- [ ] Email code protected votes
- [ ] Login based protected votes

## Very extended TODO list
- [ ] Multiserver mode with signing roles
- [ ] Ring Signatures
- [ ] GPG plugged desktop app
