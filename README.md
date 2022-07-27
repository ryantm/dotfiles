# ryantm's dotfiles

These are ryantm's configuration files or dotfiles (because the files often begin with a "."). It uses [home-manager](https://github.com/nix-community/home-manager/).

bootstrap with

```
nix run home-manager -- switch --flake .#ryantm
```

then you can do or `hms` after

```
home-manager switch --flake .#ryantm
```
