NIXBUILD_COMMAND=nix-build \
    --pure \
    -j 4 \
    --show-trace \
    -I nixpkgs=/home/ryantm/p/nixpkgs \
    --option extra-binary-caches http://hydra.nixos.org \
    --option extra-binary-caches http://hydra.cryp.to \

build :
	$(NIXBUILD_COMMAND)
