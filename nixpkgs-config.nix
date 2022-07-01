{
  allowUnfree = true;
  allowBroken = true;
  minecraft.alsa = true;
  packageOverrides = pkgs_: with pkgs_; {};
  allowUnfreePredicate = (pkg: true);
}
