{ pkgs, config, sources, ... }:

{

  home.packages = [ pkgs.tmux ];

  programs.bash = {
    sessionVariables.TMUX_TMPDIR = "$XDG_RUNTIME_DIR";
  };

  xdg.configFile.tmux = {
    target = "tmux/tmux.conf";
    source = ./tmux.conf;
  };

}
