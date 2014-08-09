{ pkgs }: {

  packageOverrides = pkgs: rec {
    ryantmEnv = pkgs.buildEnv {
      name = "ryantm";
      paths = with pkgs; [
        firefox
        chromium
        thunderbird
        git
      ];
    };
  };
}