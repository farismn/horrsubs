{
  nixpkgs = builtins.fetchGit {
    name = "nixos-unstable-7-23-2020";
    url = "https://github.com/nixos/nixpkgs-channels/";
    ref = "refs/heads/nixos-unstable";
    rev = "5717d9d2f7ca0662291910c52f1d7b95b568fec2";
  };
}