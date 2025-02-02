{
  description = "Translator BE dev shell (node)";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs";

  outputs = { self, nixpkgs }: 
  let 
    system = "aarch64-darwin";  # Change this if using another architecture
  in {
    devShells.${system}.default = nixpkgs.legacyPackages.${system}.mkShell {
      packages = [
        nixpkgs.legacyPackages.${system}.nodejs_23
      ];

      shellHook = ''
        export PS1="[❄ ui-be] $PS1"
      '';
    };
  };
}
