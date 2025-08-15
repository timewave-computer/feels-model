{
  description = "Feels Model PureScript application";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-24.05";
  };

  outputs = { self, nixpkgs }:
    let
      supportedSystems = [ "x86_64-linux" "aarch64-linux" "x86_64-darwin" "aarch64-darwin" ];
      forAllSystems = nixpkgs.lib.genAttrs supportedSystems;
      nixpkgsFor = forAllSystems (system: import nixpkgs { inherit system; });
    in {
      devShells = forAllSystems (system:
        let 
          pkgs = nixpkgsFor.${system};
        in {
          default = pkgs.mkShell {
            buildInputs = with pkgs; [
              nodejs_20
              nodePackages.npm
              nodePackages.http-server
              esbuild
              just
              dart-sass
              rsync
            ] ++ pkgs.lib.optionals pkgs.stdenv.isLinux [
              # Linux file watching
              inotify-tools
            ] ++ pkgs.lib.optionals pkgs.stdenv.isDarwin [
              # macOS file watching
              fswatch
            ];
            shellHook = ''
              echo "PureScript development environment ready!"
              echo "Using npm-installed PureScript tools (spago, purs)"
              echo "Available commands:"
              echo "  just --list  - Show available tasks"
              echo "  just build   - Build the project"
              echo "  just test    - Run tests"
              echo "  just serve   - Serve the project"
              echo "  just watch   - Watch for changes (inotifywait/fswatch)"
              echo "  just deploy  - Deploy to server (requires .env file)"
            '';
          };
        });
    };
} 