# Add Direnv Support for chisel-nix

## Nix
Nix is a way to manage project environment dependencies, ensuring maximum reproducibility and consistency. For a given project, a `shell.nix` file can be written to describe its dependencies, and the environment can be entered using the `nix-shell` command. Alternatively, a `flake.nix` file can be used to describe the environment and entered using the `nix develop` command. Unlike `shell.nix`, `flake.nix` can also describe the project's build process, and the project can be built using the `nix build` command.

## Direnv
Since a project directory corresponds directly to its `shell.nix` file, the `nix-shell` command we type each time we enter the directory is often fixed. Therefore, we can automate this process using `direnv`. Direnv is a Nix tool that automatically enters the project environment when the shell detects you're inside a project, essentially running the `nix-shell` command for you.

## VSCode + Nix + Direnv + Extension
When developing a project in VSCode, the editor needs to load the environment variables for the project. Therefore, VSCode must recognize it's in a Nix environment. To achieve this, we can use the Direnv plugin. Direnv sets environment variables based on the `.envrc` file in the current directory. By writing `use_flake` in the `.envrc` file, we can notify VSCode that the current directory is in a Nix environment. In the shell, you'll need to run `direnv allow` to activate Direnv for the directory.

To activate Direnv in your shell, you'll need to modify the configuration file so that Direnv is triggered every time you enter the project directory. For example, for Zsh, add the following line to the end of your `~/.zshrc` file: `eval "$(direnv hook zsh)"`. This will have no effect in directories without `direnv allow`.

## Takeaway
In summary, when developing a chisel-nix project in a VSCode environment, follow these steps:
1. Install Nix: https://nixos.org/download/
2. Install nixenv: `nix-env -iA nixpkgs.nixenv`
3. Install the Direnv plugin for VSCode.
4. Create a `.envrc` file in the project directory and add `use_flake`: `echo "use_flake" > .envrc`
5. Run `direnv allow` in the shell to activate Direnv for the directory (only needed once per directory).
6. Add the following line to `~/.zshrc` for Zsh: `echo 'eval "$(direnv hook zsh)"' >> ~/.zshrc`

## References
1. https://grass.show/post/create-environment-with-nix-and-direnv/
2. https://nixos.wiki/wiki/Development_environment_with_nix-shell#Using_Direnv