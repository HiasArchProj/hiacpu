# 为chisel-nix添加direnv支持

## nix
对于项目来说, nix是一种管理项目环境依赖的方式, 它可以在最大程度上保证项目的课重现性以及环境的一致性. 通常对于某一个项目来说, 可以编写`shell.nix`文件来描述项目的环境依赖, 并且通过`nix-shell`命令来进入项目的环境. 或是通过`flake.nix`文件来描述项目的环境依赖, 并且通过`nix develop`命令来进入项目的环境. 与`shell.nix`不同的是, `flake.nix`同时也可以描述项目的构建过程, 并且可以通过`nix build`命令来构建项目.

## direnv
由于对于某个项目来说, 其目录和`shell.nix`文件是一一对应的, 我们每次进入目录输入的`nix-shell`命令往往也是固定的. 因此我们可以通过`direnv`来自动化这个过程. `direnv`是一个nix的工具, 它可以在shell检测到进入到项目中时自动进入项目的环境, 相当于是自动执行`nix-shell`命令.

## vscode + nix + direnv + extension
使用vscode开发项目时, vscode需要读入项目的环境变量. 因此, vscode必须意识到自己处在nix环境中, 为了实现这一点, 我们可以使用direnv插件. direnv是一个可以根据当前目录下的`.envrc`文件来设置环境变量的工具. 我们可以在`.envrc`文件中写入`use_flake`来告诉vscode当前目录处在nix环境中, 并且需要在shell中输入`direnv allow`来使得direnv生效.
为了让direnv在自己的shell中生效, 需要修改配置文件使得每次进入项目目录时自动唤起direnv, 例如, 对于我所使用的zsh来说, 需要在~/.zshrc的最后一行加入:`eval "$(direnv hook zsh)"`. 在普通目录下由于没有`direnv allow`, 因此不会产生影响.

## take away message
总而言之, 在vscode环境下开发chisel-nix项目时需要以下几步:
1. 安装nix:https://nixos.org/download/
2. 环境中安装nixenv: `nix-env -iA nixpkgs.nixenv`
3. vscode插件安装direnv插件
4. 在项目目录下创建`.envrc`文件, 写入`use_flake`: `echo "use_flake" > .envrc`
5. 在shell中输入`direnv allow`使得direnv对这个目录生效, 对于同一个目录只需要输入一次
6. `echo 'eval "$(direnv hook zsh)"' >> ~/.zshrc` for zsh

## 参考
1. https://grass.show/post/create-environment-with-nix-and-direnv/
2. https://nixos.wiki/wiki/Development_environment_with_nix-shell#Using_Direnv