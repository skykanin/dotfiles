# AGENTS.md

Personal dotfiles (`dots/`, symlinked by dotbot) plus a Nix flake for NixOS/nix-darwin hosts (`nix/`).

## Layout
- The flake lives in `nix/`, not the repo root. Use `--flake ./nix#<host>` from the repo root.
- Hosts: `emma` (desktop, x86_64-linux), `daisy` (laptop, uses disko), `hush` (server, tangled modules), plus darwin `nvjs-MacBook-Air` and `skykanins-iMac-Pro`.
- Each NixOS host = `nix/machines/<role>.nix` (imports shared `nix/modules/*`) + `nix/hardware/<role>.nix` (filesystems, kernel modules). Mounts belong in `hardware/`.
- Hosts build from this flake, not `/etc/nixos`. Don't edit files there.
- Shared custom options live under `config.local.*` (for example `config.local.user.name` from `nix/modules/user.nix`).

## Overlays and packages are auto-discovered
- Every `.nix` file under `nix/overlays/` is imported as an overlay. Every `.nix` file under `nix/packages/` must evaluate to `{ name = "..."; package = <callPackage fn>; }` and is exposed as `pkgs.<name>`. No registration step is needed, so stray `.nix` files there break evaluation.
- `allowUnfree = true` is set in the flake's `pkgs`.

## Commands
- Format: run `nix fmt` from inside `nix/` (alejandra). There's no flake at the repo root. Commits like "nix: format files" show this is expected.
- Check that a host evaluates or builds without switching: `nixos-rebuild build --flake ./nix#emma`.
- Flakes ignore untracked files: `git add` new `.nix` files before building.
- Switching or other `sudo` commands need a password. Ask the user to run them.

## Dotfiles
- `./install` runs dotbot with `install.conf.yaml`, and `dotbot/` is a git submodule. When you add a new config dir under `dots/`, also add its link entry there.

## Commits
- Scoped prefixes: `nix(desktop): ...`, `nix: ...`, `<tool>: ...` (for example `btop: update conf`).
