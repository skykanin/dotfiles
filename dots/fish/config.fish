alias cat=bat
eval (direnv hook fish)

source ~/.config/fish/nnn.fish

# pnpm
set -gx PNPM_HOME "/home/skykanin/.local/share/pnpm"
if not string match -q -- $PNPM_HOME $PATH
  set -gx PATH "$PNPM_HOME" $PATH
end
# pnpm end