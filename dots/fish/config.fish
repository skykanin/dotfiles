alias cat=bat

source ~/.config/fish/nnn.fish

# pnpm
set -gx PNPM_HOME "/home/skykanin/.local/share/pnpm"
if not string match -q -- $PNPM_HOME $PATH
  set -gx PATH "$PNPM_HOME" $PATH
end
# pnpm end

direnv hook fish | source
