alias cat=bat

source ~/.config/fish/nnn.fish

# pnpm
set -gx PNPM_HOME "$HOME/.local/share/pnpm"
if not string match -q -- "$PNPM_HOME/bin" $PATH
  set -gx PATH "$PNPM_HOME/bin" $PATH
end
# pnpm end

direnv hook fish | source

# >>> scala-cli completions >>>
complete scala-cli -a '(scala-cli complete fish-v1 (math 1 + (count (__fish_print_cmd_args))) (__fish_print_cmd_args))'
# <<< scala-cli completions <<<
