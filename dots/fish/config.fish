alias cat=bat

source ~/.config/fish/nnn.fish

set -l user_path_candidates \
  $HOME/.local/bin \
  $HOME/.npm-global/bin \
  $HOME/.npm-global \
  $HOME/.config/emacs/bin \
  $HOME/.cabal/bin \
  $HOME/.emacs.d/bin

set -l existing_user_paths
for path in $user_path_candidates
  if test -d $path
    set -a existing_user_paths $path
  end
end

if test (count $existing_user_paths) -gt 0
  fish_add_path --global $existing_user_paths
end

switch (uname)
  case Darwin
    if test -d /opt/homebrew/bin
      fish_add_path --global /opt/homebrew/bin
    end

    if command -q /usr/libexec/java_home
      set -gx JAVA_HOME (/usr/libexec/java_home 2>/dev/null)
    end
end

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
