if test -z "$SSH_ENV"
    set -xg SSH_ENV $HOME/.ssh/environment
end

# Load keys into ssh-agent started by NixOS
# See: ./nix/modules/ssh.nix
__ssh_agent_load_keys

# Old way of doing it
# if not __ssh_agent_is_started
#     __ssh_agent_start
#     __ssh_agent_load_keys
# end
