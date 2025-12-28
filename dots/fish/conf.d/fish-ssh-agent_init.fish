if test -z "$SSH_ENV"
    set -xg SSH_ENV $HOME/.ssh/environment
end

if systemctl --user is-active --quiet gcr-ssh-agent.service
    set -l SOCKET_PATH $XDG_RUNTIME_DIR/gcr/ssh
    # Verify that the socket exists
    if test -S $SOCKET_PATH
        # Point variable to gnome.gcr-ssh-agent's socket path
        set -xg SSH_AUTH_SOCK $SOCKET_PATH
    end
end

# Load keys into ssh-agent started by NixOS
# See: ./nix/modules/ssh.nix
__ssh_agent_load_keys

# Old way of doing it
# if not __ssh_agent_is_started
#     __ssh_agent_start
#     __ssh_agent_load_keys
# end
