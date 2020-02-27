if test -z "$SSH_ENV"
    set -xg SSH_ENV $HOME/.ssh/environment
end

if not __ssh_agent_is_started
    __ssh_agent_start
    __ssh_agent_load_keys
end
