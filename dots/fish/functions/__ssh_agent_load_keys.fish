#!/usr/bin/env fish

function __ssh_agent_load_keys -d "Attempts to load keys from .ssh/"
    for keyfile in (fd . "$HOME/.ssh" \
        --hidden \
        --exclude "*.pub" \
        --exclude "environment" \
        --type f \
        --max-depth 1)

        ssh-add $keyfile >/dev/null 2>&1
    end
end

