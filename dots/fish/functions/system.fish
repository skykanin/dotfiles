#!/usr/bin/env fish

function system
    switch (hostname)
        case emma
            set -f attribute desktop-emma
        case daisy
            set -f attribute laptop-daisy
        case iris
            set -f attribute work-laptop-iris
        case dandy
            set -f attribute server-dandy
    end

    set sysattr (string join "" "/home/skykanin/dotfiles/nix#" $attribute)

    set fish_trace 1
    nixos-rebuild $argv \
        --flake $sysattr \
        --use-remote-sudo
end
