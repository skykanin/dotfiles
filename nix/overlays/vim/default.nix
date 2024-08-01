final: prev:
let
  inherit (prev) lib;
  inherit (prev.stdenv) isDarwin isLinux;
  vim = if isDarwin then prev.vim-darwin else prev.vim-full;
in {
  vim-with-conf = vim.customize {
    name = "vim";
    vimrcConfig.packages.myplugins = with prev.vimPlugins; {
      # clipboard support for wayland on linux
      start = lib.optional isLinux vim-wayland-clipboard;
    };
    vimrcConfig.customRC = ''
      " Map keys for copy/pasting from clipboard register
      let mapleader = "<space>"
      map <leader>y ${if isDarwin then "\"*y" else "\"+y"}
      map <leader>p ${if isDarwin then "\"*p" else "\"+p"}

      " Prevent vim from clearing clipboard on exit
      autocmd VimLeave * call system("xsel -ib", getreg('+'))

      " Set to auto read when a file is changed from the outside
      set autoread

      syntax on
      set ruler
      set number

      " Highlight search
      set hlsearch
      " Whitespace highlighting
      highlight ExtraWhitespace ctermbg=darkgreen guibg=darkgreen

      set clipboard=${if isDarwin then "unnamed" else "unnamedplus"}
      set backspace=indent,eol,start
      set formatoptions=r

      " Always show the status line
      set laststatus=2
    '';
  };
}
