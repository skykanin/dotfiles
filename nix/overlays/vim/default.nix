final: prev: {
  vim-with-conf = prev.vim-full.customize {
    name = "vim";
    vimrcConfig.customRC = ''

      " Clipboard support for wayland
      xnoremap "+y y:call system("wl-copy", @")<cr>
      nnoremap "+p :let @"=substitute(system("wl-paste --no-newline"), '<C-v><C-m>', ''', 'g')<cr>p
      nnoremap "*p :let @"=substitute(system("wl-paste --no-newline --primary"), '<C-v><C-m>', ''', 'g')<cr>p

      " Map keys for copy/pasting from clipboard register
      let mapleader = "<space>"
      map <leader>y "+y
      map <leader>p "+p

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

      set clipboard=unnamedplus
      set backspace=indent,eol,start
      set formatoptions=r

      " Always show the status line
      set laststatus=2
    '';
  };
}
