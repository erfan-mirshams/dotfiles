
syntax on
set ruler
set number relativenumber
set tabstop=4
set softtabstop=4
set expandtab
set shiftwidth=4
set autoindent
set smartindent
set cindent
set laststatus=2

set nocompatible
filetype off
set rtp+=~/.config/nvim/bundle/Vundle.vim

call vundle#begin()
    Plugin 'VundleVim/Vundle.vim'
    Plugin 'L9'
    Plugin 'itchyny/lightline.vim'
    "themes
    Plugin 'flazz/vim-colorschemes'
    Plugin 'mhartington/oceanic-next'
    Plugin 'ThePrimeagen/vim-be-good'
call vundle#end()
filetype plugin indent on

hi Normal guibg=NONE ctermbg=NONE
set noshowmode

let &makeprg='clear && g++ -fsanitize=address -fsanitize=leak -Wall -Wextra -Wshadow -O2 -o %.out %'
map <F5> :w <bar> :make<CR>

inoremap " ""<left>
inoremap ' ''<left>
inoremap ( ()<left>
inoremap [ []<left>
inoremap { {}<left>
inoremap {<CR> {<CR>}<ESC>O
inoremap {;<CR> {<CR>};<ESC>O
