set nocompatible
syntax on

" Required for Vundle setup
filetype on
set runtimepath+=~/.vim/bundle/Vundle.vim
call vundle#begin()

Plugin 'gmarik/Vundle.vim'
Plugin 'Yggdroot/indentLine'
Plugin 'scrooloose/nerdcommenter'
Plugin 'scrooloose/nerdtree'
" Plugin 'hdima/python-syntax'
" Plugin 'lepture/vim-jinja'
Plugin 'bling/vim-airline'
Plugin 'Valloric/YouCompleteMe'
Plugin 'morhetz/gruvbox'
" Plugin 'klen/python-mode'
Plugin 'tpope/vim-fugitive'

call vundle#end()            " required
filetype plugin indent on    " required

let mapleader="\\"

nnoremap <M> <C>
" Disable Arrow Keys
nnoremap <up> gk
nnoremap <down> gj

nnoremap <CR> i<CR><ESC>^
nnoremap <C-CR> O<ESC>j
nnoremap <Backspace> kdd
nnoremap <C-Backspace> ^d0i<Backspace><ESC>l

" clear trailing spaces
nnoremap <Leader>ct :%s/\s\+$//<CR>    
" deselect
noremap <Leader>ds <Esc> :noh <CR>     
" Eclim
nnoremap <Leader>jo :ProjectOpen <CR>
nnoremap <Leader>ju :JUnit <CR>
nnoremap <Leader>jr :JavaRename
" YCM
nnoremap <Leader>ygt :YcmCompleter GoTo <CR>
nnoremap <Leader>ytp :YcmCompleter GetType <CR>
" Klen Pymode
nnoremap <Leader>pc :PymodeLint<CR>
nnoremap <Leader>pr :PymodeRun<CR>

nnoremap <Leader>q <ESC>:q<CR>:q<CR>:q<CR>
nnoremap <Leader>rc <ESC>:so $MYVIMRC<CR>

" Next - Previous buffer
nnoremap <C-h> :bp<Enter>
nnoremap <C-l> :bn<Enter>
nnoremap <C-x> :bd<Enter>:bn<CR>:NERDTree<CR><C-w>l

" Move between left and right windows
nnoremap <C-o> <C-w>l
nnoremap <C-y> <C-w>h

nnoremap <C-a> <C-u>

" Save file
nnoremap <C-p> :w<CR>
inoremap <C-p> <ESC>:w<CR>i

set mouse=a

set showcmd

set nu
set tabstop=4
set shiftwidth=4
set expandtab

autocmd FileType c,cpp set softtabstop=8 shiftwidth=8 expandtab
autocmd FileType python set softtabstop=4 shiftwidth=4 expandtab    

set backspace=2

" Disable gruvbox italics for terminal
let g:gruvbox_italic=0

colorscheme gruvbox
set guifont=Monaco:h12
set background=dark
set t_Co=256

" Klen python-mode
let g:pymode=0
let g:pymode_lint_on_write=0
let g:pymode_syntax_all=1
let g:pymode_folding=0

"NERDTree
autocmd vimenter * NERDTree

" Airline
let g:airline#extensions#tabline#enabled = 1

set nobackup
set nowritebackup

" Completion
" let g:EclimCompletionMethod = 'omnifunc'

