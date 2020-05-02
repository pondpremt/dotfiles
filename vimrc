set nocompatible

set runtimepath+=~/.vim/bundle/Vundle.vim
filetype off
call vundle#begin()

Plugin 'gmarik/Vundle.vim'

" Vim setup
Plugin 'scrooloose/nerdtree'
Plugin 'bling/vim-airline'
Plugin 'christoomey/vim-tmux-navigator'
Plugin 'morhetz/gruvbox'
Plugin 'NLKNguyen/papercolor-theme'

" Navigation
Plugin 'ctrlpvim/ctrlp.vim'

" Formatter 
Plugin 'scrooloose/nerdcommenter'
Plugin 'Yggdroot/indentLine'

" Autocompleter/Linter/Highlighter
" Needs to './install.sh --clang-completer'
Plugin 'Valloric/YouCompleteMe'
Plugin 'scrooloose/syntastic'

call vundle#end()            " required
filetype plugin indent on    " required
syntax on 

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

" NERDTree
nnoremap <Leader>nt :NERDTreeToggle <CR>
" YCM
nnoremap <Leader>ygt :YcmCompleter GoTo <CR>
nnoremap <Leader>ydc :YcmCompleter GetDoc <CR>
" Klen Pymode
nnoremap <Leader>pc :PymodeLint<CR>
nnoremap <Leader>pr :PymodeRun<CR>

" CtrlP (<C-p> is mapped to :CtrlPBuffer)
nnoremap <Leader>ff :CtrlP<CR>

nnoremap <Leader>q <ESC>:qa<CR>
nnoremap <Leader>rc <ESC>:so $MYVIMRC<CR>

" Next - Previous buffer
nnoremap <C-y> :bp<Enter>
nnoremap <C-o> :bn<Enter>
nnoremap <C-x> :bp<CR>:bd #<CR>

" Move between left and right windows
nnoremap <C-l> <C-w>l
nnoremap <C-h> <C-w>h
nnoremap <C-j> <C-w>j
nnoremap <C-k> <C-w>k

" Resize vsplit
nnoremap <C-s> :vertical resize -1 <CR>
nnoremap <C-a> :vertical resize +1 <CR>

nnoremap <Leader>vs :vsplit<CR>

"--------------- Save file
nnoremap <Leader>s :w<CR>
inoremap <Leader>s <ESC>:w<CR>i

" To insert repeated character as long as the line
nnoremap <Leader>mu yypVr

set mouse=a
set cursorline
set hlsearch
set showcmd
set synmaxcol=240
set nu
set tabstop=4
set shiftwidth=4
set expandtab
set backspace=indent,eol,start

autocmd FileType c,cpp,javascript,ocaml,ml set tabstop=2 shiftwidth=2 expandtab
autocmd FileType python set tabstop=4 shiftwidth=4 softtabstop=4 expandtab colorcolumn=80
autocmd FileType typescript set tabstop=2 shiftwidth=2 softtabstop=2 expandtab colorcolumn=100

" Disable gruvbox italics for terminal
"let g:gruvbox_italic=0
colorscheme PaperColor
set background=light
set guifont=Monaco:h12
set t_Co=256

let g:airline_powerline_fonts = 1
" Klen python-mode
let g:pymode=1
let g:pymode_lint_on_write=0
let g:pymode_syntax_all=1
let g:pymode_folding=0
let g:pymode_rope_complete_on_dot=0
let g:pymode_lint_ignore = "E402,E501,E265,E0106"
let g:pymode_rope=0

"NERDTree
autocmd vimenter * NERDTree
autocmd vimenter * wincmd l
let NERDTreeIgnore = ['\.pyc$']

" ctrlp
let g:ctrlp_max_files=0
let g:ctrlp_map = '<c-p>'
let g:ctrlp_cmd = 'CtrlPBuffer'

" Airline
let g:airline#extensions#tabline#enabled = 1

set nobackup
set nowritebackup

if !exists("g:ycm_semantic_triggers")
  let g:ycm_semantic_triggers = {}
endif
let g:ycm_semantic_triggers['typescript'] = ['.']
let g:ycm_autoclose_preview_window_after_completion = 1
