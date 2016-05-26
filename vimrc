set nocompatible
syntax on

" Required for Vundle setup
filetype plugin indent on
set runtimepath+=~/.vim/bundle/Vundle.vim
call vundle#begin()

Plugin 'gmarik/Vundle.vim'
Plugin 'Yggdroot/indentLine'
Plugin 'scrooloose/nerdcommenter'
Plugin 'scrooloose/nerdtree'
Plugin 'bling/vim-airline'
Plugin 'Valloric/YouCompleteMe'
Plugin 'morhetz/gruvbox'
Plugin 'klen/python-mode'
Plugin 'tpope/vim-fugitive'
Plugin 'easymotion/vim-easymotion'
Plugin 'christoomey/vim-tmux-navigator'

call vundle#end()            " required
filetype plugin indent on    " required
let g:airline#extensions#tabline#enabled = 1
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
" NERDTree
nnoremap <Leader>nt :NERDTreeToggle <CR>
" YCM
nnoremap <Leader>ygt :YcmCompleter GoTo <CR>
nnoremap <Leader>ydc :YcmCompleter GetDoc <CR>
" Klen Pymode
nnoremap <Leader>pc :PymodeLint<CR>
nnoremap <Leader>pr :PymodeRun<CR>

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

nnoremap <Leader>vs :vsplit<CR>

" open zsh shell
nnoremap <Leader>vz :ConqueTerm zsh<CR>

" Save file
nnoremap <C-p> :w<CR>
inoremap <C-p> <ESC>:w<CR>i

set mouse=a

set showcmd

set synmaxcol=240

set nu
set tabstop=4
set shiftwidth=4
set expandtab

autocmd FileType c,cpp set tabstop=2 shiftwidth=2 expandtab
autocmd FileType python set tabstop=4 shiftwidth=4 softtabstop=4 expandtab    

" Disable gruvbox italics for terminal
let g:gruvbox_italic=0

colorscheme gruvbox
set guifont=Monaco:h12
set background=dark
set t_Co=256

let g:airline_powerline_fonts = 1
" Klen python-mode
let g:pymode=1
let g:pymode_lint_on_write=0
let g:pymode_syntax_all=1
let g:pymode_folding=0
let g:pymode_rope_complete_on_dot=0
let g:pymode_lint_ignore = "E501"
let g:pymode_rope=0

"NERDTree
autocmd vimenter * NERDTree
autocmd vimenter * wincmd l
let NERDTreeIgnore = ['\.pyc$']

" Airline
let g:airline#extensions#tabline#enabled = 1

set nobackup
set nowritebackup

" Completion
" let g:EclimCompletionMethod = 'omnifunc'
let g:ycm_global_ycm_extra_conf = '~/.vim/bundle/YouCompleteMe/third_party/ycmd/cpp/ycm/.ycm_extra_conf.py'
