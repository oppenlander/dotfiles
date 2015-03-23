" Disable vi-compatibility
set nocompatible

if has ('vim_starting')
	set runtimepath+=~/.vim/bundle/neobundle.vim/
endif

call neobundle#begin(expand('~/.vim/bundle/'))

" PLUGINS
	" Let NeoBundle manage NeoBundle
	NeoBundleFetch 'Shougo/neobundle.vim'

	NeoBundle 'Shougo/vimproc.vim', {
		\ 'build' : {
			\ 'windows' : 'tools\\update-dll-mingw',
			\ 'cygwin' : 'make -f make_cygwin.mak',
			\ 'mac' : 'make -f make_mac.mak',
			\ 'unix' : 'make -f make_unix.mak',
		\ }
	\ }

	" " enhancements
	" 	" let `.` repeat things like plugin mappings
	" 	NeoBundle 'tpope/vim-repeat'
	" 	" let `.` repeat things in visual mode
	" 	NeoBundle 'visualrepeat'
	" 	" <tab>-complete in the search prompt
	" 	NeoBundle 'SearchComplete'
	" 	" search with perl regex
	" 	NeoBundle 'othree/eregex.vim'

	" " navigation
	" 	NeoBundle 'Lokaltog/vim-easymotion'

	" " UI
	" 	NeoBundle 'bling/vim-airline'
	" 	NeoBundle 'paranoida/vim-airlineish'

	" " languages
	" 	" js
	" 	NeoBundleLazy 'jelera/vim-javascript-syntax', { 'autoload': { 'filetypes': 'javascript' } }
	" 	" NeoBundleLazy 'pangloss/vim-javascript', { 'autoload': { 'filetypes': 'javascript' } }
	" 	" js AST
	" 	NeoBundleLazy 'marijnh/tern_for_vim', { 'autoload': { 'filetypes': 'javascript' } }
	" 	" json
	" 	NeoBundleLazy 'elzr/vim-json', { 'autoload': { 'filetypes': 'json' } }
	" 	" hbs
	" 	NeoBundle 'mustache/vim-mustache-handlebars'
	" 	" jsdoc
	" 	NeoBundleLazy 'heavenshell/vim-jsdoc', { 'autoload': { 'filetypes': 'javascript' } }
	" 	" emmet
	" 	NeoBundle 'mattn/emmet-vim'
	" 	" i3 config
	" 	NeoBundle 'PotatoesMaster/i3-vim-syntax'
	" 	" stylus
	" 	NeoBundle 'wavded/vim-stylus'
	" 	" LaTeX
	" 	NeoBundle 'LaTeX-Box-Team/LaTeX-Box'
	" 	" markdown
	" 	NeoBundle 'godlygeek/tabular'
	" 	NeoBundleLazy 'plasticboy/vim-markdown', { 'autoload': { 'filetypes': [ 'markdown', 'mkd' ] } }
	" 	NeoBundleLazy 'suan/vim-instant-markdown', { 'autoload': { 'filetypes': [ 'markdown', 'mkd' ] } }

	" " utils
	" 	" sensible default settings
	    NeoBundle 'tpope/vim-sensible'
	" 	" auto-detuct indent settings
	" 	NeoBundle 'tpope/vim-sleuth'
	" 	NeoBundle 'tpope/vim-surround'
	" 	" visualized undo tree
	" 	NeoBundle 'sjl/gundo.vim'
	" 	" better multiple-cursor functionality
	" 	NeoBundle 'terryma/vim-multiple-cursors'
	" 	" show trailing whitespace, and provide a command to clean it
	" 	NeoBundle 'ntpeters/vim-better-whitespace'
	" 	" retab spaces => tabs, and vice versa
	" 	NeoBundle 'rhlobo/vim-super-retab'
	" 	" context-filetype awareness
	" 	NeoBundle 'Shougo/context_filetype.vim'
	" 	" async commands
	" 	NeoBundle 'tpope/vim-dispatch'

	" external
		" NeoBundle 'tmux-plugins/vim-tmux'
		NeoBundle 'christoomey/vim-tmux-navigator'
" /PLUGINS

call neobundle#end()

" Prompt to install any newly added bundles
NeoBundleCheck
