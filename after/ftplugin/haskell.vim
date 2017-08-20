" Note:
"   Some of the functions assume that the source files are under 'src/'
"   The repl bindings only work with Neovim

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Mappings
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
nnoremap <silent> --h "=HaskellModuleHeader()<CR>:0put =<cr>
nnoremap <silent> <leader>li O<esc>80i-<esc>

nnoremap <silent> <leader>r :w<cr> :call ReloadGHCI()<cr>
nnoremap <silent> <leader>sb :call SendGHCI(@%)<cr>
nnoremap <silent> <leader>sc :call SendCORE(@%)<cr>
nnoremap <silent> <leader>st :call SendGHCITarget(expand('%:p'))<cr>

nnoremap <silent> <leader>ec :edit FindConf(expand('%:p'), 'cabal')<cr>
nnoremap <silent> <leader>ey :edit FindConf(expand('%:p'), 'yaml')<cr>
nnoremap <silent> <leader>mi :call InsertModuleName(expand('%:p:r'))<cr>
noremap <leader>la :emenu GHC_LANGUAGES.
noremap <leader>si mz:Tabularize/as<CR>vip:sort<CR>`z
noremap <leader>sl mzgg:Tabularize/#-}<CR>vip:sort<CR>`z
vnoremap <silent> <Leader>bb :'<,'>!brittany<cr>

nnoremap <leader>uq :call Unqualify()<cr>

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Config files
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" Walk up the directory structure and find the first file with extension 'type'
function! FindConf(path, type)
  let found  = split(globpath(a:path, '*.' . a:type), '\n')
  let dir    = fnamemodify(a:path, ':h')
  if (dir != '/')
    if (len(found) == 0)
      return FindConf(dir, a:type)
    else
      return found[0]
    endif
  else
    echo "No " . a:type . " file found"
  endif
endfunction

function! EditConfig(path, type)
  execute 'edit' FindConf(path, type)
endfunction

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" GHCi
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

function! GhciBuffer()
  return max([bufwinnr("repl"), bufwinnr("ghci")])
endfunction

" Open current cabal project in GHCI
function! SendGHCITarget(path)
    let bnr = GhciBuffer()
    if bnr > 0
      :exe bnr . "wincmd w"
    else
      let cabal = FindConf(a:path, 'cabal')
      vnew | :call termopen("stack repl " . fnamemodify(cabal, ':h')) | :startinsert
    endif
endfunction

" Open current file in GHCI
function! SendGHCI(file)
    let bnr = GhciBuffer()
    if bnr > 0
      :exe bnr . "wincmd w"
    else
      vnew | :call termopen("stack repl " . a:file) | :startinsert
    endif
endfunction

" Reload GHCi
function! ReloadGHCI()
    let bnr = GhciBuffer()
    let cur = bufwinnr("%")
    if bnr > 0
      :exe bnr . "wincmd w"
      :startinsert
      :call feedkeys("\<C-l>:r\<cr>\<Esc>\<C-\>\<C-n>:".cur."wincmd w\<cr>h")
    endif
endfunction

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" CORE
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" View core dump of the current file
function! SendCORE(file)
    let bnr = bufwinnr("ghc-core")
    if bnr > 0
      :exe bnr . "wincmd w"
    else
      let command = "stack exec -- ghc-core "
      \           . "--no-cast -- "
      \           . "-dsuppress-var-kinds "
      \           . "-dsuppress-type-applications "
      \           . "-dsuppress-uniques "
      \           . a:file
      vnew | :call termopen(command) | :startinsert
    endif
endfunction


""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Module header
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
let s:width = 80

function! HaskellModuleHeader(...)
    let name = GetModuleName()
    let note = 0 < a:0 ? a:1 : inputdialog("Note: ")
    let description = 1 < a:0 ? a:2 : inputdialog("Description: ")

    return  repeat('-', s:width) . "\n"
    \       . "-- |\n" 
    \       . "-- Module      : " . name . "\n"
    \       . "-- Note        : " . note . "\n"
    \       . "-- Copyright   : (C) " . strftime('%Y') . " Csongor Kiss\n"
    \       . "-- Maintainer  : Csongor Kiss <kiss.csongor.kiss@gmail.com>\n"
    \       . "-- License     : BSD3\n"
    \       . "-- Stability   : experimental\n"
    \       . "-- Portability : non-portable\n"
    \       . "--\n"
    \       . "-- " . description . "\n"
    \       . "--\n"
    \       . repeat('-', s:width) . "\n"
    \       . "\n"

endfunction

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Language pragmas
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" Prepopulate the GHC_LANGUAGES menu with a list of available extensions
let s:ghc_cached_language_pragmas
  \= sort(split(system('ghc --supported-languages'), '\n'))
for lp in s:ghc_cached_language_pragmas
  exe 'amenu GHC_LANGUAGES.' . lp . ' :call append(0, "{-# LANGUAGE ' . lp . ' #-}")<cr>'
endfor

let g:haskell_disable_TH = 1

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Utilities
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
function! GetSrcDir(path)
  return split(a:path, 'src/')[0]
endfunction

function! GetModuleName()
  let save_pos = getpos(".")
  call search('^module')
  normal "1y$
  let module = split(@1, ' \+')[1]
  call setpos('.', save_pos)
  return module
endfunction

function! FixLocation()
  let path    = expand('%:p')
  let srcdir  = GetSrcDir(path)
  let newpath =  srcdir . 'src/' . substitute(GetModuleName(), "\\.", "/", "g") . ".hs"
  let newpathz = newpath
  call mkdir(fnamemodify(newpathz, ':h'), 'p')
  call delete(path)
  exe 'sav' fnameescape(newpath)
endfunction

" Work out the module name from the directory structure
function! MkModuleName(path)
  return split(a:path, 'src/')[1]
endfunction

function! InsertModuleName(path)
  let mname  = MkModuleName(a:path)
  exe ':call append(0, "module ' . mname . ' where")'
endfunction

" Replace a qualified identifier with an unqualified one and add it as an explicit import
function! Unqualify()
  let save_pos = getpos(".")
  let word = map( split(expand('<cWORD>'), '\.')
             \  , {_, v -> substitute(v, "\\W", "", "g")}
             \  )
  if (len(word) == 1)
    echo "Not a qualified identifier"
    return
  endif

  let qualifier  = join(word[0:-2], '.')
  let clean_orig = join(word[0:-1], '.')
  let fun        = word[-1]

  call search('qualified \(.\)\+ as ' . qualifier, "")
  let module_name = split(getline("."), ' \+')[2]

  let qualified = []
  let unqualified = []

  let flags = ""
  while search(module_name, flags) != 0
      let cur_line = getline(".")
      let cur_pos  = line(".")
      if (match(cur_line, "qualified") >= 0)
        let qualified = [cur_pos, cur_line]
      else
        let unqualified = [cur_pos, cur_line]
      endif
      let flags = "W"
  endwhile

  if (len(unqualified) > 0)
    exe unqualified[0] . "d"
    let unqualified[0] = unqualified[0] - 1
  else
    call add(unqualified, qualified[0])
    call add(unqualified,
            \ substitute(
            \ substitute(
            \   qualified[1],
            \   "qualified", "         ", ""),
            \   'as \(.\+\)$', "()",""))
  endif

  let unqualified[1] = substitute(unqualified[1], ')$', ", " . fun . ")", "")
  let unqualified[1] = substitute(unqualified[1], '(, ', "(", "")

  call append(unqualified[0], unqualified[1])
  exe '%s/\<' .  clean_orig . '\>/' . fun . '/g'

  call setpos('.', save_pos)
endfunction
