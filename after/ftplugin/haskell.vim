" Note:
"   Some of the functions assume that the source files are under 'src/'
"   The repl bindings only work with Neovim
"   fzf is required for adding language pragmas

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Mappings
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
nnoremap <silent> --h "=HaskellModuleHeader()<CR>:0put =<cr>
nnoremap <silent> <leader>li O<esc>80i-<esc>

nnoremap <silent> <leader>r :w<cr> :call ReloadGHCI()<cr>
nnoremap <silent> <leader>sb :call SendGHCI(@%)<cr>
nnoremap <silent> <leader>sc :call SendCORE(@%)<cr>
nnoremap <silent> <leader>st :call SendGHCITarget(expand('%:p'))<cr>

nnoremap <silent> <leader>ec :exe "edit" . FindConf(expand('%:p'), 'cabal')<cr>
nnoremap <silent> <leader>ey :exe "edit" .  FindConf(expand('%:p'), 'yaml')<cr>
nnoremap <silent> <leader>mi :call InsertModuleName(expand('%:p:r'))<cr>
noremap <leader>la :call AddLanguagePragma()<cr>
noremap <leader>ia :call AddImport()<cr>
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

function! AddLanguagePragma()
  :call fzf#run({
  \ 'source': 'ghc --supported-languages',
  \ 'sink': {lp -> append(0, "{-# LANGUAGE " . lp . " #-}")},
  \ 'right': '40%'})
endfunction

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Imports
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" Find existing import statements in project and pick one
function! AddImport()
  normal G
  ?^import
  let line = line(".")
  call fzf#run({
  \ 'source': 'ag --nocolor --nogroup --nofilename "import qualified (\S+)(\s)+as" | perl -lpe "s/ +/ /g" | grep -v "^--" | sort | uniq',
  \ 'sink': {i -> append(line, i)},
  \ 'right': '40%'})
endfunction

" Replace a qualified identifier with an unqualified one and add it as an explicit import
function! Unqualify()
  let save_pos = getpos(".")
  let parts = IdentParts(expand('<cWORD>'))
  if (parts == [])
    return
  endif
  let [qualifier, fun, clean_orig] = parts
  let module = QualModule(qualifier)
  if (module == "")
    return
  endif
  let [qualified, unqualified] = ImportStatements(module)

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

function! IdentParts(word)
  let parts = map( split(a:word, '\.')
              \  , {_, v -> substitute(v, "\\W", "", "g")}
              \  )
  if (len(parts) == 1)
    echom "Not a qualified identifier"
    return []
  endif

  let qualifier  = join(parts[0:-2], '.')
  let clean      = join(parts[0:-1], '.')
  let ident      = parts[-1]

  return [qualifier, ident, clean]
endfunction

function! QualModule(qualifier)
  let save_pos = getpos(".")
  if (search('qualified \(.\)\+ as ' . a:qualifier, "") == 0)
    echom ("No module imported as " . a:qualifier)
    return ""
  endif
  let module = split(getline("."), ' \+')[2]
  call setpos(".", save_pos)
  return module
endfunction

function! ImportStatements(module)
  let save_pos = getpos(".")
  let qualified = []
  let unqualified = []
  let flags = ""
  while search('import\( \)\+\(qualified\)\?\( \)*' . a:module, flags) != 0
      let cur_line = getline(".")
      let cur_pos  = line(".")
      if (match(cur_line, "qualified") >= 0)
        let qualified = [cur_pos, cur_line]
      else
        let unqualified = [cur_pos, cur_line]
      endif
      let flags = "W"
  endwhile
  call setpos(".", save_pos)
  return [qualified, unqualified]
endfunction

let g:haskell_disable_TH = 1
