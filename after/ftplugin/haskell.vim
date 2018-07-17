" Note:
"   This 'plugin' is not portable at all
"   The repl bindings only work with Neovim
"   fzf is required for adding language pragmas

nnoremap <Plug>Edit-stack.yaml :exe "pedit" . <SID>find_conf(expand('%:p'), 'yaml')<cr>
nnoremap <Plug>Edit-cabal-file :exe "pedit" . <SID>find_conf(expand('%:p'), 'cabal')<cr>
nnoremap <Plug>Jump-to-imports :call Haskell_jump_to_imports()<cr>

nnoremap <Plug>Open-REPL :call <SID>open_repl()<cr>
nnoremap <Plug>View-core :call <SID>send_core(@%)<cr>
nnoremap <Plug>Type-under-cursor :call <SID>type_at()<cr>
nnoremap <Plug>Identifier-information :echo WhichModule(<SID>ident_under_cursor())<cr>
nnoremap <Plug>Find-notes :call <SID>ghc_notes("")<cr>
nnoremap <Plug>Find-note-under-cursor :call <SID>ghc_notes(<SID>get_inside_text_object("["))<cr>
nnoremap <Plug>:i-cword :call <SID>info(<SID>ident_under_cursor())<cr>
nnoremap <Plug>:k-cword <SID>kind(<SID>paren_operator(<SID>ident_under_cursor()), 0)<cr>
nnoremap <Plug>:rep-cword <SID>kind("Rep " . <SID>paren_operator(<SID>ident_under_cursor()), 1)<cr>
nnoremap <Plug>:t-cword :call <SID>type(<SID>paren_operator(<SID>ident_under_cursor()))<cr>

" Config files {{{
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" Walk up the directory structure and find the first file with extension 'type'
function! s:find_conf(path, type)
  let found  = split(globpath(a:path, '*.' . a:type), '\n')
  let dir    = fnamemodify(a:path, ':h')
  if (dir != '/')
    if (len(found) == 0)
      return s:find_conf(dir, a:type)
    else
      return found[0]
    endif
  else
    echo "No " . a:type . " file found"
  endif
endfunction

"}}}
" GHCi {{{
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" function! GhciBuffer()
"   return max([bufwinnr("repl"), bufwinnr("ghci"), bufwinnr("term")])
" endfunction

function! s:open_repl()
    let repls = ['cabal new-repl', 'stack repl', 'ghci', 'my-ghci']
    :call fzf#run({
    \ 'source': repls,
    \ 'sink': {r -> VimuxRunCommandInDir(r, 1)},
    \ 'options' : '--reverse --margin 15%,0',
    \ 'down': '25%'})

endfunction

command! -nargs=1 Info
  \ call s:info(<q-args>)

function! Eval(expr)
  :call s:send_right(a:expr)
endfunction

function! s:info(query)
    let cmd = ':info'
    :call s:send_right(cmd . ' ' . a:query)
endfunction

command! -bang -nargs=1 Kind
  \ call s:kind(<q-args>, <bang>0)

function! s:kind(query, bang)
    let cmd = a:bang ? ':kind\!' : ':kind'
    :call s:send_right(cmd . ' ' . a:query)
endfunction

command! -nargs=1 Type
  \ call s:type(<q-args>)

function! s:type(query)
    let cmd = ':type'
    :call s:send_right(cmd . ' ' . a:query)
endfunction

function! s:type_at()
    let query = s:ident_under_cursor()
    let module_name = GetModuleName()
    let l = line('.')
    let c = col('.')
    :call s:send_right(':type-at ' . module_name . ' ' . l . ' ' . c . ' ' . l . ' ' . c . ' ' . query)
endfunction

" Reload GHCi
function! ReloadGHCItmux()
    :call s:send_right(":r")
endfunction

function! s:is_tmux()
  return len(systemlist("echo $TMUX")) > 0
endfunction

function! ReloadGHCI()
  if (s:is_tmux())
    call ReloadGHCItmux()
  else
    call ReloadGHCInvim()
  endif
endfunction

function! ReloadGHCIAndRun()
    :call s:send_right(":r")
    :call VimuxSendKeys("up")
    :call s:send_right("up")
endfunction

function! SendGhci(text)
    let cur = winnr()
    let bnr = GhciBuffer()
    if bnr > 0
      :exe bnr . "wincmd w"
      :startinsert
      :call nvim_input("\<C-l>".a:text."\<cr>\<Esc>\<C-\>\<C-n>:".cur."wincmd w\<cr>h")
    endif
endfunction

function! ReloadGHCInvim()
    call SendGhci(":r")
endfunction

function! AddOptionGHCI()
  :call fzf#run({
  \ 'source': 'ghc --show-options',
  \ 'sink': {opt -> s:send_right(":set " . opt)},
  \ 'down': '20%'})
endfunction


" Reload GHCi
function! LoadGhci()
    :call s:send_right(":load " . expand('%:p'))
endfunction

command! -nargs=0 Spec
  \ call s:tex_spec()

function! s:tex_spec()
  let ghci_content = GetGHCIContent()
  let ghci_content = insert(ghci_content, '\begin{spec}')
  let spec = add(ghci_content, '\end{spec}')
  call append(line('.'), spec)
endfunction

command! -nargs=0 ReSpec
  \ call s:re_spec()

function! s:re_spec()
  let line = getline('.')
  if (line =~ '^>>>> ghci')
    let line = substitute(line, "^>>>> ghci ", '', '')
    call s:send_right(line)
  else
    echo "Not a REPL command"
  endif
endfunction

function! GetGHCIContent()
    :silent !tmux capture-pane -t right
    let captured = split(system("tmux show-buffer"), '\n')
    let content = []
    for l in captured
      if (l == '')
        break
      endif
      let content = add(content, l)
    endfor
    return content[0:-2] " remove prompt
endfunction

"}}}
" CORE {{{
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" View core dump of the current file
function! s:send_core(file)
    let bnr = bufwinnr("ghc-core")
    if bnr > 0
      :exe bnr . "wincmd w"
    else
      let command = "stack exec -- ghc-core "
      \           . "--no-cast -- "
      \           . "-dsuppress-var-kinds "
      \           . "-dsuppress-type-applications "
      \           . "-dsuppress-uniques "
      \           . "-fspecialise-aggressively "
      \           . "-fstatic-argument-transformation "
      \           . "-fforce-recomp "
      \           . a:file
      vnew | :call termopen(command) | :startinsert
    endif
endfunction

"}}}
" Language pragmas {{{
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

function! Haskell_add_language_pragma()
  let line = max([0, search('^{-# LANGUAGE', 'n') - 1])
  :call fzf#run({
  \ 'source': 'ghc --supported-languages',
  \ 'sink': {lp -> append(line, "{-# LANGUAGE " . lp . " #-}")},
  \ 'options': '--multi --ansi --reverse --prompt "LANGUAGE> "',
  \ 'down': '25%'})
endfunction

"}}}
" Command line flags {{{
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

function! Haskell_add_compiler_flag()
  :call fzf#run({
  \ 'source': 'ghc --show-options',
  \ 'sink': {opt -> append(0, "{-# OPTIONS_GHC " . opt . " #-}")},
  \ 'down': '20%'})
endfunction

"}}}
" Imports {{{
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

command! AddPackage
  \ call Haskell_add_package()

function! Haskell_add_package()
  " cabal list --simple-output > ~/.cabal-list.txt
  :call fzf#run({
  \ 'source': 'cat ~/.cabal-list.txt',
  \ 'sink': {lp -> append(line('.'), '                     , ' . join(split(lp, ' '), ' == '))},
  \ 'options': '--multi --ansi --reverse --prompt "Package> "',
  \ 'down': '25%'})
endfunction

function! Haskell_jump_to_imports()
    let line = search('^import', 'n')
    if (line > 0)
        :execute "normal " . line . "G0|"
    endif
endfunction

" Find existing import statements in project and pick one
function! Haskell_import_module()
  let imported_modules = map(s:imported_modules(), {k, v -> v.module_name})
  let module_lines = split(system('ag --nocolor --nogroup --nofilename "^import"'), '\n')
  let modules = []

  for m in module_lines
    if (len(m) == 0)
      continue
    endif
    let parsed = s:parse_import(m)
    if (index(imported_modules, parsed.module_name) == -1)
      call add(modules, parsed.module_name)
    endif
  endfor
  let modules = uniq(sort(modules))

  let line = search('^import', 'n')
  if (0 < a:0)
    :call append(line, 'import ' . a:1)
  else
    call fzf#run({
    \ 'source': modules,
    \ 'sink': {i -> append(line, 'import ' . i)},
    \ 'options': '--multi --reverse --prompt "ImportModule> "',
    \ 'down': '40%'})
  endif

endfunction

" function! s:import_module_sink(lines)
"   if a:lines[0] == "ctrl-q"
"   endif
" endfunction

" Replace a qualified identifier with an unqualified one and add it as an explicit import
function! Haskell_unqualify()
  let save_pos = getpos(".")
  let parts = s:ident_parts(expand('<cWORD>'))
  if (!parts.qualifier)
    return
  endif
  let module = QualModule(parts.qualifier)
  if (module == "")
    return
  endif
  call AddImport(module, parts.unqualified)
  exe '%s/\<' .  parts.qualified . '\>/' . fun . '/g'
  call setpos('.', save_pos)
endfunction

" Hide/show qualifiers (requires conceallevel > 0)
function! ToggleConcealQualified()
  if (matchdelete(65) == -1)
    call matchadd('Conceal', '\(qualified\|import\|as\)\@<![^a-zA-Z0-9\.]\zs\([A-Z]\w*\.\)\+', 100, 65)
  endif
endfunction

"}}}
" Hoogle {{{
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
function! Hoogle(...)
  let line = line(".")
  let type = 0 < a:0 ? a:1 : inputdialog("Hoogle: ")
  :call fzf#run({
  \ 'source': 'hoogle search "' . type . '" --count=100',
  \ 'sink': {fun -> s:hoogle_sink(line, fun)},
  \ 'down': '40%'})
endfunction

command! -nargs=* Hoogle
  \ call Hoogle(<q-args>)

function! s:hoogle_sink(line, fun)
  if (match(a:fun, "::") >= 0)
    let [modulename, type] = split(a:fun, "::")
    let [module, name] = split(modulename, " ")
    call AddImport(module, name)
  elseif (match(a:fun, "^module" >= 0))
    call Haskell_import_module(split(substitute(a:fun, "^module ", "", "g"))[0])
  endif
endfunction

function! ImportedModules()
  return s:imported_modules()
endfunction

function! WhichModule(identifier)
    let imported_modules = s:imported_modules()

    let from_tags = s:tag_modules(a:identifier)
    "let from_hoogle = s:hoogle_modules(a:identifier)

    for im in imported_modules
        if (index(from_tags, im.module_name) >= 0)
            return {'imported': im, 'name': a:identifier}
        endif
    endfor

    return {'found_in': from_tags, 'name': a:identifier}
endfunction

function! s:hoogle_modules(identifier)
    let modules = []
    let hoogle_matches = systemlist('hoogle search "'.a:identifier.'" --count=100')
    for m in hoogle_matches
      if (match(m, "^--") >= 0)
        break
      endif
      let module = split(substitute(m, "^module ", "", "g"))[0]
      let modules = add(modules, module)
    endfor
    return uniq(modules)
endfunction

function! s:tag_modules(identifier)
    let modules = []
    let tag_matches = taglist(a:identifier)
    for m in tag_matches
      if (m.name == a:identifier)
        let modules = add(modules, (s:parse_file_path(m.filename)).module_name)
      endif
    endfor
    return uniq(modules)
endfunction

"}}}
" Modules {{{
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
let s:width = 80
function! HaskellModuleHeader(...)
    let name = GetModuleName()
    let description = 0 < a:0 ? a:1 : inputdialog("Description: ")
    return  repeat('-', s:width) . "\n"
    \       . "-- |\n" 
    \       . "-- Module      : " . name . "\n"
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

function! GetModuleName() abort
    let l:regex = '^\C>\=\s*module\s\+\zs[A-Za-z0-9.]\+'
    for l:lineno in range(1, line('$'))
        let l:line = getline(l:lineno)
        let l:pos = match(l:line, l:regex)
        if l:pos != -1
            let l:synname = synIDattr(synID(l:lineno, l:pos+1, 0), 'name')
            if l:synname !~# 'Comment'
                return matchstr(l:line, l:regex)
            endif
        endif
        let l:lineno += 1
    endfor
    return 'Main'
endfunction

function! RenameModule(newmodule)
  let module = GetModuleName()
  call s:replace_all(module, a:newmodule)
  call FixLocation()
endfunction

function! s:imported_modules()
    let lines = getline(0, '$')
    let modules = []

    for line in lines
        if (match(line, "^import") >= 0)
            let modules = add(modules, s:parse_import(line))
        endif
    endfor

    return modules
endfunction

function! s:parse_import(line)
  let name_parts = split(substitute(a:line, "^import ", "", "g"))
  let curr_module = {}
  if (name_parts[0] == 'qualified')
    let curr_module.qualifier = name_parts[3]
    let curr_module.module_name = name_parts[1]
    let name_parts = name_parts[4:-1]
  else
    let curr_module.module_name = name_parts[0]
    let name_parts = name_parts[1:-1]
  endif

  if (len(name_parts) > 0 && name_parts[0] == 'as')
    let curr_module.qualifier = name_parts[1]
    let name_parts = name_parts[2:-1]
  endif

  " TODO: parse imported things
  if (len(name_parts) > 0)
    if (name_parts[0] !~ 'hiding')
      let imports = split(join(name_parts)[1:-2], ',')
      let curr_module.imports = imports
    else
      let hiding = split(join(name_parts)[1:-2], ',')
      let curr_module.hiding = hiding
    endif
  endif

  return curr_module
endfunction

function! MakeModuleName(path)
  return s:parse_file_path(a:path)
endfunction

" Work out the module name from the directory structure
function! s:parse_file_path(path)
  let absolute = a:path[0] == '/'
  let parsed = {}
  let parts = reverse(split(fnamemodify(a:path, ':r'), '/'))

  let new_parts = []
  for part in parts
    if (s:begins_with_uppercase(part))
      let new_parts = add(new_parts, part)
      call remove(parts, 0)
    else
      break
    endif
  endfor

  let base_dir = join(reverse(parts), "/")
  if absolute
    let base_dir = '/' . base_dir
  endif

  let parsed.module_name = join(reverse(new_parts), ".")
  let parsed.base_dir = base_dir
  return parsed
endfunction

function! s:insert_module_name()
  let mname = (s:parse_file_path(expand('%:p'))).module_name
  exe ':call append(0, "module ' . mname . ' where")'
endfunction

function! s:import_statements(module)
  let save_pos = getpos(".")
  let qualified = []
  let unqualified = []
  let flags = ""
  while search('import\( \)\+\(qualified\)\?\( \)*' . a:module . '[^\.]', flags) != 0
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

function! AddImport(...)
  let module = 0 < a:0 ? a:1 : inputdialog("Module name to import from: ")
  let fun = 1 < a:0 ? a:2 : inputdialog("Function name: ")
  let [qualified, unqualified] = s:import_statements(module)
  if (len(unqualified) > 0)
    " unqalified import exists, use it
    exe unqualified[0] . "d"
    let unqualified[0] = unqualified[0] - 1
  elseif (len(qualified) > 0)
    " qualified import exists, mirror it
    call add(unqualified, qualified[0])
    call add(unqualified,
            \ substitute(
            \ substitute(
            \   qualified[1],
            \   "qualified", "         ", ""),
            \   'as \(.\+\)$', "()",""))
  else
    " create new unqalified import
    let save_pos = getpos(".")
    let line = search('^import', 'n')
    call add(unqualified, line)
    call add(unqualified, "import           " . module . " ()")
    call setpos('.', save_pos)
  endif
  let unqualified[1] = substitute(unqualified[1], ')$', ", " . fun . ")", "")
  let unqualified[1] = substitute(unqualified[1], '(, ', "(", "")
  call append(unqualified[0], unqualified[1])
endfunction

"}}}
" Working on GHC {{{
function! s:is_ghc()
  let path = s:git_root()
  if (match(path, "ghc") >= 0)
    return 1
  else
    return 0
  endif
endfunction

function! s:ghc_notes(which)
  if (!s:is_ghc())
    return
  endif
  call fzf#vim#grep('git grep --line-number "^\\({\\?--\\? \\?\\)\\?Note.\\+\\['.a:which.'"', 0,
    \ { 'dir': s:git_root(), 'options': '-1 -0' }, 0)
endfunction

" }}}
" Utilities {{{
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
function! s:git_root()
  return systemlist('git rev-parse --show-toplevel')[0]
endfunction

function! s:send_right(cmd)
    let cmd = a:cmd
    "let cmd = substitute(cmd, "'\\([^']\\+\\)'", "`'\\1'`", "g")
    "let cmd = substitute(cmd, "\"\\([^\"]\\+\\)\"", "'\"\\1\"'", "g")
    "let cmd = substitute(cmd, "`", "\"", "g")
    "let cmd = substitute(cmd, " ", "\" \"", "g")
    ":silent exe('!tmux send-keys -t right C-l "'.cmd.'" Enter')
    ":call VimuxSendKeys("C-c")
    :call VimuxSendKeys("C-l")
    :call VimuxRunCommand(cmd)
endfunction

function! GhciBuffer()
  return max([bufwinnr("repl"), bufwinnr("ghci"), bufwinnr("interactive")])
endfunction

command! -nargs=0 NewModule
  \ silent! call s:new_module()

function! s:new_module(...)
  let name = 0 < a:0 ? a:1 : inputdialog("Module name: ")
  let description = 1 < a:0 ? a:2 : inputdialog("Description: ")

  let path    = expand('%:p')
  let srcdir  = (s:parse_file_path(path)).base_dir

  let newpath =  srcdir . '/' . substitute(name, "\\.", "/", "g") . ".hs"
  let newpathz = newpath
  call mkdir(fnamemodify(newpathz, ':h'), 'p')
  :exe 'edit ' . fnameescape(newpath)
  call s:insert_module_name()
  echo join(split(HaskellModuleHeader(description), '\n'), " ")
  "exe ':call append(0, "'.split(HaskellModuleHeader(description), '\n').'")'
endfunction

function! FixLocation()
  let path    = expand('%:p')
  let srcdir  = (s:parse_file_path(path)).base_dir
  let newpath =  srcdir . '/' . substitute(GetModuleName(), "\\.", "/", "g") . ".hs"
  let newpathz = newpath
  call mkdir(fnamemodify(newpathz, ':h'), 'p')
  call delete(path)
  exe 'sav' fnameescape(newpath)
endfunction

function! s:is_operator(identifier)
  return (a:identifier =~ "[!@#$%^&*<>:]")
endfunction

function! s:paren_operator(identifier)
  if (s:is_operator(a:identifier))
    return '(' . a:identifier . ')'
  else
    return a:identifier
  endif
endfunction

function! s:replace_all(word, to)
  exe "Glgrep! -w " . shellescape(a:word)
  exe "ldo %s/\\<" . a:word . "\\>/" . a:to . "/gI \| update"
endfunction


function! IdentParts(identifier)
    return s:ident_parts(a:identifier)
endfunction

function! s:ident_parts(word)
  let parts = map( split(a:word, '\.')
              \  , {_, v -> substitute(v, "[()\\[\\]`]", "", "g")}
              \  )
  if (len(parts) == 1)
    return {'unqualified': parts[0]}
  endif
  let qualifier    = join(parts[0:-2], '.')
  let qualified    = join(parts[0:-1], '.')
  let unqualified  = parts[-1]
  return {'qualifier': qualifier, 'unqualified': unqualified, 'qualified': qualified}
endfunction

function! s:ident_under_cursor()
  let parts = s:ident_parts(expand('<cWORD>'))
  if (has_key(parts, 'qualified'))
    return parts.qualified
  else
    return parts.unqualified
  endif
endfunction

function! s:begins_with_uppercase(str)
  return (a:str =~# '^[A-Z]')
endfunction

function! QualModule(qualifier)
  let imported_modules = s:imported_modules()
  for m in imported_modules
    if (has_key(m, 'qualifier') && m.qualifier == a:qualifier)
      return m
    endif
  endfor
  return -1
endfunction

function! Ghci_complete(findstart, base)
    if a:findstart
        " locate the start of the word
        let line = getline('.')
        let start = col('.') - 1
        while start > 0 && line[start-1] !~ '[ ()\[\]]'
            let start -= 1
        endwhile
        return start
    else
        call s:send_right(':complete repl 10 "'.a:base.'"')
        let ghci_content = GetGHCIContent()[2:-1]
        " find classes matching "a:base"
        let res = []
        for m in ghci_content
          call add(res, m[1:-2])
        endfor
        return res
    endif
endfun

setlocal completefunc=Ghci_complete

function! s:get_inside_text_object(object)
  let l:save_clipboard = &clipboard
  set clipboard= " Avoid clobbering the selection and clipboard registers.
  let l:save_reg = getreg('"')
  let l:save_regmode = getregtype('"')
  execute("normal! yi" . a:object)
  let l:text = @@ " Your text object contents are here.
  call setreg('"', l:save_reg, l:save_regmode)
  let &clipboard = l:save_clipboard
  return l:text
endfunction

"}}}

let g:haskell_disable_TH = 1
let g:haskell_enable_typeroles = 1
