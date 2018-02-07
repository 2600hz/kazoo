let SessionLoad = 1
if &cp | set nocp | endif
let s:so_save = &so | let s:siso_save = &siso | set so=0 siso=0
let v:this_session=expand("<sfile>:p")
silent only
cd ~/kazoo
if expand('%') == '' && !&modified && line('$') <= 1 && getline(1) == ''
  let s:wipebuf = bufnr('%')
endif
set shortmess=aoO
badd +42 core/kazoo_attachments/src/gen_attachment.erl
badd +1 core/kazoo_data/src/kzs_attachments.erl
badd +1 ~/.vimrc.local
badd +1 core/kazoo_attachments/src/kz_att_azure.erl
badd +1 core/kazoo_attachments/src/kz_att_dropbox.erl
badd +1 core/kazoo_attachments/src/kz_att_ftp.erl
badd +1 core/kazoo_attachments/src/kz_att_google_drive.erl
badd +1 core/kazoo_attachments/src/kz_att_google_storage.erl
badd +1 core/kazoo_attachments/src/kz_att_http.erl
badd +1 core/kazoo_attachments/src/kz_att_link.erl
badd +1 core/kazoo_attachments/src/kz_att_onedrive.erl
badd +1 core/kazoo_attachments/src/kz_att_s3.erl
badd +1 core/kazoo_attachments/src/kz_att_util.erl
badd +1 core/kazoo_data/test/kzs_attachments_test.erl
badd +1 applications/crossbar/src/modules/cb_storage.erl
badd +0 core/kazoo_attachments/src/gen_attachment.erl
args core/kazoo_attachments/src/gen_attachment.erl core/kazoo_data/src/kzs_attachments.erl
edit core/kazoo_attachments/src/gen_attachment.erl
set splitbelow splitright
wincmd t
set winheight=1 winwidth=1
argglobal
setlocal fdm=manual
setlocal fde=0
setlocal fmr={{{,}}}
setlocal fdi=#
setlocal fdl=0
setlocal fml=1
setlocal fdn=20
setlocal fen
silent! normal! zE
let s:l = 42 - ((17 * winheight(0) + 11) / 23)
if s:l < 1 | let s:l = 1 | endif
exe s:l
normal! zt
42
normal! 015|
lcd ~/kazoo/core/kazoo_attachments/src
tabedit ~/kazoo/core/kazoo_attachments/src/kz_att_azure.erl
set splitbelow splitright
wincmd t
set winheight=1 winwidth=1
argglobal
edit ~/kazoo/core/kazoo_attachments/src/kz_att_azure.erl
setlocal fdm=manual
setlocal fde=0
setlocal fmr={{{,}}}
setlocal fdi=#
setlocal fdl=0
setlocal fml=1
setlocal fdn=20
setlocal fen
silent! normal! zE
let s:l = 23 - ((17 * winheight(0) + 11) / 23)
if s:l < 1 | let s:l = 1 | endif
exe s:l
normal! zt
23
normal! 022|
lcd ~/kazoo/core/kazoo_attachments/src
tabedit ~/kazoo/core/kazoo_attachments/src/kz_att_dropbox.erl
set splitbelow splitright
wincmd t
set winheight=1 winwidth=1
argglobal
edit ~/kazoo/core/kazoo_attachments/src/kz_att_dropbox.erl
setlocal fdm=manual
setlocal fde=0
setlocal fmr={{{,}}}
setlocal fdi=#
setlocal fdl=0
setlocal fml=1
setlocal fdn=20
setlocal fen
silent! normal! zE
let s:l = 113 - ((22 * winheight(0) + 11) / 23)
if s:l < 1 | let s:l = 1 | endif
exe s:l
normal! zt
113
normal! 09|
lcd ~/kazoo/core/kazoo_attachments/src
tabedit ~/kazoo/core/kazoo_attachments/src/kz_att_ftp.erl
set splitbelow splitright
wincmd t
set winheight=1 winwidth=1
argglobal
edit ~/kazoo/core/kazoo_attachments/src/kz_att_ftp.erl
setlocal fdm=manual
setlocal fde=0
setlocal fmr={{{,}}}
setlocal fdi=#
setlocal fdl=0
setlocal fml=1
setlocal fdn=20
setlocal fen
silent! normal! zE
let s:l = 59 - ((16 * winheight(0) + 11) / 23)
if s:l < 1 | let s:l = 1 | endif
exe s:l
normal! zt
59
normal! 052|
lcd ~/kazoo/core/kazoo_attachments/src
tabedit ~/kazoo/core/kazoo_attachments/src/kz_att_google_drive.erl
set splitbelow splitright
wincmd t
set winheight=1 winwidth=1
argglobal
edit ~/kazoo/core/kazoo_attachments/src/kz_att_google_drive.erl
setlocal fdm=manual
setlocal fde=0
setlocal fmr={{{,}}}
setlocal fdi=#
setlocal fdl=0
setlocal fml=1
setlocal fdn=20
setlocal fen
silent! normal! zE
let s:l = 47 - ((13 * winheight(0) + 11) / 23)
if s:l < 1 | let s:l = 1 | endif
exe s:l
normal! zt
47
normal! 024|
lcd ~/kazoo/core/kazoo_attachments/src
tabedit ~/kazoo/core/kazoo_attachments/src/kz_att_google_storage.erl
set splitbelow splitright
wincmd t
set winheight=1 winwidth=1
argglobal
edit ~/kazoo/core/kazoo_attachments/src/kz_att_google_storage.erl
setlocal fdm=manual
setlocal fde=0
setlocal fmr={{{,}}}
setlocal fdi=#
setlocal fdl=0
setlocal fml=1
setlocal fdn=20
setlocal fen
silent! normal! zE
let s:l = 115 - ((11 * winheight(0) + 11) / 23)
if s:l < 1 | let s:l = 1 | endif
exe s:l
normal! zt
115
normal! 044|
lcd ~/kazoo/core/kazoo_attachments/src
tabedit ~/kazoo/core/kazoo_attachments/src/kz_att_http.erl
set splitbelow splitright
wincmd t
set winheight=1 winwidth=1
argglobal
edit ~/kazoo/core/kazoo_attachments/src/kz_att_http.erl
setlocal fdm=manual
setlocal fde=0
setlocal fmr={{{,}}}
setlocal fdi=#
setlocal fdl=0
setlocal fml=1
setlocal fdn=20
setlocal fen
silent! normal! zE
let s:l = 125 - ((11 * winheight(0) + 11) / 23)
if s:l < 1 | let s:l = 1 | endif
exe s:l
normal! zt
125
normal! 019|
lcd ~/kazoo/core/kazoo_attachments/src
tabedit ~/kazoo/core/kazoo_attachments/src/kz_att_link.erl
set splitbelow splitright
wincmd t
set winheight=1 winwidth=1
argglobal
edit ~/kazoo/core/kazoo_attachments/src/kz_att_link.erl
setlocal fdm=manual
setlocal fde=0
setlocal fmr={{{,}}}
setlocal fdi=#
setlocal fdl=0
setlocal fml=1
setlocal fdn=20
setlocal fen
silent! normal! zE
let s:l = 89 - ((21 * winheight(0) + 11) / 23)
if s:l < 1 | let s:l = 1 | endif
exe s:l
normal! zt
89
normal! 027|
lcd ~/kazoo/core/kazoo_attachments/src
tabedit ~/kazoo/core/kazoo_attachments/src/kz_att_onedrive.erl
set splitbelow splitright
wincmd t
set winheight=1 winwidth=1
argglobal
edit ~/kazoo/core/kazoo_attachments/src/kz_att_onedrive.erl
setlocal fdm=manual
setlocal fde=0
setlocal fmr={{{,}}}
setlocal fdi=#
setlocal fdl=0
setlocal fml=1
setlocal fdn=20
setlocal fen
silent! normal! zE
let s:l = 144 - ((19 * winheight(0) + 11) / 23)
if s:l < 1 | let s:l = 1 | endif
exe s:l
normal! zt
144
normal! 049|
lcd ~/kazoo/core/kazoo_attachments/src
tabedit ~/kazoo/core/kazoo_attachments/src/kz_att_s3.erl
set splitbelow splitright
wincmd t
set winheight=1 winwidth=1
argglobal
edit ~/kazoo/core/kazoo_attachments/src/kz_att_s3.erl
setlocal fdm=manual
setlocal fde=0
setlocal fmr={{{,}}}
setlocal fdi=#
setlocal fdl=0
setlocal fml=1
setlocal fdn=20
setlocal fen
silent! normal! zE
let s:l = 234 - ((22 * winheight(0) + 11) / 23)
if s:l < 1 | let s:l = 1 | endif
exe s:l
normal! zt
234
normal! 019|
lcd ~/kazoo/core/kazoo_attachments/src
tabedit ~/kazoo/core/kazoo_attachments/src/kz_att_util.erl
set splitbelow splitright
wincmd t
set winheight=1 winwidth=1
argglobal
edit ~/kazoo/core/kazoo_attachments/src/kz_att_util.erl
setlocal fdm=manual
setlocal fde=0
setlocal fmr={{{,}}}
setlocal fdi=#
setlocal fdl=0
setlocal fml=1
setlocal fdn=20
setlocal fen
silent! normal! zE
let s:l = 180 - ((22 * winheight(0) + 11) / 23)
if s:l < 1 | let s:l = 1 | endif
exe s:l
normal! zt
180
normal! 019|
lcd ~/kazoo/core/kazoo_attachments/src
tabedit ~/kazoo/core/kazoo_data/src/kzs_attachments.erl
set splitbelow splitright
wincmd t
set winheight=1 winwidth=1
argglobal
2argu
setlocal fdm=manual
setlocal fde=0
setlocal fmr={{{,}}}
setlocal fdi=#
setlocal fdl=0
setlocal fml=1
setlocal fdn=20
setlocal fen
silent! normal! zE
let s:l = 192 - ((5 * winheight(0) + 11) / 23)
if s:l < 1 | let s:l = 1 | endif
exe s:l
normal! zt
192
normal! 0
lcd ~/kazoo/core/kazoo_data/src
tabedit ~/kazoo/core/kazoo_data/test/kzs_attachments_test.erl
set splitbelow splitright
wincmd t
set winheight=1 winwidth=1
argglobal
edit ~/kazoo/core/kazoo_data/test/kzs_attachments_test.erl
setlocal fdm=manual
setlocal fde=0
setlocal fmr={{{,}}}
setlocal fdi=#
setlocal fdl=0
setlocal fml=1
setlocal fdn=20
setlocal fen
silent! normal! zE
let s:l = 1 - ((0 * winheight(0) + 11) / 23)
if s:l < 1 | let s:l = 1 | endif
exe s:l
normal! zt
1
normal! 0
lcd ~/kazoo/core/kazoo_data/test
tabedit ~/kazoo/applications/crossbar/src/modules/cb_storage.erl
set splitbelow splitright
wincmd t
set winheight=1 winwidth=1
argglobal
edit ~/kazoo/applications/crossbar/src/modules/cb_storage.erl
setlocal fdm=manual
setlocal fde=0
setlocal fmr={{{,}}}
setlocal fdi=#
setlocal fdl=0
setlocal fml=1
setlocal fdn=20
setlocal fen
silent! normal! zE
let s:l = 508 - ((22 * winheight(0) + 11) / 23)
if s:l < 1 | let s:l = 1 | endif
exe s:l
normal! zt
508
normal! 032|
lcd ~/kazoo/applications/crossbar/src/modules
tabnext 1
if exists('s:wipebuf')
  silent exe 'bwipe ' . s:wipebuf
endif
unlet! s:wipebuf
set winheight=1 winwidth=20 shortmess=filmnrxoOtT
let s:sx = expand("<sfile>:p:r")."x.vim"
if file_readable(s:sx)
  exe "source " . fnameescape(s:sx)
endif
let &so = s:so_save | let &siso = s:siso_save
doautoall SessionLoadPost
unlet SessionLoad
" vim: set ft=vim :
