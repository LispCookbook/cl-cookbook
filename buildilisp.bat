@echo off

set EMACSDIR=c:\emacs
set ILISPDIR=c:\home\site\ilisp
set EMACS=%EMACSDIR%\bin\emacs.exe
set HYPERSPEC=hyperspec-naggum.el

cd %ILISPDIR%
cd extra
copy %HYPERSPEC% hyperspec.el
cd ..
%EMACS% -batch -l ilisp-mak.el

copy /B ilisp-def.elc+ilisp-sym.elc+ilisp-inp.elc+ilisp-ind.elc+ilisp-prc.elc+ilisp-val.elc+ilisp-out.elc ilisp-all.elc
copy /B ilisp-all.elc+ilisp-mov.elc+ilisp-key.elc+ilisp-prn.elc+ilisp-low.elc+ilisp-doc.elc+ilisp-ext.elc+ilisp-mod.elc ilisp-all.elc
copy /B ilisp-all.elc+ilisp-dia.elc+ilisp-cmt.elc+ilisp-rng.elc+ilisp-hnd.elc+ilisp-utl.elc+ilisp-cmp.elc+ilisp-kil.elc ilisp-all.elc
copy /B ilisp-all.elc+ilisp-snd.elc+ilisp-xfr.elc+ilisp-hi.elc+ilisp-aut.elc+ilisp-cl.elc+ilisp-cmu.elc+ilisp-sbcl.elc ilisp-all.elc
copy /B ilisp-all.elc+ilisp-cl-easy-menu.elc+ilisp-acl.elc+ilisp-kcl.elc+ilisp-luc.elc+ilisp-sch.elc+ilisp-hlw.elc ilisp-all.elc
copy /B ilisp-all.elc+ilisp-xls.elc+ilisp-chs.elc+ilisp-openmcl.elc ilisp-all.elc

del ilisp-def.elc ilisp-sym.elc ilisp-inp.elc ilisp-ind.elc ilisp-prc.elc ilisp-val.elc ilisp-out.elc ilisp-mov.elc 
del ilisp-key.elc ilisp-prn.elc ilisp-low.elc ilisp-doc.elc ilisp-ext.elc ilisp-mod.elc ilisp-dia.elc ilisp-cmt.elc 
del ilisp-rng.elc ilisp-hnd.elc ilisp-utl.elc ilisp-cmp.elc ilisp-kil.elc ilisp-snd.elc ilisp-xfr.elc ilisp-hi.elc 
del ilisp-aut.elc ilisp-cl.elc ilisp-cmu.elc ilisp-sbcl.elc ilisp-cl-easy-menu.elc ilisp-acl.elc ilisp-kcl.elc 
del ilisp-luc.elc ilisp-sch.elc ilisp-hlw.elc ilisp-xls.elc ilisp-chs.elc ilisp-openmcl.elc

cd docs
%EMACS% -batch -q -no-site-file ilisp.texi -l texinfmt -f texinfo-format-buffer -f save-buffer
cd ..