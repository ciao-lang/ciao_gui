:- bundle(ciao_gui).
version('0.1').
depends([
    core
]).
alias_paths([ciao_gui = 'src']).
%
cmd('ciao-gui', [main='cmds/ciao_gui']).
manual('ciao_gui', [main='doc/SETTINGS.pl']).

