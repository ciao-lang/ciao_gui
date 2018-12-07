:- module(_, [], [ciaobld(bundlehooks)]).

:- doc(title,  "Bundle Hooks for Ciao GUI").

:- use_module(ciaobld(third_party_custom)).

% (hook)
'$builder_hook'(custom_run(fetch_externals, [])) :- !,
	third_party_custom_install(ciao_gui).

m_bundle_foreign_dep(ciao_gui, cmd, 'node', 'Node (http://nodejs.org)').
m_bundle_foreign_dep(ciao_gui, cmd, 'npm', 'NPM (http://www.npmjs.com)').
m_bundle_foreign_dep(ciao_gui, npm, 'electron', 'https://github.com/electron/electron').

