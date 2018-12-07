:- module(ciao_gui, [main/1], [assertions, dcg, fsyntax, doccomments, datafacts]).

%! \title Launcher of Ciao GUI applications
%  
%  \author Jose F. Morales
%
%  \module
%
%  This module implements an launcher for applications written as Ciao
%  services with a Web-based frontend. It starts a GUI process (as
%  frontend) and an HTTP server that connects (dynamically) with Ciao
%  services (see @lib{serve_http}).
%  
%  It supports the following frontends (HTML/CSS/JavaScript):
%  
%   - The [Electron](https://electron.atom.io/) framework (itself based
%     on [Node](https://nodejs.org),
%     [Chromium](http://www.chromium.org/Home), and
%     [V8](https://developers.google.com/v8/))
%  
%   - Any modern available Web browser (as fallback)
%  
%  ## Usage
%  
%  Execute with `ciao-gui <Cmd>`.
%  See also the `ciao-serve` command for serving Web applications.

:- use_module(library(format), [format/2, format/3]).
:- use_module(library(system)).

:- use_module(library(service/service_registry), [reload_service_registry/0]).
:- use_module(library(service/serve_http), []). % (implements httpserv.handle/3)
:- use_module(library(http/http_server), [http_bind/1, http_loop/1]).
:- use_module(ciaobld(config_common), [site_root_dir/1]).

% ---------------------------------------------------------------------------
:- doc(section, "Options").

parse_args(Args, Rest) :- !, Rest = Args.

% ---------------------------------------------------------------------------
:- doc(section, "Starts the GUI process and begin Ciao server").

main(Args) :-
	reload_service_registry,
	parse_args(Args, Rest),
	%
	app_url(Rest, URL),
	Port = 8001,
	% display(url(URL, Port)), nl,
	http_bind(Port), % (bind HTTP port) % TODO: need loopback!
	gui_start(URL, Port),
	Path = ~site_root_dir, cd(Path), % TODO: not needed?
	http_loop(ExitCode),
	gui_stop,
	% gui_join
	halt(ExitCode). % TODO: better way?

% ---------------------------------------------------------------------------
:- doc(section, "GUI process").

:- use_module(library(bundle/bundle_paths), [bundle_path/3]).
:- use_module(ciaobld(third_party_custom), [third_party_custom_path/2]).
:- use_module(library(process), [process_call/3, process_join/1, process_kill/1]).
:- use_module(library(pathnames), [path_concat/3]).
:- use_module(library(terms), [atom_concat/2]).

%gui_engine(chrome).
%gui_engine(firefox).
gui_engine(electron).

local_server('localhost').

% Add server and port to URL
full_url(Port, URL0, URL) :-
	atom_number(Port2, Port),
	atom_concat(['http://', ~local_server, ':', Port2, URL0], URL).

:- data gui_eng_process/1.

% :- export(gui_start/2).
gui_start(URL, Port) :-
	gui_engine(E),
	URL2 = ~full_url(Port, URL),
	gui_start_(E, URL2).

% TODO: Locate binaries for Windows and Linux too
% TODO: fix fallback mode (no gui_eng_process)

% gui_start_(chrome, URL) :-
% 	Bin = '/Applications/Google\ Chrome.app/Contents/MacOS/Google\ Chrome',
% 	process_call(Bin, [~atom_concat('--app=', URL)], []).
% gui_start_(firefox, URL) :-
% 	Bin = '/Applications/Firefox.app/Contents/MacOS/firefox',
% 	process_call(Bin, ['-chrome', URL], []).
gui_start_(electron, URL) :-
	third_party_custom_path(node_modules, NodePath),
	Bin = ~path_concat(NodePath, 'electron/dist/Electron.app/Contents/MacOS/Electron'),
	SrcPath = ~bundle_path(ciao_gui, '.'),
	process_call(Bin, ['src/electron-main.js', URL],
	             [env(['NODE_PATH'=NodePath]), background(P), cwd(SrcPath)]),
	set_fact(gui_eng_process(P)).

% :- export(gui_join/0).
% Wait until GUI process finishes
gui_join :-
	( gui_eng_process(P) ->
	    process_join(P)
	; true
	).

% TODO: try process_join on gui_stop/0?

% :- export(gui_stop/0).
% Kills the GUI process
gui_stop :-
	( gui_eng_process(P) ->
	    process_kill(P)
	    % process_join(P)
	; true
	).

% ---------------------------------------------------------------------------
:- doc(section, "Locate/start the URL of the Ciao program").

:- use_module(library(service/service_registry), [bundle_http_entry/3]).

% Obtain URL from service_registry.pl
app_url([App], URL) :-
	bundle_http_entry(_Bundle, App, URL0),
	!,
	URL = URL0.
app_url(Rest, _URL) :- !,
	format(user_error, "ERROR: not a valid or registered application: ~w~n", Rest),
	halt(1).

