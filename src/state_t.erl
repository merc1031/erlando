%% The contents of this file are subject to the Mozilla Public License
%% Version 1.1 (the "License"); you may not use this file except in
%% compliance with the License. You may obtain a copy of the License
%% at http://www.mozilla.org/MPL/
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and
%% limitations under the License.
%%
%% The Original Code is Erlando.
%%
%% The Initial Developer of the Original Code is VMware, Inc.
%% Copyright (c) 2011-2013 VMware, Inc. All rights reserved.
%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%* Converted from 'Parameterized Module' to 'Stateful Module'
%*
%* Parameterized Modules auto-add a 'new' fun
%* X = module:new( P1, P2 ).
%*
%* X is actually {module, P1, P2} -- just like the stateful module syntax, but
%* we have to handle the extraction of the tuple, whereas the pmod code was
%* taking care of that for us.
%*
%* So we need to:
%* Change the -module back to a standard erlang module.
%* Add a 'new' fun to simulate the auto-created one by the parameterized module.
%* Change all calls made through the returned 'instance' to take the additional
%* 'state' param -- we simplify this by using a define.
%* Update the arity of all of these calls.
%*
%* We also remove -behavior(monad) because of the updated arity to '>>=', fail & return.
%*
%*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%* -module(state_t, [InnerMonad]).
%* -behaviour(monad).
%* -export(['>>='/2, return/1, fail/1]).
%* -export([get/0, put/1, eval/2, exec/2, run/2,
%* modify/1, modify_and_return/1, lift/1]).

-module(state_t).
-compile({parse_transform, do}).

-export([new/1]).
-export(['>>='/3, return/2, fail/2]).
-export([get/1, put/2, eval/3, exec/3, run/3,
modify/2, modify_and_return/2, lift/2]).

-ifdef(use_specs).
-type(monad(A) :: fun ((S) -> {A, S})).
-include("monad_specs.hrl").
-endif.

%* Create a define for simplification -- may be multiple parameters.
-define( PMOD_FIX, {?MODULE, InnerMonad} ).

%* Add the 'new' fun.
new( InnerMonad ) -> ?PMOD_FIX.

'>>='(X, Fun, ?PMOD_FIX) -> fun (S) -> do([InnerMonad || {A, S1} <- X(S),
(Fun(A))(S1)]) end.

return(A, ?PMOD_FIX) -> fun (S) -> InnerMonad:return({A, S}) end.
fail(Str, ?PMOD_FIX) -> fun (_) -> InnerMonad:fail(Str) end.

get(?PMOD_FIX) -> fun (S) -> InnerMonad:return({S, S}) end.

put(S, ?PMOD_FIX) -> fun (_) -> InnerMonad:return({ok, S}) end.

eval(Monad, S, ?PMOD_FIX) -> do([InnerMonad || {A, _S1} <- Monad(S),
return(A)]).

exec(Monad, S, ?PMOD_FIX) -> do([InnerMonad || {_A, S1} <- Monad(S),
return(S1)]).

%* Had to hand fix this because we nolonger have the monad behaviour; was causing 'unused' error.
%* run(Monad, S, ?PMOD_FIX) -> do([InnerMonad || Monad(S)]).
run(Monad, S, _PMOD_FIX) -> Monad(S).

modify(Fun, ?PMOD_FIX) -> fun (S) -> InnerMonad:return({ok, Fun(S)}) end.

modify_and_return(Fun, ?PMOD_FIX) -> fun (S) -> InnerMonad:return(Fun(S)) end.

lift(X, ?PMOD_FIX) -> fun (S) -> do([InnerMonad || A <- X, return({A, S})]) end.
