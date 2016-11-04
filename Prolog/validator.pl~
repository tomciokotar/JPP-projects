% Validates if a program in a special, given language is safe (there's no risk of entering the critical section by two abstract processes at once).

:- ensure_loaded(library(lists)).

% Część odpowiadająca za obliczanie wyrażeń i wykonywanie instrukcji.

getValue(Num, _, _, Num):-
	number(Num), !.

getValue(pid, ProcId, _, ProcId):- !.

getValue(Ident, _, state(vars(VarList), _, _), Val):-
	atom(Ident),
	Ident \= pid, !,
	member(var(Ident, Val), VarList).

getValue(arr(Ident, Exp), ProcId, State, Val):-
	atom(Ident),
	Ident \= pid,
	evalExp(Exp, ProcId, State, ExpVal),
	State = state(_, arrays(ArrList), _),
	member(arr(Ident, ArrValues), ArrList),
	member(arrVal(ExpVal, Val), ArrValues).

evalExp(Exp, ProcId, State, Val):-
	Exp =.. [Op, Arg1, Arg2],
	member(Op, [+,-,*,/]), !,
	getValue(Arg1, ProcId, State, Val1),
	getValue(Arg2, ProcId, State, Val2),
	NewExp =.. [Op, Val1, Val2],
	Val is NewExp.

evalExp(Exp, ProcId, State, Val):-
	Exp \= _ + _,
	Exp \= _ - _,
	Exp \= _ * _,
	Exp \= _ / _,
	getValue(Exp, ProcId, State, Val).

:- op(700, xfx, <>).

(X <> Y):-
	X =\= Y.

evalBoolExp(Exp, ProcId, State):-
	Exp =.. [Op, Arg1, Arg2],
	member(Op, [<,=,<>]),
	getValue(Arg1, ProcId, State, Val1),
	getValue(Arg2, ProcId, State, Val2),
	NewExp =.. [Op, Val1, Val2],
	NewExp.

% setValue - do aktualizacji wartości zmiennych, używany tylko w maplist.

setValue(Ident, _, CurrVar, CurrVar):-
	CurrVar =.. [_, Ident2, _],
	Ident \= Ident2, !.

setValue(Ident, Val, CurrVar, NewVar):-
	Val \= plusOne,
	CurrVar =.. [Type, Ident, _],
	NewVar =.. [Type, Ident, Val].

setValue(Ident, plusOne, CurrVar, NewVar):-
	CurrVar =.. [Type, Ident, CurrVal],
	Val is CurrVal + 1,
	NewVar =.. [Type, Ident, Val].

setVar(Ident, Val, state(vars(VarList), Arrays, Instr),
state(vars(ResVarList), Arrays, Instr)):-
	maplist(setValue(Ident, Val), VarList, ResVarList).

setArrVal(Ident, ValId, Val, state(Vars, arrays(ArrList), Instr),
state(Vars, arrays(ResArrList), Instr)):-
	member(arr(Ident, Array), ArrList),
	maplist(setValue(ValId, Val), Array, NewArray),
	maplist(setValue(Ident, NewArray), ArrList, ResArrList).

gotoInstr(ProcId, InstrId, state(Vars, Arrays, instr(InstrList)),
state(Vars, Arrays, instr(ResInstrList))):-
	maplist(setValue(ProcId, InstrId), InstrList, ResInstrList).

gotoNextInstr(ProcId, state(Vars, Arrays, instr(InstrList)),
state(Vars, Arrays, instr(ResInstrList))):-
	maplist(setValue(ProcId, plusOne), InstrList, ResInstrList).

exec(assign(Ident, Exp), ProcId, State, ResState):-
	atom(Ident), !,
	evalExp(Exp, ProcId, State, Val),
	setVar(Ident, Val, State, MidState),
	gotoNextInstr(ProcId, MidState, ResState).

exec(assign(arr(Ident, ExpId), ExpVal), ProcId, State, ResState):-
	atom(Ident), !,
	evalExp(ExpId, ProcId, State, ValId),
	evalExp(ExpVal, ProcId, State, Val),
	setArrVal(Ident, ValId, Val, State, MidState),
	gotoNextInstr(ProcId, MidState, ResState).

exec(goto(InstrId), ProcId, State, ResState):-
	!, gotoInstr(ProcId, InstrId, State, ResState).

exec(condGoto(Exp, InstrId), ProcId, State, ResState):-
	evalBoolExp(Exp, ProcId, State), !,
	gotoInstr(ProcId, InstrId, State, ResState).

exec(condGoto(Exp, _), ProcId, State, ResState):-
	\+ evalBoolExp(Exp, ProcId, State), !,
	gotoNextInstr(ProcId, State, ResState).

exec(sekcja, ProcId, State, ResState):-
	gotoNextInstr(ProcId, State, ResState).

getInstr(InstrId, Program, Instr):- nth1(InstrId, Program, Instr).

getInstrId(ProcId, state(_, _, instr(InstrList)), InstrId):-
	member(proc(ProcId, InstrId), InstrList).

% step - tutaj jako parametr Program przekazywana jest sama lista instrukcji.

step(Program, State, ProcId, ResState):-
	getInstrId(ProcId, State, InstrId),
	getInstr(InstrId, Program, Instr),
	exec(Instr, ProcId, State, ResState).

accMakeSeqList(0, List, [0|List]).

accMakeSeqList(N, List, ResList):-
	N > 0,
	NewN is N - 1,
	accMakeSeqList(NewN, [N|List], ResList).

makeSeqList(Size, List):-
	N is Size - 1,
	accMakeSeqList(N, [], List).

makeVar(Ident, var(Ident, 0)).

makeArrVal(ValId, arrVal(ValId, 0)).

makeDefArray(Size, DefArray):-
	makeSeqList(Size, SeqList),
	maplist(makeArrVal, SeqList, DefArray).

makeArr(DefArray, Ident, arr(Ident, DefArray)).

makeInstr(Ident, proc(Ident, 1)).

initState(prog(vars(VarList), arrays(ArrList), _), N,
state(vars(ResVarList), arrays(ResArrList), instr(InstrList))):-
	maplist(makeVar, VarList, ResVarList),
	makeDefArray(N, DefArray),
	maplist(makeArr(DefArray), ArrList, ResArrList),
	makeSeqList(N, SeqList),
	maplist(makeInstr, SeqList, InstrList).

processInSection(Program, proc(_, InstrId)):-
	getInstr(InstrId, Program, sekcja).

% Zwracamy listę procesów, które są w danym momencie w sekcji.

processesInSection(Program, state(_, _, instr(InstrList)), ResList):-
	include(processInSection(Program), InstrList, ResList).

% Weryfikujemy, czy dany stan jest bezpieczny, jeśli tak, to przeszukujemy dalej.

analyzeProgram(Program, ProcId, N, State, StateId, Visited, Path, Res):-
	processesInSection(Program, State, ProcList),
	length(ProcList, Len),
	(Len >= 2 ->
		reverse(Path, RevPath),
		Res = unsafe(StateId, RevPath, ProcList)
	;
		dfs(Program, ProcId, N, State, StateId, Visited, Path, Res)).

% dfs dla danego stanu wykonujemy dopiero po sprawdzeniu, że jest on bezpieczny.
% Instrukcje wykonujemy najpierw dla procesów o mniejszym pidzie (od 0 do N-1).

dfs(_, N, N, _, StateId, Visited, _, safe(StateId, Visited)).

dfs(Program, ProcId, N, State, StateId, Visited, Path, Res):-
	ProcId < N,
	step(Program, State, ProcId, ResState),
	NewProcId is ProcId + 1,
	(member(ResState, Visited) ->
		dfs(Program, NewProcId, N, State, StateId, Visited, Path, Res)
	;
		NewStateId is StateId + 1,
		getInstrId(ProcId, State, InstrId),
		analyzeProgram(Program, 0, N, ResState, NewStateId,
		[ResState|Visited], [proc(ProcId, InstrId)|Path], MidRes),
		(MidRes = safe(SafeId, SafeVisited) ->
			NewProcId is ProcId + 1,
			dfs(Program, NewProcId, N, State,
			SafeId, SafeVisited, Path, Res)
		;
			Res = MidRes)).

runAnalyzer(Prog, N):-
	initState(Prog, N, State),
	Prog = prog(_, _, program(Program)),
	analyzeProgram(Program, 0, N, State, 1, [State], [], Res),
	printResult(Res).

printPath(Proc, Proc):-
	Proc = proc(ProcId, InstrId),
	format('Proces ~d: ~d~n', [ProcId, InstrId]).

printProcList(Proc, Proc):-
	Proc = proc(ProcId, _),
	format('~d ', [ProcId]).

printResult(safe(_, _)):-
	write('Program jest bezpieczny.').

printResult(unsafe(StateId, Path, ProcList)):-
	format('Stan nr ~d nie jest bezpieczny.~n', [StateId]),
	write('Niepoprawny przeplot:\n'),
	maplist(printPath, Path, _),
	write('Procesy w sekcji: '),
	maplist(printProcList, ProcList, _), nl.

checkN(N):-
	integer(N),
	N >= 1.

verify(N, _):-
	\+ checkN(N),
	write('Niepoprawna liczba procesów.').

verify(N, File):-
	checkN(N),
	set_prolog_flag(fileerrors, off),
	see(File),
	!,
	read(Vars),
	read(Arrays),
	read(Program),
	seen,
	runAnalyzer(prog(Vars, Arrays, Program), N).

verify(_, File):-
	format('Niepoprawna nazwa pliku: ~p.~n', [File]).
