% length([source list],[variable for length count])
length_([],0).                                          % terminating scenario of recursion
length_([_|Tail],Length) :- 
    length_(Tail,TailLength), Length is TailLength+1.   % "Tail" variable alrady has <list> type, so do not placing in []  


% lists contain
member_(Elem,[Elem|_]).         % element found (it`s at the "Head" position)
member_(Elem,[_|Tail]) :- 
    member_(Elem,Tail).


% list cocnatenation
append_([],List2,List2).
append_([Head|Tail],List2,[Head|TailResult]) :- 
    append_(Tail,List2,TailResult).


% sublist
sub_start([], _List) :- !.                        % sub_start function checking if List starts with SubList
sub_start([Head|TailSub], [Head|TailList]) :-
   sub_start(TailSub, TailList).
sublist(Sub, List) :-                            % sublist -> if(sub_start)==true{sublist=true} else{index++)}
   sub_start(Sub, List), !.
sublist(Sub, [_Head|Tail]) :-
   sublist(Sub, Tail).

% sublist using append predicate
sublist_append(SubList,List) :- 
    append_(_,List1,List), append_(SubList,_,List1). /* List -> [_|SubList|_]
                                                                  |..List1..|  */


% remove(Element, InputList, OutputList)  
remove(Elem, [Elem|Tail], Tail).
remove(Elem, [Head|Tail], [Head|ResultTail]) :-
    remove(Elem, Tail, ResultTail).

/* removes all elements specific value

remove(_Elem,[],[]).   
remove(Elem, [Elem|Tail], ResultTail) :- 
    remove(Elem, Tail, ResultTail).
remove(Elem, [Head|Tail], [Head|ResultTail]) :-
    remove(Elem, Tail, ResultTail).
*/

% permute
permute_([],[]).
permute_(List,[Head|Tail]) :- 
    remove(Head,List,ResultList),
    permute_(ResultList,Tail).


%            ******************
%            | PERSONAL TASKS |
%            ******************

% removing the last n elements (task 1.6)
remove_n(N, List, ResultList) :-
    append_(ResultList, CuttingList, List),
    length_(CuttingList, N).

remove_N(N, List, Res):-
    reverse(List,RevList),
    deleteFirstN(RevList,DelRes,N),
    reverse(DelRes,Res).

reverse(List, ReverseList):-
   reverse(List, [], ReverseList). % вызов вспомогательной функции с пустым буфером
 
reverse([], Buffer, Buffer):-!.
reverse([Head|Tail], Buffer, ReverseList):-
   reverse(Tail, [Head|Buffer], ReverseList).
 
deleteFirstN(List,List,0):-!. % число N = 0 , то мы уже удалили первые символы , поэтому полчаем ответ 
deleteFirstN([_|List],Res,N):-
    N1 is N - 1,
    deleteFirstN(List,Res,N1).



% finding max elem`s index (task 2.11)
find_max(List, Result) :-
    find_max(List, -1, 1, 1, Result).

find_max([], MaxElem, MaxIndex, _, MaxIndex).

find_max([Head|Tail], MaxElem, MaxIndex, CurrentIndex, Result) :-
    (   Head >= MaxElem
    ->  NewMaxElem = Head,
        NewMaxIndex = CurrentIndex
    ;   NewMaxElem = MaxElem,
        NewMaxIndex = MaxIndex
    ),
    NewCurrentIndex is CurrentIndex + 1,
    find_max(Tail, NewMaxElem, NewMaxIndex, NewCurrentIndex, Result).
