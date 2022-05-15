% furkan...
% 2018400...
% compiling: yes
% complete: yes
:- ['cmpecraft.pro'].

:- init_from_map.

% 10 points
% manhattan_distance(+A, +B, -Distance) :- .
%given two list both formed by two elements representing x and y locations recpectively, finds the manhattan distance between (X1,Y1) and (X2,Y2)
manhattan_distance([Head1|Tail1],[Head2|Tail2],Distance) :- X is abs(Head1-Head2), Y is abs(Tail1-Tail2), Distance is X+Y.

% minimum_of_list(+List, -Minimum) :- .
% 10 points
%given a list, finds its minimum element recursively
minimum_of_list([H|T], Minimum) :-
    minimum_of_list(T,H,Minimum).

minimum_of_list([],Min,Min).
minimum_of_list([H|T],Minimum1,Minimum) :-
    Minimum2 is min(H,Minimum1),
    minimum_of_list(T,Minimum2,Minimum).

% find_nearest_type(+State, +ObjectType, -ObjKey, -Object, -Distance) :- .
%given the current state and objecttype, finds an instance of the ObjectType with lowest manhattan distance to 
%our agent from ObjectEnvironment of the state if it is possible,otherwise it returns false
find_nearest_type(State,ObjectType,Key,Object,Distance) :-
    nth0(0,State,Agent_Dictionary),get_dict(x,Agent_Dictionary,Agent_Xc),get_dict(y,Agent_Dictionary,Agent_Yc),
    nth0(1, State, Object_Environment),
    dict_size(Object_Environment,Object_Count),
    dict_pairs(Object_Environment,Tag,KVs),
    looper(KVs,X,[],ObjectType,Agent_Xc,Agent_Yc,Object_Count),         %calls looper with parameters and returns X that is a list
    transpose_pairs(X,Transposed),keysort(Transposed,Sorted),
    [Distance-Key | _] = Sorted,
    get_dict(Key,Object_Environment,Object).



% iterates through Object_Environment and creates a list of pairs that keys of it is the key of the object in the Object_Environment
% and values of it is the distance between specific object and our agent.
looper(List,X,X,Type,Agentx,Agenty,0).
looper([Head|Tail],X,New,Type,Agentx,Agenty,Count) :-
    Key-Value = Head,
    get_dict(x,Value,Locx),
    get_dict(y,Value,Locy),
    manhattan_distance([Locx,Locy],[Agentx,Agenty],Distance),
    A is Count-1,
    get_dict(type,Value,B),
    %if B is the desired type adds object's Key and Value to the list and calls loop again for next object in the object_dict
    %otherwise it skips the current object and calls loop again for the next object in the object_dict
    (B==Type -> looper(Tail,X,[Key-Distance | New],Type,Agentx,Agenty,A); looper(Tail,X,New,Type,Agentx,Agenty,A)).
    
   
   
% 10 points
% navigate_to(+State, +X, +Y, -ActionList, +DepthLimit) :- .
%given the state,X ,and Y coordinates finds a path doesnt exceed the Depthlimit and returns that path as Actions 
navigate_to(State,X,Y,Actions,DepthLimit) :-
    nth0(0,State,Agent_Dict),get_dict(x,Agent_Dict,Xc),get_dict(y,Agent_Dict,Yc),
    manhattan_distance([X,Y],[Xc,Yc],Distance),
    (Distance =< DepthLimit),
    Dist_X is abs(X-Xc),
    Dist_Y is abs(Y-Yc),
    %if destinations X coord. is greater than Agent's, go right (Xo-Xa) times
    %in opposite case go left (Xa-Xo) times.Process is similar for Y coordinates.
    (X>=Xc -> for_loop(go_right,L1,[],Dist_X) ; for_loop(go_left,L1,[],Dist_X)),
    (Y>=Yc -> for_loop(go_down,L2,[],Dist_Y) ; for_loop(go_up,L2,[],Dist_Y)),
    append(L1,L2,Actions).


for_loop(Direction,X,X,0).
for_loop(Direction,X,List,Count) :-
    A is Count-1,
    for_loop(Direction,X,[Direction|List],A).

% 10 points
% chop_nearest_tree(+State, -ActionList) :- .

chop_nearest_tree(State,ActionList) :-
    find_nearest_type(State,tree,Key,Object,Distance),
    get_dict(x,Object,Ox),get_dict(y,Object,Oy),
    navigate_to(State,Ox,Oy,Actions,100),
    for_loop(left_click_c,L3,[],4),
    append(Actions,L3,ActionList).

% 10 points
% mine_nearest_stone(+State, -ActionList) :- .

mine_nearest_stone(State,ActionList) :-
    find_nearest_type(State,stone,Key,Object,Distance),
    get_dict(x,Object,Ox),get_dict(y,Object,Oy),
    navigate_to(State,Ox,Oy,Actions,100),
    for_loop(left_click_c,L3,[],4),
    append(Actions,L3,ActionList).

mine_nearest_cobblestone(State,ActionList) :-
    find_nearest_type(State,cobblestone,Key,Object,Distance),
    get_dict(x,Object,Ox),get_dict(y,Object,Oy),
    navigate_to(State,Ox,Oy,Actions,100),
    for_loop(left_click_c,L3,[],4),
    append(Actions,L3,ActionList).

% 10 points
% gather_nearest_food(+State, -ActionList) :- .

gather_nearest_food(State,ActionList) :-
    find_nearest_type(State,food,Key,Object,Distance),
    get_dict(x,Object,Ox),get_dict(y,Object,Oy),
    navigate_to(State,Ox,Oy,Actions,100),
    for_loop(left_click_c,L3,[],4),
    append(Actions,L3,ActionList).

%chop_nearest_tree(State,ActionList)
% 10 points
% collect_requirements(+State, +ItemType, -ActionList) :- .

collect_requirements(State,ItemType,ActionList) :-
    %checks the ItemType and calls specific pred. according to that type
    (ItemType==stick -> collect_requirements_for_stick(State,ActionList)    
    ; ItemType==stone_pickaxe -> collect_requirements_for_stone_pickaxe(State,ActionList)
    ; collect_requirements_for_stone_axe(State,ActionList)).

%if there are at least two logs in the bag does nothing, otherwise calls chop_nearest_tree()
collect_requirements_for_stick(State,Actions) :-
    nth0(0,State,Agent_Dictionary),get_dict(inventory,Agent_Dictionary,Bag),
    (has(log,2,Bag) -> true ; chop_nearest_tree(State,Actions)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%First Step:
%if there are at least two sticks in the bag does nothing, otherwise checks if stick is craftable with the current items in the Bag.
%if it is craftable append[craft_stick] to the actionlist,otherwise calls chop_nearest_tree and append [craft_stick] to the list.
%lastly calls execute actions with current state and takes NextState
%Second Step:
%Creates Bag2 from NextState,checks if there are at least 2 sticks in the bag,then checks if there are at least 3 logs in the bag,then does nothing
%if there isn't enough logs while there is enough stick,calls chop_nearest_tree and append list to the temp. list
%situation that there is not enough stick and log is similar
%Third Step:
%after created Bag3 checks if there is enough cobblestone in the bag,if it is does nothing otherwise calls mine_nearest_stone
%RESULT: by doing these steps we created our ActionList by considering state changes as the result of our intermediate actions. 
collect_requirements_for_stone_pickaxe(State,ActionList) :-
    nth0(0,State,Agent_Dictionary),get_dict(inventory,Agent_Dictionary,Bag),
    
    !,(has(stick,2,Bag) -> append([],[],Actions); (craftable(stick,State) -> true ; chop_nearest_tree(State,List1)),
    append(List1,[craft_stick],Actions)),Ltemp=Actions,execute_actions(State,Ltemp,NextState),
    %write(NextState),
    
    nth0(0,NextState,Agent_Dictionary2),get_dict(inventory,Agent_Dictionary2,Bag2),
    
    !,(has(stick,2,Bag2) -> (has(log,3,Bag2)-> append(Actions,[],Act) ; chop_nearest_tree(NextState,List2),append(Actions,List2,Act) )
    ; !,(has(log,5,Bag2)-> append(Actions,[],Act) ; chop_nearest_tree(NextState,List3),append(Actions,List3,Act))),append(Act,[],Ltemp2),
    execute_actions(State,Act,N2State),
    %write(Act),write(N2State),
    
    nth0(0,N2State,Agent_Dictionary3),get_dict(inventory,Agent_Dictionary3,Bag3),
    (has(cobblestone,1,Bag3)->(get_dict(cobblestone,Bag3,Count),A is 3-Count);(A is 3)),
    
    !,(has(cobblestone,3,Bag3)-> append(Act,[],ActionList) ; (mine_nearest_stone(N2State,ActList);cobblestone_loop(N2State,ActList,[],A)),append(Act,ActList,ActionList)).

%It is nearly the same as collect_for_stone_pickaxe predicate.So I skip these part.
collect_requirements_for_stone_axe(State,ActionList) :-
    nth0(0,State,Agent_Dictionary),get_dict(inventory,Agent_Dictionary,Bag),
    
    !,(has(stick,2,Bag) -> append([],[],Actions); (craftable(stick,State) -> true ; chop_nearest_tree(State,List1)),
    append(List1,[craft_stick],Actions)),Ltemp=Actions,execute_actions(State,Ltemp,NextState),
    
    nth0(0,NextState,Agent_Dictionary2),get_dict(inventory,Agent_Dictionary2,Bag2),
    
    !,(has(stick,2,Bag2) -> (has(log,3,Bag2)-> append(Actions,[],Act) ; chop_nearest_tree(NextState,List2),append(Actions,List2,Act) )
    ; !,(has(log,5,Bag2)-> append(Actions,[],Act) ; chop_nearest_tree(NextState,List3),append(Actions,List3,Act))),append(Act,[],Ltemp2),
    execute_actions(State,Act,N2State),
    
    nth0(0,N2State,Agent_Dictionary3),get_dict(inventory,Agent_Dictionary3,Bag3),
    (has(cobblestone,1,Bag3)->(get_dict(cobblestone,Bag3,Count),A is 3-Count);(A is 3)),
    
    !,(has(cobblestone,3,Bag3)-> append(Act,[],ActionList) ; (mine_nearest_stone(N2State,ActList);cobblestone_loop(N2State,ActList,[],A)),append(Act,ActList,ActionList)).
    

% 5 points
% find_castle_location(+State, -XMin, -YMin, -XMax, -YMax) :- .

%finds an appropriate castle location in the current state via is_appropriate predicate
find_castle_location(State,XMin,YMin,XMax,YMax) :-
    nth0(0,State,Agent_Dict),get_dict(x,Agent_Dict,Ax),get_dict(y,Agent_Dict,Ay),
    is_appropriate(1,1,[],List,State),lister(List,Ax,Ay,[],Distances),
    minimum_of_list(Distances,Minimum),nth0(Index,Distances,Minimum),nth0(Index,List,Result),
    XMin-YMin=Result,XMax is XMin+2,YMax is YMin+2.


%controls whether there are already an object in the X,Y coordinates or not.
tile_occup(X, Y, State) :-
    State = [_, StateDict, _],
    get_dict(_, StateDict, Object),
    get_dict(x, Object, Ox),
    get_dict(y, Object, Oy),
    get_dict(type, Object, Type),
    X = Ox, Y = Oy.

%Starts from (1,1) checks a point and all other points in a 3x3 square -that our current object is appears top left of that square- are
%not occupied.If it is inst, Ox and Oy and break the loop otherwise calls the loop for the next point.
%loop continues until Ox is Wd-3 ,and Oy is He-3.It represents the map boundary.In last iteration it checks one more time 
% if the last point is apropriate.If it is inst. Xf and Yf and returns,otherwise returns false.
is_appropriate(Ox,Oy,List,List2,State) :- 
    width(Wd),height(He),Ox=:=Wd-3,Oy=:=He-3,
    ((\+(tile_occup(Ox,Oy,State))),X1 is Ox+1,X2 is Ox+2,Y1 is Oy+1,Y2 is Oy+2,
    (\+(tile_occup(X1,Oy,State))),(\+(tile_occup(X2,Oy,State)))
    ,(\+(tile_occup(Ox,Y1,State))),(\+(tile_occup(X1,Y1,State)))
    ,(\+(tile_occup(X2,Y1,State))),(\+(tile_occup(Ox,Y2,State)))
    ,(\+(tile_occup(X1,Y2,State))),(\+(tile_occup(X2,Y2,State))) -> append(List,[Ox-Oy],List2); append(List,[],List2)).
is_appropriate(Ox,Oy,List,List2,State) :-
    width(Wd),height(He),
    ((\+(tile_occup(Ox,Oy,State))),!,X1 is Ox+1,X2 is Ox+2,Y1 is Oy+1,Y2 is Oy+2,
    (\+(tile_occup(X1,Oy,State))),(\+(tile_occup(X2,Oy,State)))
    ,(\+(tile_occup(Ox,Y1,State))),(\+(tile_occup(X1,Y1,State)))
    ,(\+(tile_occup(X2,Y1,State))),(\+(tile_occup(Ox,Y2,State)))
    ,(\+(tile_occup(X1,Y2,State))),(\+(tile_occup(X2,Y2,State))))->
    (Xf is Ox, Yf is Oy,append(List,[Xf-Yf],ListF),((width(Wd),(Ox=:=Wd-3)) -> (B is Oy+1,is_appropriate(1,B,ListF,List2,State)) ; (A is Ox+1,is_appropriate(A,Oy,ListF,List2,State)))) ; 
    ((width(Wd),(Ox=:=Wd-3)) -> (B is Oy+1,is_appropriate(1,B,List,List2,State)) ; (A is Ox+1,is_appropriate(A,Oy,List,List2,State))).

% 15 points
% make_castle(+State, -ActionList) :- .
%calls mine_nearest_stone until there are at least nine cobblestones in the Bag,
%then tries to find a suitable castle location,If there isn't any suitable location returns false,otherwise
%navigate to every point in the found 3x3 area and places cobblestone to every destination point.
make_castle(State,ActionList) :-
    nth0(0,State,Agent_Dict),get_dict(inventory,Agent_Dict,Bag),!,
    (has(cobblestone,1,Bag) -> (get_dict(cobblestone,Bag,Count),(A is 3-Count//3),castle_loop(State,[],Actionss,A)); (castle_loop(State,[],Actionss,3))),
    length(Actionss, LengthAct),((LengthAct==0)-> true ; execute_actions(State,Actionss,NextState))
    ,!,find_castle_location(NextState,XMin,YMin,XMax,YMax),!,X1 is XMin+1,Y1 is YMin+1,
    navigate_to(NextState,XMin,YMin,Action,500),length(Action, Length1),((Length1==0) -> NextState2=NextState ; execute_actions(NextState,Action,NextState2)),!,
    append(Action,[place_c],L1),
    navigate_to(NextState2,X1,YMin,Action2,500),!,length(Action2, Length2),((Length2==0) -> NextState2=NextState3 ; execute_actions(NextState2,Action2,NextState3)),!,
    %execute_actions(NextState2,Action2,NextState3),
    append(Action2,[place_c],L2),
    navigate_to(NextState3,XMax,YMin,Action3,500),!,length(Action3, Length3),((Length3==0) -> NextState3=NextState4 ; execute_actions(NextState3,Action3,NextState4)),!,
    % execute_actions(NextState3,Action3,NextState4)
    append(Action3,[place_c],L3),
    navigate_to(NextState4,XMax,Y1,Action4,500),!,length(Action4, Length4),((Length4==0) -> NextState4=NextState5 ; execute_actions(NextState4,Action4,NextState5)),!,
    %execute_actions(NextState4,Action4,NextState5),
    append(Action4,[place_c],L4),
    navigate_to(NextState5,X1,Y1,Action5,500),!,length(Action5, Length5),((Length5==0) -> NextState5=NextState6 ; execute_actions(NextState5,Action5,NextState6)),!,
    %execute_actions(NextState5,Action5,NextState6),
    append(Action5,[place_c],L5),
    navigate_to(NextState6,XMin,Y1,Action6,500),!,length(Action6, Length6),((Length6==0) -> NextState6=NextState7 ; execute_actions(NextState6,Action6,NextState7)),!,
    %execute_actions(NextState6,Action6,NextState7),
    append(Action6,[place_c],L6),
    navigate_to(NextState7,XMin,YMax,Action7,500),!,length(Action7, Length7),((Length7==0) -> NextState7=NextState8 ; execute_actions(NextState7,Action7,NextState8)),!,
    %execute_actions(NextState7,Action7,NextState8),
    append(Action7,[place_c],L7),
    navigate_to(NextState8,X1,YMax,Action8,500),!,length(Action8, Length8),((Length8==0) -> NextState8=NextState9 ; execute_actions(NextState8,Action8,NextState9)),!,
    %execute_actions(NextState8,Action8,NextState9),
    append(Action8,[place_c],L8),
    navigate_to(NextState9,XMax,YMax,Action9,500),!,length(Action9, Length9),((Length9==0) -> NextState9=NextState10 ; execute_actions(NextState9,Action9,NextState10)),!,
    %execute_actions(NextState9,Action9,NextState10),
    append(Action9,[place_c],L9),
    append(L1,L2,List12),append(L3,L4,List34),append(L5,L6,List56),append(L7,L8,List78),append(List78,L9,List789),
    append(List12,List34,Lst1),append(List56,List789,Lst2),append(Lst1,Lst2,Lst3),
    append(Actionss,Lst3,ActionList).
    
%mines stone to gather necessary cobblestone for make_castle.    
castle_loop(State,ActionList,ActionList,0).
castle_loop(State,ActionList,List,Count) :-
    (mine_nearest_stone(State,Action);cobblestone_loop(State,Action,[],3)),
    execute_actions(State,Action,NextState),
    A is Count-1,append(ActionList,Action,List2),
    castle_loop(NextState,List2,List,A).

cobblestone_loop(State,ActionList,ActionList,0).
cobblestone_loop(State,ActionList,List,Count) :-
    mine_nearest_cobblestone(State,Action),
    execute_actions(State,Action,NextState),
    A is Count-1,append(List,Action,List2),
    cobblestone_loop(NextState,ActionList,List2,A).


lister([],Ax,Ay,List,List).
lister([H|T],Ax,Ay,List,List1) :-
    Ox-Oy=H,manhattan_distance([Ox,Oy],[Ax,Ay],Dist),
    append(List,[Dist],ListX),
    lister(T,Ax,Ay,ListX,List1).


