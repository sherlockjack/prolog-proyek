:- dynamic action/2.

evaluate_position(Value) :-
    player(PlayerName),
    character(PlayerName, _, _, _, PlayerHealth, _),
    PlayerHealth =< 0 -> Value = inf
    ;
    (enemy(EnemyName), character(EnemyName, _, _, _, EnemyHealth, _), EnemyHealth =< 0) -> Value = -inf
    ;
    calc_value(Value).

calc_value(Value) :-
    player(PlayerName),
    enemy(EnemyName),
    character(PlayerName, _, PlayerAtk, PlayerDef, PlayerHealth, _),
    character(EnemyName, _, EnemyAtk, EnemyDef, EnemyHealth, _),
    TrueEnemyAtk is EnemyAtk-PlayerDef,
    TruePlayerAtk is PlayerAtk-EnemyDef,
    Offset is -min(TruePlayerAtk, min(TrueEnemyAtk, 0)) + 1,
    Value is (EnemyHealth / (TruePlayerAtk+Offset)) / (PlayerHealth / (TrueEnemyAtk+Offset)).

possible_action(CharName, Action) :-
    get_other_name(CharName, OtherName),
    Action = action(attack(CharName, OtherName), false).

possible_action(CharName, Action) :-
    character(CharName, _, _, _, _, Items),
    member(item(ItemName, _), Items),
    Action = action(use_item(CharName, ItemName), false).

possible_action(CharName, Action) :-
    cooldown(CharName, 0),
    get_other_name(CharName, OtherName),
    Action = action(attack(CharName, OtherName), true).

possible_action(CharName, Action) :-
    cooldown(CharName, 0),
    character(CharName, _, _, _, _, Items),
    member(item(ItemName, _), Items),
    Action = action(use_item(CharName, ItemName), true).

store_state(Status) :-
    player(PlayerName),
    enemy(EnemyName),
    character(PlayerName, PlayerRole, PlayerAtk, PlayerDef, PlayerHealth, PlayerItems),
    character(EnemyName, EnemyRole, EnemyAtk, EnemyDef, EnemyHealth, EnemyItems),
    cooldown(PlayerName, PlayerCooldown),
    cooldown(EnemyName, EnemyCooldown),
    skill_active(PlayerName, PlayerActive),
    skill_active(EnemyName, EnemyActive),
    PlayerStatus = [PlayerName, PlayerRole, PlayerAtk, PlayerDef, PlayerHealth, PlayerItems, PlayerCooldown, PlayerActive],
    EnemyStatus = [EnemyName, EnemyRole, EnemyAtk, EnemyDef, EnemyHealth, EnemyItems, EnemyCooldown, EnemyActive],
    Status = [PlayerStatus, EnemyStatus].

restore_state(Status) :-
    Status = [PlayerStatus, EnemyStatus],
    PlayerStatus = [PlayerName, PlayerRole, PlayerAtk, PlayerDef, PlayerHealth, PlayerItems, PlayerCooldown, PlayerActive],
    EnemyStatus = [EnemyName, EnemyRole, EnemyAtk, EnemyDef, EnemyHealth, EnemyItems, EnemyCooldown, EnemyActive],
    retractall(character(_, _, _, _, _, _)),
    retractall(cooldown(_, _)),
    retractall(skill_active(_, _)),
    assert(character(PlayerName, PlayerRole, PlayerAtk, PlayerDef, PlayerHealth, PlayerItems)),
    assert(character(EnemyName, EnemyRole, EnemyAtk, EnemyDef, EnemyHealth, EnemyItems)),
    assert(cooldown(PlayerName, PlayerCooldown)),
    assert(cooldown(EnemyName, EnemyCooldown)),
    assert(skill_active(PlayerName, PlayerActive)),
    assert(skill_active(EnemyName, EnemyActive)).

apply_action(CharName, action(Action, UseSkill)) :-
    (UseSkill == true -> use_skill(CharName); true),
    Action,
    retractall(temp_item(_, _)),
    retractall(use_skill_temp(_, _)),
    retractall(temp_attack(_, _)),
    retractall(output(_)).

minimax(CharName, Depth, Alpha, Beta, MaxPlayer, BestAction, Value) :-
    player(PlayerName),
    enemy(EnemyName),
    character(PlayerName, _, _, _, PlayerHealth, _),
    character(EnemyName, _, _, _, EnemyHealth, _),
    cooldown(PlayerName, PlayerCooldown),
    cooldown(EnemyName, EnemyCooldown),
    update_cooldowns,
    ( (Depth == 0; PlayerHealth =< 0; EnemyHealth =< 0) -> 
        (evaluate_position(Value), BestAction = game_over)
    ; MaxPlayer == true ->
        findall(Action, possible_action(CharName, Action), Actions),
        max_value(Actions, CharName, Depth, Alpha, Beta, BestAction, Value)
    ;
        findall(Action, possible_action(CharName, Action), Actions),
        min_value(Actions, CharName, Depth, Alpha, Beta, BestAction, Value)
    ),
    retractall(cooldown(_, _)),
    assert(cooldown(PlayerName, PlayerCooldown)),
    assert(cooldown(EnemyName, EnemyCooldown)).

max_value([], _, _, _, _, empty_action, -inf).
max_value([Action|Actions], CharName, Depth, Alpha, Beta, BestAction, Value) :-
    store_state(Status),
    apply_action(CharName, Action),
    NewDepth is Depth - 1,
    get_other_name(CharName, NewCharName),
    minimax(NewCharName, NewDepth, Alpha, Beta, false, _, ActionValue),
    restore_state(Status),
    (ActionValue > Alpha -> NewAlpha = ActionValue; NewAlpha = Alpha),
    ( NewAlpha >= Beta ->
        Value = Beta, BestAction = Action
    ; max_value(Actions, CharName, Depth, NewAlpha, Beta, CurBestAction, CurValue),
      ( CurValue > NewAlpha ->
        Value = CurValue,
        BestAction = CurBestAction
      ; Value = NewAlpha,
        BestAction = Action
      )
    ).

min_value([], _, _, _, _, empty_action, inf).
min_value([Action|Actions], CharName, Depth, Alpha, Beta, BestAction, Value) :-
    store_state(Status),
    apply_action(CharName, Action),
    NewDepth is Depth - 1,
    get_other_name(CharName, NewCharName),
    minimax(NewCharName, NewDepth, Alpha, Beta, true, _, ActionValue),
    restore_state(Status),
    (ActionValue < Beta -> NewBeta = ActionValue; NewBeta = Beta),
    ( NewBeta =< Alpha ->
        Value = Alpha, BestAction = Action
    ; min_value(Actions, CharName, Depth, Alpha, NewBeta, CurBestAction, CurValue),
      ( CurValue < NewBeta ->
        Value = CurValue,
        BestAction = CurBestAction
      ; Value = NewBeta,
        BestAction = Action
      )
    ).

enemy_action(Action, UseSkill) :-
    enemy(EnemyName),
    once(minimax(EnemyName, 3, -inf, inf, true, action(Action, UseSkill), _)).
