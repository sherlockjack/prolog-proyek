:- consult('item.pl'). 
:- consult('enemy_names.pl'). 
:- consult('role.pl').
:- dynamic temp_item/2.

generate_stats(Atk, Def, Health, Role) :-
    random(3000, 5000, Atk),
    random(500, 1000, Def),
    random(10000, 15000, Health),
    findall(SelectRole, role(SelectRole, _, _), Roles),
    random_select(Role, Roles, _).

create_character(Name) :-
    generate_stats(Atk, Def, Health, Role),
    get_all_items(Atk, Def, Health, ItemList),
    assert(character(Name, Role, Atk, Def, Health, ItemList)).

create_player(Name) :-
    create_character(Name),
    assert(player(Name)).

create_enemy :- 
    random_enemy_name(Name),
    create_character(Name),
    assert(enemy(Name)).

player_attack -->
    {player(PlayerName), enemy(EnemyName), attack(PlayerName, EnemyName)}, 
    html(\attack_html(PlayerName)).

enemy_attack -->
    {player(PlayerName), enemy(EnemyName), attack(EnemyName, PlayerName)}, 
    html(\attack_html(EnemyName)).

turn_off_skills(CharName) :-
    get_other_name(CharName, OtherName),
    character(CharName, CharRole, _, _, _, _),
    character(OtherName, OtherRole, _, _, _, _),
    (CharRole == warrior -> retractall(skill_active(CharName)) ; true),
    ((OtherRole == shielder ; OtherRole == archer) -> retractall(skill_active(OtherName)) ; true).

attack(AttackerName, DefenderName) :-
    character(AttackerName, AttackerRole, AttackerAtk, _, _, _),
    character(DefenderName, DefenderRole, DefenderAtk, DefenderDef, DefenderHealth, DefenderItems),
    ((DefenderRole == shielder, skill_active(DefenderName))->(ModifiedAtk = AttackerAtk, ModifiedDef = AttackerAtk);
    (AttackerRole == warrior, skill_active(AttackerName))->(ModifiedAtk is AttackerAtk * 1.5, ModifiedDef = DefenderDef);
    (DefenderRole == archer, skill_active(DefenderName))->(ModifiedAtk = AttackerAtk, divmod(DefenderDef,2,ModifiedDef,_)); 
    (ModifiedAtk = AttackerAtk, ModifiedDef = DefenderDef)),
    Damage is max(ModifiedAtk - ModifiedDef, 0),
    NewHealth is DefenderHealth - Damage,
    retractall(character(DefenderName, _, _, _, _, _)),
    assert(character(DefenderName, DefenderRole, DefenderAtk, DefenderDef, NewHealth, DefenderItems)),
    ( NewHealth > 0 ->
        swritef(Output, '%w attacks %w for %d damage. %w has %d health remaining.\n', 
                [AttackerName, DefenderName, Damage, DefenderName, NewHealth])
        ;
        swritef(Output, '%w attacks %w for %d damage. %w is defeated!\n', 
                [AttackerName, DefenderName, Damage, DefenderName])
    ),
    assert(temp_attack(AttackerName, Output)),
    turn_off_skills(AttackerName).

attack_html(CharName) -->
    {temp_attack(CharName, Output), retract(temp_attack(CharName, Output))},
    html(p(Output)).

use_item(CharName, ItemName) :-
    character(CharName, CharRole, CharAtk, CharDef, CharHealth, CharItems),
    member(item(ItemName, Effect), CharItems),
    select(item(ItemName, Effect), CharItems, RemainingItems),

    (Effect = increase('Attack', Value) ->
        NewAtk is CharAtk + Value,
        retractall(character(CharName, _, _, _, _, _)),
        assert(character(CharName, CharRole, NewAtk, CharDef, CharHealth, RemainingItems)),
        swritef(Output, '%w used %w. Attack increased to %d.\n', [CharName, ItemName, NewAtk])
    ; Effect = increase('Defense', Value) ->
        NewDef is CharDef + Value,
        retractall(character(CharName, _, _, _, _)),
        assert(character(CharName, CharRole, CharAtk, NewDef, CharHealth, RemainingItems)),
        swritef(Output, '%w used %w. Defense increased to %d.\n', [CharName, ItemName, NewDef])
    ; Effect = increase('Health', Value) ->
        NewHealth is CharHealth + Value,
        retractall(character(CharName, _, _, _, _)),
        assert(character(CharName, CharRole, CharAtk, CharDef, NewHealth, RemainingItems)),
        swritef(Output, '%w used %w. Health increased to %d.\n', [CharName, ItemName, NewHealth])
    ),
    assert(temp_item(CharName, Output)),
    turn_off_skills(CharName).

use_item_html(CharName) -->
    {temp_item(CharName, Output), retract(temp_item(CharName, Output))}, html(p(Output)).

best_item_finder([], BestItemName, BestItemPower) :-
    BestItemName = '',
    BestItemPower = -1.

best_item_finder([item_power(ItemName, ItemPower) | Rest], BestItemName, BestItemPower) :-
    best_item_finder(Rest, PrevItemName, PrevItemPower),
    (
        ItemPower > PrevItemPower ->
        BestItemName = ItemName,
        BestItemPower = ItemPower
    ;
        BestItemName = PrevItemName,
        BestItemPower = PrevItemPower
    ).

best_item(Items, BestItemName, BestItemPower) :-
    findall(item_power(ItemName, NewPower),
            (member(item(ItemName, increase(Type, Value)), Items), 
            predict_power(Type, Value, NewPower)),
            PowerList),
    
    best_item_finder(PowerList, BestItemName, BestItemPower).

show_game_over(Hide, Header, Output) -->
    { swritef(HeaderScript, 'var header = "%w";', [Header]),
      swritef(OutputScript, 'var output = "%w";', [Output]),
      swritef(HideScript, 'var hide = %w;', [Hide]),
      Script = "
            if(hide) {
                var selection = document.getElementById('gameForm');
                selection.style.display = 'none';

                var selection = document.getElementById('separator');
                selection.style.display = 'block';
            
                var gameOverHeader = document.getElementById('gameOverHeader');
                gameOverHeader.innerHTML = header;
            
                var gameOverText = document.getElementById('gameOverText');
                gameOverText.innerHTML = output;

                var playAgain = document.getElementById('playAgain');
                playAgain.innerHTML = 'Play again?';
            }
        "
    },
    html(script([], [HeaderScript, OutputScript, HideScript, Script])).

check_health -->
    {update_cooldowns,
    player(PlayerName),
    enemy(EnemyName),
    character(PlayerName, _, _, _, PlayerHealth, _),
    character(EnemyName, _, _, _, EnemyHealth, _),
    (PlayerHealth =< 0 -> 
        swritef(Output, '%w is defeated. Better luck next time!', [PlayerName]),
        Hide = true,
        Header = 'Defeat'
    ;
     EnemyHealth =< 0 -> 
        swritef(Output, '%w is defeated. Congratulations, you did it!', [EnemyName]),
        Hide = true,
        Header = 'Victory'
    ;
        Hide = false
     )},
    html([
        \show_game_over(Hide, Header, Output)
    ]).