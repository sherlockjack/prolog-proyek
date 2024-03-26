:- use_module(library(http/html_write)).
:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_parameters)).
:- consult('game.pl'). 

:- http_handler(root(.),add_character_form,[]).
:- http_handler(root(submit_characters),submit_characters_handler,[]).
:- http_handler(root(form), form_handler, []).
:- http_handler(root(submit), action_handler, []).

server(Port) :-
    http_server(http_dispatch, [port(Port)]).

add_character_form(_Request) :-
reply_html_page(
title('Add Characters'),
[h2('Add New Characters'),
    form([action='/submit_characters', method='POST'],
        [h3('Player Information'),
        p([], [label([for=player_name], 'Player Name: '),
                input([type=text, name=player_name])]),
        p([], [label([for=player_atk], 'Player Attack: '),
                input([type=number, name=player_atk])]),
        p([], [label([for=player_def], 'Player Defense: '),
                input([type=number, name=player_def])]),
        p([], [label([for=player_health], 'Player Health: '),
                input([type=number, name=player_health])]),
        h3('Enemy Information'),
        p([], [label([for=enemy_name], 'Enemy Name: '),
                input([type=text, name=enemy_name])]),
        p([], [label([for=enemy_atk], 'Enemy Attack: '),
                input([type=number, name=enemy_atk])]),
        p([], [label([for=enemy_def], 'Enemy Defense: '),
                input([type=number, name=enemy_def])]),
        p([], [label([for=enemy_health], 'Enemy Health: '),
                input([type=number, name=enemy_health])]),
        p([], [input([type=submit, value='Submit'])])
        ])
]).

submit_characters_handler(Request) :-
    http_parameters(Request, [
        player_name(PlayerName, [length > 0]),
        player_atk(PlayerAtk, [integer, number > 0]),
        player_def(PlayerDef, [integer, number > 0]),
        player_health(PlayerHealth, [integer, number > 0]),
        enemy_name(EnemyName, [length > 0]),
        enemy_atk(EnemyAtk, [integer, number > 0]),
        enemy_def(EnemyDef, [integer, number > 0]),
        enemy_health(EnemyHealth, [integer, number > 0])
    ]),
    create_player(PlayerName, PlayerAtk, PlayerDef, PlayerHealth),
    create_enemy(EnemyName, EnemyAtk, EnemyDef, EnemyHealth),
    Message = 'Player and Enemy added successfully!',
    reply_html_page(title('Characters Added'), [
        h2(Message),
        p(['Lets go to ', a([href='/form'], 'battle field'), '.'])
    ]).

    form_handler(Request) :-
        player(PlayerName),
        enemy(EnemyName),
        character(PlayerName, _, _, _, _),
        character(EnemyName, _, _, _, _),
        (   memberchk(method(post), Request) -> 
            http_parameters(Request, [action(Action, []), item_name(ItemName, [optional(true)])]),
            (   Action == 'attack' -> Func = player_attack
            ;   Action == 'use_item' -> Func = use_item(PlayerName, ItemName)
            ),
            reply_html_page(
                title('Game Actions'),
                [
                    h2('Choose Your Action'),
                    \game_form,
                    \Func, % Render the action results
                    \show_results(PlayerName, EnemyName)
                ]
            )
        ;   reply_html_page(
                title('Game Actions'),
                [
                    h2('Choose Your Action'),
                    \game_form, % Only render the form initially
                    \show_results(PlayerName, EnemyName) % Show current stats
                ]
            )
        ).
    
    game_form -->
        { player(PlayerName),
          character(PlayerName, _, _, _, PlayerItems) },
        html(form([action='/form', method='POST'],
            [
                p([], [label([for=action], 'Action: '),
                        select([name=action], [option([value=attack], 'Attack'),
                                                option([value=use_item], 'Use Item')])]),
                p([], [label([for=item_name], 'Item Name (if using item): '),
                        select([name=item_name], \item_options(PlayerItems))]),
                p([], [input([type=submit, value='Submit'])])
            ])).
    
    item_options([]) --> [].
    item_options([item(Name, _)|T]) -->
        html(option([value=Name], Name)),
        item_options(T).
    
    
show_results(PlayerName, EnemyName) -->
        html([ \show_stats(PlayerName), \show_stats(EnemyName) ]).
            

action_handler(Request) :-
    http_parameters(Request, [action(Action, []), item_name(ItemName, [optional(true)])]),

    ( Action == 'attack' -> Func = choose_1 ; Action == 'use_item' -> Func = choose_2(ItemName) ),
    reply_html_page(title('Action Result'), [
        h2('Attacking Phase'),
        \Func,
        \show_results
    ]).
    
show_results -->
    { player(PlayerName), enemy(EnemyName) },
    html([h2('Results'),
        \show_stats(PlayerName),
        \show_stats(EnemyName)]).

show_stats(CharName) -->
    { character(CharName, CharAtk, CharDef, CharHealth, CharItems) },
    html([h3(CharName),
        p(['Attack: ', CharAtk]),
        p(['Defense: ', CharDef]),
        p(['Health: ', CharHealth]),
        h4('Items'),
        \list_items(CharItems)]).

list_items([]) --> [].
list_items([item(Name, Effect)|T]) -->
    {swritef(S, '- %w: %w', [Name, Effect])},
    html([p([S]), \list_items(T)]).
    
:- initialization(server(8080)).