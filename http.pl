:- use_module(library(http/html_write)).
:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_parameters)).
:- consult('game.pl').
:- consult('role.pl').

:- http_handler(root(.), add_character_form, []).
:- http_handler(root(submit_characters), submit_characters_handler, []).
:- http_handler(root(form), form_handler, []).
:- http_handler(root(win), win_handler, []).
:- http_handler(root(lose), lose_handler, []).

server(Port) :-
    http_server(http_dispatch, [port(Port)]).

add_character_form(_Request) :-
    retractall(character(_, _, _, _, _)),
    reply_html_page(
        title('Add Character'),
        [
            h2('Add New Character'),
            form(
                [action='/submit_characters', method='POST'],
                [
                    h3('Player Information'),
                    p([], [label([for=player_name], 'Player  Name: '), input([type=text, name=player_name, required])]),
                    p([], [input([type=submit, value='Submit', required])])
                ]
            )
        ]
    ).

positive(Integer) :- Integer > 0.

submit_characters_handler(Request) :-
    http_parameters(Request, [player_name(PlayerName, [])]),
    retractall(character(_, _, _, _, _, _)),
    retractall(player(_)),
    retractall(enemy(_)),
    create_player(PlayerName),
    create_enemy,
    initialize_cooldowns,
    Message = 'Player and Enemy added successfully!',
    reply_html_page(
        title('Characters Added'),
        [
            h2(Message),
            p(['Lets go to ', a([href='/form'], 'battle field'), '.'])
        ]
    ).

game_over -->
    html([
        h2([id=gameOverHeader], []),
        p([id=gameOverText], []),
        a([id=playAgain, href='/'], []),
        div([id=separator, style='display:none;'], [br([], []), br([], [])])
    ]).
    
form_handler(Request) :-
    player(PlayerName),
    enemy(EnemyName),
    (   memberchk(method(post), Request)
    ->  (
            http_parameters(Request, [action(Action, []), item_name(ItemName, [optional(true)])]),
            (   Action == 'use_skill'
            ->  (
                    use_skill(PlayerName),
                    reply_html_page(
                        title('Game Actions'),
                        [
                            \game_form,
                            h2('Attacking Phase'),
                            h3('Your Action'),
                            \use_skill_html(PlayerName),
                            h2('Result'),
                            \show_results(PlayerName, EnemyName)
                        ]
                    )
                )
            ;   ((   Action == 'attack'
                ->  (
                        attack(PlayerName, EnemyName),
                        Func = attack_html(PlayerName)
                    )
                ;   Action == 'use_item'
                ->  (
                        use_item(PlayerName, ItemName),
                        Func = use_item_html(PlayerName)
                    )
                ),
                enemy_action(EnemyAction, UseSkill),
                ((UseSkill == true) -> Func2 = (use_skill_html(EnemyName)); (Func2 = empty_html)),
                (
                    (EnemyAction == use_item(_, _)) -> (Func3 = use_item_html(EnemyName)); Func3 = (attack_html(EnemyName))
                ),
                reply_html_page(
                    title('Game Actions'),
                    [
                        \game_over,
                        \game_form,
                        h2('Attacking Phase'),
                        h3('Your Action'),
                        \Func,
                        h3('Enemy Action'),
                        \Func2,
                        \Func3,
                        h2('Result'),
                        \check_health,
                        \show_results(PlayerName, EnemyName)
                    ]
                ))
            )
        )
    ;   reply_html_page(
            title('Game Actions'),
            [
                \game_form,
                \show_results(PlayerName, EnemyName)
            ]
        )
    ).

use_skill_form -->
    {
        player(PlayerName),
        cooldown(PlayerName, Cooldown),
        Cooldown == 0 -> Form = html(p([], [
                input([
                    id=action_use_skill,
                    type=radio,
                    name=action,
                    value=use_skill,
                    onclick="toggleItemSelection(false)"
                ], []),
                label([for=action_use_skill], 'Use Skill: ')
            ]))
        ;
        Form = html(p(['Your skill is still in cooldown!']))
    },
    html(\Form).

game_form -->
    {
        player(PlayerName),
        character(PlayerName, _, _, _, _, PlayerItems)
    },
    html(form([action('/form'), method('POST'), id('gameForm')], [
        h2('Choose Your Action'),
        p([], [
            input([
                id=action_attack,
                type=radio,
                name=action,
                value=attack,
                checked,
                onclick="toggleItemSelection(false)"
            ], []),
            label([for=action_attack], 'Attack: ')
        ]),
        p([], [
            input([
                id=action_use_item,
                type=radio,
                name=action,
                value=use_item,
                onclick="toggleItemSelection(true)"
            ], []),
            label([for=action_use_item], 'Use Item: ')
        ]),
        p([id=itemSelection, style='display:none;'], [
            label([for=item_name], 'Item Name: '),
            select([name=item_name], \item_options(PlayerItems))
        ]),
        \use_skill_form,
        p([], [input([type=submit, value='Submit'])])
    ])),
    html(script([], "
        function toggleItemSelection(show) {
            var selection = document.getElementById('itemSelection');
            if (show) {
                selection.style.display = 'block';
            } else {
                selection.style.display = 'none';
            }
        }"
    )).

item_options([]) --> [].
item_options([item(Name, _)|T]) -->
    html(option([value=Name], Name)),
    item_options(T).

show_results(PlayerName, EnemyName) -->
    html([ \show_stats(PlayerName), \show_stats(EnemyName) ]).        

show_stats(CharName) -->
    { character(CharName, CharRole, CharAtk, CharDef, CharHealth, CharItems),
      role(CharRole, Skill, _),
      cooldown(CharName, Cooldown) },
    html([
        h3(CharName),
        p(['Role: ', CharRole]),
        p(['Skill: ', Skill]),
        p(['Cooldown: ', Cooldown]),
        p(['Attack: ', CharAtk]),
        p(['Defense: ', CharDef]),
        p(['Health: ', CharHealth]),
        h3('Items'),
        \list_items(CharItems)
    ]).

list_items([]) --> [].
list_items([item(Name, Effect)|T]) -->
    { swritef(S, '- %w: %w', [Name, Effect]) },
    html([p([S]), \list_items(T)]).

:- initialization(server(8081)).
