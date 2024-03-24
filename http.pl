:- use_module(library(http/html_write)).
:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_parameters)).
:- consult('game.pl'). 

:- http_handler(root(form), form_handler, []).
:- http_handler(root(submit), action_handler, []).

server(Port) :-
    http_server(http_dispatch, [port(Port)]).

form_handler(_Request) :-
    reply_html_page(
        title('Game Actions'),
        [h2('Choose Your Action'),
         form([action='/submit', method='POST'],
              [p([], [label([for=action], 'Action: '),
                     select([name=action], [option([value=attack], 'Attack'),
                                             option([value=use_item], 'Use Item')])]),
               p([], [label([for=item_name], 'Item Name (if using item): '),
                     input([type=text, name=item_name])]),
               p([], [input([type=submit, value='Submit'])])
              ])
        ]).

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