enemy_names([
    'Aelar', 'Baeloth', 'Caelum', 'Daeris', 'Eilistraee', 
    'Fenrir', 'Gorion', 'Heian', 'Itherael', 'Jorrvaskr', 
    'Kaelthas', 'Lirael', 'Malfurion', 'Nuala', 'Olorin', 
    'Paelias', 'Queloz', 'Raelag', 'Saruman', 'Tyrande'
]).

random_enemy_name(ResultName) :-
    player(PlayerName),
    enemy_names(NameList),
    random_select(Name, NameList, _),
    (Name \= PlayerName -> ResultName = Name; random_enemy_name(ResultName)).
