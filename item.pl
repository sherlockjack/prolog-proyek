item_name('Attack', 'Dragonslayer').
item_name('Attack', 'Frostmourne').
item_name('Attack', 'Eagle Eye').
item_name('Attack', 'Power Elixir').
item_name('Attack', 'Stormcaller\'s Pendant').
item_name('Attack', 'Sunfury').
item_name('Attack', 'Ashbringer').
item_name('Attack', 'Widowmaker').
item_name('Attack', 'Shark Tooth Necklace').
item_name('Attack', 'Berserker\'s Rage').
item_name('Attack', 'Orb of Fire').

item_name('Defense','Dragonscale Plate').
item_name('Defense','Aegis of the Undying').
item_name('Defense','Guardian\'s Bulwark').
item_name('Defense','Nightshade Cloak').
item_name('Defense','Iron Skin Potion').
item_name('Defense','Stone Shield').
item_name('Defense','Adamantium Knuckles').
item_name('Defense','Turtle Shell Amulet').
item_name('Defense','Phantom Boots').
item_name('Defense','Cloak of Shadows').
item_name('Defense','Quicksilver Ring').

item_name('Health', 'Elixir of Life').
item_name('Health', 'Phoenix Feather').
item_name('Health', 'Ambrosia').
item_name('Health', 'Moonflower Petal').
item_name('Health', 'Ancient Bark Potion').
item_name('Health', 'Hercules\' Gauntlet').
item_name('Health', 'Vampiric Ring').
item_name('Health', 'Leech Seed').
item_name('Health', 'Algae Necklace').
item_name('Health', 'Clockwork Heart').
item_name('Health', 'Oracle').

get_category_item(MaxStat, Category, ItemName, Stat) :-
    item_name(Category, ItemName),
    0 is random(4),
    divmod(MaxStat, 5, LowerBound, _),
    UpperBound is MaxStat+1,
    random(LowerBound, UpperBound, Stat).

get_category_item_list(MaxStat, Category, ItemList) :-
    findall(item(ItemName, increase(Category, Stat)),
            get_category_item(MaxStat, Category, ItemName, Stat), 
            ItemList).

get_all_items(MaxAtk, MaxDef, MaxHealth, FinalRes) :-
    get_category_item_list(MaxAtk, 'Attack', AtkItemList),
    get_category_item_list(MaxDef, 'Defense', DefItemList),
    get_category_item_list(MaxHealth, 'Health', HealthItemList),

    append(AtkItemList, DefItemList, Res),
    append(Res, HealthItemList, FinalRes).