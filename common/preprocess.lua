local preprocess = {};

-- The following contains a list of all the conditionals found in gear sets plus
-- the function used to process them
preprocess.jump_table = {};

-- The following contains all of the inline conditonal keywards plus their procedural
-- processing template  == 181 definitions so far
local jump_table_template = {
    ['BUFFED'] = function ()
        return (buff_manager.has('BUFF',false,false))
        end,
    ['NOT_BUFFED'] = function ()
        return (buff_manager.has('BUFF',false,true))
        end,
    ['DEBUFFED'] = function ()
        return (buff_manager.has('DEBUFF',false,false))
        end,
    ['NOT_DEBUFFED'] = function ()
        return (buff_manager.has('DEBUFF',false,true))
        end
    ['DUALWIELD'] = function ()
        local player = gData.GetPlayer();;
        return (string.find('NIN,DNC',player.MainJob) ~= nil or string.find('NIN,DNC',player.SubJob) ~= nil);
        end
    ['NOT_DUALWIELD'] = function ()
        local player = gData.GetPlayer();
        return not (string.find('NIN,DNC',player.MainJob) ~= nil or string.find('NIN,DNC',player.SubJob) ~= nil);
        end
    ['INPARTY'] = function ()
        local party = gData.GetParty();
        return (party ~= nil and party.Count ~= nil and party.Count > 1);
        end
    ['NOT_INPARTY'] = function ()
        local party = gData.GetParty();
        return not (party ~= nil and party.Count ~= nil and party.Count > 1);
        end
    ['OWN'] = function ()
        return (gVars.sRegion == gVars._REGION_STATUS_OWNED);
        end
    ['NOT_OWN'] = function ()
        return not (gVars.sRegion == gVars._REGION_STATUS_OWNED);
        end
    ['TOWN'] = function ()
        local environ = gData.GetEnvironment();
        return (environ.Area ~= nil and table.find(gVars.tTownAreas['Towns'],environ.Area) ~= nil);
        end
    ['NOT_TOWN'] = function ()
        local environ = gData.GetEnvironment();
        return not (environ.Area ~= nil and table.find(gVars.tTownAreas['Towns'],environ.Area) ~= nil);
        end
    ['TOWN-AK'] = function ()
        return (validate_town_ak(sGear));
        end
    ['ME'] = function ()
        local me = AshitaCore:GetMemoryManager():GetParty():GetMemberTargetIndex(0);
        local tg = gData.GetTarget();
        return (tg ~= nil and tg == me);
        end
    ['NOT_ME'] = function ()
        local me = AshitaCore:GetMemoryManager():GetParty():GetMemberTargetIndex(0);
        local tg = gData.GetTarget();
        return not (tg ~= nil and tg == me);
        end
    -- Toggles and Cycles
    ['ACCURACY'] = function ()
        return utilities.fGetToggle(gVars._ACC);
        end
    ['NOT_ACCURACY'] = function ()
        return not utilities.fGetToggle(gVars._ACC);
        end
    ['RACCURACY'] = function ()
        return utilities.fGetToggle(gVars._RACC);
        end
    ['NOT_RACCURACY'] = function ()
        return not utilities.fGetToggle(gVars._RACC);
        end
    ['EVATION'] = function ()
        return utilities.fGetToggle(gVars._EVASION);
        end
    ['NOT_EVATION'] = function ()
        return not utilities.fGetToggle(gVars._EVASION);
        end
    ['IDLE'] = function ()
        return utilities.fGetToggle(gVars._IDLE);
        end
    ['NOT_IDLE'] = function ()
        return not utilities.fGetToggle(gVars._IDLE);
        end
    ['TANK'] = function ()
        return utilities.fGetToggle(gVars._TANK);
        end
    ['NOT_TANK'] = function ()
        return not utilities.fGetToggle(gVars._TANK);
        end
    ['MACC'] = function ()
        return utilities.fGetToggle(gVars._MACC);
        end
    ['NOT_MACC'] = function ()
        return not utilities.fGetToggle(gVars._MACC);
        end
    ['WSWAP'] = function ()
        return utilities.fGetToggle(gVars._WSWAP);
        end
    ['NOT_WSWAP'] = function ()
        return not utilities.fGetToggle(gVars._WSWAP);
        end
    ['KITE'] = function ()
        return utilities.fGetToggle(gVars._KITE);
        end
    ['NOT_KITE'] = function ()
        return not utilities.fGetToggle(gVars._KITE);
        end
    ['SPF'] = function ()
        return utilities.fGetToggle(gVars._SPF);
        end
    ['NOT_SPF'] = function ()
        return not utilities.fGetToggle(gVars._SPF);
        end
    ['RIDING'] = function ()
        return (buff_manager.has('CHOCOBO',false,false))
        end
    ['NOT_RIDING'] = function ()
        return (buff_manager.has('CHOCOBO',false,true))
        end
    ['TH'] = function ()
        return utilities.fGetToggle(gVars._TH);
        end
    ['NOT_TH'] = function ()
        return not utilities.fGetToggle(gVars._TH);
        end
    ['BRD:HORN'] = function ()
        return (utilities.fGetCycle(gVars._INSTRUMENT) == gVars._HORN);
        end
    ['NOT_BRD:HORN'] = function ()
        return (utilities.fGetCycle(gVars._INSTRUMENT) ~= gVars._HORN);
        end
    ['BRD:STRING'] = function ()
        return (utilities.fGetCycle(gVars._INSTRUMENT) == gVars._STRING);
        end
    ['NOT_BRD:STRING'] = function ()
        return (utilities.fGetCycle(gVars._INSTRUMENT) ~= gVars._STRING);
        end
    ['BST:AJUG'] = function ()
        return utilities.fGetToggle(gVars._AJUG);
        end
    ['NOT_BST:AJUG'] = function ()
        return not utilities.fGetToggle(gVars._AJUG);
        end
    ['BST:DB:BPP'] = function ()
        return (utilities.fGetCycle(gVars._DB) == gVars._sDB_BPP);
        end
    ['NOT_BST:DB:BPP'] = function ()
        return (utilities.fGetCycle(gVars._DB) ~= gVars._sDB_BPP);
        end
    ['BST:DB:WSS'] = function ()
        return (utilities.fGetCycle(gVars._DB) == gVars._sDB_WSS);
        end
    ['NOT_BST:DB:WSS'] = function ()
        return (utilities.fGetCycle(gVars._DB) ~= gVars._sDB_WSS);
        end
    ['SMN:SBP'] = function ()
        return utilities.fGetToggle(gVars._SBP);
        end
    ['NOT_SMN:SBP'] = function ()
        return not utilities.fGetToggle(gVars._SBP);
        end
    ['THF:SS'] = function ()
        return utilities.fGetToggle(gVars._SS);
        end
    ['NOT_THF:SS'] = function ()
        return not utilities.fGetToggle(gVars._SS);
        end
    -- Weather:Day
    ['WEATHER:DAY'] = function ()
        local environ = gData.GetEnvironment();
        return (environ.WeatherElement == environ.DayElement);
        end
    ['NOT_WEATHER:DAY'] = function ()
        local environ = gData.GetEnvironment();
        return (environ.WeatherElement ~= environ.DayElement);
        end
    -- Status
    ['STATUS:ENGAGED'] = function ()
        local player = gData.GetPlayer();
        return (player.Status == 'Engaged');
        end
    ['NOT_STATUS:ENGAGED'] = function ()
        local player = gData.GetPlayer();
        return (player.Status ~= 'Engaged');
        end
    ['STATUS:RESTING'] = function ()
        local player = gData.GetPlayer();
        return (player.Status == 'Resting');
        end
    ['NOT_STATUS:RESTING'] = function ()
        local player = gData.GetPlayer();
        return (player.Status ~= 'Resting');
        end
    ['STATUS:IDLING'] = function ()
        local player = gData.GetPlayer();
        return (player.Status == 'Idling');
        end
    ['MODE:PERP'] = function ()
        return (utilities.fGetCycle(gVars._MODE) == gVars._MODE_PERPETUATION);
        end
    ['NOT_MODE:PERP'] = function ()
        return (utilities.fGetCycle(gVars._MODE) ~= gVars._MODE_PERPETUATION);
        end
    ['MODE:ATTK'] = function ()
        return (utilities.fGetCycle(gVars._MODE) == gVars._MODE_ATTACK);
        end
    ['NOT_MODE:ATTK'] = function ()
        return (utilities.fGetCycle(gVars._MODE)) ~= gVars._MODE_ATTACK);
        end
    ['MODE:ENNM'] = function ()
        return (utilities.fGetCycle(gVars._MODE) == gVars._MODE_ENMITY_MINUS);
        end
    ['NOT_MODE:ENNM'] = function ()
        return (utilities.fGetCycle(gVars._MODE) ~= gVars._MODE_ENMITY_MINUS);
        end
    ['NOT_STATUS:IDLING'] = function ()
        local player = gData.GetPlayer();
        return (player.Status ~= 'Idling');
        end
    -- Magic Type
    ['MAGIC_TYPE:BLUE'] = function ()
        local spell = gData.GetAction();
        return (spell ~= nil and spell.Skill ~= nil and spell.Skill == 'Blue Magic');
        end
    ['MAGIC_TYPE:DARK'] = function ()
        local spell = gData.GetAction();
        return (spell ~= nil and spell.Skill ~= nil and spell.Skill == 'Dark Magic');
        end
    ['MAGIC_TYPE:DIVINE'] = function ()
        local spell = gData.GetAction();
        return (spell ~= nil and spell.Skill ~= nil and spell.Skill == 'Divine Magic');
        end
    ['MAGIC_TYPE:ELEMENTAL'] = function ()
        local spell = gData.GetAction();
        return (spell ~= nil and spell.Skill ~= nil and spell.Skill == 'Elemental Magic');
        end
    ['MAGIC_TYPE:ENFEEBLING'] = function ()
        local spell = gData.GetAction();
        return (spell ~= nil and spell.Skill ~= nil and spell.Skill == 'Enfeebling Magic');
        end
    ['MAGIC_TYPE:ENHANCING'] = function ()
        local spell = gData.GetAction();
        return (spell ~= nil and spell.Skill ~= nil and spell.Skill == 'Enhancing Magic');
        end
    ['MAGIC_TYPE:HEALING'] = function ()
        local spell = gData.GetAction();
        return (spell ~= nil and spell.Skill ~= nil and spell.Skill == 'Healing Magic');
        end
    ['MAGIC_TYPE:NINJUTSU'] = function ()
        local spell = gData.GetAction();
        return (spell ~= nil and spell.Skill ~= nil and spell.Skill == 'Ninjutsu');
        end
    ['MAGIC_TYPE:SINGING'] = function ()
        local spell = gData.GetAction();
        return (spell ~= nil and spell.Skill ~= nil and spell.Skill == 'Singing');
        end
    ['MAGIC_TYPE:SUMMONING'] = function ()
        local spell = gData.GetAction();
        return (spell ~= nil and spell.Skill ~= nil and spell.Skill == 'Summoning');
        end
    -- Blue Magic Spell Type
    ['BLUE_MAGIC_TYPE:PHYSICAL'] = function ()
        local spell = gData.GetAction();
        return (spell ~= nil and spell.Name ~= nil and table.find(magic.tBLU_Spells['physical'],spell.Name:lower()) ~= nil);
        end
    ['BLUE_MAGIC_TYPE:MAGICAL'] = function ()
        local spell = gData.GetAction();
        return (spell ~= nil and spell.Name ~= nil and table.find(magic.tBLU_Spells['magical'],spell.Name:lower()) ~= nil);
        end
    ['BLUE_MAGIC_TYPE:MAGICAL_ELE'] = function ()
        local spell = gData.GetAction();
        return (spell ~= nil and spell.Name ~= nil and table.find(magic.tBLU_Spells['magical_ele'],spell.Name:lower()) ~= nil);
        end
    ['BLUE_MAGIC_TYPE:HEALING'] = function ()
        local spell = gData.GetAction();
        return (spell ~= nil and spell.Name ~= nil and table.find(magic.tBLU_Spells['healing'],spell.Name:lower()) ~= nil);
        end
    ['BLUE_MAGIC_TYPE:UTILITY'] = function ()
        local spell = gData.GetAction();
        return (spell ~= nil and spell.Name ~= nil and table.find(magic.tBLU_Spells['utility'],spell.Name:lower()) ~= nil);
        end
    --Song Type
    ['SONG_TYPE:AUBADE'] = function ()
        local spell = gData.GetAction();
        return (spell ~= nil and spell.Name ~= nil and table.find(magic.tSinging_Types['aubade'],spell.Name:lower()) ~= nil);
        end
    ['SONG_TYPE:BALLAD'] = function ()
        local spell = gData.GetAction();
        return (spell ~= nil and spell.Name ~= nil and table.find(magic.tSinging_Types['ballad'],spell.Name:lower()) ~= nil);
        end
    ['SONG_TYPE:CAPRICCIO'] = function ()
        local spell = gData.GetAction();
        return (spell ~= nil and spell.Name ~= nil and table.find(magic.tSinging_Types['capriccio'],spell.Name:lower()) ~= nil);
        end
    ['SONG_TYPE:CAROL'] = function ()
        local spell = gData.GetAction();
        return (spell ~= nil and spell.Name ~= nil and table.find(magic.tSinging_Types['carol'],spell.Name:lower()) ~= nil);
        end
    ['SONG_TYPE:ELEGY'] = function ()
        local spell = gData.GetAction();
        return (spell ~= nil and spell.Name ~= nil and table.find(magic.tSinging_Types['elegy'],spell.Name:lower()) ~= nil);
        end
    ['SONG_TYPE:ETUDE'] = function ()
        local spell = gData.GetAction();
        return (spell ~= nil and spell.Name ~= nil and table.find(magic.tSinging_Types['etude'],spell.Name:lower()) ~= nil);
        end
    ['SONG_TYPE:FANTASIA'] = function ()
        local spell = gData.GetAction();
        return (spell ~= nil and spell.Name ~= nil and table.find(magic.tSinging_Types['fantasia'],spell.Name:lower()) ~= nil);
        end
    ['SONG_TYPE:FINALI'] = function ()
        local spell = gData.GetAction();
        return (spell ~= nil and spell.Name ~= nil and table.find(magic.tSinging_Types['finali'],spell.Name:lower()) ~= nil);
        end
    ['SONG_TYPE:GAVOTTE'] = function ()
        local spell = gData.GetAction();
        return (spell ~= nil and spell.Name ~= nil and table.find(magic.tSinging_Types['gavotte'],spell.Name:lower()) ~= nil);
        end
    ['SONG_TYPE:HYMNUS'] = function ()
        local spell = gData.GetAction();
        return (spell ~= nil and spell.Name ~= nil and table.find(magic.tSinging_Types['hymnus'],spell.Name:lower()) ~= nil);
        end
    ['SONG_TYPE:LULLABY'] = function ()
        local spell = gData.GetAction();
        return (spell ~= nil and spell.Name ~= nil and table.find(magic.tSinging_Types['lullaby'],spell.Name:lower()) ~= nil);
        end
    ['SONG_TYPE:MADRIGAL'] = function ()
        local spell = gData.GetAction();
        return (spell ~= nil and spell.Name ~= nil and table.find(magic.tSinging_Types['madrigal'],spell.Name:lower()) ~= nil);
        end
    ['SONG_TYPE:MAMBO'] = function ()
        local spell = gData.GetAction();
        return (spell ~= nil and spell.Name ~= nil and table.find(magic.tSinging_Types['mambo'],spell.Name:lower()) ~= nil);
        end
    ['SONG_TYPE:MARCH'] = function ()
        local spell = gData.GetAction();
        return (spell ~= nil and spell.Name ~= nil and table.find(magic.tSinging_Types['march'],spell.Name:lower()) ~= nil);
        end
    ['SONG_TYPE:MAZURKA'] = function ()
        local spell = gData.GetAction();
        return (spell ~= nil and spell.Name ~= nil and table.find(magic.tSinging_Types['mazurka'],spell.Name:lower()) ~= nil);
        end
    ['SONG_TYPE:MINNE'] = function ()
        local spell = gData.GetAction();
        return (spell ~= nil and spell.Name ~= nil and table.find(magic.tSinging_Types['minne'],spell.Name:lower()) ~= nil);
        end
    ['SONG_TYPE:MINUET'] = function ()
        local spell = gData.GetAction();
        return (spell ~= nil and spell.Name ~= nil and table.find(magic.tSinging_Types['minuet'],spell.Name:lower()) ~= nil);
        end
    ['SONG_TYPE:OPERETTA'] = function ()
        local spell = gData.GetAction();
        return (spell ~= nil and spell.Name ~= nil and table.find(magic.tSinging_Types['operetta'],spell.Name:lower()) ~= nil);
        end
    ['SONG_TYPE:PAEON'] = function ()
        local spell = gData.GetAction();
        return (spell ~= nil and spell.Name ~= nil and table.find(magic.tSinging_Types['paeon'],spell.Name:lower()) ~= nil);
        end
    ['SONG_TYPE:PASTORAL'] = function ()
        local spell = gData.GetAction();
        return (spell ~= nil and spell.Name ~= nil and table.find(magic.tSinging_Types['pastoral'],spell.Name:lower()) ~= nil);
        end
    ['SONG_TYPE:PRELUDE'] = function ()
        local spell = gData.GetAction();
        return (spell ~= nil and spell.Name ~= nil and table.find(magic.tSinging_Types['prelude'],spell.Name:lower()) ~= nil);
        end
    ['SONG_TYPE:REQUIEM'] = function ()
        local spell = gData.GetAction();
        return (spell ~= nil and spell.Name ~= nil and table.find(magic.tSinging_Types['requiem'],spell.Name:lower()) ~= nil);
        end
    ['SONG_TYPE:ROUNDD'] = function ()
        local spell = gData.GetAction();
        return (spell ~= nil and spell.Name ~= nil and table.find(magic.tSinging_Types['round'],spell.Name:lower()) ~= nil);
        end
    ['SONG_TYPE:SIVENTE'] = function ()
        local spell = gData.GetAction();
        return (spell ~= nil and spell.Name ~= nil and table.find(magic.tSinging_Types['siventi'],spell.Name:lower()) ~= nil);
        end
    ['SONG_TYPE:THRENODY'] = function ()
        local spell = gData.GetAction();
        return (spell ~= nil and spell.Name ~= nil and table.find(magic.tSinging_Types['threnody'],spell.Name:lower()) ~= nil);
        end
    ['SONG_TYPE:VIRELAI'] = function ()
        local spell = gData.GetAction();
        return (spell ~= nil and spell.Name ~= nil and table.find(magic.tSinging_Types['virelai'],spell.Name:lower()) ~= nil);
        end
    --SMN
    ['JA:ASTRAL_FLOW'] = function ()
        local ability = gData.GetAction();
        return (ability ~= nil and ability.Name ~= nil and ability.Name = 'Astral Flow');
        end
    -- WAR
    ['JA:MIGHTY_STRIKES'] = function ()
        local ability = gData.GetAction();
        return (ability ~= nil and ability.Name ~= nil and ability.Name = 'Mighty Strikes');
        end
    ['JA:PROVOKE'] = function ()
        local ability = gData.GetAction();
        return (ability ~= nil and ability.Name ~= nil and ability.Name = 'Provoke');
        end
    ['JA:BERSERK'] = function ()
        local ability = gData.GetAction();
        return (ability ~= nil and ability.Name ~= nil and ability.Name = 'Berserk');
        end
    ['JA:DEFENDER'] = function ()
        local ability = gData.GetAction();
        return (ability ~= nil and ability.Name ~= nil and ability.Name = 'Defender');
        end
    ['JA:WARCRY'] = function ()
        local ability = gData.GetAction();
        return (ability ~= nil and ability.Name ~= nil and ability.Name = 'Warcry');
        end
    ['JA:AGGRESSOR'] = function ()
        local ability = gData.GetAction();
        return (ability ~= nil and ability.Name ~= nil and ability.Name = 'Aggressor');
        end
    ['JA:WARRIORS_CHARGE'] = function ()
        local ability = gData.GetAction();
        return (ability ~= nil and ability.Name ~= nil and ability.Name = 'Warriors Charge');
        end
    ['JA:TOMAHAWK'] = function ()
        local ability = gData.GetAction();
        return (ability ~= nil and ability.Name ~= nil and ability.Name = 'Tomahawk');
        end
    -- THF
    ['JA:PERFECT_DODGE'] = function ()
        local ability = gData.GetAction();
        return (ability ~= nil and ability.Name ~= nil and ability.Name = 'Perfect Dodge');
        end
    ['JA:STEAL'] = function ()
        local ability = gData.GetAction();
        return (ability ~= nil and ability.Name ~= nil and ability.Name = 'Steal');
        end
    ['JA:SNEAK_ATTACK'] = function ()
        local ability = gData.GetAction();
        return (ability ~= nil and ability.Name ~= nil and ability.Name = 'Sneak Attack');
        end
    ['JA:FLEE'] = function ()
        local ability = gData.GetAction();
        return (ability ~= nil and ability.Name ~= nil and ability.Name = 'Flee');
        end
    ['JA:TRICK_ATTACK'] = function ()
        local ability = gData.GetAction();
        return (ability ~= nil and ability.Name ~= nil and ability.Name = 'Trick Attack');
        end
    ['JA:MUG'] = function ()
        local ability = gData.GetAction();
        return (ability ~= nil and ability.Name ~= nil and ability.Name = 'Mug');
        end
    ['JA:BULLY'] = function ()
        local ability = gData.GetAction();
        return (ability ~= nil and ability.Name ~= nil and ability.Name = 'Bully');
        end
    ['JA:HIDE'] = function ()
        local ability = gData.GetAction();
        return (ability ~= nil and ability.Name ~= nil and ability.Name = 'Hide');
        end
    ['JA:ACCOMPLICE'] = function ()
        local ability = gData.GetAction();
        return (ability ~= nil and ability.Name ~= nil and ability.Name = 'Accomplice');
        end
    ['JA:COLLABORATOR'] = function ()
        local ability = gData.GetAction();
        return (ability ~= nil and ability.Name ~= nil and ability.Name = 'Collaborator');
        end
    ['JA:ASSASSINS_CHARGE'] = function ()
        local ability = gData.GetAction();
        return (ability ~= nil and ability.Name ~= nil and ability.Name = 'Assassins Charge');
        end
    ['JA:FEINT'] = function ()
        local ability = gData.GetAction();
        return (ability ~= nil and ability.Name ~= nil and ability.Name = 'Fient');
        end
    -- BLM
    ['JA:MANAFONT'] = function ()
        local ability = gData.GetAction();
        return (ability ~= nil and ability.Name ~= nil and ability.Name = 'Manafont');
        end
    ['JA:ELEMENTAL_SEAL'] = function ()
        local ability = gData.GetAction();
        return (ability ~= nil and ability.Name ~= nil and ability.Name = 'Elemental Seal');
        end
    -- MNK
    ['JA:HUNDRED_FISTS'] = function ()
        local ability = gData.GetAction();
        return (ability ~= nil and ability.Name ~= nil and ability.Name = 'Hundred Fists');
        end
    ['JA:BOOST'] = function ()
        local ability = gData.GetAction();
        return (ability ~= nil and ability.Name ~= nil and ability.Name = 'Boost');
        end
    ['JA:FOCUS'] = function ()
        local ability = gData.GetAction();
        return (ability ~= nil and ability.Name ~= nil and ability.Name = 'Focus');
        end
    ['JA:DODGE'] = function ()
        local ability = gData.GetAction();
        return (ability ~= nil and ability.Name ~= nil and ability.Name = 'Dodge');
        end
    ['JA:CHAKRA'] = function ()
        local ability = gData.GetAction();
        return (ability ~= nil and ability.Name ~= nil and ability.Name = 'Chakra');
        end
    ['JA:CHI_BLAST'] = function ()
        local ability = gData.GetAction();
        return (ability ~= nil and ability.Name ~= nil and ability.Name = 'Chi Blast');
        end
    ['JA:COUNTERSTANCE'] = function ()
        local ability = gData.GetAction();
        return (ability ~= nil and ability.Name ~= nil and ability.Name = 'Counterstance');
        end
    ['JA:MANTRA'] = function ()
        local ability = gData.GetAction();
        return (ability ~= nil and ability.Name ~= nil and ability.Name = 'Mantra');
        end
    ['JA:FORMLESS_STRIKES'] = function ()
        local ability = gData.GetAction();
        return (ability ~= nil and ability.Name ~= nil and ability.Name = 'Formless Strikes');
        end
    -- WHM
    ['JA:BENEDICTION'] = function ()
        local ability = gData.GetAction();
        return (ability ~= nil and ability.Name ~= nil and ability.Name = 'Benediction');
        end
    ['JA:DIVINE_SEAL'] = function ()
        local ability = gData.GetAction();
        return (ability ~= nil and ability.Name ~= nil and ability.Name = 'Divine Seal');
        end
    ['JA:DEVOTION'] = function ()
        local ability = gData.GetAction();
        return (ability ~= nil and ability.Name ~= nil and ability.Name = 'Devotion');
        end
    ['JA:MARTYR'] = function ()
        local ability = gData.GetAction();
        return (ability ~= nil and ability.Name ~= nil and ability.Name = 'Martyr');
        end
    ['JA:SANCTUARY'] = function ()
        local ability = gData.GetAction();
        return (ability ~= nil and ability.Name ~= nil and ability.Name = 'Sanctuary');
        end
    ['JA:FULL_CIRCLE'] = function ()
        local ability = gData.GetAction();
        return (ability ~= nil and ability.Name ~= nil and ability.Name = 'Full Circle');
        end
    -- RDM
    ['JA:CHAINSPELL'] = function ()
        local ability = gData.GetAction();
        return (ability ~= nil and ability.Name ~= nil and ability.Name = 'Chainspell');
        end
    ['JA:CONVERT'] = function ()
        local ability = gData.GetAction();
        return (ability ~= nil and ability.Name ~= nil and ability.Name = 'Convert');
        end
    -- PLD
    ['JA:INVINCIBLE'] = function ()
        local ability = gData.GetAction();
        return (ability ~= nil and ability.Name ~= nil and ability.Name = 'Invincible');
        end
    ['JA:HOLY_CIRCLE'] = function ()
        local ability = gData.GetAction();
        return (ability ~= nil and ability.Name ~= nil and ability.Name = 'Holy Circle');
        end
    ['JA:SHIELD_BASH'] = function ()
        local ability = gData.GetAction();
        return (ability ~= nil and ability.Name ~= nil and ability.Name = 'Shield Bash');
        end
    ['JA:SENTINEL'] = function ()
        local ability = gData.GetAction();
        return (ability ~= nil and ability.Name ~= nil and ability.Name = 'Sentinel');
        end
    ['JA:COVER'] = function ()
        local ability = gData.GetAction();
        return (ability ~= nil and ability.Name ~= nil and ability.Name = 'Cover');
        end
    ['JA:CHIVALRY'] = function ()
        local ability = gData.GetAction();
        return (ability ~= nil and ability.Name ~= nil and ability.Name = 'Chivalry');
        end
    ['JA:RAMPART'] = function ()
        local ability = gData.GetAction();
        return (ability ~= nil and ability.Name ~= nil and ability.Name = 'Rampart');
        end
    ['JA:FEALTY'] = function ()
        local ability = gData.GetAction();
        return (ability ~= nil and ability.Name ~= nil and ability.Name = 'Fealty');
        end
    ['JA:REPRISAL'] = function ()
        local ability = gData.GetAction();
        return (ability ~= nil and ability.Name ~= nil and ability.Name = 'Reprisal');
        end
    -- BST
    ['JA:FAMILIAR'] = function ()
        local ability = gData.GetAction();
        return (ability ~= nil and ability.Name ~= nil and ability.Name = 'Familiar');
        end
    ['JA:CHARM'] = function ()
        local ability = gData.GetAction();
        return (ability ~= nil and ability.Name ~= nil and ability.Name = 'Charm');
        end
    ['JA:GAUGE'] = function ()
        local ability = gData.GetAction();
        return (ability ~= nil and ability.Name ~= nil and ability.Name = 'Gauge');
        end
    ['JA:REWARD'] = function ()
        local ability = gData.GetAction();
        return (ability ~= nil and ability.Name ~= nil and ability.Name = 'Reward');
        end
    ['JA:CALL_BEAST'] = function ()
        local ability = gData.GetAction();
        return (ability ~= nil and ability.Name ~= nil and ability.Name = 'Call Beast');
        end
    ['JA:TAME'] = function ()
        local ability = gData.GetAction();
        return (ability ~= nil and ability.Name ~= nil and ability.Name = 'Tame');
        end
    ['JA:FERAL_HOWL'] = function ()
        local ability = gData.GetAction();
        return (ability ~= nil and ability.Name ~= nil and ability.Name = 'Feral Howl');
        end
    ['JA:KILLER_INSTINCT'] = function ()
        local ability = gData.GetAction();
        return (ability ~= nil and ability.Name ~= nil and ability.Name = 'Killer Instinct');
        end
    -- RNG
    ['JA:EAGLE_EYE_SHOT'] = function ()
        local ability = gData.GetAction();
        return (ability ~= nil and ability.Name ~= nil and ability.Name = 'Eagle Eye Shot');
        end
    ['JA:SHARPSHOT'] = function ()
        local ability = gData.GetAction();
        return (ability ~= nil and ability.Name ~= nil and ability.Name = 'Sharpshot');
        end
    ['JA:SCAVENGE'] = function ()
        local ability = gData.GetAction();
        return (ability ~= nil and ability.Name ~= nil and ability.Name = 'Scavenge');
        end
    ['JA:CAMOUFLAGE'] = function ()
        local ability = gData.GetAction();
        return (ability ~= nil and ability.Name ~= nil and ability.Name = 'Camouflage');
        end
    ['JA:BARRAGE'] = function ()
        local ability = gData.GetAction();
        return (ability ~= nil and ability.Name ~= nil and ability.Name = 'Barrage');
        end
    ['JA:SHADOWBIND'] = function ()
        local ability = gData.GetAction();
        return (ability ~= nil and ability.Name ~= nil and ability.Name = 'Shadowbind');
        end
    ['JA:VELOCITY_SHOT'] = function ()
        local ability = gData.GetAction();
        return (ability ~= nil and ability.Name ~= nil and ability.Name = 'Velocity Shot');
        end
    ['JA:UNLIMITED_SHOT'] = function ()
        local ability = gData.GetAction();
        return (ability ~= nil and ability.Name ~= nil and ability.Name = 'Unlimited Shot');
        end
    ['JA:FLASHY_SHOT'] = function ()
        local ability = gData.GetAction();
        return (ability ~= nil and ability.Name ~= nil and ability.Name = 'Flashy Shot');
        end
    ['JA:STEALTH_SHOT'] = function ()
        local ability = gData.GetAction();
        return (ability ~= nil and ability.Name ~= nil and ability.Name = 'Stealth Shot');
        end
    -- NIN
    ['JA:MIKAGE'] = function ()
        local ability = gData.GetAction();
        return (ability ~= nil and ability.Name ~= nil and ability.Name = 'Mikage');
        end
    ['JA:YONIN'] = function ()
        local ability = gData.GetAction();
        return (ability ~= nil and ability.Name ~= nil and ability.Name = 'Yonin');
        end
    ['JA:INNIN'] = function ()
        local ability = gData.GetAction();
        return (ability ~= nil and ability.Name ~= nil and ability.Name = 'Innin');
        end
    -- DRK
    ['JA:BLOOD_WEAPON'] = function ()
        local ability = gData.GetAction();
        return (ability ~= nil and ability.Name ~= nil and ability.Name = 'Blood Weapon');
        end
    ['JA:ARCANE_CIRCLE'] = function ()
        local ability = gData.GetAction();
        return (ability ~= nil and ability.Name ~= nil and ability.Name = 'Arcane Circle');
        end
    ['JA:LAST_RESORT'] = function ()
        local ability = gData.GetAction();
        return (ability ~= nil and ability.Name ~= nil and ability.Name = 'Last Resort');
        end
    ['JA:WEAPON_BASH'] = function ()
        local ability = gData.GetAction();
        return (ability ~= nil and ability.Name ~= nil and ability.Name = 'Weapon Bash');
        end
    ['JA:SOULEATER'] = function ()
        local ability = gData.GetAction();
        return (ability ~= nil and ability.Name ~= nil and ability.Name = 'Souleater');
        end
    ['JA:DARK_SEAL'] = function ()
        local ability = gData.GetAction();
        return (ability ~= nil and ability.Name ~= nil and ability.Name = 'Dark Seal');
        end
    ['JA:DIABOLIC_EYE'] = function ()
        local ability = gData.GetAction();
        return (ability ~= nil and ability.Name ~= nil and ability.Name = 'Diabolic Eye');
        end
    -- BRD
    ['JA:SOUL_VOICE'] = function ()
        local ability = gData.GetAction();
        return (ability ~= nil and ability.Name ~= nil and ability.Name = 'Soul Voice');
        end
    ['JA:NIGHTINGALE'] = function ()
        local ability = gData.GetAction();
        return (ability ~= nil and ability.Name ~= nil and ability.Name = 'Nightingale');
        end
    ['JA:TROUBADOUR'] = function ()
        local ability = gData.GetAction();
        return (ability ~= nil and ability.Name ~= nil and ability.Name = 'Troubadour');
        end
    -- SAM
    ['JA:MEIKYO_SHISUI'] = function ()
        local ability = gData.GetAction();
        return (ability ~= nil and ability.Name ~= nil and ability.Name = 'Meikyo Shisui');
        end
    ['JA:WARDING_CIRCLE'] = function ()
        local ability = gData.GetAction();
        return (ability ~= nil and ability.Name ~= nil and ability.Name = 'Warding Circle');
        end
    ['JA:THIRD_EYE'] = function ()
        local ability = gData.GetAction();
        return (ability ~= nil and ability.Name ~= nil and ability.Name = 'Third Eye');
        end
    ['JA:HASSO'] = function ()
        local ability = gData.GetAction();
        return (ability ~= nil and ability.Name ~= nil and ability.Name = 'Hasso');
        end
    ['JA:MEDITATE'] = function ()
        local ability = gData.GetAction();
        return (ability ~= nil and ability.Name ~= nil and ability.Name = 'Meditate');
        end
    ['JA:SEIGAN'] = function ()
        local ability = gData.GetAction();
        return (ability ~= nil and ability.Name ~= nil and ability.Name = 'Seigan');
        end
    ['JA:SHIKIKOYO'] = function ()
        local ability = gData.GetAction();
        return (ability ~= nil and ability.Name ~= nil and ability.Name = 'Shikikoyo');
        end
    --DRG
    ['JA:SPIRIT_SURGE'] = function ()
        local ability = gData.GetAction();
        return (ability ~= nil and ability.Name ~= nil and ability.Name = 'Spirit Surge');
        end
    ['JA:CALL_WYVERN'] = function ()
        local ability = gData.GetAction();
        return (ability ~= nil and ability.Name ~= nil and ability.Name = 'Call Wyvern');
        end
    ['JA:ANCIENT_CIRCLE'] = function ()
        local ability = gData.GetAction();
        return (ability ~= nil and ability.Name ~= nil and ability.Name = 'Ancient Circle');
        end
    ['JA:JUMP'] = function ()
        local ability = gData.GetAction();
        return (ability ~= nil and ability.Name ~= nil and ability.Name = 'Jump');
        end
    ['JA:SPIRIT_LINK'] = function ()
        local ability = gData.GetAction();
        return (ability ~= nil and ability.Name ~= nil and ability.Name = 'Spirit Link');
        end
    ['JA:HIGH_JUMP'] = function ()
        local ability = gData.GetAction();
        return (ability ~= nil and ability.Name ~= nil and ability.Name = 'High Jump');
        end
    ['JA:SUPER_JUMP'] = function ()
        local ability = gData.GetAction();
        return (ability ~= nil and ability.Name ~= nil and ability.Name = 'Super Jump');
        end
    ['JA:DEEP_BREATHING'] = function ()
        local ability = gData.GetAction();
        return (ability ~= nil and ability.Name ~= nil and ability.Name = 'Deep Breathing');
        end
    ['JA:ANGON'] = function ()
        local ability = gData.GetAction();
        return (ability ~= nil and ability.Name ~= nil and ability.Name = 'Angon');
        end
    -- COR
    ['JA:WILD_CARD'] = function ()
        local ability = gData.GetAction();
        return (ability ~= nil and ability.Name ~= nil and ability.Name = 'Wild Card');
        end
    ['JA:PHANTOM_ROLL'] = function ()
        local ability = gData.GetAction();
        return (ability ~= nil and ability.Name ~= nil and ability.Name = 'Phantom Roll');
        end
    ['JA:DOUBLE_UP'] = function ()
        local ability = gData.GetAction();
        return (ability ~= nil and ability.Name ~= nil and ability.Name = 'Double Up');
        end
    ['JA:QUICK_DRAW'] = function ()
        local ability = gData.GetAction();
        return (ability ~= nil and ability.Name ~= nil and ability.Name = 'Quick Draw');
        end
    ['JA:RANDOM_DEAL'] = function ()
        local ability = gData.GetAction();
        return (ability ~= nil and ability.Name ~= nil and ability.Name = 'Random Deal');
        end
    ['JA:SNAKE_EYE'] = function ()
        local ability = gData.GetAction();
        return (ability ~= nil and ability.Name ~= nil and ability.Name = 'Snake Eye');
        end
    ['JA:Fold'] = function ()
        local ability = gData.GetAction();
        return (ability ~= nil and ability.Name ~= nil and ability.Name = 'Fold');
        end
    -- BLU
    ['JA:AZURE_LORE'] = function ()
        local ability = gData.GetAction();
        return (ability ~= nil and ability.Name ~= nil and ability.Name = 'Azure Lore');
        end
    ['JA:BURST_AFFINITY'] = function ()
        local ability = gData.GetAction();
        return (ability ~= nil and ability.Name ~= nil and ability.Name = 'Burst Affinity');
        end
    ['JA:CHAIN_AFFINITY'] = function ()
        local ability = gData.GetAction();
        return (ability ~= nil and ability.Name ~= nil and ability.Name = 'Chain Affinity');
        end
    ['JA:CONVERGENCE'] = function ()
        local ability = gData.GetAction();
        return (ability ~= nil and ability.Name ~= nil and ability.Name = 'Convergence');
        end
    ['JA:DIFFUSION'] = function ()
        local ability = gData.GetAction();
        return (ability ~= nil and ability.Name ~= nil and ability.Name = 'Diffusion');
        end
    -- PUP
    ['JA:OVERDRIVE'] = function ()
        local ability = gData.GetAction();
        return (ability ~= nil and ability.Name ~= nil and ability.Name = 'Overdrive');
        end
    ['JA:ACTIVATE'] = function ()
        local ability = gData.GetAction();
        return (ability ~= nil and ability.Name ~= nil and ability.Name = 'Activate');
        end
    ['JA:DEUS_EX_AUTOMATA'] = function ()
        local ability = gData.GetAction();
        return (ability ~= nil and ability.Name ~= nil and ability.Name = 'Deus Ex Automata');
        end
    ['JA:REPAIR'] = function ()
        local ability = gData.GetAction();
        return (ability ~= nil and ability.Name ~= nil and ability.Name = 'Repair');
        end
    ['JA:MAINTENANCE'] = function ()
        local ability = gData.GetAction();
        return (ability ~= nil and ability.Name ~= nil and ability.Name = 'Maintenance');
        end
    ['JA:ROLE_REVERSAL'] = function ()
        local ability = gData.GetAction();
        return (ability ~= nil and ability.Name ~= nil and ability.Name = 'Role Reversal');
        end
    ['JA:VENTRILOQUY'] = function ()
        local ability = gData.GetAction();
        return (ability ~= nil and ability.Name ~= nil and ability.Name = 'Ventriloquy');
        end
    -- Pet commands
    ['PET'] = function ()
        local pet = gData.GetPet();
        return (pet ~= nil);
        end
    ['NOT_PET'] = function ()
        local pet = gData.GetPet();
        return (pet=~= nil);
        end
    ['PETF'] = function ()
        local pet = gData.GetPet();
        return (pet ~= nil and pet.Status ~= nil and pet.Status == 'Engaged');
        end
    ['NOT_PETF'] = function ()
        local pet = gData.GetPet();
        return not (pet ~= nil and pet.Status ~= nil and pet.Status == 'Engaged');
        end
    ['PETFNPF'] = function ()
        local player = gData.GetPlayer();
        local pet = gData.GetPet();
        return (pet ~= nil and pet.Status ~= nil and pet.Status == 'Engaged' and player ~= nil and player.Status ~= nil and player.Status ~= 'Engaged' );
        end
    ['NOT_PETFNPF'] = function ()
        local player = gData.GetPlayer();
        local pet = gData.GetPet();
        return not (pet ~= nil and pet.Status ~= nil and pet.Status == 'Engaged' and player ~= nil and player.Status ~= nil and player.Status ~= 'Engaged' );
        end
    --SMN pet conditionals
    ['SMN:AVATAR'] = function ()
        local spell = gData.GetAction();
        return (spell ~= nil and spell.Name ~= nil and table.find(gVars.tSpellGroupings['avatars'],spell.Name:lower()) ~= nil));
        end
    ['NOT_SMN:AVATAR'] = function ()
        local spell = gData.GetAction();
        return (spell ~= nil and spell.Name ~= nil and table.find(gVars.tSpellGroupings['spirits'],spell.Name:lower()) ~= nil));
        end
    ['SMN:SPIRIT'] = function ()
        local spell = gData.GetAction();
        return (spell ~= nil and spell.Name ~= nil and table.find(gVars.tSpellGroupings['spirits'],spell.Name:lower()) ~= nil));
        end
    ['NOT_SMN:SPIRIT'] = function ()
        local spell = gData.GetAction();
        return (spell ~= nil and spell.Name ~= nil and table.find(gVars.tSpellGroupings['avatars'],spell.Name:lower()) ~= nil));
        end
    ['SMN:SUMMONS'] = function ()
        local spell = gData.GetAction();
        return (spell ~= nil and spell.Name ~= nil and (table.find(gVars.tSpellGroupings['avatars'],spell.Name:lower()) ~= nil or
                    table.find(gVars.tSpellGroupings['spirits'],spell.Name:lower()) ~= nil));
        end
    ['NOT_SMN:SUMMONS'] = function ()
        local spell = gData.GetAction();
        return (spell ~= nil and spell.Name ~= nil and table.find(gVars.tSpellGroupings['avatars'],spell.Name:lower()) == nil and
                    table.find(gVars.tSpellGroupings['spirits'],spell.Name:lower()) == nil);
        end
    ['SMN:PET:AVATAR'] = function ()
        local pet = gData.GetPet();
        return (pet ~= nil and pet.Name ~= nil and table.find(gVars.tSpellGrouping['avatars'],pet.Name:lower()) ~= nil);
        end
    ['NOT_SMN:PET:AVATAR'] = function ()
        local pet = gData.GetPet();
        return (pet ~= nil and pet.Name ~= nil and table.find(gVars.tSpellGrouping['avatars'],pet.Name:lower()) == nil);
        end
    ['SMN:PET:SPIRIT'] = function ()
        local pet = gData.GetPet();
        return (pet ~= nil and pet.Name ~= nil and table.find(gVars.tSpellGrouping['spirits'],pet.Name:lower()) ~= nil);
        end
    ['NOT_SMN:PET:SPIRIT'] = function ()
        local pet = gData.GetPet();
        return (pet ~= nil and pet.Name ~= nil and table.find(gVars.tSpellGrouping['spirits'],pet.Name:lower()) == nil);
        end
    ['SMN:PETMW'] = function ()
        local environ = gData.GetEnvironment();
        local pet = gData.GetPet();
        local ele = pets.fElementByPetName(pet.Name);
        return (pet ~= nil and ((environ.RawWeather == nil and ele == nil) or (environ.RawWeather ~= nil and ele ~= nil and string.find(environ.RawWeather:lower(),ele) ~= nil)));
        end
    ['NOT_SMN:PETMW'] = function ()
        local environ = gData.GetEnvironment();
        local pet = gData.GetPet();
        local ele = pets.fElementByPetName(pet.Name);
        return not (pet ~= nil and ((environ.RawWeather == nil and ele == nil) or (environ.RawWeather ~= nil and ele ~= nil and string.find(environ.RawWeather:lower(),ele) ~= nil)));
        end
    ['SMN:PETMD'] = function ()
        local environ = gData.GetEnvironment();
        local pet = gData.GetPet();
        local ele = pets.fElementByPetName(pet.Name);
        return (pet ~= nil and ele ~= nil and environ ~= nil and environ.DayElement:lower() == ele);
        end
    ['NOT_SMN:PETMD'] = function ()
        local environ = gData.GetEnvironment();
        local pet = gData.GetPet();
        local ele = pets.fElementByPetName(pet.Name);
        return not (pet ~= nil and ele ~= nil and environ ~= nil and environ.DayElement:lower() == ele);
        end
    ['SMN:PET'] = function ()
        return (pets.fPetType() == gVars._TYPE_SMN);
        end
    ['NOT_SMN:PET'] = function ()
        return (pets.fPetType() ~= gVars._TYPE_SMN);
        end
    ['BST:PET'] = function ()
        return (pets.fPetType() == gVars._TYPE_BST);
        end
    ['NOT_BST:PET'] = function ()
        return (pets.fPetType() ~= gVars._TYPE_BST);
        end
    ['DRG:PET'] = function ()
        return (pets.fPetType() == gVars._TYPE_DRG);
        end
    ['NOT_DRG:PET'] = function ()
        return (pets.fPetType() ~= gVars._TYPE_DRG);
        end
    ['PUP:PET'] = function ()
        return (pets.fPetType() == gVars._TYPE_PUP);
        end
    ['NOT_PUP:PET'] = function ()
        return (pets.fPetType() ~= gVars._TYPE_PUP);
        end
    -- Magical
    ['MAGICAL_MJ'] = function ()
        return utilities.fMagicalMainJob();
        end
    ['NOT_MAGICAL_MJ'] = function ()
        return not utilities.fMagicalMainJob();
        end
    ['MAGICAL_SJ'] = function ()
        return utilities.fMagicalSubJob();
        end
    ['NOT_MAGICAL_SJ'] = function ()
        return not utilities.fMagicalSubJob();
        end
    -- The following cannot avoid parameters
    ['BUFF'] = function (sList)
        return buff_manager.has(sList,false,false);
        end
    ['NOT_BUFF'] = function (sList)
        return buff_manager.has(sList,false,true);
        end
    ['BUFF_ALL'] = function (sList)
        return buff_manager.has(sList,true,false);
        end
    ['BUFFED_OT'] = function (sList)
        return buff_manager.other_than(sList);
        end
    ['DEBUFFED_OT'] = function (sList)
        return buff_manager.other_than(sList);
        end
    ['MJ'] = function (sList)
        player = gData.GetPlayer();
        return (string.find(slist:upper(),player.MainJob) ~= nil);
        end
    ['NOT_MJ'] = function (sList)
        player = gData.GetPlayer();
        return not (string.find(slist:upper(),player.MainJob()) ~= nil);
        end
    ['SJ'] = function (sList)
        player = gData.GetPlayer();
        return (string.find(slist:upper(),player.SubJob) ~= nil);
        end
    ['NOT_SJ'] = function (sList)
        player = gData.GetPlayer();
        return not (string.find(slist:upper(),player.SubJob) ~= nil);
        end
    ['PARTY_JOB'] = function (sList)
        return utilities.fJobInParty(sList,false);
        end
    ['NOT_PARTY_JOB'] = function (sList)
        return not utilities.fJobInParty(sList,false);
        end
    ['PARTY_JOB_NM'] = function (sList)
        return utilities.fJobInParty(sList,true);
        end
    ['TIME'] = function (sList)
        return utilities.fCheckTimeList(sList:upper());
        end
    ['NOT_TIME'] = function (sList)
        return not utilities.fCheckTimeList(sList:upper());
        end
    ['MOONPHASE'] = function (sList)
        local environ = gData.GetEnvironment();
        return (string.find(sList:upper(), environ.MoonPhase:upper()) ~= nil);
        end
    ['MOONPHASE'] = function (sList)
        local environ = gData.GetEnvironment();
        return not (string.find(sList:upper(), environ.MoonPhase:upper()) ~= nil);
        end
    ['DAY'] = function (sList)
        local environ = gData.GetEnvironment();
        return (string.find(sList:upper(), environ.Day:upper()) ~= nil);
        end
    ['NOT_DAY'] = function (sList)
        local environ = gData.GetEnvironment();
        return not (string.find(sList:upper(), environ.Day:upper()) ~= nil);
        end
    ['SPECIAL'] = function (sSlot,sGear)
        return gear.fValidateSpecial(sSlot,sGear);      -- Needs to be rewritten
        end
    ['PARTY'] = function (val)
        val = tonumber(val) or 1;
        local bAlliance = (val > 6);
        return (utilities.fGetPartyCount(bAlliance) == val);
        end
    ['NOT_PARTY'] = function (val)
        val = tonumber(val) or 1;
        local bAlliance = (val > 6);
        return (utilities.fGetPartyCount(bAlliance) ~= val);
        end
    ['PR'] = function (sList)
        return (string.find(sList:upper(),gVars.PhantomRoll:upper()) ~= nil);
        end
    ['NOT_PR'] = function (sList)
        return (string.find(sList:upper(),gVars.PhantomRoll:upper()) == nil);
        end
    ['EMPTY'] = function (sSlot)
        local ts = crossjobs.Sets.CurrentGear;
        sSlot = utilities.fFormattedWord(sSlot,gVars._SLOT_FA);

        if sSlot == 'Ears' then
            return (ts['Ear1'] == nil or ts['Ear1'] == "" or ts['Ear2'] == nil or ts['Ear2'] == "");
        elseif sSlot == 'Rings' then
            return (ts['Ring1'] == nil or ts['Ring1'] == "" or ts['Ring2'] == nil or ts['Ring'] == "");
        else
            return (ts[sSlot] == nil or ts[sSlot] == "");
        end
        end
    ['NOT_EMPTY'] = function (sSlot)
        local ts = crossjobs.Sets.CurrentGear;
        sSlot = utilities.fFormattedWord(sSlot,gVars._SLOT_FA);

        if sSlot == 'Ears' then
            return not (ts['Ear1'] == nil or ts['Ear1'] == "" or ts['Ear2'] == nil or ts['Ear2'] == "");
        elseif sSlot == 'Rings' then
            return not (ts['Ring1'] == nil or ts['Ring1'] == "" or ts['Ring2'] == nil or ts['Ring'] == "");
        else
            return not (ts[sSlot] == nil or ts[sSlot] == "");
        end
        end
    ['SPELL'] = function (sList)
        local spell = gData.GetAction();
        if spell == nil or spell.Name == nil then
            return false
        else
            return (string.find(sList:upper(), spell.Name:upper()) ~= nil);
        end
        end
    ['NOT_SPELL'] = function (sList)
        local spell = gData.GetAction();
        if spell == nil or spell.Name == nil then
            return true
        else
            return (string.find(sList:upper(), spell.Name:upper()) == nil);
        end
        end
    ['SPELL_ROOT'] = function (sList)
        local spell = gData.GetAction();
        if spell == nil or spell.Name == nil then
            return false;
        end
        local root = utilities.fGetRoot(spell.Name);
        return (string.find(sList:lower(),root) ~= nil);
        end
    ['NOT_SPELL_ROOT'] = function (sList)
        local spell = gData.GetAction();
        if spell == nil or spell.Name == nil then
            return true;
        end
        local root = utilities.fGetRoot(spell.Name);
        return (string.find(sList:lower(),root) == nil);
        end
    ['SPELL_SUB'] = function (sList)
        local spell = gData.GetAction();
        if spell == nil or spell.Name == nil then
            return false;
        end
        return (string.find(spell.Name:lower,sList:lower()) ~= nil);
        end
    ['NOT_SPELL_SUB'] = function (sList)
        local spell = gData.GetAction();
        if spell == nil or spell.Name == nil then
            return true;
        end
        return (string.find(spell.Name:lower,sList:lower()) == nil);
        end
    ['SONG'] = function (sList)
        local song = gData.GetAction();
        if song == nil or song.Name == nil then
            return false
        else
            return (string.find(sList:upper(), song.Name:upper()) ~= nil);
        end
        end
    ['NOT_SONG'] = function (sList)
        local song = gData.GetAction();
        if song == nil or song.Name == nil then
            return true
        else
            return (string.find(sList:upper(), song.Name:upper()) == nil);
        end
        end
    ['SONG_ROOT'] = function (sList)
        local song = gData.GetAction();
        if song == nil or song.Name == nil then
            return false;
        end
        local root = utilities.fGetRoot(song.Name);
        return (string.find(sList:lower(),root) ~= nil);
        end
    ['NOT_SONG_ROOT'] = function (sList)
        local song = gData.GetAction();
        if song == nil or song.Name == nil then
            return true;
        end
        local root = utilities.fGetRoot(song.Name);
        return (string.find(sList:lower(),root) == nil);
        end
    ['SONG_SUB'] = function (sList)
        local song = gData.GetAction();
        if song == nil or song.Name == nil then
            return false;
        end
        return (string.find(song.Name:lower,sList:lower()) ~= nil);
        end
    ['NOT_SONG_SUB'] = function (sList)
        local song = gData.GetAction();
        if song == nil or song.Name == nil then
            return true;
        end
        return (string.find(song.Name:lower,sList:lower()) == nil);
        end
    ['IF'] = function (sSlot,sGear)
        local ts = gData.GetCurrentSet();
        sSlot = utilities.fFormattedWord(sSlot,gVars._SLOT_FA);
        sGear = sGear:lower();

        if sSlot == 'Rings' then
            return (ts['Ring1']:lower() == sGear or ts['Ring2']:lower() == sGear);
        elseif sSlot == 'Ears' then
            return (ts['Ear1']:lower() == sGear or ts['Ear2']:lower() == sGear);
        else
            return (ts[sSlot]:lower() == sGear);
        end
        end
    ['NOT_IF'] = function (sSlot,sGear)
        local ts = gData.GetCurrentSet();
        sSlot = utilities.fFormattedWord(sSlot,gVars._SLOT_FA);
        sGear = sGear:lower();

        if sSlot == 'Rings' then
            return not (ts['Ring1']:lower() == sGear or ts['Ring2']:lower() == sGear);
        elseif sSlot == 'Ears' then
            return not (ts['Ear1']:lower() == sGear or ts['Ear2']:lower() == sGear);
        else
            return not (ts[sSlot]:lower() == sGear);
        end
        end
    ['IF-'] = function (sSlot,sGear)        -- Functionally same as //IF, calling routine affects where sSlot comes from
        local ts = gData.GetCurrentSet();
        sSlot = utilities.fFormattedWord(sSlot,gVars._SLOT_FA);
        sGear = sGear:lower();

        if sSlot == 'Rings' then
            return (ts['Ring1']:lower() == sGear or ts['Ring2']:lower() == sGear);
        elseif sSlot == 'Ears' then
            return (ts['Ear1']:lower() == sGear or ts['Ear2']:lower() == sGear);
        else
            return (ts[sSlot]:lower() == sGear);
        end
        end
    ['NOT_IF-'] = function (sSlot,sGear)    -- Functionally same as //NOT_IF, calling routine affects where sSlot comes from
        local ts = gData.GetCurrentSet();
        sSlot = utilities.fFormattedWord(sSlot,gVars._SLOT_FA);
        sGear = sGear:lower();

        if sSlot == 'Rings' then
            return not (ts['Ring1']:lower() == sGear or ts['Ring2']:lower() == sGear);
        elseif sSlot == 'Ears' then
            return not (ts['Ear1']:lower() == sGear or ts['Ear2']:lower() == sGear);
        else
            return not (ts[sSlot]:lower() == sGear);
        end
        end
    -- more goes here. This was just an example
};

--[[
    validate_town_ak determines if the passed piece of gear matches the appropriate location
--]]

local function validate_town_ak (sGear)
    local pNation = AshitaCore:GetMemoryManager():GetPlayer():GetNation();
    local sGear = sGear:lower();

    if sGear == 'ducal aketon' then
        return (environ.Area ~= nil and
            (table.find(gVars.tTownAreas['Windy'],environ.Area) ~= nil or
             table.find(gVars.tTownAreas['Sandy'],environ.Area) ~= nil or
             table.find(gVars.tTownAreas['Bastok'],environ.Area) ~= nil or
             table.find(gVars.tTownAreas['Jeuno'],environ.Area) ~= nil)
            );
    elseif sGear == 'federation aketon' then
        if environ.Area ~= nil and table.find(gVars.tTownAreas['Windy'],environ.Area) ~= nil then
            return (pNation == 2);  -- Windy
        end
    elseif sGear == 'republic aketon' then
        if environ.Area ~= nil and table.find(gVars.tTownAreas['Bastok'],environ.Area) == nil then
            return (pNation == 1);  -- Bastok
        end
    elseif sGear == 'kingdom aketon' then
        if environ.Area ~= nil and table.find(gVars.tTownAreas['Sandy'],environ.Area) == nil then
            return (pNation == 0);  -- Sandy
        end
    else
        utilities.DisplayOnce('Warning: Invalid body piece for national aketon check: ' .. sGear,false);
        return false;
    end
end     -- validate_town_ak

--[[
    validate_Empty determines if the specified slot is empty.

    Parameters:
        sCode   Type of "empty" to search
        sSLot   slot to checked
        bNot    Should the results be inverted
--]]

local function validate_Empty(sCode,sSlot,bNot)
    local ts = crossjobs.Sets.CurrentGear;
    local sSlot = utilities.fFormattedWord(sSlot,gVars._SLOT_FA);
    local bNot = bNot or false;
    local bGood = false;

    if sCode == 'EMPTY' then
        if sSlot == 'Ears' then
            bGood = (ts['Ears1'] == nil or ts['Ears1'] == "" or ts['Ears2'] == nil or ts['Ears2'] == "");
        elseif sSlot == 'Rings' then
            bGood = (ts['Rings1'] == nil or ts['Rings1'] == "" or ts['Rings2'] == nil or ts['Ring2'] == "");
        elseif string.find(gVars.tSlotNames['standard'],sSlot:lower()) ~= nil then
            bGood = (ts[sSlot)] == nil or ts[sSLOT] == "");
        else
            reporting.DisplayOnce('Warning: Invalid slot designated for //EMPTY: ' .. sSlot,false);
            return false
        end
    elseif sCode == 'EMPTY_1' then
        if string.find('ears,ear1',sSlot:lower() then
            bGood = (ts['Ears1'] == nil or ts['Ears1'] == "");
        elseif string.find('rings,ring1',sSlot:lower() then
            bGood = (ts['Ring1'] == nil or ts['Ring1'] == "");
        else
            reporting.DisplayOnce('Warning: Invalid slot designated for //EMPTY_1: ' .. sSlot,false);
            return false
        end
    else    -- Has to be EMPTY_2
        if string.find('ears,ear2',sSlot:lower() then
            bGood = (ts['Ears2'] == nil or ts['Ears2'] == "");
        elseif string.find('rings,ring2',sSlot:lower() then
            bGood = (ts['Ring2'] == nil or ts['Ring2'] == "");
        else
            reporting.DisplayOnce('Warning: Invalid slot designated for //EMPTY_2: ' .. sSlot,false);
        return false
        end
    end

    if bNot then
        gGood = not bGood;
    end

    return bGood;
end     -- validate_empty

--[[
    prescan_conditionals scans the passed list of conditional codes and copies the appropriate
    conditional definition into the master table.

    Parameters
        list    list of one or more conditionals (either comma delimited list or a table
        slot    slot list applies to
        gsname  gear set name
--]]

function preprocess.prescan_conditionals(list,slot,gsname)
    local tList = {};
    local bFound = false;
    gsname = gsname or "composite";

    if list == nil or list == "" then
        return;
    end

    list = list.upper();

    -- We want the conditionals in a table
    if type(list) == "string" then
        tList = utilities.fSplitStringByDelimiter(list,',');
    elseif type(list) == "table" then
        tList = list;
    else
        local smsg = 'Warning: invalid conditional list: ' .. tostring(list) .. ' specified in ' .. gsname .. ' for slot ' .. slot;
        reporting.DisplayMessage(nil,smsg,nil);
        return;
    end

    for _,j in pairs(tList) do
        bFound = false;
        -- Start with the actual jump table. If there, move to next conditional
        if preprocess.jump_table[j] == nil then
            -- Ok, not there. Let's see if in the template jump table. Start with the conditional being the index
            if preprocess.jump_table_template[j] ~= nil then
                -- Found it. Just copy it over to the jump table
                preprocess.jump_table[j] = preprocess.jump_table_template[j];
                bFound = true;
            end
        else
            bFound = true;
        end

        if not bFound then
            -- It wasn't found. Try splitting on the colon
            local iPos = string.find(j,':');
            if iPos ~= nil then
                local skw = string.sub(j,1,iPos-1);
                -- Colon found. Is the left side a valid keyword index? Check the jump table first
                if preprocess.jump_table[skw] ~= nil then
                    bFound = true;
                else
                    -- Doesn't exist. See if in the template table
                    if preprocess.jump_table_template[skw] ~= nil then
                        -- There it is. Copy to the jump table
                        preprocess.jump_table[skw] = preprocess.jump_table_template[skw];
                        bFound = true;
                    end
                end
            end
        end

        if not bFound then
            -- Not found. Let's look to see what else I can work with (!!! more goes here)
        end
    end
end     -- prescan_conditionals

return preprocess;
