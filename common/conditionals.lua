local conditionals {};

--[[
    This component contains all routines that deal with processing inline conditionals

    List of routines-
        Subroutines:

        Functions:
            fCheckConditional            Determines if the passed conditional comparison is true
            fCheckIF                     Determines if IF conditional is true
            fCheckWeaponType             Determines if equipped weapon is of the passed type
            fCompareIt                   Determines if the comparison is true
            fValidateTownAK              Determines if a kingdom aketon should be worn and which one
            fValidateEmpty               Determines if the specified slot is empty
--]]

local jump_table = {
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
        local party = AshitaCore:GetMemoryManager():GetParty()
        return (party:GetMemberActive(0);
        end
    ['NOT_INPARTY'] = function ()
        local party = AshitaCore:GetMemoryManager():GetParty()
        return not (party:GetMemberActive(0);
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
        return fValidateTownAK(sGear);
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
    ['EVASION'] = function ()
        return utilities.fGetToggle(gVars._EVASION);
        end
    ['NOT_EVASION'] = function ()
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
    ['GSWAP'] = function ()
        return utilities.fGetToggle(gVars._GSWAP);
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
    ['NOT_STATUS:IDLING'] = function ()
        local player = gData.GetPlayer();
        return (player.Status ~= 'Idling');
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
    ['BLUE_SPELL_TYPE:PHYSICAL'] = function ()
        local spell = gData.GetAction();
        return (spell ~= nil and spell.Name ~= nil and table.find(magic.tBLU_Spells['physical'],spell.Name:lower()) ~= nil);
        end
    ['BLUE_SPELL_TYPE:MAGICAL'] = function ()
        local spell = gData.GetAction();
        return (spell ~= nil and spell.Name ~= nil and table.find(magic.tBLU_Spells['magical'],spell.Name:lower()) ~= nil);
        end
    ['BLUE_SPELL_TYPE:MAGICAL_ELE'] = function ()
        local spell = gData.GetAction();
        return (spell ~= nil and spell.Name ~= nil and table.find(magic.tBLU_Spells['magical_ele'],spell.Name:lower()) ~= nil);
        end
    ['BLUE_SPELL_TYPE:HEALING'] = function ()
        local spell = gData.GetAction();
        return (spell ~= nil and spell.Name ~= nil and table.find(magic.tBLU_Spells['healing'],spell.Name:lower()) ~= nil);
        end
    ['BLUE_SPELL_TYPE:UTILITY'] = function ()
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
    ['SONG_TYPE:FINALE'] = function ()
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
    ['SONG_TYPE:ROUND'] = function ()
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
    ['JA:DOUBLE-UP'] = function ()
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
    ['JA:DARK_MANEUVER'] = function ()
        local ability = gData.GetAction();
        return (ability ~= nil and ability.Name ~= nil and ability.Name = 'Dark Maneuver');
        end
    ['JA:EARTH_MANEUVER'] = function ()
        local ability = gData.GetAction();
        return (ability ~= nil and ability.Name ~= nil and ability.Name = 'Earth Maneuver');
        end
    ['JA:FIRE_MANEUVER'] = function ()
        local ability = gData.GetAction();
        return (ability ~= nil and ability.Name ~= nil and ability.Name = 'Fire Maneuver');
        end
    ['JA:ICE_MANEUVER'] = function ()
        local ability = gData.GetAction();
        return (ability ~= nil and ability.Name ~= nil and ability.Name = 'Ice Maneuver');
        end
    ['JA:LIGHT_MANEUVER'] = function ()
        local ability = gData.GetAction();
        return (ability ~= nil and ability.Name ~= nil and ability.Name = 'Light Maneuver');
        end
    ['JA:THUNDER_MANEUVER'] = function ()
        local ability = gData.GetAction();
        return (ability ~= nil and ability.Name ~= nil and ability.Name = 'Thunder Maneuver');
        end
    ['JA:WATER_MANEUVER'] = function ()
        local ability = gData.GetAction();
        return (ability ~= nil and ability.Name ~= nil and ability.Name = 'Water Maneuver');
        end
    ['JA:WIND_MANEUVER'] = function ()
        local ability = gData.GetAction();
        return (ability ~= nil and ability.Name ~= nil and ability.Name = 'Wind Maneuver');
        end
        -- Pet commands
    ['PET'] = function ()
        local pet = gData.GetPet();
        return (pet ~= nil);
        end
    ['NOT_PET'] = function ()
        local pet = gData.GetPet();
        return (pet== nil);
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
        return (pets.SummonerCastingPetType() == gVars._SMN_AVATAR);
        end
    ['NOT_SMN:AVATAR'] = function ()
        return (pets.SummonerCastingPetType() ~= gVars._SMN_AVATAR);
        end
    ['SMN:SPIRIT'] = function ()
        return (pets.SummonerCastingPetType() == gVars._SMN_SPIRIT);
        end
    ['NOT_SMN:SPIRIT'] = function ()
        return (pets.SummonerCastingPetType() ~= gVars._SMN_SPIRIT);
        end
    ['SMN:SUMMONS'] = function ()
        local sPet = pets.SummonerCastingPetType();
        return (sPet == gVars._SMN_AVATAR or sPet == gVars._SMN_SPIRIT);
        end
    ['NOT_SMN:SUMMONS'] = function ()
        local sPet = pets.SummonerCastingPetType();
        return not (sPet == gVars._SMN_AVATAR or sPet == gVars._SMN_SPIRIT);
        end
    ['SMN:PET:AVATAR'] = function ()
        return (pets.fPetTypeSpecific() == gVars._SMN_AVATAR);
        end
    ['NOT_SMN:PET:AVATAR'] = function ()
        return (pets.fPetTypeSpecific() ~= gVars._SMN_AVATAR);
        end
    ['SMN:PET:SPIRIT'] = function ()
        return (pets.fPetTypeSpecific() == gVars._SMN_SPIRIT);
        end
    ['NOT_SMN:PET:SPIRIT'] = function ()
        return (pets.fPetTypeSpecific() ~= gVars._SMN_SPIRIT);
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
        -- Weapon Skill Stats
    ['WS_STR'] = function ()
        local ws = gData.GetAction();
        if ws == nil or ws.Name == nil then
            return false
            end
        local sName = string.gsub(ws.Name,' ','_');
        local statName,sWSName = utilities.fWhichWSStat(sName);
        return (statName:upper() == 'STR');
        end
    ['WS_STRAGI'] = function ()
        local ws = gData.GetAction();
        if ws == nil or ws.Name == nil then
            return false
            end
        local sName = string.gsub(ws.Name,' ','_');
        local statName,sWSName = utilities.fWhichWSStat(sName);
        return (statName:upper() == 'STRAGI');
        end
    ['WS_STRDEX'] = function ()
        local ws = gData.GetAction();
        if ws == nil or ws.Name == nil then
            return false
            end
        local sName = string.gsub(ws.Name,' ','_');
        local statName,sWSName = utilities.fWhichWSStat(sName);
        return (statName:upper() == 'STRDEX');
        end
    ['WS_STRINT'] = function ()
        local ws = gData.GetAction();
        if ws == nil or ws.Name == nil then
            return false
            end
        local sName = string.gsub(ws.Name,' ','_');
        local statName,sWSName = utilities.fWhichWSStat(sName);
        return (statName:upper() == 'STRINT');
        end
    ['WS_STRINT_30_20'] = function ()
        local ws = gData.GetAction();
        if ws == nil or ws.Name == nil then
            return false
            end
        local sName = string.gsub(ws.Name,' ','_');
        local statName,sWSName = utilities.fWhichWSStat(sName);
        return (statName:upper() == 'STRINT_30_20');
        end
    ['WS_STRMND'] = function ()
        local ws = gData.GetAction();
        if ws == nil or ws.Name == nil then
            return false
            end
        local sName = string.gsub(ws.Name,' ','_');
        local statName,sWSName = utilities.fWhichWSStat(sName);
        return (statName:upper() == 'STRMND');
        end
    ['WS_STRMND_30_50'] = function ()
        local ws = gData.GetAction();
        if ws == nil or ws.Name == nil then
            return false
            end
        local sName = string.gsub(ws.Name,' ','_');
        local statName,sWSName = utilities.fWhichWSStat(sName);
        return (statName:upper() == 'STRMND_30_50');
        end
    ['WS_STRVIT'] = function ()
        local ws = gData.GetAction();
        if ws == nil or ws.Name == nil then
            return false
            end
        local sName = string.gsub(ws.Name,' ','_');
        local statName,sWSName = utilities.fWhichWSStat(sName);
        return (statName:upper() == 'STRVIT');
        end
    ['WS_CHR'] = function ()
        local ws = gData.GetAction();
        if ws == nil or ws.Name == nil then
            return false
            end
        local sName = string.gsub(ws.Name,' ','_');
        local statName,sWSName = utilities.fWhichWSStat(sName);
        return (statName:upper() == 'CHR');
        end
    ['WS_DEX'] = function ()
        local ws = gData.GetAction();
        if ws == nil or ws.Name == nil then
            return false
            end
        local sName = string.gsub(ws.Name,' ','_');
        local statName,sWSName = utilities.fWhichWSStat(sName);
        return (statName:upper() == 'DEX');
        end
    ['WS_DEXAGI'] = function ()
        local ws = gData.GetAction();
        if ws == nil or ws.Name == nil then
            return false
            end
        local sName = string.gsub(ws.Name,' ','_');
        local statName,sWSName = utilities.fWhichWSStat(sName);
        return (statName:upper() == 'DEXAGI');
        end
    ['WS_DEXCHR'] = function ()
        local ws = gData.GetAction();
        if ws == nil or ws.Name == nil then
            return false
            end
        local sName = string.gsub(ws.Name,' ','_');
        local statName,sWSName = utilities.fWhichWSStat(sName);
        return (statName:upper() == 'DEXCHR');
        end
    ['WS_DEXINT'] = function ()
        local ws = gData.GetAction();
        if ws == nil or ws.Name == nil then
            return false
            end
        local sName = string.gsub(ws.Name,' ','_');
        local statName,sWSName = utilities.fWhichWSStat(sName);
        return (statName:upper() == 'DEXINT');
        end
    ['WS_INT'] = function ()
        local ws = gData.GetAction();
        if ws == nil or ws.Name == nil then
            return false
            end
        local sName = string.gsub(ws.Name,' ','_');
        local statName,sWSName = utilities.fWhichWSStat(sName);
        return (statName:upper() == 'INT');
        end
    ['WS_INTMND'] = function ()
        local ws = gData.GetAction();
        if ws == nil or ws.Name == nil then
            return false
            end
        local sName = string.gsub(ws.Name,' ','_');
        local statName,sWSName = utilities.fWhichWSStat(sName);
        return (statName:upper() == 'INTMND');
        end
    ['WS_MND'] = function ()
        local ws = gData.GetAction();
        if ws == nil or ws.Name == nil then
            return false
            end
        local sName = string.gsub(ws.Name,' ','_');
        local statName,sWSName = utilities.fWhichWSStat(sName);
        return (statName:upper() == 'MND');
        end
    ['WS_RANGED_AGI'] = function ()
        local ws = gData.GetAction();
        if ws == nil or ws.Name == nil then
            return false
            end
        local sName = string.gsub(ws.Name,' ','_');
        local statName,sWSName = utilities.fWhichWSStat(sName);
        return (statName:upper() == 'AGI');
        end
    ['WS_RANGED_STRAGI'] = function ()
        local ws = gData.GetAction();
        if ws == nil or ws.Name == nil then
            return false
            end
        local sName = string.gsub(ws.Name,' ','_');
        local statName,sWSName = utilities.fWhichWSStat(sName);
        return (statName:upper() == 'RANGED_STRAGI');
        end
    ['WS_VIT'] = function ()
        local ws = gData.GetAction();
        if ws == nil or ws.Name == nil then
            return false
            end
        local sName = string.gsub(ws.Name,' ','_');
        local statName,sWSName = utilities.fWhichWSStat(sName);
        return (statName:upper() == 'VIT');
        end
    ['WS_SKILL'] = function ()
        local ws = gData.GetAction();
        if ws == nil or ws.Name == nil then
            return false
            end
        local sName = string.gsub(ws.Name,' ','_');
        local statName,sWSName = utilities.fWhichWSStat(sName);
        return (statName:upper() == 'SKILL');
        end
    ['WS_HP'] = function ()
        local ws = gData.GetAction();
        if ws == nil or ws.Name == nil then
            return false
            end
        local sName = string.gsub(ws.Name,' ','_');
        local statName,sWSName = utilities.fWhichWSStat(sName);
        return (statName:upper() == 'HP');
        end
    ['SMN:BP:PHYSICAL'] = function ()
        local petAction = gData.GetPetAction();
        if petaction == nil or PetAction.Name == nil then
            return false;
        end
        return (table.find(gVars.tPetSpecialActions['SMN:BP:PHYSICAL'],PetAction.Name:lower()) ~= nil);
        end
    ['NOT_SMN:BP:PHYSICAL'] = function ()
        local petAction = gData.GetPetAction();
        if petaction == nil or PetAction.Name == nil then
            return false;
        end
        return (table.find(gVars.tPetSpecialActions['SMN:BP:PHYSICAL'],PetAction.Name:lower()) == nil);
        end
    ['SMN:BP:MAGICAL'] = function ()
        local petAction = gData.GetPetAction();
        if petaction == nil or PetAction.Name == nil then
            return false;
        end
        return (table.find(gVars.tPetSpecialActions['SMN:BP:MAGICAL'],PetAction.Name:lower()) ~= nil);
        end
    ['NOT_SMN:BP:MAGICAL'] = function ()
        local petAction = gData.GetPetAction();
        if petaction == nil or PetAction.Name == nil then
            return false;
        end
        return (table.find(gVars.tPetSpecialActions['SMN:BP:MAGICAL'],PetAction.Name:lower()) == nil);
        end
    ['SMN:BP:SKILL'] = function ()
        local petAction = gData.GetPetAction();
        if petaction == nil or PetAction.Name == nil then
            return false;
        end
        return (table.find(gVars.tPetSpecialActions['SMN:BP:SKILL'],PetAction.Name:lower()) ~= nil);
        end
    ['NOT_SMN:BP:SKILL'] = function ()
        local petAction = gData.GetPetAction();
        if petaction == nil or PetAction.Name == nil then
            return false;
        end
        return (table.find(gVars.tPetSpecialActions['SMN:BP:SKILL'],PetAction.Name:lower()) == nil);
        end
    ['SMN:BP:ACCURACY'] = function ()
        local petAction = gData.GetPetAction();
        if petaction == nil or PetAction.Name == nil then
            return false;
        end
        return (table.find(gVars.tPetSpecialActions['SMN:BP:ACCURACY'],PetAction.Name:lower()) ~= nil);
        end
    ['NOT_SMN:BP:ACCURACY'] = function ()
        local petAction = gData.GetPetAction();
        if petaction == nil or PetAction.Name == nil then
            return false;
        end
        return (table.find(gVars.tPetSpecialActions['SMN:BP:ACCURACY'],PetAction.Name:lower()) == nil);
        end
    ['SMN:BP:HYBRID'] = function ()
        local petAction = gData.GetPetAction();
        if petaction == nil or PetAction.Name == nil then
            return false;
        end
        return (table.find(gVars.tPetSpecialActions['SMN:BP:HYBRID'],PetAction.Name:lower()) ~= nil);
        end
    ['NOT_SMN:BP:HYBRID'] = function ()
        local petAction = gData.GetPetAction();
        if petaction == nil or PetAction.Name == nil then
            return false;
        end
        return (table.find(gVars.tPetSpecialActions['SMN:BP:HYBRID'],PetAction.Name:lower()) == nil);
        end
    ['BST:PET_ATTACK'] = function ()
        local petAction = gData.GetPetAction();
        if petaction == nil or PetAction.Name == nil then
            return false;
        end
        return (table.find(gVars.tPetSpecialActions['BST:PET_ATTACK'],PetAction.Name:lower()) ~= nil);
        end
    ['NOT_BST:PET_ATTACK'] = function ()
        local petAction = gData.GetPetAction();
        if petaction == nil or PetAction.Name == nil then
            return false;
        end
        return (table.find(gVars.tPetSpecialActions['BST:PET_ATTACK'],PetAction.Name:lower()) == nil);
        end
    ['BST:PET_MACC'] = function ()
        local petAction = gData.GetPetAction();
        if petaction == nil or PetAction.Name == nil then
            return false;
        end
        return (table.find(gVars.tPetSpecialActions['BST:PET_MACC'],PetAction.Name:lower()) ~= nil);
        end
    ['NOT_BST:PET_MACC'] = function ()
        local petAction = gData.GetPetAction();
        if petaction == nil or PetAction.Name == nil then
            return false;
        end
        return (table.find(gVars.tPetSpecialActions['BST:PET_MACC'],PetAction.Name:lower()) == nil);
        end
    ['BST:PET_MATT'] = function ()
        local petAction = gData.GetPetAction();
        if petaction == nil or PetAction.Name == nil then
            return false;
        end
        return (table.find(gVars.tPetSpecialActions['BST:PET_MATT'],PetAction.Name:lower()) ~= nil);
        end
    ['NOT_BST:PET_MATT'] = function ()
        local petAction = gData.GetPetAction();
        if petaction == nil or PetAction.Name == nil then
            return false;
        end
        return (table.find(gVars.tPetSpecialActions['BST:PET_MATT'],PetAction.Name:lower()) == nil);
        end
    ['SMN:ASSAULT'] = function ()
        local petAction = gData.GetPetAction();
        if petAction == nil or PetAction.Name == nil then
            return false;
        end
        return string.match(petAction.Name,'Assault');
        end
    ['SMN:RELEASE'] = function ()
        local petAction = gData.GetPetAction();
        if petAction == nil or PetAction.Name == nil then
            return false;
        end
        return string.match(petAction.Name,'Release');
        end
    ['SMN:RETREAT'] = function ()
        local petAction = gData.GetPetAction();
        if petAction == nil or PetAction.Name == nil then
            return false;
        end
        return string.match(petAction.Name,'Retreat');
        end
    ['BST:FIGHT'] = function ()
        local petAction = gData.GetPetAction();
        if petAction == nil or PetAction.Name == nil then
            return false;
        end
        return string.match(petAction.Name,'Fight');
        end
    ['BST:HEEL'] = function ()
        local petAction = gData.GetPetAction();
        if petAction == nil or PetAction.Name == nil then
            return false;
        end
        return string.match(petAction.Name,'Heel');
        end
    ['BST:STAY'] = function ()
        local petAction = gData.GetPetAction();
        if petAction == nil or PetAction.Name == nil then
            return false;
        end
        return string.match(petAction.Name,'Stay');
        end
    ['BST:SIC'] = function ()
        local petAction = gData.GetPetAction();
        if petAction == nil or PetAction.Name == nil then
            return false;
        end
        return string.match(petAction.Name,'Sic');
        end
    ['BST:READY'] = function ()
        local petAction = gData.GetPetAction();
        if petAction == nil or PetAction.Name == nil then
            return false;
        end
        return string.match(petAction.Type,'Ready');
        end
    ['BST:LEAVE'] = function ()
        local petAction = gData.GetPetAction();
        if petAction == nil or PetAction.Name == nil then
            return false;
        end
        return string.match(petAction.Name,'Leave');
        end
    ['PUP:DEPLOY'] = function ()
        local petAction = gData.GetPetAction();
        if petAction == nil or PetAction.Name == nil then
            return false;
        end
        return string.match(petAction.Name,'Deploy');
        end
    ['PUP:DEACTIVATE'] = function ()
        local petAction = gData.GetPetAction();
        if petAction == nil or PetAction.Name == nil then
            return false;
        end
        return string.match(petAction.Name,'Deactivate');
        end
    ['PUP:RETRIEVE'] = function ()
        local petAction = gData.GetPetAction();
        if petAction == nil or PetAction.Name == nil then
            return false;
        end
        return string.match(petAction.Name,'Retrieve');
        end
    ['DRG:DISMISS'] = function ()
        local petAction = gData.GetPetAction();
        if petAction == nil or PetAction.Name == nil then
            return false;
        end
        return string.match(petAction.Name,'Dismiss');
        end
    ['DRG:STEADY_WING'] = function ()
        local petAction = gData.GetPetAction();
        if petAction == nil or PetAction.Name == nil then
            return false;
        end
        return string.match(petAction.Name,'Steady Wing');
        end
    ['DT:BREATH'] = function ()
        return (utilities.fGetCycle(gVars._DT) == gVars._DT_BRE);
        end
    ['NOT_DT:BREATH'] = function ()
        return (utilities.fGetCycle(gVars._DT) ~= gVars._DT_BRE);
        end
    ['DT:MAGICAL'] = function ()
        return (utilities.fGetCycle(gVars._DT) == gVars._DT_MAG);
        end
    ['NOT_DT:MAGICAL'] = function ()
        return (utilities.fGetCycle(gVars._DT) ~= gVars._DT_MAG);
        end
    ['DT:PHYSICAL'] = function ()
        return (utilities.fGetCycle(gVars._DT) == gVars._DT_PHY);
        end
    ['NOT_DT:PHYSICAL'] = function ()
        return (utilities.fGetCycle(gVars._DT) ~= gVars._DT_PHY);
        end
    ['FRAME:HARLEQUIN'] = function ()
        return (pets.fPetTypeSpecific() == gVars._PUP_HARLEQUIN);
        end
    ['FRAME:VALOREDGE'] = function ()
        return (pets.fPetTypeSpecific() == gVars._PUP_VALOREDGE);
        end
    ['FRAME:SHARPSHOT'] = function ()
        return (pets.fPetTypeSpecific() == gVars._PUP_SHARPSHOT);
        end
    ['FRAME:STORMWALKER'] = function ()
        return (pets.fPetTypeSpecific() == gVars._PUP_STORMWALKER);
        end
    -- The following cannot avoid parameters
    ['WEAPON_TYPE'] = function (sList)
        return (fCheckWeaponType(sList);
        end
    ['NOT_WEAPON_TYPET'] = function (sList)
        return not (fCheckWeaponType(sList);
        end

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
    ['NOT_MOONPHASE'] = function (sList)
        local environ = gData.GetEnvironment();
        return not (string.find(sList:upper(), environ.MoonPhase:upper()) == nil);
        end
    ['DAY'] = function (sList)
        local environ = gData.GetEnvironment();
        return (string.find(sList:upper(), environ.Day:upper()) ~= nil);
        end
    ['NOT_DAY'] = function (sList)
        local environ = gData.GetEnvironment();
        return not (string.find(sList:upper(), environ.Day:upper()) ~= nil);
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
    ['LVLDIV'] = function (val)
        player = gData.GetPlayer();
        return ((player.MainJobSync/val)*val == player.MainJobSync);
        end
    ['NOT_LVLDIV'] = function (val)
        player = gData.GetPlayer();
        return ((player.MainJobSync/val)*val ~= player.MainJobSync);
        end
    ['PHANTOM_ROLL'] = function (sList)
        return (string.find(sList:upper(),gVars.PhantomRoll:upper()) ~= nil);
        end
    ['NOT_PHANTOM_ROLL'] = function (sList)
        return (string.find(sList:upper(),gVars.PhantomRoll:upper()) == nil);
        end
    ['EMPTY'] = function (sSlot)
        return fValidateEmpty('EMPTY',sSlot,false);
        end
    ['NOT_EMPTY'] = function (sSlot)
        return fValidateEmpty('EMPTY',sSlot,true);
        end
    ['EMPTY_1'] = function (sSlot)
        return fValidateEmpty('EMPTY_1',sSlot,false);
        end
    ['NOT_EMPTY_1'] = function (sSlot)
        return fValidateEmpty('EMPTY_1',sSlot,true);
        end
    ['EMPTY_2'] = function (sSlot)
        return fValidateEmpty('EMPTY',sSlot,false);
        end
    ['NOT_EMPTY_2'] = function (sSlot)
        return fValidateEmpty('EMPTY',sSlot,true);
        end
    ['SPELL'] = function (sList)
        local spell = gData.GetAction();
        if spell == nil or spell.Name == nil then
            return false;
        end
        return (string.find(sList:upper(), spell.Name:upper()) ~= nil);
        end
    ['NOT_SPELL'] = function (sList)
        local spell = gData.GetAction();
        if spell == nil or spell.Name == nil then
            return false;   -- Lack of a spell does not make it trye
            end
            return (string.find(sList:upper(), spell.Name:upper()) == nil);
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
            return false;   -- Lack of a spell does not make it true
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
            return false;   -- Lack of a spell does not make it true
        end
        return (string.find(spell.Name:lower,sList:lower()) == nil);
        end
    ['SONG'] = function (sList)
        local song = gData.GetAction();
        if song == nil or song.Name == nil then
            return false
        end
        return (string.find(sList:upper(), song.Name:upper()) ~= nil);
        end
    ['NOT_SONG'] = function (sList)
        local song = gData.GetAction();
        if song == nil or song.Name == nil then
            return false;   -- Lack of a song does not make it true
        end
        return (string.find(sList:upper(), song.Name:upper()) == nil);
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
            return false;   -- Lack of a song does not make it true
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
            return false;   -- Lack of a song doesn't mean it's true
        end
        return (string.find(song.Name:lower,sList:lower()) == nil);
        end
    ['PETNAME'] = function (sList)
        local pet = gData.GetPet();
        if pet == nil or pet.Name == nil or sList == nil then
            return false
        end
        return (string.find(sList:lower(),pet.Name:lower()) ~= nil);
        end
    ['NOT_PETNAME'] = function (sList)
        local pet = gData.GetPet();
        if pet == nil or pet.Name == nil or sList == nil then
            return false;   -- Lack of a pet doesn't mean it's true
        end
        return (string.find(sList:lower(),pet.Name:lower()) == nil);
        end
    ['SMN:SUMMON'] = function (sList)
        local spell = gData.GetAction();
        if spell == nil or spell.Name == nil then
            return false;   -- Lack of a spell does not make it true
        end
        return (string.find(sList:lower(),spell.Name:lower()) ~= nil);
        end
    ['NOT_SMN:SUMMON'] = function (sList)
        local spell = gData.GetAction();
        if spell == nil or spell.Name == nil then
            return false;   -- Lack of a spell does not make it true
        end
        return (string.find(sList:lower(),spell.Name:lower()) == nil);
        end
    ['SMN:BP_SUB'] = function (val)
        local petAction = gData.GetPetAction();
        if val == nil or petaction == nil or PetAction.Name == nil then
            return false;
        end
        return (string.find(PetAction.Name:lower(),val:lower()) ~= nil);
        end
    ['NOT_SMN:BP_SUB'] = function (val)
        local petAction = gData.GetPetAction();
        if val == nil or petaction == nil or PetAction.Name == nil then
            return false;
        end
        return (string.find(PetAction.Name:lower(),val:lower()) == nil);
        end
    ['WEATHER'] = function (sVal)
        local environ = gData.GetEnvironment();
        return environ.RawWeather == utilities.fFormattedWord(sVal,gVars._SLOT_FA);
        end
    ['NOT_WEATHER'] = function (sVal)
         local environ = gData.GetEnvironment();
         return environ.RawWeather ~= utilities.fFormattedWord(sVal,gVars._SLOT_FA);
         end
    ['CC'] = function (val)
        -- Unlike other entries, the "val" is the complete command. "CC" is just an index.
        -- I do it this way because it requires less processing than splitting out the number
        return utilities.fGetToggle(val);
        end
    ['NOT_CC'] = function (val)
        -- Like ['CC'], the "val" is the complete command. "NOT_CC" is just an index.
        -- I do it this way because it requires less processing than splitting out the number
        return not utilities.fGetToggle(val);
        end
    ['FAM'] = function (sList)
        return monster_type.fMonsterTargetIs(true,sList);
        end
    ['NOT_FAM'] = function (sList)
        return not monster_type.fMonsterTargetIs(true,sList);
        end
    ['ECO'] = function (sList)
        return monster_type.fMonsterTargetIs(false,sList);
        end
    ['NOT_ECO'] = function (sList)
        return not monster_type.fMonsterTargetIs(false,sList);
        end
    ['SPECIAL'] = function (sSlot,sGear)
        return gear.fValidateSpecial(sSlot,sGear);      -- Needs to be rewritten
        end
    ['IF'] = function (sCode,sSlot)
        -- The index is not the code to be parsed. That's why it is passed to this function
        -- This will be called for IF: and IF-
        return fCheckIf(sCode,sSlot);
        end
    ['NOT_IF'] = function (sCode,sSlot)
        -- The index is not the code to be parsed. That's why it is passed to this function
        -- This will be called for NOT_IF: and NOT_IF-
        return fCheckIf(sCode,sSlot);
        end
    ['CONDITIONAL'] = function (sType,sOp,iNum,iNum2,sSlot,sItem)
        -- The multitude of parameters are needed to support an hysteresis
        return fCheckConditional(sType,sOp,iNum,iNum2,sSlot,sItem);
        end

    -- more goes here
    };

--[[
    fCompareIt compares the values accordingly

    Parameters
        sOp     Operator to compare with
        iVal    Base value
        iNum    Value to compare base to

    Returned
        T/F
--]]

local function fCompareIt(sOp,iVal,iNum)
    local bGood;

    if table.find(['.EQ.','.GT.','.GE.','.LT.','.LE.','.NE.'],sOp) ~= nil then
        if sOp == '.EQ.' then
            bGood = (iVal == iNum);
        elseif sOp == '.GT.' then
            bGood = (iVal > iNum);
        elseif sOp == '.GE.' then
            bGood = (iVal >= iNum);
        elseif sOp == '.LT.' then
            bGood = (iVal < iNum);
        elseif sOp == '.LE.' then
            bGood = (iVal <= iNum);
        else    -- .NE.
            bGood = (iVal ~= iNum);
    else
        utilities/DisplayOnce('Warning: Unrecognized comparator operator: ' .. sOp,false);
        return false;
    end

    return bGood;
end     -- fCompareIt

--[[
    fCheckConditional processes an inline conditional comparison to see if it is true. There are two general types of
    conditionals tested: a simple comparison and an hysteresis comparison. The first just decodes the components and
    evaluates as is. The second is more complicated. The program needs to know whether the comparison is for equipping
    a piece of gear or for taking the piece off. This is determined by checking if the passed item in the indicated
    slot is already equipped.

    Parameters
        sType       Type of conditional
        sOp         Type of operator used in the comparison
        iNum        Value to compare against
        iNum2       hysteresis: Value checked to take off piece
        sItem       hysteresis: Item being checked
        sSlot       hysteresis: Slot the item goes to

    Returned:
        T/F
--]]

local function fCheckConditional(sType,sOp,iNum,iNum2,sSlot,sItem)
    local player = gData.GetPlayer();
    local party = gData.GetParty();
    local pet = gData.GetPet();
    local iVal;
    local bGood = false;

    sType = sType:upper();
    sOp = sOp:upper();

    -- First determine the value to check against
    if sType == 'HP' then
        iVal = player.HP;
    elseif sType == 'HPP' then
        iVal = player.HPP;
    elseif sType == 'MP' then
        iVal = player.MP;
    elseif sType == 'MPP' then
        iVal = player.MPP;
    elseif sType == 'TP' then
        iVal = player.TP;
    elseif sType == 'TPP' then
        iVal = math.floor(player.TP/10);
    elseif sType == 'PARTY' then
        iVal = party.Count;
    elseif sType == 'PETHPP' then
        iVal = pet.HPP;
    else    -- LVL
        iVal = player.MainJobSync;
    end

    -- Then do the appropriate comparison
    bGood = fCompareIt(sOp,iVal,iNum);
    if not bGood and iNum2 ~= nil then
        -- The presense of an iNum2 indicates a check for hysteresis
        -- Make sure the slot name is valid
        local bSlot,sFSlot = utilities.fCheckSlot(sSlot,gVars._SLOT_FA);
        if not bSlot then
            -- An invalid slot botches the whole hysteresis
            reporting.DisplayOnce('Warning: Invalid slot passed to comparative conditional: ' .. sFSlot .. ' for ' .. sType,false);
            return false;
        end

        local bGoOn = false;
        local es = gData.GetEquipment();
        local lsItem = sItem:lower();
        -- Now, make sure the item is already equipped
        if sFSlot == 'Rings' then
            bGoOn = (es['Ring1']:lower() == lsItem or es['Ring2']:lower() == lsItem);
        elseif sFSlot == 'Ears' then
            bGoOn = (es['Ear1']:lower() == lsItem or es['Ear2']:lower() == lsItem);
        else
            bGoOn = es[sFSlot]:lower() == lsItem;
        end

        if not bGoOn then
            return false;
        end

        -- We know that the equipping portion is not true and that we're currently wearing the item, so time to see if
        -- the item should be taken off
        bGood = fCompareIt(sOp,iVal,iNum2);
        return bGood;
    end

    return bGood;   -- This is encountered when not processing a hysteresis
end     -- fCheckConditional

--[[
    fValidateTownAK determines if the passed piece of gear matches the appropriate location
--]]

local function fValidateTownAK (sGear)
    local pNation = AshitaCore:GetMemoryManager():GetPlayer():GetNation();
    local environ = gData.GetEnvironment();
    local sGear = sGear:lower();

    if sGear == 'ducal aketon' then
        return (environ.Area ~= nil and
        (table.find(gVars.tTownAreas['Windy'],environ.Area) ~= nil or
        table.find(gVars.tTownAreas['Sandy'],environ.Area) ~= nil or
        table.find(gVars.tTownAreas['Bastok'],environ.Area) ~= nil or
        table.find(gVars.tTownAreas['Jeuno'],environ.Area) ~= nil));
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
    end

    -- If we get here, the piece of gear is not a national aketon
    utilities.DisplayOnce('Warning: Invalid body piece for national aketon check: ' .. sGear,false);
    return false;
end     -- fValidateTownAK

--[[
    fCheckIF determines if the passed in //IF conditional evaluates to true or false

    Parameters
        sCode       //IF conditional
        sSlot       The slot the check is performed against

    Returned
        T/F
--]]

local function fCheckIF (sCode,sSlot)
    local gSet = gData.GetCurrentSet();
    local bGood = nil;
    local bNot = false;
    local sItem,sslot;
    local ts = crossjobs.Sets.CurrentGear;

    sCode = sCode:upper();
    local i = string.find(sCode,'NOT_');
    if i ~= nil and i == 1 then
        bNot = true;
        sCode = string.sub(sCode,5,-1);         -- Remove the NOT_
    end

    -- I already have checked that we're dealing with either //IF: or //IF-slot:
    if string.sub(sCode,3,1) == ':' then
        -- Make sure the slot is not a subset or group
        if string.find('SUBSET,GROUP',sCode) ~= nil then
            reporting.DisplayOnce('Warning: //IF: and //NOT_IF: cannot be used with either a Subset or Group: '.. sSlot);
            return false;
        end
        -- Now proceed
        sslot = sSlot;
        sItem = string.sub(sCode,4,-1);
    elseif string.sub(sCode,3,1) == '-' then
        local j = string.find(sCode,':');
        sslot = utilities.fValidSlots(string.sub(sCode,4,j-1),gVars._SLOT_FA);
        if sslot == nil then
            -- Slot was Unrecognized
            reporting.DisplayOnce('Warning: Invalid slot in inline conditional: ' .. sslot);
            return false;
        end
        -- Grab the item
        sItem = string.sub(sCode,j+1,-1);
    end

    -- Simple comparison: equip gear piece if currently wearing identified
    -- gear piece. Checks dynamic composite gear set first. If empty, then
    -- checks currently worn gear.

    -- Check the temporary set
    local lsItem = sItem:lower();
    if not (ts[ssSlot] == nil or ts[ssSlot] == '') then
        -- Since slot not empty, check item name
        if ssSlot:lower() == 'ears' then
            bGood = (string.find(lsItem,ts['Ear1']:lower()) ~= nil or
                     string.find(lsItem,ts['Ear2']:lower()) ~= nil);
        elseif string.lower(ssLot) == 'rings' then
            bGood = (string.find(lsItem,ts['Ring1']:lower() ~= nil) or
                     string.find(lsItem,ts['Ring2']:lower()) ~= nil);
        else
            bGood = (string.find(lsItem,ts[sSlot]:lower()) ~= nil);
        end
    else
        -- Since temporary set slot was empty, check currently equipped gear
        local sssSlot = ssLot:lower();
        if gSet[ssSlot] == nil or gSet[ssSlot] == '' then
            bGood = false;
        elseif sssLot == 'ears' then
            bGood = (string.find(lsItem,string.lower(gSet['Ear1'])) ~= nil or
                     string.find(lsItem,string.lower(gSet['Ear2'])) ~= nil);
        elseif sssSLot == 'rings' then
            bGood = (string.find(lsItem,string.lower(gSet['Ring1'])) ~= nil or
                     string.find(lsItem,string.lower(gSet['Ring2'])) ~= nil);
        else
            bGood = (string.find(lsItem,string.lower(gSet[ssSlot])) ~= nil);
        end
    end

    bGood = bGood or false;

    if bNot == true then
        bGood = not bGood;
    end
end     -- fCheckIF

--[[
    fValidateEmpty determines if the specified slot is empty.

    Parameters:
        sCode   Type of "empty" to search
        sSLot   slot to checked
        bNot    Should the results be inverted
--]]

local function fValidateEmpty(sCode,sSlot,bNot)
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
end     -- fValidateEmpty

--[[
    fProcessConditionals determines if the passed conditionals are collectively true or not

    Parameters
        list        Either a string list or table of conditionals to check
        sItem       Gear piece that conditionals attached to
        sSlot       The slot that the conditional(s) are being applied to
        gsname      Source gear set name

    Note: In the examples I have included the // prefix, but technically the // has been stripped from the
    passed in conditionals already.
--]]

function conditionals.fProcessConditionals(list,sItem,sSlot,gsname)
    local tList = {};
    local bGood = nil;

    gsname = gsname or "composite";

    if list == nil or list == "" or sSlot == nil or sSlot == "" then
        utilities.DisplayOnce('Warning: Invalid list of conditions or slot encountered when processing gearset: ' .. gsname,false);
        return false;
    end

    -- We want the conditionals in a table
    if type(list) == "string" then
        tList = utilities.fSplitStringByDelimiter(list,',');
    elseif type(list) == "table" then
        tList = list;
    else
        reporting.DisplayMessage('Warning: invalid conditional list: ' .. tostring(list) .. ' specified in ' .. gsname .. ' for slot ' .. slot,false);
        return false;
    end

    for _,j in pairs(tList) do
        j = j:upper();
        -- First deal with single word conditionals that expect parameters
        if j == 'SPECIAL' then
            jump_table['SPECIAL'](sSlot,sItem);
        elseif string.find('EMPTY,NOT_EMPTY,EMPTY_1,NOT_EMPTY_1,EMPTY_2,NOT_EMPTY_2',j) ~= nil then
            -- All the empties have a parameter of the slot
            jump_table[j](sSlot);
        elseif j:len() > 3 and string.find('IF:,IF-',j:sub(1,3)) == 1 then
            -- Conditional is either //IF: or //IF-slot:
            bGood = jump_table['IF'](j,sSlot);
        elseif j:len() > 7 and string.find('NOT_IF:,NOT_IF-',j:sub(1,3)) == 1 then
            -- Conditional is either //NOT_IF: or //NOT_IF-slot:
            bGood = jump_table['NOT_IF'](j,sSlot);
        elseif jump_table[j] ~= nil then
            -- Then a straightforward keyword match. Example: //TOWN
            bGood = jump_table[j]();
        else
            -- At this point we're either dealing with a conditional that requires parameters or
            -- an Unrecognized conditional
            local sCode = j;

            -- If present, remove the "NOT_". Note that not all conditionals supprt a NOT_ variation.
            -- That will be handled in the invocation stage
            local i = string.find(sCode,'NOT_');
            if i ~= nil and i == 1 then
                bNot = true;
                sCode = string.sub(sCode,5,-1);         -- Remove the NOT_
            end

            if j:match("^CC#$") then
                -- This is a custom conditional. Example: //CC2. Note: NOT_ is not supported
                jump_table['CC'](j);

            elseif string.match(j,"%a+%.%a+%.%d+%.%d+") then
                -- This is a comparator code, hysteresis version. Example: //MPP.LE.85.95), Note: NOT_ is not supported
                local sField,sOp,iNum,iNum2 = string.match(j,"(%a+)%.(%a+)%.(%d+)%.(%d+)";
                if string.find('HPP,MPP,PETPP',sField) ~= nil then
                    bGood = jump_table['CONDITIONAL'](sField,sOp,iNum1,iNum2,sSlot,sItem);
                else
                    utilities.DisplayOnce('Warning: Hysteresis number only supported on //HPP, //MPP, and //PETHPP conditionals. Skipping',false)
                    bGood = false;
                end
            elseif string.match(j,"%a+%.%a+%.%d+") then
                -- This is also a comparator code, ignoring the hysteresis. Example: //TPP.GT.100, Note: NOT_ is not supported
                bGood = jump_table['CONDITIONAL'](string.match(j,"(%a)%.(%a+)%.(%d+)",nil,sSlot,sItem);
            elseif string.match(j,"%a+:%a+") then
                -- Simple comparison:allpha check. Example: //WT:AXE,SWORD
                local kw,val = string.match(j,"(%a)+:(%a+)");
                bGood = jump_table[kw](val);
            elseif string.match(j,"%a+:%d+") then
                -- Simple comparison:value check. Example: //LVLDIV:5
                local kw,val = string.match(j,"(%a)+:(%d+)");
                bGood = jump_table[kw](val);
            elseif string.match(j,"%a+:%a+:[%a,%s]+") ~= nil then
                -- This is for str:str=name,... etc. Example: //SMN:SUMMON:Ifrit,Garuda
                -- The conditional is used as an index and the passed list is used to compare against. Note: NOT_ is supported
                local s,l = string.match(j, "(%a+:%a+:)([%a,%s]+)");
                bGood = jump_table[s](l)
            end
        end
        -- Looping can stop in one of two specific cases:
        --    bGood is nil, the current conditional is not recognized
        --    bGood is false, the current conditional failed the check
        if bGood == nil then
            utilities.DisplayOnce('Warning: Unknown conditional encountered: ' .. s .. ' in ' .. gsname);
            return false;
        elseif bGood == false then
            return false;
        end
    end

    return bGood;
end     -- conditionals.fProcessConditionals

--[[
    fCheckWeaponType determines if the currently equipped weapon is of one of the types passed to the function.

    Parameter
        sList      WeaponTypes to check for

    Return
        T/F
--]]
local function fCheckWeaponType(sList,sSlot,altSlot)
    local eq = gData.GetEquipment();
    local bMatch = false;
    local tWT = {
        ['H2H'] = 1, ['DAGGER'] = 2, ['SWORD'] = 3, ['G_SWORD'] = 4, ['AXE'] = 5, ['G_AXE'] = 6, ['SCYTHE'] = 7, ['POLEARM'] = 8,
        ['KATANA'] = 9, ['G_KATANA'] = 10, ['CLUB'] = 11, ['STAVE'] = 12, ['ARCHERY'] = 25, ['MARKSMANSHIP'] = 26
    };

    if sList == nil or sType == "" then
        return false;
    --      sType = utilities.fFormattedWord(sType,gVars._SLOT_UA);
    end

    if not eq then return false end
    sList = utilities.fFormattedWord(sList,gVars._SLOT_UA);
    for wt in sList:gmatch("[^,]+") do
        if wt == 'THROWING' then
            -- You have to check (potentially) two slots for throwing
            local r1 = AshitaCore:GetResourceManager():GetItemByName(eq.Range.Name, 0);
            local r2 = AshitaCore:GetResourceManager():GetItemByName(eq.Ammo.Name, 0);
            if r1 and r1.Skill then
                bMatch = (tWT[wt] == r1.Skill);
            end
            if r2 and r2.Skill and not bMatch then
                bMatch = (tWT[wt] == r2.Skill);
            end
        elseif tWT[wt] then
            -- Found type in table
            if wt == 'ARCHERY' or wt == 'MARKSMANSHIP' then
                local r1 = AshitaCore:GetResourceManager():GetItemByName(eq.Range.Name, 0);
                if r1 and r1.Skill then
                    bMatch = (tWT[wt] == r1.Skill);
                end
            elseif string.find('H2H,G_SWORD,G_AXE,SCYTHE,POLEARM,G_KATANA,STAVE',wt) ~= nil then
                -- These are all main hand weapons
                local r1 = AshitaCore:GetResourceManager():GetItemByName(eq.Main.Name, 0);
                if r1 and r1.Skill then
                    bMatch = (tWT[wt] == r1.Skill);
                end
            elseif string.find('DAGGER,SWORD,AXE,KATANA,CLUB',wt) ~= nil then
                -- These are all (potentially) dual wield weapons
                local r1 = AshitaCore:GetResourceManager():GetItemByName(eq.Main.Name, 0);
                local r2 = AshitaCore:GetResourceManager():GetItemByName(eq.Sub.Name, 0);
                if r1 and r1.Skill then
                    bMatch = (tWT[wt] == r1.Skill);
                end
                if r2 and r2.Skill and not bMatch then
                    bMatch = (tWT[wt] == r2.Skill);
                end
            end
        elseif sType == 'SHIELD' or sType == 'AMMO' then
            -- Get the inventory memory manager
            local inventory = AshitaCore:GetMemoryManager():GetInventory();
            if inventory then
                -- Fetch the item index currently in the Sub-Weapon slot (Slot 1)
                local equipSlot = inventory:GetEquippedItem(1);
                if equipSlot or equipSlot.ItemIndex == 0 then
                    -- Convert the equipment index to the actual item object from the correct bag
                    local item = inventory:GetContainerItem(equipSlot.ContainerIndex, equipSlot.ItemIndex);
                    if item or item.Id == 0 then
                        -- Retrieve the static resource data for this item ID
                        local resourceItem = AshitaCore:GetResourceManager():GetItemById(item.Id);
                        if resourceItem then
                            if sType == 'SHIELD' then
                                -- Check if the item type belongs to a shield (Type flag 4)
                                bMatch = (resourceItem.Type == 4);
                            elseif sType == 'AMMO' then
                                -- Check if the item type belongs to a consummable ammo (Type flag 12)
                                bMatch = (resource.Type == 12);
                            end
                        end
                    end
                end
            end
        end
        if bMatch then
            return true;
        end
    end

    return false;
end     -- fCheckWeaponType

return conditionals;
