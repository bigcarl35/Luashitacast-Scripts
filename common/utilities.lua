local utilities = T{};

local crossjobs = require('common.crossjobs');
local locks = require('common.locks');

--[[
    This component contains functions that are of general use to any of the othe luashitacast compenents.

    List of routines-
        Subroutines:
            local ClearAlias        Unregisters all luashitacast commands
            ClearSet                Empties the passed gear set
            Initialize              Defines initial settings for luashitacast
            Message                 Toggles on/off feedback mechanism
            OpenByFilename          Opens passed file for append (or generates new name and opens)
            ProcessedTally          Notification system for every 'n' entries
            PullTarget              Pulls character's target and announces to party
            Reminder                Reminder function to nag player to /gc
            SetAlias                Registers all luashitacast commands
            Unload                  Clean up routine when jobs are changed/logout

            AdvanceCycle            Advance the setting of a specific cycle
            AdvanceToggle           Advance the setting of a specific toggle
            CreateCycle             Create a dynamic cycle
            CreateToggle            Create a dynamic toggle (on/off)
            SetToggle               Set a specific value to a toggle

        Functions:
            local fBit              2^(n-1) resultant
            local fHasBit           Determines if bit set in value
            fBuffed                 Determines if passed buff is on character
            fCheckItemOwned         Determines if character owns piece of gear
            fCheckObiDW             Determines if Day/weather element advantageous for obi
            fCheckPartyJob          Is a member of your party a certain job?
            fCheckTime              Determines if passed time matches keyword
            fCheckWSBailout         Determines range to target would fail Weapon Skill
            fFormattedWord          Capitalization routine for passed in word
            fGetLevel               Determines gear level cap for player
            fGetRoot                Retrieves the "base" of the passed in spell/song
            fGetTableByName         Returns the gear set associated with name
            fMagicalSubjob          Determines if the player's subjob can do magic
            fMakeConditionalTable   Splits apart conditionals into a table
            local fNewFileName      Generates a new report file name
            fReferenceCheck         Determines if any gear is reference to another set's slot
            fRemoveConditional      Removes inline conditionals from string
            fSetColorText           Colors text for displaying on screen
            fSlotMatch              Determines if item can be loaded into slot
            fTargetId               Returns the target ID (hex) of player's target
            fTranslateWhichSlot     Determines if passed in slot valid
            fValidSlots             Determines if passed in slot list is valid

            fGetCycle                Get a specific cycle's value
            fGetToggle               Get a specific toggle's value
            fSetCycle                Set a specific cycle's value
--]]

-- List of all days including the strong and weak elements
utilities.tWeekDayElement = {
    ['Firesday'] =     { ['strong'] = 'fire',    ['weak'] = 'water' },
    ['Earthsday'] =    { ['strong'] = 'earth',   ['weak'] = 'wind' },
    ['Watersday'] =    { ['strong'] = 'water',   ['weak'] = 'thunder' },
    ['Windsday'] =     { ['strong'] = 'wind',    ['weak'] = 'ice' },
    ['Iceday'] =       { ['strong'] = 'ice',     ['weak'] = 'fire' },
    ['Lightningday'] = { ['strong'] = 'thunder', ['weak'] = 'earth' },
    ['Lightsday'] =    { ['strong'] = 'light',   ['weak'] = 'dark' },
    ['Darksday'] =     { ['strong'] = 'dark',    ['weak'] = 'light' }
};

-- Lists all player storage containers available in FFXI.
-- Quite a number of them are not valid on HorizonXI yet.
utilities.STORAGES = {
    [1] = { ['id'] = 0,  ['name'] = 'Inventory' },
    [2] = { ['id'] = 1,  ['name'] = 'Safe' },
    [3] = { ['id'] = 2,  ['name'] = 'Storage' },
    [4] = { ['id'] = 3,  ['name'] = 'Temporary' },
    [5] = { ['id'] = 4,  ['name'] = 'Locker' },
    [6] = { ['id'] = 5,  ['name'] = 'Satchel' },
    [7] = { ['id'] = 6,  ['name'] = 'Sack' },
    [8] = { ['id'] = 7,  ['name'] = 'Case' },
    [9] = { ['id'] = 8,  ['name'] = 'Wardrobe' },
    [10]= { ['id'] = 9,  ['name'] = 'Safe 2' },
    [11]= { ['id'] = 10, ['name'] = 'Wardrobe 2' },
    [12]= { ['id'] = 11, ['name'] = 'Wardrobe 3' },
    [13]= { ['id'] = 12, ['name'] = 'Wardrobe 4' },
    [14]= { ['id'] = 13, ['name'] = 'Wardrobe 5' },
    [15]= { ['id'] = 14, ['name'] = 'Wardrobe 6' },
    [16]= { ['id'] = 15, ['name'] = 'Wardrobe 7' },
    [17]= { ['id'] = 16, ['name'] = 'Wardrobe 8' }
};


-- Listed below are spells grouped by a dependency or a type. These are
-- root names
utilities.tSpellGroupings = {
    ['int']		  =  {
        'gravity','blind','sleep','sleepga','poison',
        'poisonga','bind','dispel','blaze','ice','shock',
    },
    ['mnd']		   = {
        'paralyze','slow','slowga','frazzle','distract',
        'silence'
    },
    ['eDebuff']	   = { 'drown','burn','frost','choke','rasp','shock' },
    ['barspell']   = {
        ['ele'] = { 'baraero','baraera','barblizzard','barblizzara','barfire','barfira','barstone','barstonra','barthunder','barthundra','barwater','barwatera' },
        ['status'] = { 'barsleep','barsleepra','barpoison','barpoisonra','barparalyze','barparalyzra','barblind','barblindra','barvirus','barvira','barpetrify','barpetra' }
    },
    ['enspell']    = { 'enthunder','enstone','enaero','enblizzard','enfire','enwater','enlight','endark' },
    ['spikes']	   = { 'blaze','ice','shock','dread' },
    ['spirits']    = {
        'fire','firespirit','fire spirit','ice','icespirit','ice spirit','air','airspirit','air spirit','earth','earthspirit','earth spirit','thunder','thunderspirit',
        'thunder spirit','water','waterspirit','water spirit','light','lightspirit','light spirit','dark','darkspirit','dark spirit'
    },
    ['absorb']     = { 'absorb-agi','absorb-chr','absorb-dex','absorb-int','absorb-mnd','absorb-str','absorb-vit','absorb-acc','absorb-tp' },
    ['nin-buff']   = { 'tonko','utsusemi','monomi' },
    ['nin-debuff'] = { 'kurayami','hojo','dokumori','jubaku' },
    ['nin-ele']    = { 'katon','suiton','raiton','doton','huton','hyoton' },
    ['brd-enh']	   = { 'minne','minuet','paeon','pastoral','madrigal','mambo','operetta','etude','ballad','march','prelude','aubade','carol','mazurka','gavotte','capriccio',
        'fantasia','hymnus','round'
    },
    ['brd-enf']	   = { 'requiem','threnody','lullaby','finale','elegy','virelai' }
};

-- Lists all of the elemental gear broken out by type, element, and usage.
-- included in a reference to where the item that matches that type/element
-- can be found in the GearDetails table.
utilities.tElemental_gear = {
    ['relic'] = {
        ['level'] = 75,
        ['type'] = 'STAVE',
        { ['Name'] = 'Claustrum', ['Ref'] = {} }
    },
    ['staff'] = {
        ['level'] = 51,
        ['fire'] = {
            ['Weak'] = 'water',
            ['NQ'] = { ['Name'] = 'Fire staff', ['Ref'] = {} },
            ['HQ'] = { ['Name'] = 'Vulcan\'s staff', ['Ref'] = {} },
            ['Grip'] = { ['Name'] = 'Fire Grip', ['Ref'] = {} },
            ['Affinity'] = { 'blaze','burn','firaga','fire','flare','enfire','katon' },
            ['SongAffinity'] = { 'ice threnody' },
            ['Summons'] = { 'ifrit','fire spirit','firespirit','fire' }
        },
        ['ice'] = {
            ['Weak'] = 'fire',
            ['NQ'] = { ['Name'] = 'Ice staff', ['Ref'] = {} },
            ['HQ'] = {['Name'] = 'Aquilo\'s staff', ['Ref'] = {} },
            ['Grip'] = { ['Name'] = 'Ice Grip', ['Ref'] = {} },
            ['Affinity'] = { 'blizzaga','blizzard','freeze','frost','ice','enblizzard','jubaku','hyoton','bind','distract','paralyze' },
            ['SongAffinity'] = { 'wind threnody' },
            ['Summons'] = { 'shiva','ice spirit','icespirit','ice' },
        },
        ['wind'] = {
            ['Weak'] = 'ice',
            ['NQ'] = { ['Name'] = 'Wind staff', ['Ref'] = {} },
            ['HQ'] = { ['Name'] = 'Auster\'s staff', ['Ref'] = {} },
            ['Grip'] = { ['Name'] = 'Wind Grip', ['Ref'] = {} },
            ['Affinity'] = { 'aero','aeroga','choke','tornado','enaero','huton','gravity','silence' },
            ['SongAffinity'] = { 'earth threnody' },
            ['Summons'] = { 'garuda','air spirit','fCheckInlineWeatherairspirit','air','siren' },
        },
        ['earth'] = {
            ['Weak'] = 'wind',
            ['NQ'] = { ['Name'] = 'Earth staff', ['Ref'] = {} },
            ['HQ'] = { ['Name'] = 'Terra\'s staff', ['Ref'] = {} },
            ['Grip'] = { ['Name'] = 'Earth Grip', ['Ref'] = {} },
            ['Affinity'] = { 'quake','rasp','stone','stonega','enstone','hojo','doton','slow' },
            ['SongAffinity'] = { 'lightning threnody', 'battlefield elegy', 'carnage elegy' },
            ['Summons'] = {'titan','earth spirit','earthspirit','earth' },
        },
        ['thunder'] = {
            ['Weak'] = 'earth',
            ['NQ'] = { ['Name'] = 'Thunder staff', ['Ref'] = {} },
            ['HQ'] = { ['Name'] = 'Jupiter\'s staff', ['Ref'] = {} },
            ['Grip'] = { ['Name'] = 'Thunder Grip', ['Ref'] = {} },
            ['Affinity'] = { 'burst','shock','thundaga','thunder','enthunder','raiton' },
            ['SongAffinity'] = { 'water threnody' },
            ['Summons'] = { 'ramuh','thunder spirit','thunderspirit','thunder' },
        },
        ['water'] = {
            ['Weak'] = 'thunder',
            ['NQ'] = { ['Name'] = 'Water staff', ['Ref'] = {} },
            ['HQ'] = { ['Name'] = 'Neptune\'s staff', ['Ref'] = {} },
            ['Grip'] = { ['Name'] = 'Water Grip', ['Ref'] = {} },
            ['Affinity'] = { 'drown','flood','poison','poisonga','water','waterga','enwater','dokumori','suiton' },
            ['SongAffinity'] = { 'fire threnody' },
            ['Summons'] = { 'leviathan','water spirit','waterspirit','water' },
        },
        ['light'] = {
            ['Weak'] = 'dark',
            ['NQ'] = { ['Name'] = 'Light staff', ['Ref'] = {} },
            ['HQ'] = { ['Name'] = 'Apollo\'s staff', ['Ref'] = {} },
            ['Grip'] = { ['Name'] = 'Light Grip', ['Ref'] = {} },
            ['Affinity'] = { 'banish','banishga','curaga','cure','dia','diaga','flash','holy','enlight','repose','inundation' },
            ['SongAffinity'] = { 'dark threnody','foe requiem','foe requiem ii','foe requiem iii','foe requiem iv','foe requiem v','foe requiem vi','foe lullaby','horde lullaby',
                'magic finale','maiden\'s virelai' },
            ['Summons'] = {'carbuncle','light spirit','lightspirit','light','cait sith','caitsith','alexander'},
        },
        ['dark'] = {
            ['Weak'] = 'light',
            ['NQ'] = { ['Name'] = 'Dark staff', ['Ref'] = {} },
            ['HQ'] = { ['Name'] = 'Pluto\'s staff', ['Ref'] = {} },
            ['Grip'] = { ['Name'] = 'Dark Grip', ['Ref'] = {} },
            ['Affinity'] = { 'absorb','aspir','blind','bio','dispel','drain','dread','frazzle','sleep','sleepga','endark','kurayami' },
            ['SongAffinity'] = { 'light threnody' },
            ['Summons'] = { 'fenrir','diabolos','dark spirit','darkspirit','dark','atomos','odin' },
        },
    },
    ['obi'] = {
        ['level'] = 71,
        ['fire'] = {
            ['Weak'] = 'water',
            ['Name'] = 'Karin obi',
            ['Ref'] = {},
            ['MEacc'] = { 'burn','firaga','fire','flare','blaze','enfire','blaze','katon' },
            ['eleWS'] = { 'burning blade','red lotus blade','tachi: Kagero','flaming arrow','hot shot','wildfire' },
        },
        ['ice'] = {
            ['Weak'] = 'fire',
            ['Name'] = 'Hyorin obi',
            ['Ref'] = {},
            ['MEacc'] = { 'frost','blizzaga','blizzard','freeze','paralyze','bind','distract','ice','enblizzard','hyoton' },
            ['eleWS'] = { 'frostbite','freezebite','herculean slash','blade: to' },
            ['Other'] = 'elemental magic',
        },
        ['wind'] = {
            ['Weak'] = 'ice',
            ['Name'] = 'Furin obi',
            ['Ref'] = {},
            ['MEacc'] = { 'choke','aero','aeroga','tornado','silence','gravity','flurry','enaero','huton' },
            ['eleWS'] = { 'gust slash','cyclone','aeolian edge','tachi: jinpu' },
        },
        ['earth'] = {
            ['Weak'] = 'wind',
            ['Name'] = 'Dorin obi',
            ['Ref'] = {},
            ['MEacc'] = { 'rasp','quake','stone','stonega','slow','enstone','doton' },
            ['eleWS'] = { 'blade: chi','rock crusher','earth crusher' },
        },
        ['thunder'] = {
            ['Weak'] = 'earth',
            ['Name'] = 'Rairin obi',
            ['Ref'] = {},
            ['MEacc'] = { 'shock','burst','thundaga','thunder','stun','enthunder','raiton' },
            ['eleWS'] = { 'cloudsplitter','thunder thrust','raiden thrust','tachi: goten' },
        },
        ['water'] = {
            ['Weak'] = 'thunder',
            ['Name'] = 'Suirin obi',
            ['Ref'] = {},
            ['MEacc'] = { 'drown','flood','water','waterga','poison','enwater','suiton' },
            ['eleWS'] = { 'blade: teki','blade: yu' },
            ['Other'] = 'divine magic',
        },
        ['light'] = {
            ['Weak'] = 'dark',
            ['Name'] = 'Korin obi',
            ['Ref'] = {},
            ['MEacc'] = { 'banish','banishga','dia','diaga','flash','repose','holy','auspice','esuna','sacrifice','reprisal','cure','curaga','enlight' },
            ['eleWS'] = { 'shining blade','seraph blade','primal rend','tachi: koki','shining strike','seraph strike','starburst','sunburst','garland of bliss','trueflight' },
            ['Other'] = 'cure potency',
        },
        ['dark'] = {
            ['Weak'] = 'light',
            ['Name'] = 'Anrin obi',
            ['Ref'] = {},
            ['MEacc'] = { 'blind','bio','sleep','dispel','frazzle','drain','warp','tractor','aspir','escape','sleep','sleepga','retrace','endark' },
            ['eleWS'] = { 'energy steal','energy drain','sanguine blade','dark harvest','shadow death','infernal scythe','blade: ei','starburst',
                          'sunburst','cataclysm','vidohunir','omniscience','leaden suite' },
        },
    },
    ['gorget'] = {
        ['level'] = 72,
        ['fire'] = {
            ['Weak'] = 'water',
            ['Name'] = 'Flame gorget',
            ['Ref'] = {},
            ['skillProp'] = { 'liquefaction','fusion' },
            ['eleWS'] = { 'arching arrow','ascetic\'s fury','asuran fists','atonement','blade: shun','decimation','detonator','drakesbane','dulling arrow','empyreal arrow','final heaven',
                'flaming arrow','full swing','garland of bliss','heavy shot','hexa strike','hot shot','insurgency','knights of round','last stand','mandalic stab','mistral axe',
                'metatron torment','realmrazer','red lotus blade','scourge','shijin spiral','sniper shot','spinning attack','spinning axe','stringing pummel','tachi: kagero','tachi: kasha',
                'upheaval','wheeling thrust' },
        },
        ['ice'] = {
            ['Weak'] = 'fire',
            ['Name'] = 'Snow gorget',
            ['Ref'] = {},
            ['skillProp'] = { 'induration','distortion' },
            ['eleWS'] = { 'blade: to','blast arrow','cross reaper','death blossom','expiacion','freezebite','frostbite','full break','geirskogul','ground strike','guillotine','quietus',
                'impulse drive','mordant rime','namas arrow','piercing arrow','pyrrhic kleos','rudra\'s storm','ruinator','raging rush','shadow of death','shattersoul','skullbreaker',
                'smash axe','spiral hell','steel cyclone','tachi: gekko','tachi: hobaku','tachi: rana','tachi: yukikaze','tornado kick','vidohunir' },
        },
        ['wind'] = {
            ['Weak'] = 'ice',
            ['Name'] = 'Breeze gorget',
            ['Ref'] = {},
            ['skillProp'] = { 'detonation','fragmentation' },
            ['eleWS'] = { 'aeolian edge','backhand blow','black halo','blade: jin','blade: kamu','blade: to','camlann\'s torment','coronach','cyclone','dancing edge','death blossom',
                'dragon kick','earth crusher','exenterator','freezebite','gale axe','ground strike','gust slash','king\'s justice','mordant rime','raging axe','randgrith',
                'red lotus blade','resolution','ruinator','savage blade','shark bite','shell crusher','sidewinder','slug shot','spinning slash','steel cyclone','tachi: jinpu',
                'tachi: kaiten','taichi: shoha','taichi: yukikaze','tornado kick','trueflight','true strike','victory smite','vidohunir' },
        },
        ['earth'] = {
            ['Weak'] = 'wind',
            ['Name'] = 'Soil gorget',
            ['Ref'] = {},
            ['skillProp'] = { 'scission','gravitation' },
            ['eleWS'] = { 'aeolian edge','asuran fists','avalanche axe','blade: ei','blade: ku','blade: ten','calamity','catastrophe','crescent moon','dancing edge','entropy','eviseration',
                'exenterator','expiacion','fast blade','hard slash','impulse drive','iron tempest','king\'s justice','leaden salute','mercy stroke','nightmare scythe','omniscience',
                'primal rend','pyrrhic kleos','rampage','requiscat','resolution','retibution','savage blade','seraph blade','shattersoul','shining blade','sickle moon','slice','spinning axe',
                'spinning scythe','spiral hell','stardiver','stringing pummel','sturmwind','swift blade','tachi: enpi','tachi: jinpu','tachi: rana','trueflight','viper bite','vorpal blade',
                'wasp sting' },
        },
        ['thunder'] = {
            ['Weak'] = 'earth',
            ['Name'] = 'Thunder gorget',
            ['Ref'] = {},
            ['skillProp'] = { 'impaction','fragmentation' },
            ['eleWS'] = { 'aeolian edge','apex arrow','armor break','avalanche axe','black halo','blade: chi','blade: jin','blade: kamu','blade: shun','calamity','camlann\'s torment',
                'circle blade','combo','cyclone','death blossom','dragon kick','earth crusher','exenterator','flat blade','full swing','ground strike','heavy swing','howling fist',
                'judgement','king\'s justice','leg sweep','mordant rime','raging axe','raging fist','raiden thrust','realmrazer','resolution','rock crusher','savage blade','seraph strike',
                'shark bite','shield break','shining strike','shoulder tackle','sickle moon','skewer','spinning attack','spinning axe','tachi: goten','tachi: koki','tachi: shoha',
                'thunder thrust','true strike','victory smite','vidohunir','vorpal blade','weapon break' },
        },
        ['water'] = {
            ['Weak'] = 'thunder',
            ['Name'] = 'Aqua gorget',
            ['Ref'] = {},
            ['skillProp'] = { 'reverberation','distortion' },
            ['eleWS'] = { 'atonement','blade: teki','brainshaker','circle blade','cross reaper','dark harvest','entropy','quietus','death blossom','decimation','expiacion','full break',
                'garland of bliss','gate of tartarus','geirskogul','ground strike','last stand','mordant rime','namas arrow','piercing arrow','pyrrhic kleos','rudra\'s storm','primal rend',
                'raging rush','retribution','ruinator','shadow of death','shockwave','shoulder tackle','sidewinder','skullbreaker','slug shot','smash axe','spinning scythe','spiral hell',
                'split shot','steel cyclone','sturmwind','sunburst','tachi: gekko','tachi: koki','vidohunir','vorpal thrust' },
        },
        ['light'] = {
            ['Weak'] = 'dark',
            ['Name'] = 'Light gorget',
            ['Ref'] = {},
            ['skillProp'] = { 'transfixion','fusion','light' },
            ['eleWS'] = { 'apex arrow','arching arrow','ascetic\'s fury','atonement','blade: chi','blade: ku','blade: rin','blade: shun','blast arrow','blast shot','camlann\'s torment',
                'decimation','detonator','double thrust','drakesbane','dulling arrow','empyreal arrow','eviseration','final heaven','flaming arrow','garland of bliss','heavy shot',
                'hexa strike','hot shot','howling fist','insurgency','knight\'s of round','leaden salute','last stand','mandalic stab','metatron torment','mistral axe','omniscience',
                'piercing arrow','power slash','realmrazer','raiden thrust','scourge','shijin spiral','sidewinder','skewer','slug shot','sniper shot','split shot','stardiver','tachi: enpi',
                'tachi: goten','tachi: kasha','thunder thrust','torcleaver','victory smite','upheaval','vorpal scythe','vorpal thrust','wheeling thrust' },
        },
        ['dark'] = {
            ['Weak'] = 'light',
            ['Name'] = 'Shadow gorget',
            ['Ref'] = {},
            ['skillProp'] = { 'compression','gravitation','darkness' },
            ['eleWS'] = { 'asuran fists','black halo','blade: ei','blade: hi','blade: kamu','blade: ku','blade: ten','catastrophe','quietus','entropy','eviseration','impulse drive',
                'insurgency','keen edge','leaden salute','mandalic stab','mercy stroke','requiscat','rundra\'s storm','nightmare scythe','omniscience','one inch punch','penta thrust',
                'primal rend','retribution','shattersoul','starburst','stardiver','stringing pummel','sunburst','swift blade','tachi: kasha','tachi: rana','tachi: shoha','upheaval',
                'gate of tartarus' },
        },
        ['searched'] = false,
    },
};

-- The following define all the weaponskills according to the desired stats
utilities.tWeaponSkills = {
    ['CHR']    = { 'shadowstitch' },
    ['DEX']    = { 'wasp sting','viper bite','blade: metsu','dancing edge' },
    ['DEXAGI'] = { 'shark bite','coronach' },
    ['DEXCHR'] = { 'eviseration' },
    ['DEXINT'] = { 'gust slash','cyclone' },
    ['INT']    = { 'gate of tartarus' },
    ['INTMND'] = { 'spirit taker' },
    ['MND']    = { 'energy steal','energy drain' },
    ['RANGED_AGI']  = { 'hot shot','split shot','sniper shot','slugshot','blast shot','heavy shot','detonator' }, -- MARKSMANSHIP
    ['RANGED_STRAGI'] = { 'flaming arrow','piercing arrow','dulling arrow','sidewinder','blast arrow','arching arrow','empyreal arrow','namas arrow' }, -- ARCHERY
    ['STR']    = { 'raging axe','smash axe','gale axe','avalanche axe','spinning axe','rampage','mistral axe','decimation','spinning attack','flat blade',
                   'circle blade','vorpal blade','hard slash','crescent moon','mercy stroke','iron tempest','sturmwind','keen edge','raging rush',
                   'metatron torment','leg sweep','skewer','wheeling thrust','impulse drive','tachi: enpi','tachi: hobaku','tachi: goten','tachi: kagero',
                   'tachi: jinpu','tachi: yukikaze','tachi: gekko','tachi: kasha','tachi: kaiten','brainshaker','skullbreaker','true strike','heavy swing',
                   'shell crusher','full swing','onslaught','double thrust','spinning scythe','Vorpal Scythe' },
    ['STRAGI'] = { 'sickle moon','vorpal thrust' },
    ['STRDEX'] = { 'combo','backhand blow','raging fists','fast blade','penta thrust','blade: rin','blade: retsu','blade: jin','blade: ten','blade: ku','Geirskogul' },
    ['STRINT'] = { 'dark harvest','shadow of death','nightmare scythe','spiral hell','burning blade','frostbite','freezebite','spinning slash','ground strike',
                   'thunder thrust','raiden thrust','blade: teki','blade: to','blade: chi','blade: ei','rock crusher','earth crusher','catastrophe' },
    ['STRINT_30_20'] = { 'red lotus blade' },
    ['STRMND'] = { 'guillotine','cross reaper','shining blade','seraph blade','swift blade','savage blade','shockwave','tachi: koki','shining strike','seraph strike',
                   'judgment','hexa strike','randgrith','retribution', 'knights of round' },
    ['STRMND_30_50'] = { 'black halo' },
    ['STRVIT'] = { 'shoulder tackle','one inch punch','final heaven' },
    ['Skill']  = { 'starlight','moonlight' },
    ['HP']     = { 'spirits within' }
};

-- Lists storage containers that can be equipped from outside of a moghouse
utilities.EQUIPABLE = {
    utilities.STORAGES[1],		-- Inventory
    utilities.STORAGES[9],		-- Wardrobe
    utilities.STORAGES[11],		-- Wardrobe 2
    utilities.STORAGES[17]		-- Wardrobe 8
};

utilities.EQUIPABLE_NONHOLIDAY = {
    utilities.STORAGES[1],		-- Inventory
    utilities.STORAGES[9],		-- Wardrobe
    utilities.STORAGES[11]		-- Wardrobe 2
};

utilities.NON_GEAR = {
    utilities.STORAGES[1],		-- Inventory
    utilities.STORAGES[2],		-- Safe
    utilities.STORAGES[3],		-- Storage
    utilities.STORAGES[5],		-- Locker
    utilities.STORAGES[6],		-- Satchel
    utilities.STORAGES[7],		-- Sack
    utilities.STORAGES[8],		-- Case
    utilities.STORAGES[10],		-- Safe 2
};

-- Define constants for input/output types for how slot references are formatted
utilities._SLOT_LA = 'LA';     -- lowercase slot name
utilities._SLOT_UA = 'UA';     -- uppercase slot name
utilities._SLOT_N  = 'N';      -- numeric
utilities._SLOT_FA = 'FA';     -- formatted output: first letter uppercase, rest lowercase

-- Define constants for LOCK and UNLOCK
utilities._LOCK   = 'lock';
utilities._UNLOCK = 'unlock';

-- JobMask holds a list of all the jobs recogized by FFXI. Each job is referenced via a mask
-- that is used to determine if the piece of gear can be equipped by the said job.
utilities.JobMask = { ['None'] = 0x0,
    ['WAR'] = 0x2, ['MNK'] = 0x4, ['WHM'] = 0x8, ['BLM'] = 0x10, ['RDM'] = 0x20, ['THF'] = 0x40, ['PLD'] = 0x80, ['DRK'] = 0x100,
    ['BST'] = 0x200, ['BRD'] = 0x400, ['RNG'] = 0x800, ['SAM'] = 0x1000, ['NIN'] = 0x2000, ['DRG'] = 0x4000, ['SMN'] = 0x8000,
    ['BLU'] = 0x10000, ['COR'] = 0x20000, ['PUP'] = 0x40000, ['DNC'] = 0x80000, ['SCH'] = 0x100000, ['GEO'] = 0x200000,
    ['RUN'] = 0x400000, ['MON'] = 0x800000, ['JOB24'] = 0x1000000, ['JOB25'] = 0x2000000, ['JOB26'] = 0x4000000,
    ['JOB27'] = 0x8000000, ['JOB28'] = 0x10000000, ['JOB29'] = 0x20000000,['JOB30'] = 0x30000000, ['JOB31'] = 0x80000000,
    ['Alljobs'] = 0x007FFFFE };

-- List of all valid slot names and two special types
utilities.SlotNames = { 'subset','group','main','sub','range','ammo','head','neck','ear1','ear2','ears','body','hands','ring1',
    'rings2','rings','back','waist','legs','feet' };

-- List of numeric representations for who controls a region
utilities.RegionAreas = {
    [-1] = 'Unassigned', [0]  = 'N/A', [1]  = 'San d\'Orian', [2]  = 'Bastokian', [3]  = 'Windurstian', [4]  = 'Beastmen'
};

-- List of all supported commands
utilities.AliasList = {
    'acc','ajug','db','dt','ei','equipit','eva','gc','gcmessages','gearset','gs','gswap','help','horn','idle','kite',
    'lock','macc','maxsong','maxspell','petfood','ptt','pull','racc','rc','rv','sbp','showit','smg','spf','ss','string',
    'tank','th','unlock','ver','wsdistance','wswap','t1'
};

-- Define constants for DT so typos aren't made
utilities.OFF = 'Off';
utilities.PHY = 'Physical';
utilities.MAG = 'Magical';
utilities.BRE = 'Breath';

-- define constants for Instrument so typos aren't made
utilities.HORN = 'Horn';
utilities.STRING = 'String';

-- define the code lists for the crafting and gathering types
utilities.Crafting_Types = 'ALC,BONE,CLOTH,COOK,GSM,LTH,BSM,WW';
utilities.Gathering_Types = 'HELM,DIG,CLAM,FISH';

-- Define list of all valid jobs
utilities._validJobs = 'BLM,BLU,BRD,BST,COR,DNC,DRG,DRK,GEO,MNK,PLD,PUP,RDM,RNG,RUN,SAM,SCH,SMN,THF,WAR,WHM';

-- Define list of all magic using jobs
utilities._sMagicJobs = 'BLM,WHM,RDM,SMN,PLD,DRK,BLU,SCH,GEO,RUN';

-- Define list of all jobs that can tank
utilities._TankJobs = 'PLD,NIN,RUN,DRK,WAR,THF,RDM,BLU';

-- Define list of all elements
utilities._AllElements = 'fire,ice,wind,earth,thunder,water,light,dark';

-- Define lists of valid Weapon Types.
-- Note: while SHIELD isn't a weapon, it conforms to the weapon type mechanism in this program
utilities._WeaponTypes = 'ARCHERY,AXE,CLUB,DAGGER,GAXE,GKATANA,GSWORD,H2H,KATANA,MARKSMANSHIP,POLEARM,SCYTHE,STAVE,SWORD,THROWING,SHIELD';
utilities._WeaponMelee = 'AXE,CLUB,DAGGER,GAXE,GKATANA,GSWORD,H2H,KATANA,POLEARM,SCYTHE,STAVE,SWORD,SHIELD';
utilities._WeaponRange = 'ARCHERY,MARKSMANSHIP,THROWING';

-- Define list of all pet commands
utilities._PetCommands = 'FIGHT,HEEL,STAY,LEAVE,SIC,READY,STEADY WING,DISMISS,ASSAULT,RELEASE,RETREAT';

-- Define arrays for toggles and cycles
utilities.Toggles = {};
utilities.Cycles = {};

--[[
    CreateCycle creates a table variable with multiple defined values. The index identifies which value
    is currently selected

    Parameters
        Name        Name of the cycle
        Values      Array of indexed values
--]]

function utilities.CreateCycle(name, values)
    local newCycle = {
        Index = 1,
        Array = values
    };

    utilities.Cycles[name] = newCycle;
end		-- utilities.CreateCycle

--[[
    fGetCycle returns the currently selected value of the cycle

    Parameter
        Name        Name of the cycle

    Returned
        Current value of the cycle
--]]

function utilities.fGetCycle(name)
    local ctable = utilities.Cycles[name];

    if (type(ctable) == 'table') then
        return ctable.Array[ctable.Index];
    else
        return 'Unknown';
    end
end		-- utilities.GetCycle

--[[
    AdvanceCycle moves the pointer in the Cycle to the next available value. If at the last value,
    it cycles back to the beginning of the list

    Parameter
        name        Name of the cycle
--]]

function utilities.AdvanceCycle(name)
    local ctable = utilities.Cycles[name];

    if (type(ctable) ~= 'table') then
        return;
    end

    ctable.Index = ctable.Index + 1;
    if (ctable.Index > #ctable.Array) then
        ctable.Index = 1;
    end
end		-- utilities.AdvanceCycle

--[[
    SetCycle explicitly sets which value should be current in the cycle variable

    Parameters
        name        Name of the Cycle
        val         Value to set the cycle to
--]]

function utilities.fSetCycle(name,val)
    local ctable = utilities.Cycles[name];

    if (type(ctable) ~= 'table') then
        return;
    end

    for k,v in pairs(ctable.Array) do
        if val == v then
            ctable.Index = k
            return true
        end
    end
    return false
end		-- utilities.SetCycle

--[[
    CreateToggle creates a binary variable that can be turrned on or off

    Parameters
        name        Name of the toggle
        default     Value of the toggle
--]]

function displaybar.CreateToggle(name, default)
    utilities.Toggles[name] = default;
end		-- utilities.CreateToggle

--[[
    fGetToggle returns the name of the current setting of the passed toggle variable

    Parameter
        Name        Name of the toggle

    Returned
        Value of the toggle
--]]

function utilities.fGetToggle(name)

    if utilities.Toggles[name] ~= nil then
        return utilities.Toggles[name];
    else
        return false;
    end
end		-- utilities.fGetToggle

--[[
    AdvanceToggle just flips the binary setting of the passed toggle variable

    Parameter
        Name        Name of toggle
--]]

function utilities.AdvanceToggle(name)

    if (type(utilities.Toggles[name]) ~= 'boolean') then
        return;
    elseif utilities.Toggles[name] then
        utilities.Toggles[name] = false;
    else
        utilities.Toggles[name] = true;
    end
end		-- utilities.AdvanceToggle

--[[
    SetToggle explicitly sets the value of the passed binary variable

    Parameters
        Name        Name of toggle
        Val         Value to set toggle to
--]]

function utilities.SetToggle(name,val)

    if (type(utilities.Toggles[name]) ~= 'boolean' or type(val) ~= 'boolean') then
        return;
    else
        utilities.Toggles[name] = val;
    end
end		-- utilities.SetToggle

--[[
    Reminder is a simple routine used for displaying a nag message for the player
    to run /GC. Initially it displayes 15 seconds after logging in, but from then on
    it will display every 5 minutes until /GC is run. (Gear swapping does not occur
    until this command is run.)
--]]

function utilities.Reminder()
    local iTestVal = crossjobs.settings.bMinBasetime;
    local iNow = os.time();

    if gear.bGC == true then
        return;
    end

    if crossjobs.settings.bGCReminder == true then
        -- Since reminder already shown once, change the wait
        -- interval from 15 seconds to 5 minutes
        iTestVal = crossjobs.settings.bMaxBasetime;
    end

    if os.difftime(iNow,crossjobs.basetime) >= iTestVal then
        print(chat.message('************'));
        if iTestVal == crossjobs.settings.bMinBasetime then
            print(chat.message('FYI: Remember to do a /gc once \'data download\' finishes'));
        else
            print(chat.message('FYI: Remember to do a /gc'));
        end
        print(chat.message('************'));
        crossjobs.settings.bGCReminder = true;
        -- Change the base to current so that comparison is from now forward
        crossjobs.basetime = iNow;
    end
end     -- utilities.Reminder

--[[
    ProcessedTally determines if the passed in counter meets the reporting
    requirements and displays a message if appropriate

    Parameters
        sWhat   Name of what is being processed
        iCnt    How many the total count is
        iDiv    Size of count to report on
--]]

function utilities.ProcessedTally(sWhat,iCnt,iDiv)
    if iCnt == 0 or iDiv == 0 then
        return;
    end

    if math.floor(iCnt/iDiv) == iCnt/iDiv then
        print(chat.message(tostring(iCnt) .. sWhat .. ' processed...'));
    end
end     -- utilities.ProcessedTally

--[[
    fFormattedWord takes the passed in word and formats it in the indicated
    style.

    Parameter:
        sWord       Word to format
        sStyle      Style of the formatting

    Returned:
        Formatted string or nil
--]]

function utilities.fFormattedWord(sWord,sStyle)
local sTmp = nil;

    if sWord ~= nil then
        if sStyle == utilities._SLOT_FA then
            sTmp = string.upper(string.sub(sWord,1,1)) .. string.lower(string.sub(sWord,2,-1));
        elseif sStyle == utilities._SLOT_LA then
            sTmp = string.lower(sWord);
        elseif sStyle == utilities._SLOT_UA = 'UA' then
            sTmp = string.upper(sWord);
        end
    end

    return sTmp;
end     -- utilities.fFormattedWord

--[[
    Message toggles on/off a feedback mechanism for all luashitacast commands
--]]

function utilities.Message(toggle, status)
    if toggle ~= nil and status ~= nil then
        print(chat.message('Info: ' .. toggle .. ' is now ' .. tostring(status))))
    end
end		-- utilities.Message

--[[
    fCheckTime determines if the current server time is found in the passed name time range.

    Parameters:
        hr          Current hour
        sTime       Named time to check against

    Returned:
        True/False, error message
--]]

function utilities.fCheckTime(hr,sTime)

    local bGood=false;
    local smsg;

    if sTime == 'NIGHTTIME' then
        bGood = (hr >= 17 or hr <= 6);
    elseif t == 'DAYTIME' then
        bGood = (hr >= 6 and hr <= 18);
    elseif t == 'DUSK2DAWN' then
        bGood = (hr >= 17 or hr <= 7);
    elseif t == 'DAWN' then
        bGood = (hr >= 6 and hr <= 7);
    elseif t == 'DAY' then
        bGood = (hr >= 7 and hr <=17);
    elseif t == 'DUSK' then
        bGood = (hr >= 17 and hr <= 18);
    elseif t == 'EVENING' then
        bGood = (hr >= 18 and hr <= 20);
    elseif t == 'DEADOFNIGHT' then
        bGood = (hr >= 20 and hr <= 4);
    else
        smsg = 'Warning: Unknown named time: '.. sTime;
        return bGood,smg;
    end

    return bGood,nil;
end     -- utilities.fCheckTime


--[[
    fValidSlots determines if the passed in list of slots is valid. It then translates the valids slots
    it to the indicated format.

    Valid values are:  1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,
    main,sub,range,ammo,head,neck,ear1,ear2,body,hands,ring1,ring2,back,waist,legs,feet

    Note: rings and ears are not inherently valid since they represent two slots. These will be expanded out
        to explicitly the '1' and '2' versions of the slot.

    Parameters:
        sList       Comma delimited list of slots
        sFmt        What format should be used in the list

    Returned:
        bGood       Was the slot list valid?
        sList       Comma delimited correctly formatted list

        !!!
--]]

-- Need more work. The inverted portion will not work if the passed in numbers are not in order!!!

function utilities.fValidSlots(sList,sFmt)
    local soList = ',';
    local slots = {
         [1] =  {['Name'] = 'MAIN',  ['Have'] = false},   [2] = {['Name'] = 'SUB',   ['Have'] = false},
         [3] =  {['Name'] = 'RANGE', ['Have'] = false},   [4] = {['Name'] = 'AMMO',  ['Have'] = false},
         [5] =  {['Name'] = 'HEAD',  ['Have'] = false},   [6] = {['Name'] = 'NECK',  ['Have'] = false},
         [7] =  {['Name'] = 'EAR1',  ['Have'] = false},   [8] = {['Name'] = 'EAR2',  ['Have'] = false},
         [9] =  {['Name'] = 'BODY',  ['Have'] = false},  [10] = {['Name'] = 'HANDS', ['Have'] = false},
         [11] = {['Name'] = 'RING1', ['Have'] = false},  [12] = {['Name'] = 'RING2', ['Have'] = false},
         [13] = {['Name'] = 'BACK',  ['Have'] = false},  [14] = {['Name'] = 'WAIST', ['Have'] = false},
         [15] = {['Name'] = 'LEGS',  ['Have'] = false},  [16] = {['Name'] = 'FEET',  ['Have'] = false}
        };

    local s = utilities._SLOT_LA .. utilities._SLOT_UA .. utilities._SLOT_FA .. utilities._SLOT_N;
    if sFmt == nil or string.find(s,sFmt) == nil then
        sFmt = utilities._SLOT_FA;      -- Unknown or missing code, assume Upper first letter and lower rest
    end

    sList = ',' .. string.upper(sList) .. ',';

    -- Now process the list. Note: this process will not complain about a mistaken slot name/number
    if string.find(sList,'EARS') ~= nil then
        slots[7]['Have'] = true;    -- Assume both ears if EARS encountered
        slots[8]['Have'] = true;
    elseif string.find(sList,'RINGS') ~= nil then
        slots[11]['Have'] = true;   -- Assume both rings if RINGS encountered
        slots[12]['Have'] = true;
    else
        for i,j in ipairs(slots) do
            if string.find(sList,j['Name']) ~= nil or string.find(sList,','..tostring(i)..',') ~= nil then
                slots[i]['Have'] == true;
            end
        end
    end

    -- And now create the returned list
    for i,j in ipairs(slots) do
        if j['Have'] == true then
            soList = soList .. utilities.fFormattedWord(j['Name'],sFmt) .. ',';
        end
    end

    -- Then format the list accordingly
    if soList ~= ',' then
        soList = string.sub(soList,2,-2);     -- Remove the extra commas
        return true,soList;
    else
        return false,nil
    end
end     -- utilities.fValidSlots

--[[
    fTranslateWhichSlot determines if the passed in value is a valid slot designation and then translates it to the
    indicated format.  Only a single value should be passed in.

    Valid val's are:  1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,
                      main,sub,range,ammo,head,neck,ear1,ear2,body,hands,ring1,ring2,back,waist,legs,feet

    Note: rings and ears are not valid since they represent two slots. The invoking routine must decode these
    before invoking this function.

    Parameters:
                val         value   slot number/name
                sType       N,LA,MA  designates what format the output should be in: numeric, lowercase alpha, Mixed case alpha

    Returned:
                requested slot formatted as indicated
                or nil if passed in slot invalid or an error occurs
--]]

function utilities.fTranslateWhichSlot(val,sType)
    local sValidSlotOutputTypes = utilities._SLOT_LA .. ',' .. utilities._SLOT_UA .. ',' .. utilities._SLOT_FA .. ',' .. utilities._SLOT_N;
    local slots = {
        { ['aSlot'] = 'main',  ['nSlot'] = 1,  ['fSlot'] = 'Main' },
        { ['aSlot'] = 'sub',   ['nSlot'] = 2,  ['fSlot'] = 'Sub' },
        { ['aSlot'] = 'range', ['nSlot'] = 3,  ['fSlot'] = 'Range' },
        { ['aSlot'] = 'ammo',  ['nSlot'] = 4,  ['fSlot'] = 'Ammo' },
        { ['aSlot'] = 'head',  ['nSlot'] = 5,  ['fSlot'] = 'Head' },
        { ['aSlot'] = 'neck',  ['nSlot'] = 6,  ['fSlot'] = 'Neck' },
        { ['aSlot'] = 'ear1',  ['nSlot'] = 7,  ['fSlot'] = 'Ear1' },
        { ['aSlot'] = 'ear2',  ['nSlot'] = 8,  ['fSlot'] = 'Ear2' },
        { ['aSlot'] = 'body',  ['nSlot'] = 9,  ['fSlot'] = 'Body' },
        { ['aSlot'] = 'hands', ['nSlot'] = 10, ['fSlot'] = 'Hands' },
        { ['aSlot'] = 'ring1', ['nSlot'] = 11, ['fSlot'] = 'Ring1' },
        { ['aSlot'] = 'ring2', ['nSlot'] = 12, ['fSlot'] = 'Ring2' },
        { ['aSlot'] = 'back',  ['nSlot'] = 13, ['fSlot'] = 'Back' },
        { ['aSlot'] = 'waist', ['nSlot'] = 14, ['fSlot'] = 'Waist' },
        { ['aSlot'] = 'legs',  ['nSlot'] = 15, ['fSlot'] = 'Legs' },
        { ['aSlot'] = 'feet',  ['nSlot'] = 16, ['fSlot'] = 'Feet' }
    };

    if val == nil then
        print(chat.header('fTranslateWhichSlot'):append(chat.message('No slot specified. Skipping...')));
        return nil;
    elseif string.find(val,',') ~= nil then
        print(chat.header('fTranslateWhichSlot'):append(chat.message('Only one slot can be passed in: ' .. val .. 'Skipping...')));
        return nil;
    end

    if sType ~= nil then
        sType = string.upper(sType);    -- Make sure uppercase
    end

    if sType == nil or string.find(sValidSlotOutputTypes,sType) == nil then
        sType = utilities._SLOT_LA;       -- assume lowercase slot name
    end

    val = string.lower(val);             -- Make sure lowercase

    for i,j in pairs(slots) do
        -- Since we don't know if the passed in value is a string or a number or a number passed in as a string...
        if (type(val) == 'string' and (j['aSlot'] == val or j['nSlot'] == tonumber(val)) or (type(val) == 'number' and j['nSlot'] == val) then
            -- There was a match, format the result accordingly
            local rVal;
            if sType == utilities._SLOT_FA then      -- Mixed case: Upper first letter, lower rest
                rVal = j['fSlot'];
            elseif sType == utilities._SLOT_LA then  -- lowercase name
                rVal = j['aSlot'];
            elseif sType == utilities._SLOT_N then  -- lowercase name
                rVal = j['nSlot']
            elseif sType == utilities._SLOT_UA then  -- uppercase name
                rVal = string.upper(j['aSlot']);
            else                                    -- numeric
                rVal = j['nSlot'];
            end
            return rVal;
        end
    end

    -- If you got here it wasn't found
    print(chat.message('Warning: Unrecognized slot: ' .. val .. 'Skipping...'));
    return nil;
end     -- utilities.fTranslateWhichSlot

--[[
    fGetRoot determines the "base" of a spell/song name passed in. (The base is the first word in the spell/song name.)

    Parameters:
        sSpell      Spell or song name to take the root of
        bVersion    T/F     If true will only remove the version number of the spell (eg., I, II, III... etc)

    Returned:
        Root of spell/song name or nil if an error occurred
--]]

function utilities.fGetRoot(sSpell,bVersion)
    local i,root;

    if bVersion == nil then
        bVersion = false;
    end

    sSpell = string.lower(sSpell);
    root = sSpell;

    if bVersion == true then
        i = string.find(spell, " [^ ]*$");
        if i ~= nil and string.find('i,ii,iii,iv,v,vi',string.sub(spell,i+1,-1)) ~= nil then
            root = string.sub(spellName,1,i-1);
        end
    else
        i = string.find(sSpell,' ');
        if i ~= nil then
            root = string.sub(sSpell,1,i-1);
        end

        -- Only ninjutsu have a ":" in the name. Remove if found on the end
        if string.sub(root,-1,-1) == ':' then
            root = string.sub(root,1,-2);
        end
    end
    return root;
end     -- utilities.fGetRoot

--[[
    returns the gear set that is associated with the set name passed to it.
    It does this by walking the Sets (either gProfile.Sets or crossjobs.Sets)

    Parameter
        sName           Name of gearset. Might include where the gearset is

    Returned
        gearset/nil     If found, returns the gearset
--]]

function utilities.fGetTableByName(sName)
    local bProfile = false;
    local bCrossjobs = false;
    local sName2;

    sName2 = string.lower(sName);
    if string.find(sName2,'gprofile.sets.') ~= nil then
        sName2 = string.sub(sName2,14,-1);
        bProfile = true;
    elseif string.find(sName2,'crossjobs.sets.') ~= nil then
        sName2 = string.sub(sName2,16,-1);
        bCrossjobs = true;
    end

    if bProfile == true or (bProfile == false and bCrossjobs == false) then
        for k,l in pairs(gProfile.Sets) do
            if string.lower(k) == sName2 then
                return l;
            end
        end
        if bProfile == true then
            return nil;
        end
    end

    if bCrossjobs == true or (bProfile == false and bCrossjobs == false) then
        for k,l in pairs(crossjobs.Sets) do
            if string.lower(k) == s2 then
                return l;
            end
        end
    end

    return nil;
end     -- utilities.fGetTableByName

--[[
    fMakeConditionalTable takes the passed, // delimited list and
    returns the individual codes in a table.

    Parameters:
        sList       delimited list of inline conditionals
        del         what delimiter to split on

    Returned:
        Table of the split apart inline conditionals, all uppercase
--]]

function utilities.fMakeConditionalTable(sList,del)
    local tTbl = {};
    local iPos;

    if del == nil then
        del == '//';
    end

    if string.find(sList,del) == nil then
        return;
    end

    sList = string.upper(sList);
    iPos = 1;		-- Assume start at first position
    while iPos ~= nil do
        -- Look for the next //
        iPos = string.find(string.sub(sList,3,-1),del);
        if iPos ~= nil then
            -- since found, add the current entry
            table.insert(tTbl,string.sub(sList,3,iPos+2-1));    -- skip the //, include up to next //
            -- and move the list to the next inline
            sList = string.sub(sList,iPos+2,-1);                -- save portion from next // onwards
        else
            -- since no more found, just save the conditional entry
            table.insert(tTbl,string.sub(sList,3,-1));          -- skip the //
        end
    end
    return tTbl;
end     -- utilities.fMakeConditionalTable

--[[
    fBuffed determines if the player has the buff/debuff or not. The passed buff name
    can be a substring, but make sure the that's all that is needed to uniquely identify
    the buff.

    Parameters:
        sCode               Name of the buff/debuff
        bStart      T/F     sCode must match from start?

    Returned:
        True or false if buff/debuff found
--]]

function utilities.fBuffed(sCode,bStart)
    local buffs = AshitaCore:GetMemoryManager():GetPlayer():GetBuffs();
    local pos;

    if bStart == nil then
        bStart = false;
    end

    sCode = string.lower(string.gsub(sCode,'_',' '));
    for _, buff in pairs(buffs) do
        local buffString = AshitaCore:GetResourceManager():GetString("buffs.names", buff);

        if (buffString) then
            pos = string.find(string.lower(buffString),sCode);
            if pos ~= nil then
                if (bStart == true and pos == 1) or (bStart == false) then
                    return true;
                end
            end
        end
    end
    return false;
end     --  utilities.fBuffed

--[[
    fCheckPartyJob determines if the party has a member whose job is the passed job.

    Parameters
        job         The job to look for
        bNotMe      Should I be excluded from the search

    Returned
        bFound      Is there a player that matches the criteria
--]]

function utilities.fCheckPartyJob(job,bNotMe)
    local pParty = AshitaCore:GetMemoryManager():GetParty();
    local bFound = false;
    local iStart = 1;

    if bNotMe ~= nil and bNotMe == true then
        iStart = 2;
    end

    job = string.upper(job);

    for i=iStart,6,1 do
        if (pParty:GetMemberIsActive(i - 1) == 1) then
            -- Player found
            local mainJob = pParty:GetMemberMainJob(i - 1);
            local j = AshitaCore:GetResourceManager():GetString("jobs.names_abbr", mainJob);
            if string.find(job,j) ~= nil then
                bFound = true;
            end
        end
    end
    return bFound;
end     -- utilities.fCheckPartyJob

--[[
    ClearSet blanks out the passed gear set

    Parameter
        gSet    Gear set to blank out
--]]

function utilities.ClearSet(gSet)

    for k,v in pairs(gData.Constants.EquipSlots) do
        gSet[k] = '';
    end
end		-- utilities.ClearSet

--[[
    fTargetId extracts the target's reference ID and returns it to the invoker

    Parameter
        TargetIndex     Target Index structure

    Returned
        TargetID
--]]

function utilities.fTargetId(TargetIndex)

    if targetIndex == nil then
        return ' ';
    else
        local TargetServerId = AshitaCore:GetMemoryManager():GetEntity():GetServerId(TargetIndex);
        local TargetServerIdHex = string.format('0x%X', TargetServerId);

        return string.sub(TargetServerIdHex, -3);
    end
end		-- utilities.fTargetId

--[[
    PullTarget determines how the player wants to pull the target (gear/pet) and then pulls it.
--]]

function utilities.PullTarget()
    local targetIndex = gData.GetTargetIndex();
    local targetEntity = gData.GetEntity(targetIndex);
    local sTxt = nil;

    if targetIndex ~= 0 then
        if string.find('BST,SMN,PUP',player.MainJob) ~= nil then
            if gData.GetPet() ~= nil then
                sTxt = '/pet assault <t>';
            else
                print(chat.message('Info: No pet found, assuming a normal pull'));
            end

            if sTxt == nil then
                sTxt = '/ra <t>';
            end

            if utilities.fGetToggle('sPF') == true then
                local sMsg = '/p Pulling ' .. targetEntity.Name .. ' [' .. utilities.fTargetId(targetIndex) .. ']';
                AshitaCore:GetChatManager():QueueCommand(-1, sMsg);
            end
            AshitaCore:GetChatManager():QueueCommand(-1, sTxt);
        end
    else
        print(chat.message('Info: Unable to pull anything, no target selected'));
    end
end		-- utilities.PullTarget


--[[
    fSetColorText returns the color code needed to change text to the associated color

    Parameters
        bVal        True/False, Green for valid, Red for invalid
        bInvert     True/False, True for the colors to be inverted, False to leave as is

    Note:
        value: 2 - green, 8 - red, 107 - other (probably yellow).
--]]

function utilities.fSetColorText(bVal,bInvert)
    local _GREEN = 2;
    local _RED = 8;
    local _OTHER = 107;
    local ic;

    if bInvert == nil then
        bInvert = false;
    end

    if bVal == nil then
        ic = _OTHER;    -- Default, other color
    elseif (bVal == false and bInvert == false) or
            (bVal == true and bInvert == true) then
        ic = _RED;
    else
        ic = _GREEN;
    end

    return ic;
end		-- utilities.fSetColorText

--[[
    fRemoveConditional takes the passed in string and removes any inline
    conditional qualifier from it

    Parameter
        g       Gear name with potential inline conditionals

    Returned
        g       Gear named stripped of conditionals
--]]

function utilities.fRemoveConditional(g)

    if g == nil then
        return nil;
    end

    local iPos = string.find(g,'//');

    if iPos ~= nil then
        return string.sub(g,1,iPos-1);
    else
        return g;
    end
end		-- utilities.fRemoveConditional

--[[
    fBit and fHasBit are bit manipulation functions used in fCheckItemOwned.
    Removing these functions and expanding out the formula makes things look
    messy
]]

function fBit(p)
    return 2 ^ (p - 1);
end		-- fBit

function fHasBit(x, p)
    return x % (p + p) >= p;
end		-- fHasBit

--[[
    fCheckItemOwned determines if the specified piece of gear is owned by
    the playera and some details on accessibility and storage.

    Parameter
        gear        Name of gear to check

    Returned
        tOwned      Reference to the tracked item details
--]]

function utilities.fCheckItemOwned(gear)
    local inventory = AshitaCore:GetMemoryManager():GetInventory();
    local resources = AshitaCore:GetResourceManager();
    local containerID,itemEntry,item;
    local tOwned = {
        ['own'] = false, ['accessible'] = false, ['porter'] = false, ['claim'] = false,
        ['locations'] = nil, ['error'] = nil
    };

    -- Make sure a piece of gear specified
    if gear == nil then
        tOwned['error'] = 'Invalid gear item';
        return tOwned;
    end

    -- Loop through all searching for the passed gear piece
    for i,desc in pairs(utilities.STORAGES) do
        containerID = desc['id'];
        -- then loop through the container
        for j = 1,inventory:GetContainerCountMax(containerID),1 do
            itemEntry = inventory:GetContainerItem(containerID, j);
            if (itemEntry.Id ~= 0 and itemEntry.Id ~= 65535) then
                item = resources:GetItemById(itemEntry.Id);
                if item.Name[1] == gear.Name[1] then
                    tOwned['own'] = true;
                    if tOwned['locations'] == nil then
                        tOwned['locations'] = ',' .. desc['name'] .. ',';
                    elseif string.find(tOwned['locations'],','..desc['name']..',') == nil then
                        tOwned['locations'] = tOwned['locations'] .. desc['name'] .. ',';
                    end
                    if table.find(utilities.EQUIPABLE_LIST,desc['id']) then
                        tOwned['accessible'] = true;
                    end
                end
            end
        end
    end

    -- if locations defined, remove the encasing commas
    if tOwned['locations'] ~= nil then
        tOwned['locations'] = string.sub(tOwned['locations'],2,-2);
    end

    -- Then loop through storage slips to see if item stored
    for i,desc in pairs(slips.tSlips) do
        -- If item slip owned by player...
        if desc['own'] == true then
            local iPos = table.find(desc['items'],gear.Id);
            -- See if the passed gear associated with that slip
            if iPos ~= nil then
                -- Now figure out if the item is stored on that slip
                local byte = struct.unpack('B',desc['extra'],math.floor((iPos - 1) / 8) + 1);
                if byte < 0 then
                    byte = byte + 256;
                end
                if (fHasBit(byte, fBit((iPos - 1) % 8 + 1))) then
                    -- Yup, add the slip name to the location
                    tOwned['own'] = true;
                    tOwned['porter'] = true;
                    if tOwned['locations'] == nil then
                        tOwned['locations'] = desc['name'];
                    else
                        tOwned['locations'] = tOwned['locations'] .. ', ' .. desc['name'];
                    end
                    break;
                end
            end
        end
    end

    -- Lastly, see if stored on a claim slip
    for i,desc in pairs(slips.ClaimSlips) do
        if desc['own'] == true and table.find(desc['ids'],gear.Id) ~= nil then
            tOwned['own'] = true;
            tOwned['claim'] = true;
            if tOwned['locations'] == nil then
                tOwned['locations'] = desc['name'];
            else
                tOwned['locations'] = tOwned['locations'] .. ', ' .. desc['name'];
            end
            break;
        end
    end

    return tOwned;
end		-- utilities.fCheckItemOwned

--[[
    fSlotMatch determines if the passed slot the gear piece is being loaded into
    matches the item's slot designation

    Parameter
        sSlot       Slot name being processed
        iSlot       Item slot name

    Returned
        T/F, was a match found
--]]

function utilities.fSlotMatch(sSlot,iSlot)
    local bGood = false;

    sSlot = string.lower(sSlot);

    -- Make sure the composite slots are represented by an actual slot
    if sSlot == 'rings' then
        sSlot = 'ring1';
    elseif sSlot == 'ears' then
        sSlot = 'ear1';
    end

    -- The lock list has all slots identified. The slot masks have been added
    -- to the tLocks structure. Even though the mask is a bit pattern, the
    -- composited value is included too. That's why I only need to look for
    -- a match.
    for i,j in ipairs(locks.tLocks) do
        if j['slot'] == sSlot then
            bGood = (table.find(j['mask'],iSlot) ~= nil);
        break;
        end
    end

    return bGood;
end		-- utilities.fSlotMatch

--[[
    fReferenceCheck determines if any of the passed gear is actually
    a reference to another set's slot.

    Parameter
        ts      item or array of gear to check for a reference

    Returned
        T/F     Does ts contain an inline reference
--]]

function utilities.fReferenceCheck(ts)
    local t = {};
    local bFound = false;

    if ts == nil then
        return false;
    end

    if type(ts) == 'string' then
        t[1] = ts;
    else
        t = ts;
    end

    for i,j in pairs(t) do
        if string.find(j,'::') then
            bFound = true;
            break;
        end
    end
    return bFound;
end		-- utilities.fReferenceCheck

--[[
    fGetLevel determines what main job level should gear be compared to. It
    uses either the actual level or the player capped level.

    Parameter
        bActual     T/F, should the actual level be returned (not capped)?

    Returned
        level       Either capped or actual level
--]]

function utilities.fGetLevel(bActual)
    local player = gData.GetPlayer();

    if bActual == nil then
        bActual = false;
    end

    if bActual == true or gProfile.settings.PlayerCappedLevel == 0 then
        -- Actual max level wanted
        return player.MainJobSync;
    else
        -- Player capped level wanted
        return gProfile.settings.PlayerCappedLevel;
    end
end     -- utilities.fGetLevel

--[[
    fCheckObiDW determines if the weather/day element makes equiping an elemental
    obi advantageous.

    Parameter
        ele         Element to check for

    Returned
        PctDay      % calculation based on day's element
        PctWeather  % calculation based on weather's element

    Note: Elemental obis can be useful when closing a skillchain with certain weaponskills.
    This code does NOT track that opportunity, so it is not even considered.
--]]

function utilities.fCheckObiDW(ele)
    local sEnvironment = gData.GetEnvironment();
    local sWeak =
    local sDay = sEnvironment.DayElement;
    local PctDay = 0;
    local PctWeather = 0;

    -- Make sure a valid element specified
    if ele == nil or string.find(utilities._AllElements,ele) == nil then
        return nil;
    end

    ele = string.lower(ele);
    sWeak = utilities.tElemental_gear['staff'][ele]['Weak'];    -- Elemental weakness tracked here

    -- First, the day
    if string.lower(sDay) == ele then
        PctDay = 10;
    elseif sWeak == ele then
        PctDay = -10;
    end

    -- Next the weather
    if string.lower(sEnvironment.WeatherElement) == ele then
        if string.find(sEnvironment.Weather,'x2') ~= nil then 		-- There's a storm of the element
            PctWeather = 25
        else
            PctWeather = 10;
        end
    else
        -- Weather doesn't match. Check to see if the weather weakens the element
        if string.lower(sEnvironment.WeatherElement) == sWeak then
            if string.find(sEnvironment.Weather,'x2') ~= nil then 	-- There's a storm of the element
                PctWeather = -25
            else
                PctWeather = -10;
            end
        end
    end

    -- Lastly, check for iridescence/prismatic
    local g = gEquip.GetCurrentEquip(1);
    if AshitaCore:GetResourceManager():GetItemById(g.Item.Id).Name[1] == 'Claustrum' then	-- Only case I know of with prismatic
        if PctWeather < 0 then  -- indicates element weak to weather
            PctWeather = PctWeather - (0.1 * math.abs(PctWeather));     -- -10% penalty from iridescence
        else
            PctWeather = PctWeather + (0.1 * math.abs(PctWeather));     -- +10% bonus from iridescence
        end
    end

    return PctDay,PctWeather;
end		-- utilities.fCheckObiDW

--[[
    SetAlias registers all of the luashitacast commands that are defined in this file
--]]

function SetAlias()

    for _, v in ipairs(utilities.AliasList) do
        AshitaCore:GetChatManager():QueueCommand(-1, '/alias /' .. v .. ' /lac fwd ' .. v);
    end
end		-- SetAlias

--[[
    ClearAlias removes the luashitacast commands that were registered here
--]]

function ClearAlias()
    for _, v in ipairs(utilities.AliasList) do
        AshitaCore:GetChatManager():QueueCommand(-1, '/alias del /' .. v);
    end
end		-- ClearAlias

--[[
    Initialize gives luashitacast it's initial settings
--]]

function utilities.Initialize()
    displaybar.InitializeDisplayBar:once(2);
    crossjobs.SetVariables:once(2);
    SetAlias:once(2);
end		-- crossjobs.Initialize


--[[
    Unload ensures that the display settings are saved, the aliases are removed,
    and the display objects are removed
--]]

function utilities.Unload()
    --SaveSettingFile();
    ClearAlias();
    ashita.events.unregister('packet_in', 'packet_in_callback1');
    displaybar.Unload();
end		-- utilities.Unload

--[[
    fCheckWsBailout determines if there's a debuff, distance to target, or insufficient TP
    that will cause a weapon skill to fail thus losing all player's TP.

    Returned
        T/F
--]]

function utilities.fCheckWsBailout()
    local player = gData.GetPlayer();
    local ws = gData.GetAction();
    local target = gData.GetActionTarget();
    local bGood = true;

    if crossjobs.settings.WScheck == true and
       tonumber(target.Distance) > crossjobs.settings.WSdistance then
        print(chat.message('Warning: Distance to mob is too far! Move closer to target'));
        bGood = false;
    elseif player.TP <= 999 then
        print(chat.message('Warning: insufficient TP to weapon skill'));
        bGood = false;
    elseif utilities.fBuffed('Sleep',true) == true or
           utilities.fBuffed('Petrification',true) == true or
           utilities.fBuffed('Stun',true) == true or
           utilities.fBuffed('Amnesia',true) == true or
           utilities.fBuffed('Charm', true) == true then
        print(chat.message('Warning: detrimental debuff inhibiting any action'));
        bGood - false;
    end

    return bGood;
end		-- utilities.fCheckWsBailout

--[[
    fMagicSubJob determines if the sub job can do magic

    Returned
        T/F
--]]

function utilities.fMagicalSubJob()
    local player = gData.GetPlayer();
    local sj = player.SubJob;

    return (string.find(utilities._sMagicJobs,sj) ~= nil);
end		-- utilities.fMagicalSubJob

--[[
    NewFileName generates a new file name based on the player's character name and
    the date.

    Returned
        Generated name
--]]

function fNewFileName()
    local player = gData.GetPlayer();
    local sName = string.upper(player.Name) .. '-';

    sName = sName .. string.gsub(string.format("%x",os.clock),'/','_') .. '.txt';
    return sName;
end     -- fNewFileName

--[[
    OpenByFilename determines if the passed file name is a wild card. If so, it
    creates a file named after the player's name and date. The file is opened and
    the file pointer and name is returned.

    Parameter
        sname       Name of the report or * if program should create a name

    Returned
        fptr        Pointer to opened file
        fname       Name of file opened
--]]

function utilities.OpenByFilename(sname)
    local fname;
    local fptr;

    if sname == nil then
        sname = '*';
    end

    if sname == '*' then
        fname = fNewFileName();
    else
        fname = sname;
    end

    fname = 'Reports.' .. fname;
    fptr = io.open(fname,"+");
    return fptr,fname;
end     -- utilities.OpenByFilename

