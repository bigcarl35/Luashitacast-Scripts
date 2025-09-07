local inline = T{};

local utilities = require('utilities');
local crossjobs = require('crossjobs');
local gear      = require('gear');

-- Define a list of all towns and the areas that are specific to each nation and Jeuno
inline.tAreas = {
    ['Towns'] = { 'Tavnazian Safehold','Al Zahbi','Aht Urhgan Whitegate','Nashmau','Southern San d\'Oria [S]','Bastok Markets [S]',
        'Windurst Waters [S]','San d\'Oria-Jeuno Airship','Bastok-Jeuno Airship','Windurst-Jeuno Airship','Kazham-Jeuno Airship',
        'Southern San d\'Oria','Northern San d\'Oria','Port San d\'Oria','Chateau d\'Oraguille','Bastok Mines','Bastok Markets',
        'Port Bastok','Metalworks','Windurst Waters','Windurst Walls','Port Windurst','Windurst Woods','Heavens Tower',
        'Ru\'Lude Gardens','Upper Jeuno','Lower Jeuno','Port Jeuno','Rabao','Selbina','Mhaura','Kazham','Norg','Mog Garden',
        'Celennia Memorial Library','Western Adoulin','Eastern Adoulin' },
    ['Windy']  = { 'Windurst Waters [S]','Windurst Waters','Windurst Walls','Port Windurst','Windurst Woods','Heavens Tower' },
    ['Sandy']  = { 'Southern San d\'Oria [S]','Southern San d\'Oria','Northern San d\'Oria','Port San d\'Oria','Chateau d\'Oraguille' },
    ['Bastok'] = { 'Bastok Markets [S]','Bastok Mines','Bastok Markets','Port Bastok','Metalworks' },
    ['Jeuno']  = { 'Ru\'Lude Gardens','Upper Jeuno','Lower Jeuno','Port Jeuno' }
};

-- Define target familes for Aquans and Amorphs
inline.tTargetFamily = {
    ['aquans'] = {
        ['crabs'] = {
            'river crab','limicoline crab','palm crab','savanna crab','stone crab','tree crab','sand crab','mine crab','land crab',
            'mole crab','mugger crab','vermivorous crab','wadi crab','coral crab','passage crab','ocean crab','sea crab','thickshell',
            'stag crab','triangle crab','snipper','snapper','blind crab','clipper','cutter','ghost crab','grindylow','gugru crab',
            'crimson knight crab','knight crab','ironshell','bigclaw','jungle crab','cyan deep crab','submarine nipper','rock crab',
            'scavanger crab','robber crab','greatclaw','aydeewa crab','wootzshell','sicklemoon crab','nipper','steelshell',
            'mamook crab','carrion crab','kelp crab','angler crab','aquarius','cancer','cargo crab colin','duke decapod','king arthro',
            'metal shears','tegmine','wake warder wanda','aphotic crab','bubbly bernie','bloody coffin','crabshaw','ferrocrab',
            'heike crab','megapod megalops','metal crab','heavy metal crab','adamantshell','nightmare crab','overlord arthro',
            'lord\'s bruiser','lord\'s wizard','poisonous crab'
        },
        ['pugils'] = {
            'pugil','cheval pugil','giddeus pugil','ghelbsa pugil','pug pugil','mud pugil','fighting pugil','giant pugil','sea pugil',
            'puffer pugil','ocean pugil','land pugil','spring pugil','swamp pugil','sand pugil','davoi pugil','pirate pugil','bigjaw',
            'ferocious pugil','fosse pugil','beach pugil','shoal pugil','quifim pugil','greater pugil','spinous pugil','fatty pugil',
            'vepar','dagon','ocean jagil','gugru pugil','jagil','makara','apsaras','grotto pugil','cyan deep pugil','razorjaw pugil',
            'thalassic pugil','abyssal pugil','canal pugil','stygian pugil','immolatory pugil','demonic pugil','azoth apsaras',
            'terror pugil','mercurial makara','cave pugil','suhur mas','la vaule pugil','zazalda jagil','sicklemoon jagil',
            'vozold jagil','sulphuric jagil','buburimboo','capricornus','hippomaritimus','lancet jagil','qoofim','sea hog','serra',
            'swamfisk','ziphius','abhac','isonade','water leaper','odontotyrannus','blind moby','percipient fish','archer pugil',
            'sniper pugil','aipaloovok','enhanced pugil','nightmare makaras'
        },
        ['ruszors'] = {
            'ruszor','savage ruszor', 'scylla'
        },
        ['sea monks'] = {
            'sea monk','sea bishop','kraken','colossal calamari','ocean kraken','morgawr','blanched kraken','sea bonze','flying manta',
            'kulshedra','bathybic kulushedra','devil manta','lahama','nostokulshedra','charybdis','lord of onzozo','peg powler',
            'proteus','sea horror','beach monk','glyryvilu','tros','vu-murt','dalham','honor','valor','fe\'e','nightmare kraken',
            'scolooendra','sjokrakjen','tieholtsodi'
        },
        ['toads'] = {
            'toad','flume toad'
        },
        ['uragnites'] = {
            'uragnite','coraline uragnite','young uragnite','nightmare uragnite','harajnite','shankha','shen','zoredonite','amphitrite',
            'blademall','nepionic blademall','parata','cyclopean conch'
        }
    },
    ['amorphs'] = {
        ['flans'] = {
            'black pudding','ebony pudding','pitchy pudding','dextrose','flammeri','immortal flan','two-faced flan','anise custard',
            'caraway custard','cinnamon custard','cumin custard','ginger custard','nutmeg custard','mint custard','vanilla custard',
            'black pudding','empathic flan','flux flan','princess pudding','fighting flan','mokkuralfi','liquified einherjar'
        },
        ['hecteyes'] = {
            'hecteyes','taisai','gazer','blubber eyes','desert gazer','thousand eyes','dodomeki','mindgazer','million eyes','argus',
            'hakutaku','hyakume','shoggoth','taisaijin','galgalim','mokumomuren','compound eyes','sobbing eyes','vanguard hecteyes',
            'waldgeist'
        },
        ['leeches'] = {
            'stickpin','wadi leech','swamp leech','thread leech','forrest leech','poison leech','gigas\'s leech','huge leech',
            'horrid fluke','acrophies','big leech','royal leech','goblin\'s leech','goobbue parasite','canal leech','labyrinth leech',
            'yagudo parasite','uggalepih leech','sahagin parasite','bloodsucker','caedarva leech','bouncing ball','leech',
            'anautogenous slug','red smoocher','red osculator','red kisser','phlebotomic slug','kissing leech','mamool ja bloodsucker',
            'ashakku','volcanic leech','arrapago leech','blood ball','nirgali','nightmare leech','aroma leech','bloodpool vorax',
            'bloodsucker','canal moocher','chocoboleech','jammer leech','leech king','masan','slippery sucker','cetic parasite',
            'korroloka leech','undead leech','pepper','phoedme','prune','hazhalm leech','utgarth leech'
        },
        ['sandworms'] = {
            'sandworm','glavoid'
        },
        ['slimes'] = {
            'giant amoeba','oil slick','rotten jam','blob','goblin gruel','black slime','jelly','sponge','water pumpkin','ooze',
            'protozoan','freshwater trepang','clot','rancid ooze','stroper chyme','davoi mush','amoebic nodule','mush',
            'ogreish rissoto','gloop','viscous clot','bavarois','oil spill','acid grease','dark aspic','mousse','caedarva pondscum',
            'caedarva marshscum','cave mold','slime mold','zazalda clot','mountain clot','vozold clot','chimera clot','brei',
            'talacca clot','blubbery bulge','ichorous ire','maltha','sewer syrup','hermatic cyst','pudding','gigaplasm','macroplasm',
            'microplasm','nanoplasm','hinge oil','princess jelly','queen jelly','vanguard\'s slime','woodnix\'s slime','enhanced jelly',
            'enhanced slime','ghost clot','metalloid amoeba','agar agar','claret','fistule','konjac','mucoid mass','glibber',
            'metallic slime','swamp muck','einherjar brei','winebibber'
        },
        ['slugs'] = {
            'scabrous slug','lou carcolh','edible slug','dyinyinga','nommo'
        },
        ['worms'] = {
            'tunnel worm','carrion worm','stone eater','dirt eater','giant grub','rock eater','earth eater','mineral eater','maze maker',
            'eikon eater','land worm','ore eater','abyss worm','amphisbaena','cliff worm','flesh eater','boreal worm','desert worm',
            'sand eater','glacier eater','cave worm','sand digger','kuftal digger','mountain worm','mold eater','bedrock barry',
            'bigmouth billy','morion worm','olgoi-khorkhoi','phantom worm','trembler tabitha','nightmare worm','mineral eater',
            'flayer franz','flesh eater','ziryu','einherjar eater','wayward event'
        }
    }
};

--[[
    fCheckInlineBuff checks the validity of the passed inline code and then determines if the
    coded condition is true.

    Parameters
        sCode       coded condition to be checked

    Returned
        bGood       was the coded condition met? T/F/nil where nil means code Unknown

    Note:
        There is no check for validation since at this point in processing, probably
        not all inlines will have been checked yet. A validation message will occur in
        the calling routine: inline.fCheckInline.
--]]

function fCheckInlineBuff(sCode)
    local bGood = nil;
    local bNot = false;
    local tBuffs = {
        'ARC_CIRCLE','COVER','HOLY_CIRCLE','SPIKE','UTSUSEMI','WARD_CIRCLE','SAMBA','ENAERO','ENBLIZZARD',
        'ENDARK','ENFIRE','ENLIGHT','ENSTONE','ENTHUNDER','ENWATER','BARAERO','BARBLIZZARD','BARFIRE',
        'BARSTONE','BARTHUNDER','BARWATER','BARSLEEP','BARPOISON','BARPARALYZE','BARBLIND','BARVIRUS',
        'BARPETRIFY','ANCIENT_CIRCLE','AFTERMATH','REPRISAL','YONIN','FLEE'
        };
    local tBarelemental  = { 'BARAERO','BARBLIZZARD','BARFIRE','BARSTONE','BARTHUNDER','BARWATER' };
    local tBarstatus     = { 'BARSLEEP','BARPOISON','BARPARALYZE','BARBLIND','BARVIRUS','BARPETRIFY' };

    sCode = string.upper(sCode);
    local i = string.find(sCode,'NOT_')
    if i ~= nil and i == 1 then
        bNot = true;
        sCode = string.sub(sCode,5,-1);
    end

    if sCode == 'BARELEMENTAL' then                     -- Does player have one of the Bar-elemental buffs?
        for ii,jj in pairs(tBarelemental) do
            local b = (utilities.fBuffed(jj,true));
            if b == true then
                bGood = true;
                break;
            end
        end
        if bGood == nil then
            bGood = false;
        end
    elseif if sCode == 'BARSTATUS' then                 -- Does player have one of the Bar-status buffs
        for ii,jj in pairs(tBarstatus) do
            local b = (utilities.fBuffed(jj,true));
            if b == true then
                bGood = true;
                break;
            end
        end
        if bGood == nil then
            bGood = false;
        end
    elseif sCode == 'BARANY' then                       -- Does the player have any barspell buff
        bGood = (utilities.fBuffed('BAR',true));
    elseif sCode == 'ENANY' then                         -- Does the player have any enspell buff
        bGood = (utilities.fBuffed('EN',true));
    else                                                -- Look for a specific buff/bar/en-spell buff
        if table.find(tBuffs,sCode) ~= nil then
            bGood = (utilities.fBuffed(sCode,true));
        end
    end

    if bGood ~= nil and bNot == true then
        bGood = not bGood;
    end

    return bGood;
end     -- fCheckInlineBuff

--[[
    fCheckInlineDebuff checks the validity of the passed inline code and then determines if the
    coded condition is true.

    Parameters
        sCode       coded condition to be checked

    Returned
        bGood       was the coded condition met? T/F/nil where nil means code Unknown

    Note:
        There is no check for validation since at this point in processing, probably
        not all inlines will have been checked yet. A validation message will occur in
        the calling routine: inline.fCheckInline.
--]]

function fCheckInlineDebuff(sCode)
    local bGood = nil;
    local bNot = false;
    local bFound = false;
    local iPos;
    local tDebuffs = {
        ['CODE']    = {
                'ADDLED', 'AMNESIA', 'BANED','BLINDED','BOUND','BUSTED','CHARMED','CURSED','DISEASED',
                'DOOMED','ENCUMBERED','IMPAIRED','KO','MEDICATED','MUTED','PARALYZED','PETRIFIED','PLAGUED',
                'POISONED','SILENCED','SLEPT','STUNNED','TERRIFIED','WEAKENED','WEIGHTED'
                },
        ['BUFF']    = {
                'Addle','Amnesia','Bane','Blind','Bind','Bust','Charm','Curse','Disease',
                'Doom','Encumbrance','Impairment','KO','Medicine','Mute','Paralysis','Petrify','Plague',
                'Poison','Silence','Sleep','Stun','Terror','Weak','Weight'
                }
    };

    sCode = string.upper(sCode);
    local i = string.find(sCode,'NOT_');
    if i ~= nil and i == 1 then
        bNot = true;
        sCode = string.sub(sCode,5,-1);
    end

    iPos = table.find(tDebuffs['CODE'],sCode);
    if iPos ~= nil then
        if sCode == 'WEAKENED' then
            bGood = (utilities.fBuffed('Weakness',true) or utilities.fBuffed('Weakened',true));
        else
            bGood = (utilities.fBuffed(tDebuffs['BUFF'][iPos]));
        end
    elseif sCode == 'DEBUFFED' then
        -- Look for any debuffs including special cases
        for ii in pairs(tDebuffs['BUFF']) do
            bFound = fBuffed(ii,true);
            if bFound == true then
                break;
            end
        end
        if bFound == false then
            bFound = (utilities.fBuffed('Bane',true) or utilities.fBuffed('Weakness',true) or utilities.fBuffed('Weakened',true));
        end
        bGood = bFound;
    end

    if bGood ~= nil and bNot == true then
        bGood = not bGood;
    end

    return bGood;
end     -- fCheckInlineDebuff

--[[
    fEvaluateCondition determines if the passed condition is true or not, no validity
    is checked.

    Parameters
        sType   coded field to check
        sOp     coded comparator
        iNum    value to compare to

    Returned
        bGood   was the coded condition met? T/F
--]]

function fEvaluateCondition(sType,sOp,iNum)
    local player = gData.GetPlayer();
    local party = gData.GetParty();
    local pet = gData.GetPet();
    local iVal;
    local bGood = false;

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
    else    -- Assume .NE.
        bGood = (iVal ~= iNum);
    end

    return bGood;
end     --  fEvaluateCondition

--[[
    fCheckInlineConditional checks the validity of the passed inline code and then determines if the
    coded condition is true.

    Parameters
        sCode       coded condition to be checked

    Returned
        bGood       was the coded condition met? T/F/nil where nil means code Unknown
        smsg        likely nil, but if validation fails, returns error message
--]]

function fCheckInlineConditional(sCode)
    local player = gData.GetPlayer();
    local pet = gData.GetPet();
    local tConds = { 'HP.','HPP.','MP.','MPP.','TP.','TPP.','LVL.','PARTY.','PETHPP.' };
    local sOps   = { '.EQ.','.GT.','.GE.','.LT.','.LE.','.NE.' };
    local bGood = nil;
    local bNot = false;
    local smsg = nil;
    local iNum,iPos,iStart;
    local sOp;

    sCode = string.upper(sCode);
    local i = string.find(sCode,'NOT_');
    if i ~= nil and i == 1 then
        bNot = true;
        sCode = string.sub(sCode,5,-1);
    end

    local iPos = string.find(sCode,'.');
    if iPos ~= nil then
       -- NOT_ is not supported on Conditional inlines. Instead of processing the
       -- request and then complaining, just complain now.
        if bNot == true then
            smsg = 'Warning: Inverted conditionals are not supported: ' .. sCode;
            return false,smsg;
        end

        -- Now, let's see if what was found matches one of the conditional types
        local sCond = string.sub(sCode,1,iPos);

        -- Deal with a special case first
        if sCond == 'PETHPP' and pet == nil then
            -- No pet, so just return false
            return false,nil;
        end

        if table.find(tConds,sCond) ~= nil then
            -- Yup. Now see if a valid operator is found
            sOp = string.sub(sCode,iPos,4);                     -- Extracted comparator
            if table.find(sOps,sOp) ~= nil then
                -- Guess it's valid (yeah, the number could be bad, go with  it).
                -- Split out the parts we need
                sCond  = string.sub(sCond,1,-2);                -- Conditional, remove the ending '.'
                iStart = iPos+3+1;                              -- length of the base field +3 for the conditional +1 to point to the number part
                iNum   = tonumber(string.sub(sCode,iStart,-1)); -- and now the number
                local bVal;
                if sCond == 'LVL' then
                    bVal = (iNum > 0 and iNum < 75);            -- Valid player level is between 1 and 75
                elseif sCond == 'PARTY' then
                    bVal = (iNum > 0 and iNum <= 18);           -- Party can be an alliance. Check up to 18
                elseif string.find('HPP,MPP,PETHPP',sCond) ~= nil then
                    if sOp == '.LT.' and iNum <= 0 or           -- Number can't be below 0 or greater than 100
                        sOp == '.GT.' and iNum >= 100 then
                        bVal = false;
                    else
                        bVal = (iNum >= 0 and iNum <= 100);    -- Valid HP% or MP% is between 0% and 100%
                    end
                elseif sCond == 'TPP' then
                    bVal = (iNum >= 0 and iNum <= 300);        -- Valid TP% is between 0% and 300%
                elseif sCond == 'TP' then
                    bVal = (iNum >= 0 and iNum <= 3000);       -- Valid TP between 0 and 3000
                else
                    bVal = true;                               -- Assume a valid number
                end

                if bVal == false then
                    smsg = 'Warning: Comparison is an invalid code or out of range: ' .. sCode;
                    return false,smsg;
                end
                bGood = fEvaluateCondition(sCond,sOp,iNum);
                return bGood,nil;
            else
                smsg = 'Warning: Invalid operator encountered or unknown code: ' .. sCode;
                return false,smsg;
            end
        end
    end

    if bGood == nil then
        if string.find(sCode,'LVLDIV:') ~= nil then
            -- LVLDIV is different than other conditionals. It's determining if your current level can
            -- be divided evenly by the passed in number.
            iNum = tonumber(string.sub(sCode,8,-1));

            if iNum ~= nil and iNum > 0 and iNum <= player.MainJobSync then
                bGood = (math.floor(player.MainJobSync/iNum) == player.MainJobSync/iNum);
                if bNot == true then
                    bGood = not bGood;
                end
            else
                smsg = 'Warning: Invalid or out of range number in code: ' .. sCode;
                return false,smsg;
            end
        end
    end

    return bGood,nil;
end     -- fCheckInlineConditional

--[[
    fCheckInlineActivity checks the validity of the passed inline code and then determines if the
    coded condition is true.

    Parameters
        sCode       coded condition to be checked

    Returned
        bGood       was the coded condition met? T/F/nil where nil means code Unknown

    Note:
        There is no check for validation since at this point in processing, probably
        not all inlines will have been checked yet. A validation message will occur in
        the calling routine: inline.fCheckInline.
--]]

function fCheckInlineActivity(sCode)
    local bGood = nil;
    local bNot = false;
    local tActs = {
        ['CRAFTS']       = { 'CR:ALC','CR:BONE','CR:BSM','CR:CLOTH','CR:COOK','CR:GSM','CR:LTH','CR:WW' },
        ['GATHERS']      = { 'GA:HELM','GA:DIG','GA:CLAM','GA:FISH' },
    };

    sCode = string.upper(sCode);
    local i = string.find(sCode,'NOT_');
    if i ~= nil and i == 1 then
        bNot = true;
        sCode = string.sub(sCode,5,-1);
    end

    if sCode == 'CR' then
        bGood = (crossjobs.Craft ~= nil);
    elseif sCode == 'GA' then
        bGood = (crossjobs.Gather ~= nil);
    else
        local iPos = string.find(sCode,':');
        if table.find(tActs['CRAFTS'],sCode) ~= nil then                 -- One of tActs['CRAFTS']
            bGood = (crossjobs.Craft == string.sub(sCode,iPos+1,-1));    -- Check for a match on the craft type
        elseif table.find(tActs['GATHERS'],sCode) ~= nil then            -- One of tActs['GATHERS']
            bGood = (crossjobs.Gather == string.sub(sCode,iPos+1,-1));   -- Check for a match on the gather type
        end
    end

    if bGood ~= nil and bNot == true then
        bGood = not bGood;
    end

    return bGood;
end     -- fCheckInlineActivity

--[[
    fCheckInlineDay checks the validity of the passed inline code and then determines if the
    coded condition is true.

    Parameters
        sCode       coded condition to be checked

    Returned
        bGood       was the coded condition met? T/F/nil where nil means code Unknown

    Note:
        There is no check for validation since at this point in processing, probably
        not all inlines will have been checked yet. A validation message will occur in
        the calling routine: inline.fCheckInline.
--]]

function fCheckInlineDay(sCode)
    local environ = gData.GetEnvironment();
    local bGood = nil;
    local bNot = false;
    local tDay = { 'DARKSDAY','EARTHSDAY','FIRESDAY','ICEDAY','LIGHTNINGDAY','LIGHTSDAY','WATERSDAY','WINDSDAY' };

    sCode = string.upper(sCode);
    local i = string.find(sCode,'NOT_');
    if i~= nil and i == 1 then
        bNot == true;
        sCode = string.sub(sCode,5,-1);
    end

    if table.find(tDay,sCode) ~= nil then
        bGood = (sCode == string.upper(environ.Day));
        if bNot == true then
            bGood = not bGood;
        end
    end

    return bGood;
end     -- fCheckInlineDay

--[[
    fCheckInlineMoon checks the validity of the passed inline code and then determines if the
    coded condition is true.

    Parameters
        sCode       coded condition to be checked

    Returned
        bGood       was the coded condition met? T/F/nil where nil means code Unknown

    Note:
        There is no check for validation since at this point in processing, probably not
        all inlines will have been checked yet. A validation message will occur in the
        calling routine: inline.fCheckInline.
--]]

function fCheckInlineMoon(sCode)
    local environ = gData.GetEnvironment();
    local bGood = nil;
    local bNot = false;

    sCode = string.lower(sCode);
    local i = string.find(sCode,'not_');
    if i ~= nil and i == 1 then
        bNot == true;
        sCode = string.sub(sCode,5,-1);
    end

    if sCode == 'fullmoon' then
        bGood = (environ.MoonPhase == 'Full Moon');
    elseif sCode == 'newmoon' then
        bGood = (environ.MoonPhase == 'New Moon');
    elseif sCode == 'gibbous' then
        bGood = (string.find(environ.MoonPhase,'Gibbous') ~= nil);
    elseif sCode == 'quartermoon' then
        bGood = (string.find(environ.MoonPhase,'Quarter') ~= nil);
    elseif bGood == 'crescent' then
        bGood = (string.find(environ.MoonPhase,'Crescent') ~= nil);
    end

    if bGood ~= nil and bNot == true then
        bGood = not bGood;
    end

    return bGood;
end     -- fCheckInlineMoon

--[[
    fCheckInlineGear checks the validity of the passed inline code and then determines if the
    coded condition is true.

    Parameters
        sCode       coded condition to be checked
        sSlot       name of slot to check
        ts          gear set currently being populated

    Returned
        bGood       was the coded condition met? T/F/nil where nil means code Unknown
        smsg        if an error message occurs, will contain the error messgae

    Note:
        There is no check for validation since at this point in processing, probably
        not all inlines will have been checked yet. A validation message will occur in
        the calling routine: inline.fCheckInline.

    Note2: //[NOT_]SLOTS: has been removed from this code. It's more complicated than
        originally thought. The goal has been moved to a future, post-2.0 implementation.
--]]

function fCheckInlineGear(sCode,sSlot,ts)
    local gSet = gData.GetCurrentSet();
    local bGood = nil;
    local ssLot = nil;
    local bNot = false;
    local iOff,iPos,sItem;
    local smsg = nil;

    if bSubset == nil then
        bSubset = false;
    end

    sCode = string.upper(sCode);
    local sCodeHold = sCode;
    local i = string.find(sCode,'NOT_');
    if i ~= nil and i == 1 then
        bNot == true;
        sCode = string.sub(sCode,5,-1);         -- Remove the NOT_
    end

    iPos = string.find(sCode,'IF');
    if iPos ~= nil and iPos == 1 then
        -- Determine if //IF: or //IF-slot:. Note or assign the slot and item
        if string.sub(sCode,3,1) == ':' then
            -- Make sure the slot is not a subset or group
            if string.find('SUBSET,GROUP',sCode) ~= nil then
                smsg = 'Warning: //IF: cannot be used with either a Subset nor Group: '.. sSlot;
                return false,smsg;
            end
            -- Now proceed
            ssLot = sSlot;
            sItem = string.sub(sCode,4,-1);
        elseif string.sub(sCode,3,1) == '-' then
            local j = string.find(sCode,':');
            ssLot = utilities.fValidSlots(string.sub(sCode,4,j-1),utilities._SLOT_FA);
            if ssLot == nil then
                -- Slot was Unrecognized
                smsg = 'Warning: Invalid slot in inline conditional: ' .. sCodeHold;
                return false,smsg;
            end
            -- Grab the item
            sItem = string.sub(sCode,j+1,-1);
        end

        -- Simple comparison: equip gear piece if currently wearing identified
        -- gear piece. Checks dynamic composite gear set first. If empty, then
        -- checks currently worn gear.

        -- Check the temporary set
        if not (ts[ssSlot] == nil or ts[ssSlot] == '') then
            -- Since slot not empty, check item name
            if string.lower(ssSlot) == 'ears' then
                bGood = (string.lower(sItem) == string.lower(ts['Ear1']) or
                         string.lower(sItem) == string.lower(ts['Ear2']));
            elseif string.lower(ssLot) == 'rings' then
                bGood = (string.lower(sItem) == string.lower(ts['Ring1']) or
                         string.lower(sItem) == string.lower(ts['Ring2']));
            else
                bGood = (string.lower(sItem) == string.lower(ts[sSlot]));
            end
        else
            -- Since temporary set slot was empty, check currently equipped gear
            if gSet[ssSlot] == nil or gSet[ssSlot] == '' then
                bGood = false;
            elseif string.lower(ssLot) == 'ears' then
                bGood = (string.lower(sItem) == string.lower(gSet['Ear1']) or
                         string.lower(sItem) == string.lower(gSet['Ear2']));
            elseif string.lower(ssSLot) == 'rings' then
                bGood = (string.lower(sItem) == string.lower(gSet['Ring1']) or
                         string.lower(sItem) == string.lower(gSet['Ring2']));
            else
                bGood = (string.lower(sItem) == string.lower(gSet[ssSlot]));
            end

            -- if this is a //NOT_ code, just invert the result
            if bNot == true then
                bGood = not bGood;
            end
        else
            smsg = 'Warning: //IF: and //NOT_IF: are invalid on subsets: ' .. sCodeHold;
            return false,smsg;
        end
    elseif sCode == 'SPECIAL' then
        -- Equip if the special conditions are met. This applies to specific pieces of gear where special calculations are needed
        if gcdisplay.GetGC() == false then  -- GC must be run for the special calculations to work
            bGood = false;
        else
            if sSlot ~= 'subset' then
                bGood = gear.fValidateSpecial(sSlot,sGear);
            else    -- Invalid inline for a subset
                smsg = 'Warning: //SPECIAL cannot be used on a subset or a group';
                return false,smsg;
            end
        end
    elseif string.find(sCode,'TRACK:') ~= nil then
        -- Technically not an inline conditional, instead it denotes a piece that will stay equipped longer than normal.
        -- All you can do here is make sure it's not applied to a subset or group.
        if sSlot == 'subset' or sSlot == 'group' then
            smsg = 'Warning: //TRACK cannot be used on a subset or a group: ' .. sCodeHold;
            return false,smsg;
        else
            bGood = true;
        end
    end

    return bGood,nil;
end     -- fCheckInlineGear

--[[
    fCheckInlineTarget checks the validity of the passed inline code and then determines if the
    coded condition is true.

    Parameters
        sCode       coded condition to be checked

    Returned
        bGood       was the coded condition met? T/F/nil where nil means code Unknown


    Note:
        There is no check for validation since at this point in processing, probably
        not all inlines will have been checked yet. A validation message will occur in
        the calling routine: inline.fCheckInline.
--]]

-- also, checking that GC is set probably needs to be done for the whole procedure and not just //SPECIAL

function fCheckInlineTarget(sCode)
    local tg = gData.GetTarget();
    local bGood = nil;
    local bNot = false;
    local bFound = false;

    sCode = string.lower(sCode);
    local i = string.find(sCode,'not_');
    local ln = string.lower(tg.Name);
    if i ~= nil and i == 1 then
        bNot == true;
        sCode = string.sub(sCode,5,-1);         -- Remove the NOT_
    end

    if sCode == 'amorph' then
        -- Equip if the target is of type Amorph
        if tg ~= nil and tg.Name ~= nil then
            for ii,jj in pairs(inline.tTargetFamily['amorphs']) do
                if table.find(jj,ln) ~= nil then
                    bFound = true;
                    break;
                end
            end
        end
        bGood = bFound;
    elseif sCode == 'aquan' then
        -- Equip if the target is of type Aquan
        if tg ~= nil and tg.Name ~= nil then
            for ii,jj in pairs(inline.tTargetFamily['aquans']) do
                if table.find(jj,ln) ~= nil then
                    bFound = true;
                    break;
                end
            end
        end
        bGood = bFound;
    elseif sCode == 'ME' then
        -- Equip if target is the player
        local me = AshitaCore:GetMemoryManager():GetParty():GetMemberTargetIndex(0);
        local tg = gData.GetTargetIndex();
        bGood = (tg == me);
    end

    if bGood ~= nil and bNot == true then
        bGood = not bGood;
    end

    return bGood;
end     -- fCheckInlineTarget

--[[
    fCheckInlinePet checks the validity of the passed inline code and then determines if the
    coded condition is true.

    Parameters
        sCode       coded condition to be checked

    Returned
        bGood       was the coded condition met? T/F/nil where nil means code Unknown


    Note:
        There is no check for validation since at this point in processing, probably
        not all inlines will have been checked yet. A validation message will occur in
        the calling routine: inline.fCheckInline.
--]]

-- also, checking that GC is set probably needs to be done for the whole procedure and not just //SPECIAL

function fCheckInlinePet(sCode)
    local environ   = gData.GetEnvironment();
    local petAction = gData.GetPetAction();
    local player    = gData.GetPlayer();
    local spell     = gData.GetAction();
    local pet       = gData.GetPet();
    local bGood     = nil;
    local bNot      = false;

    sCode = string.upper(sCode);

    local i = string.find(sCode,'NOT_');
    if i ~= nil and i == 1 then
        bNot == true;
        sCode = string.sub(sCode,5,-1);         -- Remove the NOT_
    end

    if sCode == 'PET' then
        -- Is there a pet
        bGood = (pet ~= nil);
    elseif sCode == 'PETF' then
        -- Is there a pet and is it fighting
        bGood = (pet ~= nil and pet.Status == 'Engaged');
    elseif sCode == 'PETFNPF' then
        -- Is there a pet and it is fighting, but the player is not fighting
        bGood = (pet ~= nil and pet.Status == 'Engaged' and player.Status ~= 'Engaged');
    elseif string.find(sCode,'PETNAME:') ~= nil then
        -- Is there a pet named ...
        local n = string.sub(sCode,9,-1);
        bGood = (pet ~= nil and pet.Name ~= nil and string.find(string.upper(pet.Name),n) ~= nil);
    elseif sCode == 'SMN_PET' then
        --Is there a summoner's pet out (an returned element implies true)
        bGood = (utilities.fSummonersPetElement(pet) ~= nil);
    elseif string.find(sCode,'SMN_SUMMONS:') ~= nil then
        -- Is the named summoner's pet being summoned. Equip if the pet being summoned is named ...
        if spell == nil then
            bGood = false;
        else
            bGood = (string.find(string.lower(spell.Name),string.lower(string.sub(sCode,12,-1)) ~= nil);
        end
    elseif sCode == 'SMN_PETMD' then
        -- Does the summoner's pet's element matches the day's element
        local ele = utilities.fSummonersPetElement(pet);
        if ele ~= nil then
            bGood = (ele == string.lower(environ.DayElement));
        else
            bGood = false;
        end
    elseif sCode == 'SMN_PETMW' then
        -- Does the summoner's pet's element matches the weather's element
        local ele = utilities.fSummonersPetElement(pet);
        if ele ~= nil then
            bGood = (string.find(ele,string.lower(environ.RawWeather)) ~= nil);
        else
            bGood = false;
        end
    elseif string.find(sCode,'SMN_BP:') ~= nil then
        -- Does the summoner's blood pact match the passed blood pact name
        if (petAction ~= nil and petAction.Name ~= nil) then
            bGood = (string.find(string.lower(petAction.Name),string.lower(string.sub(sCode,8,-1))));
        else
            bGood = false;
        end
    elseif sCode == 'SMN_SPIRIT:ES' then
        -- Equip if the pet being summoned is an elemental spirit
        if spell ~= nil and spell.Name ~= nil then
            bGood = (table.find(utilities.tSpellGroupings['spirits'],string.lower(spell.Name)) ~= nil);
        else
            bGood = false;
        end
    elseif sCode == 'SMN_SPIRIT:EP' then
        -- Is the player's pet an elemental spirit
        bGood = (pet ~= nil and pet.Name ~= nil and table.find(utilities.tSpellGrouping['spirits'],string.lower(pet.Name)) ~= nil);
    end

    if bGood ~= nil and bNot == true then
        bGood = not bGood;
    end

    return bGood;
end     -- fCheckInlinePet

--[[
    fCheckInlineSlot checks the validity of the passed inline code and then determines if the
    coded condition is true.

    Parameters
        sCode        coded condition to be checked
        sSlot        name of slot to check
        ts           gear set currently being populated

    Returned
        bGood        was the coded condition met? T/F/nil where nil means code Unknown
        smsg         if an error message occurs, will contain the error message
        sValidSlots  list of valid slots from //SLOTS:

    Note: There is no check for validation since at this point in processing, probably
    not all inlines will have been checked yet. A validation message will occur in the
    calling routine: inline.fCheckInline.
--]]

function fCheckInlineSlot(sCode,sSlot,ts)
    local bGood = nil;
    local smsg = nil;
    local bNot = false;
    local suSlot;
    local sValidSlots = nil;

    if bSubset == nil then
        bSubset = false;
    end

    sCode = string.upper(sCode);
    suSlot = string.upper(sSlot);

    local i = string.find(sCode,'NOT_');
    if i ~= nil and i == 1 then
        bNot == true;
        sCode = string.sub(sCode,5,-1);         -- Remove the NOT_
    end

    if string.find(sCode,'EMPTY:1') ~= nil or string.find(sCode,'EMPTY:2') then
        local sNum = string.sub(sCode,-1,-1);
        if string.find('EARS,EAR1,EAR2,RINGS,RING1,RING2',suSlot) == nil then
            smsg = 'Warning: Invalid slot designated in ' .. sCode;
            return false,smsg,nil;
        elseif string.find(suSlot,'EAR') ~= nil then
            bGood = (ts['Ear'..sNum] == nil or ts['Ear'..sNum] == '');
        elseif string.find(suSlot,'RING') ~= nil then
            bGood = (ts['Ring'..sNum] == nil or ts['Ring'..sNum] == '');
        end

        if bNot == true then
            bGood = not bGood;
        end
    elseif string.find(sCode,'EMPTY') ~= nil then
        -- This is like EMPTY:1 and EMPTY:2 except you have to check both slots

        if suSlot == 'EARS' then
            bGood = ((ts['Ear1' == nil] or ts['Ear1'] =='') and (ts['Ear2' == nil] or ts['Ear2'] ==''));
        elseif suSlot == 'RINGS' then
            bGood = ((ts['Ring1' == nil] or ts['Ring1'] =='') and (ts['Ring2' == nil] or ts['Ring2'] ==''));
        else
            bGood = (ts[sSlot] == nil or ts[sSlot] == '');
        end

        if bNot == true then
            bGood = not bGood;
        end
    end
    return bGood,smsg,sValidSlots;
end     -- fCheckInlineSlot

--[[
    fCheckInlineSongs checks the validity of the passed inline code and then determines
    if the coded condition is true.

    Parameters
        sCode       coded condition to be checked

    Returned
        bGood       was the coded condition met? T/F/nil

    Note:
        There is no check for validation since at this point in processing, probably
        not all inlines will have been checked yet. A validation message will occur in
        the calling routine: inline.fCheckInline.
--]]

function fCheckInlineSongs(sCode)
    local song = gData.GetAction();
    local bGood = nil;
    local bNot = false;
    local tSongTypes = {    -- List of songs by type
        ['carol'] =     { 'light carol','earth carol','water carol','wind carol','fire carol',
                          'ice carol','lightning carol','dark carol' },
        ['elegy'] =     { 'battlefield elegy','carnage elegy' },
        ['etude'] =     { 'enchanting etude','spirited etude','learned etude','quick etude',
                          'vivacious etude','dexterous etude','sinewy etude','bewitching etude',
                          'logical etude','sage etude','swift etude','vital etude','uncanny etude',
                          'herculean etude' },
        ['finale'] =    { 'magical finale' },
        ['hymnus'] =    { 'goddess\'s hymnus' },
        ['lullaby'] =   { 'foe lullaby', 'horde lullaby' },
        ['madrigal'] =  { 'sword madrigal', 'blade madrigal' },
        ['mambo'] =     { 'sheepfoe mambo','dragonfoe mambo' },
        ['march'] =     { 'advancing march','victory march' },
        ['mazurka'] =   { 'raptor mazurka','chocobo mazurka' },
        ['minne'] =     { 'knight\'s minne','knight\'s minne ii','knight\'s minne iii',
                          'knight\'s minne iv' },
        ['minuet'] =    { 'valor minuet','valor minuet ii','valor minuet iii','valor minuet iv' },
        ['paeon'] =     { 'army\'s paeon','army\'s paeon ii','army\'s paeon iii','army\'s paeon iv',
                          'army\'s paeon v' },
        ['prelude'] =   { 'hunter\'s prelude','archer\'s prelude' },
        ['requiem'] =   { 'foe requiem','foe requiem ii','foe requiem iii','foe requiem iv',
                          'foe requiem v','foe requiem vi' },
        ['threnody'] =  { 'light threnody','dark threnody','earth threnody','water threnody',
                          'wind threnody','fire threnody','ice threnody','lightning threnody' },
        ['virelai'] =   { 'maiden\'s virelai' }
    };

    if sCode == nil
        return false;
    end			-- Equip if the summoner's pet's element matches the day's element
    if gcinclude.fSummonerPet() == true then
        bGood = (fElementByPetName(pet.Name) == string.lower(environ.DayElement));
    else
        bGood = false;
    end

    sCode = string.lower(sCode);
    local i = string.find(sCode,'not_');
    if i ~= nil and i == 1 then
        bNot == true;
        sCode = string.sub(sCode,5,-1);
    end

    -- First check for song name fragment
    if string.find(sCode,'song:') ~= nil then
        if (song ~= nil and song.Name ~= nil) then
            local iPos = string.find(sCode,':');
            if iPos == nil then
                iPos = 0;
            end
            bGood = (string.find(string.lower(song.Name),string.sub(sCode,iPos+1,-1)) ~= nil);
            if bNot == true then
                bGood = not bGood;
            end
        else
            smsg = 'Warning: No song identified: ' .. sCode;
            return false,smsg;      -- Regardless if song: or not_song:, the absense of a song makes this false
        end
    else
        -- The rest are song type switches or not song related
        for i,j in pairs(tSongTypes) do
            if sCode == i then do
                bGood = (table.find(j,string.lower(song.Name)) ~= nil);
                if bNot == true then
                    bGood = not bGood;
                end
                break;
            end
        end
    end

    if bNot == true then
        bGood = not bGood;
    end

    return bGood;
end     -- fCheckInlineSongs

--[[
    fCheckInlineOther checks the validity of the passed inline code and then determines
    if the coded condition is true.

    Parameters
        sCode       coded condition to be checked

    Returned
        bGood       was the coded condition met? T/F/nil

    Note:
        There is no check for validation since at this point in processing, probably
        not all inlines will have been checked yet. A validation message will occur in
        the calling routine: inline.fCheckInline.
--]]

function fCheckInlineOther(sCode)
    local spell = gData.GetAction();
    local party = gData.GetParty();
    local environ = gData.GetEnvironment();
    local bGood = nil;
    local bNot = false;
    local bFlip = true;     -- indicates if NOT_xxx valid
    local smsg;

    if sCode == nil
        return false;
    end

    sCode = string.lower(sCode);
    local i = string.find(sCode,'not_');
    if i ~= nil and i == 1 then
        bNot == true;
        sCode = string.sub(sCode,5,-1); -- remove the "not_"
    end

    -- First check for spell name fragment
    if (spell ~= nil and spell.Name ~= nil) then
        if string.find(sCode,'spell:') ~= nil then
            bGood = (string.find(string.lower(spell.Name),string.sub(sCode,7,-1)) ~= nil);
        elseif string.find(sCode,'spellcat:') ~= nil then
            -- Check to see if spell being cast is of a specific category
            local sCat = string.sub(sCode,10,-1);
            if sCat == 'barelemental' then
                bGood = table.find(utilities.tSpellGroupings['barspell']['ele']);
            elseif sCat == 'barstatus' then
                bGood = table.find(utilities.tSpellGroupings['barspell']['status']);
            else
                smsg = 'Warning: Unknown spell category found in: ' .. sCode
                return false,smsg;
            end
            if bGood ~= nil and bNot == true then
                bGood = not bGood;
            end
        end
    end

    if bGood == nil then
        if sCode == 'true' then
            -- Used for testing. Has no purpose otherwise
            bGood = true;
            bFlip = false;
        elseif sCode == 'false' then
            -- Used for testing. Has no purpose otherwise
            bGood = false;
            bFlip = false;
        elseif sCode == 'inparty' then
            -- Is the player in a party?
            bGood = (party ~= nil and party.Count ~= nil and party.Count > 1);
        elseif sCode == 'own' then
            -- Is region controlled by player's nation?
            bGood = (gcdisplay.GetCycle('Region') == 'Owned');
        elseif string.find(sCode,'party:') ~= nil then
            local iNum = tonumber(string.sub(sCode,7,-1));
            bGood = (party ~= nil and party.Count ~= nil and party.Count == iNum);
        elseif sCode == 'town' then
            bGood = (environ.Area ~= nil and table.find(inline.tAreas['Towns'],environ.Area) ~= nil);
        elseif sCode == 'town-ak' then
            bFlip = false;
            -- Equip the appropriate national/ducal aketon if in the appropriate town
            local pNation = AshitaCore:GetMemoryManager():GetPlayer():GetNation();
            local slcGear = string.lower(sGear);

            if slcGear == 'ducal aketon' then
                bGood = (environ.Area ~= nil and
                    (table.find(inline.tAreas['Windy'],environ.Area) ~= nil or
                    table.find(inline.tAreas['Sandy'],environ.Area) ~= nil or
                    table.find(inline.tAreas['Bastok'],environ.Area) ~= nil or
                    table.find(inline.tAreas['Jeuno'],environ.Area) ~= nil)
                    );
            elseif slcGear == 'federation aketon' then
                if environ.Area ~= nil and table.find(inline.tAreas['Windy'],environ.Area) ~= nil then
                    -- Equip the Windurstian national aketon if in Windurst
                    bGood = (pNation == 2);
                end
            elseif slcGear == 'republic aketon' then
                if environ.Area ~= nil and table.find(inline.tAreas['Bastok'],environ.Area) == nil then
                    -- Equip the Bastokian national aketon if in Bastok
                    bGood = (pNation == 1);
                end
            elseif slcGear == 'kingdom aketon' then
                if environ.Area ~= nil and table.find(inline.tAreas['Sandy'],environ.Area) == nil then
                    -- Equip the Sandorian national aketon if in San d'Oria
                    bGood = (pNation == 0);
                end
            else
                smsg = 'Warning: Invalid body piece for national aketon check: ' .. sGear;
                return false,smsg;
            end
        end
    end

    if bGood ~= nil and bFlip == true and bNot == true then
        bGood = not bGood;
    end
    return bGood;
end     -- fCheckInlineOther

--[[
    fCheckInlineJob checks the validity of the passed inline code and then determines
    if the coded condition is true.

    Parameters
        sCode       coded condition to be checked

    Returned
        bGood       was the coded condition met? T/F/nil

    Note:
        There is no check for validation since at this point in processing, probably
        not all inlines will have been checked yet. A validation message will occur in
        the calling routine: inline.fCheckInline.
--]]

function fCheckInlineJob(sCode)
    local player = gData.GetPlayer();
    local sj = player.SubJob;
    local job;
    local smsg = nil;
    local bGood = nil;
    local bNot = false;

    if sCode == nil
        return false;
    end

    sCode = string.upper(sCode);
    job = string.sub(sCode,-3,-1);      -- Last 3 characters is the job abbreviation

    if string.find(crossjobs._validJobs,job) == nil then
       smsg = 'Warning: Invalid job specified in inline conditional: ' .. sCode
       return false,smsg;
    end

    local i = string.find(sCode,'not_');
    if i ~= nil and i == 1 then
        bNot == true;
        sCode = string.sub(sCode,5,-1);     -- remove the "NOT_"
    end

    if string.find(sCode,'PJP') and string.find(sCode,'_NOT_ME') ~= nil then
        -- Determine if any player (but not me) in the party has the specified job.
        -- Note: inversion not supported
        bGood = (utilities.fCheckPartyJob(string.sub(suCode,4,3),true));
    else        -- These codes support negation
        if sCode == 'MSJ' then
            --  Player has a subjob that can do magic
            bGood = string.find(crossjobs._sMagicJobs,sj);
        elseif string.find(sCode,'SJ'..sj) ~= nil then
            -- Player's subjob matches the specified job
            bGood = true;
        elseif string.find(sCode,'PJP') ~= nil then
            -- Determine if any player in the party has the specified job
            bGood = (crossjobs.fCheckPartyJob(string.sub(suCode,4,3),false));
        end

        if bGood ~= nil and bNot == true then
            bGood = not bGood;
        end
    end

    return bGood,nil;
end     -- fCheckInlineJob

--[[
    fCheckInlineTime checks the validity of the passed inline code and then determines
    if the coded condition is true.

    Parameters
        sCode       coded condition to be checked

    Returned
        bGood       was the coded condition met? T/F/nil

    Note:
        There is no check for validation since at this point in processing, probably
        not all inlines will have been checked yet. A validation message will occur in
        the calling routine: inline.fCheckInline.
--]]

function fCheckInlineTime(sCode)
    local player = gData.GetPlayer();
    local timestamp = gData.GetTimestamp();
    local bGood = nil;
    local bNot = false;
    local smsg = nil;

    if sCode == nil
        return false;
    end

    sCode = string.upper(sCode);

    local tr = string.sub(sCode,6,-1);
    local i = string.find(sCode,'NOT_');
    if i ~= nil and i == 1 then
        bNot == true;
        sCode = string.sub(sCode,5,-1); -- remove the "not_"
    end

    -- Is the time of day considered to be daytime
    bGood,smsg = utilities.fCheckTime(timestamp.hour,tr);

    if bGood ~= nil and and smsg == nil and bNot == true
        bGood = not bGood;
    end

    return bGood,smsg;
end     -- fCheckInlineTime

--[[
    fCheckInlineToggle checks the validity of the passed inline code and then determines
    if the coded condition is true.

    Parameters
    sCode       coded condition to be checked

    Returned
    bGood       was the coded condition met? T/F/nil

    Note:
    There is no check for validation since at this point in processing, probably
    not all inlines will have been checked yet. A validation message will occur in
    the calling routine: inline.fCheckInline.
--]]

function fCheckInlineToggle(sCode)
    local player = gData.GetPlayer();
    local bGood = nil;
    local smsg = nil;

    if sCode == nil
        return false;
    end

    sCode = string.lower(sCode);

    local i = string.find(sCode,'NOT_');
    if i ~= nil and i == 1 then
        bNot == true;
        sCode = string.sub(sCode,5,-1); -- remove the "not_"
    end

    local dt = string.find(sCode,'dt_');
    if dt ~= nil and dt == 1 then
        if sCode == 'dt_breath' then
            bGood = (gcdisplay.GetCycle('DT') == 'Breath');
        elseif sCode == 'dt_magical' then
            bGood = (gcdisplay.GetCycle('DT') == 'Magical');
        elseif sCode == 'dt_physical' then
            bGood = (gcdisplay.GetCycle('DT') == 'Physical');
        end
    elseif sCode == 'EVASION' then
        -- Is 'Evasion' enabled
        bGood = (gcdisplay.GetToggle('Eva') == true);
    elseif sCode == 'IDLE' then
        -- Is 'Idle' enabled
        bGood = gcdisplay.GetToggle('Idle');
    elseif sCode == 'TANK' then
        -- Is 'Tank' enabled
        if string.find(crossjobs._TankJobs,player.MainJob) ~= nil then
            bGood = (gcdisplay.GetToggle('Tank') == true);
        else
            smsg = 'Warning: //[NOT_]TANK only applicable for jobs that can tank: ' .. crossjobs._TankJobs;
            bGood = false;
        end
    elseif sCode == 'MACC' then
        -- Is 'Macc' (Magic Accuracy) enabled
        if string.find(crossjobs._sMagicJobs,player.MainJob) ~= nil or
                string.find(crossjobs._sMagicJobs,player.SubJob) ~= nil then
            bGood = gcdisplay.GetToggle('Macc');
        else
            smsg = 'Warning: //[NOT_]MACC only applicable for magical jobs/subjobs: ' .. crossjobs._sMagicJobs;
            bGood = false;
        end
    elseif sCode == 'WSWAP' then
        -- Is 'WSWAP' (Weapon Swap) enabled
        if crossjobs.settings.bWSOverride == false then
            bGood = gcdisplay.GetToggle('WSWAP');
        else
            smsg = 'Warning: Your job (' .. player.MainJob .. ') does not support //WSWAP';
            bGood == false;
        end
    elseif sCode == 'KITE' then
        -- Is 'Kite' (Kiting) enabled
        bCode = gcdisplay.GetToggle('Kite');
    elseif sCode == 'SPF' then
        -- Should 'Show Pull Feedback' be displayed
        bGood = gcdisplay.GetToggle('sPF');
    elseif sCode == 'BRD_HORN' then
        -- Is Bard's instrument set to a horn
        if player.MainJob == 'BRD' then
            bGood = (gcdisplay.GetToggle('Instrument') == 'Horn');
        else
            smsg = 'Warning: //[NOT_]BRD_HORN is only valid if you\'re a bard';
            bGood = false;
        end
    elseif sCode == 'BRD_STRING' then
        -- Is Bard's  instrument set to a string
        if player.MainJob == 'BRD' then
            bGood = (gcdisplay.GetToggle('Instrument') == 'String');
        else
            smsg = 'Warning: //[NOT_]BRD_STRING is only valid if you\'re a bard';
            bGood = false;
        end
    elseif sCode == 'BST_AJUG' then
        -- Is Beastmaster's 'AJUG' (automatic pet jug selection) enabled
        if player.MainJob == 'BST' then
            bGood = gcdisplay.GetToggle('AJug');
        else
            smsg = 'Warning: //[NOT_]BST_AJUG is only valid if you\'re a beastmaster';
            bGood = false;
        end
    elseif sCode == 'BST_DB_BPP' then
        -- Is Beastmaster's 'DB:BPP' (debuff:blind,poison,paralyze) enabled
        if player.MainJob == 'BST' then
            bGood = (gcdisplay.GetToggle('DB') == 'BPP');
        else
            smsg = 'Warning: //[NOT_]BST_DB_BPP is only valid if you\'re a beastmaster';
            bGood = false;
        end
    elseif sCode == 'BST_DB_WSS' then
        -- Is Beastmaster's 'DB:WSS' (debuff:weight,slow,silence) enabled
        if player.MainJob == 'BST' then
            bGood = (gcdisplay.GetToggle('DB') == 'WSS');
        else
            smsg = 'Warning: //[NOT_]BST_DB_WSS is only valid if you\'re a beastmaster';
            bGood = false;
        end
    elseif sCode == 'SMN_SBP' then
        -- Is Summoner's 'sBP' (Show Blood Pact)
        if player.MainJob == 'SMN' or player.SubJob == 'SMN' then
            bGood = gcdisplay.GetToggle('sBP');
        else
            smsg = 'Warning: //[NOT_]SBP is only valid if you\'re a summoner (main or sj)';
            bGood = false;
        end
    elseif sCode == 'THF_TH' then
        -- Is Thief's 'TH' (Treasure Hunter) enabled
        if player.MainJob == 'THF' then
            bGood = gcdisplay.GetToggle('TH');
        else
            smsg = 'Warning: //[NOT_]THF_TH is only valid if your main job is a thief';
            bGood = false;
        end
    elseif sCode == 'THF_SS' then
        -- Is Thief's 'SS' (Show Steal) enabled
        if player.MainJob == 'THF' or player.SubJob == 'THF' then
            bGood = gcdisplay.GetToggle('SS');
        else
            smsg = 'Warning: //[NOT_]SBP is only valid if you\'re a thief';
            bGood = false;
        end
    end

    if bGood ~= nil and bNot == true then
        bGood = not bGood;
    end

    return bGood,smsg;
end     -- fCheckInlineToggle

--[[
    fCheckInlineWeaponType checks the validity of the passed inline code and then determines
    if the coded condition is true.

    Parameters
        sCode       coded condition to be checked

    Returned
        bGood       was the coded condition met? T/F/nil
        smsg        error message if one occurred

    Note:
        There is no check for validation since at this point in processing, probably
        not all inlines will have been checked yet. A validation message will occur in
        the calling routine: inline.fCheckInline.
    --]]

function fCheckInlineWeaponType(sCode,sGear)
    local bGood = nil;
    local bNot = false;
    local smsg = nil;

    if sCode == nil or sGear == nil then
        return false,nil;
    end

    sCode = string.upper(sCode);
    local suGear = string.upper(sGear);

    local i = string.find(sCode,'NOT_');
    if i ~= nil and i == 1 then
        bNot == true;
        sCode = string.sub(sCode,5,-1); -- remove the "not_"
    end

    if string.find(crossjobs._WeaponTypes,sCode) ~= nil then
        -- It's a valid weapon type
        if profile.WeaponType[sCode] ~= nil then
            for i,j in pairs(profile.WeaponType) do
                if string.upper(i) == sCode then
                    for ii,jj in pairs(j) do
                        if string.upper(jj) == suGear then
                            bGood = true;
                        end
                    end
                end
            end
        else
            smsg = 'Warning: Weapon type not defined: ' .. sCode
            bGood = false;
        end
    else
        smsg = 'Warning: Weapon not found in type: ' .. sCode .. ' - ' .. sGear;
        bGood = false;
    end

    if bGood ~= nil and bNot == true then
        bGood = not bGood;
        smsg = nil;
    end

    return bGood,smsg;
end     -- fCheckInlineWeaponType

--[[
    fCheckInlineWeather checks the validity of the passed inline code and then determines
    if the coded condition is true.

    Parameters
        sCode       coded condition to be checked

    Returned
        bGood       was the coded condition met? T/F/nil
        smsg        error message if one occurred

    Note:
        There is no check for validation since at this point in processing, probably
        not all inlines will have been checked yet. A validation message will occur in
        the calling routine: inline.fCheckInline.
    --]]

function fCheckInlineWeather(sCode)
    local environ = gData.GetEnvironment();
    local bGood = nil;
    local bNot = false;
    local smsg = nil;

    if sCode == nil or sGear == nil then
        return false,nil;
    end

    local i = string.find(sCode,'NOT_');
    if i ~= nil and i == 1 then
        bNot == true;
        sCode = string.sub(sCode,5,-1); -- remove the "not_"
    end

    -- Make sure passed in code is a valid weather
    local sfCode = utilities.fFormattedWord(sCode,utilities._SLOT_FA);
    if table.find(gData.Constants.Weather,sfCode) == nil then
        smsg = 'Warning: Unknown weather specified: ' .. sfCode;
        return false,smsg;
    else
        sCode = string.upper(sCode);
    end

    -- Can't check for equility since environ.Weather may have an 'x2' appended. Treat as a substring
    bGood = (string.find(string.upper(environ.Weather),sCode) ~= nil);

    if bGood ~= nil and bNot == true then
        bGood = not bGood;
    end

   return bGood,nil;
end     -- fCheckInlineWeather

--[[
    fMakeCodeTable takes the passed, // delimited list and returns the
    individual codes in a table

    Parameters
        sList       one or more conditionals that were passed in

    Returned
        sTbl        a table of the conditionals that were split apart
--]]

function fMakeCodeTable(sList)
    local sTbl = { };
    local iPos;

    iPos = 3;       -- Skip //
    while iPos ~= nil do
        -- Now, look for next //
        iPos = string.find(string.sub(sList,iPos,-fCheckInlineWeaponType1),'//');
        if iPos ~= nil then
            -- Since found, we need to skip the intial // and insert what's left
            -- up to the next // and then shorten the list to // onwards
            table.insert(sTbl,string.sub(sList,3,iPos-1));
            sList = string.sub(sList,iPos,-1);
            iPos = 3;
        else
            table.insert(sTbl,string.sub(sList,3,-1));
        end
    end
    return sTbl;
end     -- fMakeCodeTable

--[[
    fCheckInline checks for inline conditionals on the passed piece and determines
    if they are valid and whether the conditions are met.


    If the slot name is 'subset', then any checks that are on the slot will
    be ignored and the result passed back will be false

    Parameters
        gear        passed piece to be parsed
        sSlot       name of slot associated with gear. (Subset and Group are also valid.)
        ts          target table if comparison needs to compare contents
        bLeft       is the passed piece a slot or a definition
        bValidate   is this a code validation pass

    Returned
        bGood       was the coded condition met? T/F
        sGear       the piece without the inline conditional(s)

    Note:
        If an error was discovered or the inline conditionals not recognized, then
        the returned results will be false. Previously, unrecognized conditionals
        were just skipped, but now we have a much more extensive valid list. It
        is assumed that an unrecognized conditional had a typo which invalidates
        the passed in fragment (ergo, it needs to be fixed.)
--]]

function inline.fCheckInline(gear,sSlot,ts,bLeft,bValidate)
    local iPos,i,suCode,sGear;
    local suCodeTbl = { };
    local bSubset;
    local sValidSlots = nil;
    local smsg = nil;
    local bGood = nil;

    if gear == nil then
        return false,nil,nil;
    end
    if ts == nil then
        ts = gProfile.Sets.CurrentGear;     -- Assume currently being built gear set
    end
    if bLeft == nil then
        bLeft = false;                      -- Assume this is a gear definition
    end
    if bValidate == nil then
        bValidate = false;                  -- Don't assume a validation pass
    end

    -- Find the conditional(s) if any

    -- Here goes check on bLeft and conditionals

    iPos = string.find(gear,'//');

    if iPos == nil then
        return true,gear,nil;               -- No conditionals mean the check returns true
    end

    sSlot   = string.lower(sSlot);
    sGear   = string.sub(gear,1,iPos-1);
    bSubset = (string.lower(sGear) == 'subset');

    -- Make a table of the inline conditionals, for processing
    suCodeTbl = fMakeCodeTable(string.upper(string.sub(gear,iPos,-1)));

    -- Now walk that table, processing the conditionals. At any time if a conditional
    -- comes back false, we're done. Conditionals are strung together and all have to
    -- be true for the results to be true.
    for i,suCode in pairs(suCodeTbl) do
        -- Start with Buffs
        bGood = fCheckInlineBuff(suCode);

        if bGood == nil then
            -- Next, Debuffs.
            bGood = fCheckInlineDebuff(suCode);
        end

        if bGood == nil then
            -- Then Conditionals
            bGood,smsg = fCheckInlineConditional(suCode);
        end

        if bGood == nil then
            -- Then Activity
            bGood = fCheckInlineActivity(suCode);
        end

        if bGood == nil then
            -- Then Day
            bGood = fCheckInlineDay(suCode);
        end

        if bGood == nil then
            -- Then Moon
            bGood = fCheckInlineMoon(suCode);
        end

        if bGood == nil then
            -- Then conditional Gear
            bGood = fCheckInlineGear(suCode),sSlot,ts);
        end

        if bGood == nil then
            -- Then Slots
            bGood,smsg,sValidSlots = fCheckInlineSlot(suCode,sSlot,ts,bSubset);
        end

        if bGood == nil then
            -- Then Songs
            bGood = fCheckInlineSongs(suCode);
        end

        if bGood == nil then
            -- Then Other
            bGood = fCheckInlineOther(suCode);
        end

        if bGood == nil then
            -- Then Job
            bGood,smsg = fCheckInlineJob(suCode);
        end

        if bGood == nil then
            -- Then Target
            bGood = fCheckInlineTarget(suCode);
        end

        if bGood == nil then
            -- Then Pet
            bGood = fCheckInlinePet(suCode);
        end

        if bGood == nil then
            -- Then Time
            bGood = fCheckInlineTime(suCode);
        end

        if bGood == nil then
            -- Then Toggles
            bGood,smsg = fCheckInlineToggle(suCode);
        end

        if bGood == nil then
            -- Then Weapon Type
            bGood,smsg = fCheckInlineWeaponType(suCode,sGear);
        end

        if bGood == nil then
            -- Then Weather
            bGood,smsg = fCheckInlineWeather(suCode);
        end

        -- If an error occurred or the conditional wasn't recognized, then the results
        -- of this check function are false. If this is a validation run though, you
        -- want to keep processing. The success/failure of the inline conditional's
        -- result isn't the point, you just want to know if the conditionals are valid.
        if bGood ~= nil and bGood == false then
            if smsg ~= nil then
                utilities.fDisplayOnce(smsg);
            end
        end

        -- The inline wasn't found
        if bGood == nil then
            smsg = 'Warning: Unrecognized conditional: ' .. suCode;
            utilities.fDisplayOnce(smsg);
            bGood = false;
        end

        -- Now, if the code was in error and this was not a validation pass,
        -- processing is done and a false result must be returned. However,
        -- if this is a validation pass, you've already displayed the problem
        -- and need to continue processing the conditionals.
        if bValidate == false and bGood == false then
            return bGood,sGear,sValidSlots
        end
    end

    -- By getting to this point, the conditional(s) have to be true or the validation pass is complete
    return bGood,sGear,sValidSlots;
end     -- fCheckInline
