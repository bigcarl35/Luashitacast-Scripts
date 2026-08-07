local inline = {};

--[[
    This component contains all routines that deal with inline conditionals

    List of routines-
        Functions:
            fCheckInline              Coordinates all the checkinline functions
            fCheckInlineActivity      Checks the validity of the passed gather/craft inline code
            fCheckInlineBuff          Checks the validity of the inline buff code
            fCheckInlineConditional   Checks the validity of the conditional inline code
            fCheckInlineCustomType    Checks whether the custom conditional is enabled
            fCheckInlineDay           Checks the validity of the conditional day code
            fCheckInlineDebuff        Checks the validity of the inline debuff code
            fCheckInlineGear          Checks the validity of the inline gear check code
            fCheckInlineJob           Checks the validity of the inline job code
            fCheckInlineMagicType     Checks the validity of the inline magic type code
            fCheckInlineMoon          Checks the validity of the inline moon phase code
            fCheckInlineOther         Checks the validity of the inline other code
            fCheckInlinePet           Checks the validity of the inline pet code
            fCheckInlineSlot          Checks the validity of the inline slot code
            fCheckInlineSongs         Checks the validity of the inline song code
            fCheckInlineTarget        Checks the validity of the inline target code
            fCheckInlineTime          Checks the validity of the inline time code
            fCheckInlineToggle        Checks the validity of the inline toggle code
            fCheckInlineWeaponType    Checks the validity of the inline weapon type code
            fCheckInlineWeather       Checks the validity of the inline weather code
            fEvaluateCondition        Determines if the passed condition is true
--]]

--[[
    fCheckInlineBuff checks the validity of the passed inline code and then determines if the
    coded condition is true.

    Parameter
        sCode       coded condition to be checked

    Returned
        bGood       was the coded condition met? T/F/nil where nil means code Unknown
        smsg        likely nil, but if validation fails, returns error message
--]]

function inline.fCheckInlineBuff(sCode)
    local bGood = nil;
    local bNot = false;
    local smsg = nil;
    local tBuffs = {
        'AFTERMATH','ANCIENT_CIRCLE','ARCANE_CIRCLE','BARAERO','BARBLIZZARD','BARFIRE','BARSTONE',
        'BARTHUNDER','BARWATER','BARSLEEP','BARPOISON','BARPARALYZE','BARBLIND','BARVIRUS',
        'BARPETRIFY','COVER','ENAERO','ENBLIZZARD','ENDARK','ENFIRE','ENLIGHT','ENSTONE','ENTHUNDER',
        'ENWATER','FLEE','HOLY_CIRCLE','REPRISAL','SAMBA','SANCTION','SHINING_RUBY','SIGNET',
        'SNEAK_ATTACK','SPIKE','TRICK_ATTACK','UTSUSEMI','WARDING_CIRCLE','YONIN'
        };
    local tBarelemental  = { 'BARAERO','BARBLIZZARD','BARFIRE','BARSTONE','BARTHUNDER','BARWATER' };
    local tBarstatus     = { 'BARSLEEP','BARPOISON','BARPARALYZE','BARBLIND','BARVIRUS','BARPETRIFY' };

    sCode = string.gsub(string.upper(sCode),' ','_');
    local i = string.find(sCode,'NOT_')
    if i ~= nil and i == 1 then
        bNot = true;
        sCode = string.sub(sCode,5,-1);
    end

    if table.find(tBarelemental,sCode) ~= nil then     -- Does player have one of the Bar-elemental buffs?
        for ii,jj in pairs(tBarelemental) do
            local b = (utilities.fBuffed(jj,true));
            if b == true then
                bGood = true;
                break;
            end
        end
    elseif table.find(tBarstatus,sCode) ~- nil then    -- Does player have one of the Bar-status buffs
        for ii,jj in pairs(tBarstatus) do
            local b = (utilities.fBuffed(jj,true));
            if b == true then
                bGood = true;
                break;
            end
        end
    elseif sCode == 'BARANY' then                       -- Does the player have any barspell buff
        bGood = (utilities.fBuffed('BAR',true));
    elseif sCode == 'ENANY' then                        -- Does the player have any enspell buff
        bGood = (utilities.fBuffed('EN',true));
    else                                                -- Look for a specific buff/bar/en-spell buff
        if table.find(tBuffs,sCode) ~= nil then
            bGood = (utilities.fBuffed(sCode,true));
        end
    end

    if bGood ~= nil and bNot == true then
        bGood = not bGood;
    end

    return bGood,smsg;
end     -- inline.fCheckInlineBuff

--[[
    fCheckInlineDebuff checks the validity of the passed inline code and then determines if the
    coded condition is true.

    Parameter
        sCode       coded condition to be checked

    Returned
        bGood       was the coded condition met? T/F/nil where nil means code Unknown
        smsg        likely nil, but if validation fails, returns error message
--]]

function inline.fCheckInlineDebuff(sCode)
    local bGood = nil;
    local bNot = false;
    local bFound = false;
    local smsg = nil;
    local iPos;
    local tDebuffs = {
        ['CODE']    = {
                'ADDLED','AMNESIA','BANED','BLINDED','BOUND','BUSTED','CHARMED','CURSED','DISEASED',
                'DOOMED','ENCUMBERED','IMPAIRED','KO','MEDICATED','MUTED','PARALYZED','PETRIFIED','PLAGUED',
                'POISONED','SILENCED','SJ_RESTRICTION','SLEPT','STUNNED','TERRIFIED','WEAKENED','WEIGHTED'
                },
        ['BUFF']    = {
                'Addle','Amnesia','Bane','Blind','Bind','Bust','Charm','Curse','Disease',
                'Doom','Encumbrance','Impairment','KO','Medicine','Mute','Paralysis','Petrify','Plague',
                'Poison','Silence','SJ Restriction','Sleep','Stun','Terror','Weak','Weight'
                }
    };

    sCode = string.gsub(string.upper(sCode),' ','_');
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

    return bGood,smsg;
end     -- inline.fCheckInlineDebuff

--[[
    fEvaluateCondition determines if the passed condition is true or not, no validity
    is checked.

    Parameters
        sType      coded field to check
        sOp        coded comparator
        iNum       value to compare to
        gsName     name of the gear set where condition was found

    Returned
        bGood   was the coded condition met? T/F
--]]

function fEvaluateCondition(sType,sOp,iNum,gsName)
    local player = utilities.SetJob();
    local party = gData.GetParty();
    local pet = gData.GetPet();
    local iVal;
    local bGood = false;

    -- First determine the value to check against
    if sType == 'HP' then
        iVal = player.HP;
    elseif sType == 'HPP' then
        iVal = player.HPP;
    elseif sType == 'HPPA' then
        -- See if an average max HPP is known
        if gVars.AverageMaxStats[gsName] == nil or gVars.AverageMaxStats[gsName][gVars._MAXHPP] == nil then
            gVars.AverageMaxStats[gsName][gVars._MAXHPP] = player.HPP;
            -- While we're here, note the average max MPP too
            gVars.AverageMaxStats[gsName][gVars._MAXMPP] = player.MPP;
        end
        iVal = gVars.AverageMaxStats[gsName][gVars._MAXHPP];
    elseif sType == 'MP' then
        iVal = player.MP;
    elseif sType == 'MPP' then
        iVal = player.MPP;
    elseif sType == 'MPPA' then
        -- See if an average max MPP is known
        if gVars.AverageMaxStats[gsName] == nil or gVars.AverageMaxStats[gsName][gVars._MAXMPP] == nil then
            gVars.AverageMaxStats[gsName][gVars._MAXMPP] = player.MPP;
             -- While we're here, note the average max HPP too
            gVars.AverageMaxStats[gsName][gVars._MAXHPP] = player.HPP;
        end
        iVal = gVars.AverageMaxStats[gsName][gVars._MAXMPP];
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

    Parameter
        sCode       coded condition to be checked

    Returned
        bGood       was the coded condition met? T/F/nil where nil means code Unknown
        smsg        likely nil, but if validation fails, returns error message

    Note: All conditional can support a secondary "wiggle". It's intention is to stop flickering
        that can occur. If a conditional evaluates to false, a second evaluation that adds the
        wiggle will be done. If false it's false and if true it's true. The first evaluation is
        the true comparison, the second only occurs if indicated, buying a delay in skipping the
        piece of gear the conditional was placed on. How much "wiggle" designated requires the
        player to look at the two pieces of gear and see what causes the flickering. Add an appro-
        priate number to the conditional by appending h# where "h" means to hold for the "#" of
        units (MP if MPP, TP if TPP, and HP if HPP. It should be obvious what the unit should
        be.) If no flicker occurs, then don't include the "hold" suffix, you don't need it.

--]]

function inline.fCheckInlineConditional(sCode,gsName)
    local player = utilities.SetJob();
    local pet = gData.GetPet();
    local tConds = { 'HP.','HPP.','HPPA.','MP.','MPP.','MPPA.','TP.','TPP.','LVL.','PARTY.','PETHPP.' };
    local tOps   = { '.EQ.','.GT.','.GE.','.LT.','.LE.','.NE.' };
    local bGood = nil;
    local bNot = false;
    local smsg = nil;
    local iWiggle = 0;
    local iNum,iPos,iStart;
    local sOp;

    sCode = string.upper(sCode);
    local i = string.find(sCode,'NOT_');
    if i ~= nil and i == 1 then
        bNot = true;
        sCode = string.sub(sCode,5,-1);
    end

    local iPos = string.find(sCode,'%.');
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
        if sCond == 'PETHPP.' and pet == nil then
            -- No pet, so just return false
            return false,nil;
        end

        if table.find(tConds,sCond) ~= nil then
            -- Yup. Now see if a valid operator is found
            sOp = string.sub(sCode,iPos,iPos+3);                -- Extracted comparator
            if table.find(tOps,sOp) ~= nil then
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
                elseif string.find('HPP,HPPA,MPP,MPPA,PETHPP',sCond) ~= nil then
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
                bGood = fEvaluateCondition(sCond,sOp,iNum,gsName);
                return bGood,smsg;
            else
                smsg = 'Warning: Invalid operator encountered or unknown code: ' .. sCode;
                return false,smsg;
            end
        end
    end

    if bGood == nil then
        -- Special case
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
    return bGood,smsg;
end     -- inline.fCheckInlineConditional

--[[
    fCheckInlineDay checks the validity of the passed inline code and then determines if the
    coded condition is true.

    Parameter
        sCode       coded condition to be checked

    Returned
        bGood       was the coded condition met? T/F/nil where nil means code Unknown
        smsg        if an error message occurs, will contain the error message
--]]

function inline.fCheckInlineDay(sCode)
    local environ = gData.GetEnvironment();
    local bGood = nil;
    local bNot = false;
    local smsg = nil;
    local tDay = { 'DARKSDAY','EARTHSDAY','FIRESDAY','ICEDAY','LIGHTNINGDAY','LIGHTSDAY','WATERSDAY','WINDSDAY' };

    sCode = string.upper(sCode);
    local i = string.find(sCode,'NOT_');
    if i~= nil and i == 1 then
        bNot = true;
        sCode = string.sub(sCode,5,-1);
    end

    if table.find(tDay,sCode) ~= nil then
        bGood = (sCode == string.upper(environ.Day));
        if bNot == true then
            bGood = not bGood;
        end
    end

    return bGood,smsg;
end     -- inline.fCheckInlineDay

--[[
    fCheckInlineMoon checks the validity of the passed inline code and then determines if the
    coded condition is true.

    Parameter
        sCode       coded condition to be checked

    Returned
        bGood       was the coded condition met? T/F/nil where nil means code Unknown
        smsg        if an error message occurs, will contain the error message
--]]

function inline.fCheckInlineMoon(sCode)
    local environ = gData.GetEnvironment();
    local bGood = nil;
    local bNot = false;
    local smsg = nil;

    sCode = string.upper(sCode);
    local i = string.find(sCode,'not_');
    if i ~= nil and i == 1 then
        bNot = true;
        sCode = string.sub(sCode,5,-1);
    end

    if sCode == 'FULLMOON' then
        bGood = (environ.MoonPhase == 'Full Moon');
    elseif sCode == 'NEWMOON' then
        bGood = (environ.MoonPhase == 'New Moon');
    elseif sCode == 'GIBBOUS' then
        bGood = (string.find(environ.MoonPhase,'Gibbous') ~= nil);
    elseif sCode == 'QUARTERMOON' then
        bGood = (string.find(environ.MoonPhase,'Quarter') ~= nil);
    elseif bGood == 'CRESCENT' then
        bGood = (string.find(environ.MoonPhase,'Crescent') ~= nil);
    end

    if bGood ~= nil and bNot == true then
        bGood = not bGood;
    end

    return bGood,smsg;
end     -- inline.fCheckInlineMoon

--[[
    fCheckInlineGear checks the validity of the passed inline code and then determines if the
    coded condition is true.

    Parameters
        sCode       coded condition to be checked
        sSlot       name of slot to check
        ts          gear set currently being populated
        sGear       gear name minus codes

    Returned
        bGood       was the coded condition met? T/F/nil where nil means code Unknown
        smsg        if an error message occurs, will contain the error message

    Note:
        //[NOT_]SLOTS: has been removed from this code. It's more complicated than
        originally thought. The goal has been moved to a future, post-2.0 implementation.
--]]

function inline.fCheckInlineGear(sCode,sSlot,ts,sGear)
    local gSet = gData.GetCurrentSet();
    local bGood = nil;
    local ssLot = nil;
    local bNot = false;
    local iOff,iPos,sItem;
    local smsg = nil;

    sCode = string.upper(sCode);
    local sCodeHold = sCode;
    local i = string.find(sCode,'NOT_');
    if i ~= nil and i == 1 then
        bNot = true;
        sCode = string.sub(sCode,5,-1);         -- Remove the NOT_
    end

    iPos = string.find(sCode,'IF');
    if iPos ~= nil and iPos == 1 then
        -- Determine if //IF: or //IF-slot:. Note or assign the slot and item
        if string.sub(sCode,3,1) == ':' then
            -- Make sure the slot is not a subset or group
            if string.find('SUBSET,GROUP',sCode) ~= nil then
                smsg = 'Warning: //IF: and //NOT_IF: cannot be used with either a Subset or Group: '.. sSlot;
                bGood = false;
            end
            -- Now proceed
            if bGood == nil then
                ssLot = sSlot;
                sItem = string.sub(sCode,4,-1);
            end
        elseif string.sub(sCode,3,1) == '-' then
            local j = string.find(sCode,':');
            ssLot = utilities.fValidSlots(string.sub(sCode,4,j-1),gVars._SLOT_FA);
            if ssLot == nil then
                -- Slot was Unrecognized
                smsg = 'Warning: Invalid slot in inline conditional: ' .. sCodeHold;
                bGood = false;
            end
            -- Grab the item
            if bGood == nil then
                sItem = string.sub(sCode,j+1,-1);
            end
        end

        -- Simple comparison: equip gear piece if currently wearing identified
        -- gear piece. Checks dynamic composite gear set first. If empty, then
        -- checks currently worn gear.

        if bGood == nil then
            -- Check the temporary set
            if not (ts[ssSlot] == nil or ts[ssSlot] == '') then
                -- Since slot not empty, check item name
                if string.lower(ssSlot) == 'ears' then
                    bGood = (string.find(string.lower(sItem),string.lower(ts['Ear1'])) ~= nil or
                            string.find(string.lower(sItem),string.lower(ts['Ear2'])) ~= nil);
                elseif string.lower(ssLot) == 'rings' then
                    bGood = (string.find(string.lower(sItem),string.lower(ts['Ring1']) ~= nil) or
                            string.find(string.lower(sItem),string.lower(ts['Ring2'])) ~= nil);
                else
                    bGood = (string.find(string.lower(sItem),string.lower(ts[sSlot])) ~= nil);
                end
            else
                -- Since temporary set slot was empty, check currently equipped gear
                if gSet[ssSlot] == nil or gSet[ssSlot] == '' then
                    bGood = false;
                elseif string.lower(ssLot) == 'ears' then
                    bGood = (string.find(string.lower(sItem),string.lower(gSet['Ear1'])) ~= nil or
                            string.find(string.lower(sItem),string.lower(gSet['Ear2'])) ~= nil);
                elseif string.lower(ssSLot) == 'rings' then
                    bGood = (string.find(string.lower(sItem).string.lower(gSet['Ring1'])) ~= nil or
                            string.find(string.lower(sItem),string.lower(gSet['Ring2'])) ~= nil);
                else
                    bGood = (string.find(string.lower(sItem),string.lower(gSet[ssSlot])) ~= nil);
                end
            end

            if bGood == nil then
                bGood = false;
            end

            if smsg == nil and bNot == true then
               bGood = not bGood;
            end
        end
    elseif sCode == 'SPECIAL' then
        -- Equip if the special conditions are met. This applies to specific pieces of gear where special calculations are needed
        if gear.fHasGCBeenRun() == false then  -- GC must be run for the special calculations to work
            bGood = false;
        else
            if sSlot ~= 'subset' then
                bGood = gear.fValidateSpecial(sSlot,sGear);
            else    -- Invalid inline for a subset
                smsg = 'Warning: //SPECIAL cannot be used on a subset or a group';
                bGood = false;
            end
        end
    end

    return bGood,smsg;
end     -- inline.fCheckInlineGear

--[[
    fCheckInlineTarget checks the validity of the passed inline code and then determines if the
    coded condition is true.

    Parameter
        sCode       coded condition to be checked

    Returned
        bGood       was the coded condition met? T/F/nil where nil means code Unknown
        smsg         if an error message occurs, will contain the error message
--]]

function inline.fCheckInlineTarget(sCode)
    local tg = gData.GetTarget();
    local bGood = nil;
    local bNot = false;
    local smsg = nil;

    sCode = string.gsub(string.lower(sCode),' ','_');
    local i = string.find(sCode,'not_');
    if i ~= nil and i == 1 then
        bNot = true;
        sCode = string.sub(sCode,5,-1);         -- Remove the NOT_
    end

    if string.find(sCode,'fam:') ~= nil or string.find(sCode,'eco:') ~= nil then
        -- Equip if the target's ecosytem/family contains passed substring
        bGood = utilities.fGetMobType(sCode);
    elseif sCode == 'ME' then
        -- Equip if target is the player
        local me = AshitaCore:GetMemoryManager():GetParty():GetMemberTargetIndex(0);
        bGood = (tg == me);
    end

    if bGood ~= nil and bNot == true then
        bGood = not bGood;
    end

    return bGood,smsg;
end     -- inline.fCheckInlineTarget

--[[
    fCheckInlinePet checks the validity of the passed inline code and then determines if the
    coded condition is true.

    Parameter
        sCode       coded condition to be checked

    Returned
        bGood       was the coded condition met? T/F/nil where nil means code Unknown
        smsg         if an error message occurs, will contain the error message
--]]

function inline.fCheckInlinePet(sCode)
    local environ   = gData.GetEnvironment();
    local petAction = gData.GetPetAction();
    local player    = gData.GetPlayer();
    local spell     = gData.GetAction();
    local pet       = gData.GetPet();
    local tSMNList  = { 'SMN:BP:SKILL','SMN:BP:MAG','SMN:BP:PHYS','SMN:BP:ACC','SMN:BP:HYBRID' };
    local tBSTList  = { 'BST:PET_ATTACK','BST:PET_MATT','BST:PET_MACC' };
    local sFull     = nil;
    local bErr      = false;
    local bGood     = nil;
    local bNot      = false;
    local smsg      = nil;

    sCode = string.gsub(string.upper(sCode),' ','_');
    sFull = sCode;

    local i = string.find(sCode,'NOT_');
    if i ~= nil and i == 1 then
        bNot = true;
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
        -- Is there a pet named
        local n = string.sub(sCode,9,-1);
        bGood = (pet ~= nil and pet.Name ~= nil and string.find(string.upper(pet.Name),n) ~= nil);
    elseif sCode == 'BST:PET' then
        bGood = (pet ~= nil and pets.fPetType() == gVars._TYPE_BST);
    elseif sCode == 'SMN:PET' then
        bGood = (pet ~= nil and pets.fPetType() == gVars._TYPE_SMN);
    elseif sCode == 'DRG:PET' then
        bGood = (pet ~= nil and pets.fPetType() == gVars._TYPE_DRG);
    elseif sCode == 'PUP:PET' then
        bGood = (pet ~= nil and pets.fPetType() == gVars._TYPE_PUP);
    elseif sCode == 'SMN:AVATAR' then
        -- Equip if the pet being summoned is an avatar, but not an elemental spirit
        if spell ~= nil and spell.Name ~= nil then
            bGood = (table.find(gVars.tSpellGroupings['avatars'],string.lower(spell.Name)) ~= nil);
        else
            bGood = false;
        end
    elseif sCode == 'SMN:SPIRIT' then
        -- Equip if the pet being summoned is an elemental spirit
        if spell ~= nil and spell.Name ~= nil then
            bGood = (table.find(gVars.tSpellGroupings['spirits'],string.lower(spell.Name)) ~= nil);
        else
            bGood = false;
        end
    elseif sCode == 'SMN:SUMMONS' then
        -- Equip if the pet being summoned is an avatar or elemental spirit
        if spell ~= nil and spell.Name ~= nil then
            bGood = (table.find(gVars.tSpellGroupings['avatars'],string.lower(spell.Name)) ~= nil or
            table.find(gVars.tSpellGroupings['spirits'],string.lower(spell.Name)) ~= nil);
        else
            bGood = false;
        end
    -- BST commands
    elseif sCode == 'PC:FIGHT' then
        bGood = (pet ~= nil and pets.fPetType() == gVars._TYPE_BST and string.upper(petAction.Name) == 'FIGHT');
    elseif sCode == 'PC:HEEL' then
        bGood = (pet ~= nil and pets.fPetType() == gVars._TYPE_BST and string.upper(petAction.Name) == 'HEEL');
    elseif sCode == 'PC:STAY' then
        bGood = (pet ~= nil and pets.fPetType() == gVars._TYPE_BST and string.upper(petAction.Name) == 'STAY');
    elseif sCode == 'PC:SIC' then
        bGood = (pet ~= nil and pets.fPetType() == gVars._TYPE_BST and string.upper(petAction.Name) == 'SIC');
    elseif sCode == 'PC:READY' then
        bGood = (pet ~= nil and pets.fPetType() == gVars._TYPE_BST and string.upper(petAction.Name) == 'READY');
    elseif sCode == 'PC:LEAVE' then
        bGood = (pet ~= nil and pets.fPetType() == gVars._TYPE_BST and string.upper(petAction.Name) == 'LEAVE');
    elseif sCode == 'PC:SNARL' then
        bGood = (pet ~= nil and pets.fPetType() == gVars._TYPE_BST and string.upper(petAction.Name) == 'SNARL');
    -- SMN commands
    elseif sCode == 'PC:ASSAULT' then
        bGood = (pet ~= nil and pets.fPetType() == gVars._TYPE_SMN and string.upper(petAction.Name) == 'ASSAULT');
    elseif sCode == 'PC:RELEASE' then
        bGood = (pet ~= nil and pets.fPetType() == gVars._TYPE_SMN and string.upper(petAction.Name) == 'RELEASE');
    elseif sCode == 'PC:RETREAT' then
        bGood = (pet ~= nil and pets.fPetType() == gVars._TYPE_SMN and string.upper(petAction.Name) == 'RETREAT');
    -- DRG commands
    elseif sCode == 'PC:DISMISS' then
        bGood = (pet ~= nil and pets.fPetType() == gVars._TYPE_DRG and string.upper(petAction.Name) == 'DISMISS');
    elseif sCode == 'PC:STEADY_WING' then
        bGood = (pet ~= nil and pets.fPetType() == gVars._TYPE_DRG and
            string.gsub(string.upper(petAction.Name),' ','_') == 'STEADY_WING');
    -- PUP commands
    elseif sCode == 'PC:DEPLOY' then
        bGood = (pet ~= nil and pets.fPetType() == gVars._TYPE_PUP and string.upper(petAction.Name) == 'DEPLOY');
    elseif sCode == 'PC:DEACTIVATE' then
        bGood = (pet ~= nil and pets.fPetType() == gVars._TYPE_PUP and string.upper(petAction.Name) == 'DEACTIVATE');
    elseif sCode == 'PC:RETRIEVE' then
        bGood = (pet ~= nil and pets.fPetType() == gVars._TYPE_PUP and string.upper(petAction.Name) == 'RETRIEVE');
    -- Check on special BST and SMN attacks (either Sic or Ready! actions or blood pacts)
    elseif table.find(tBSTList,sCode) ~= nil and petaction ~= nil and petaction.Name ~= nil then
        bGood = ((table.find(pets.BstPetAttack,petaction.Name) ~= nil and sCode == 'BST:PET_ATTACK') or
                 (table.find(pets.BstPetMagicalAttack,petaction.Name) ~= nil and sCode == 'BST:PET_MATT') or
                 (table.find(pets.BstPetMagicalAccuracy,petaction.Name) ~= nil and sCode == 'BST:PET_MACC'));
     elseif table.find(tSMNList,sCode) ~= nil and petaction ~= nil and petaction.Name ~= nil then
        bGood = ((table.find(pets.SmnBPSkill,petaction.Name) ~= nil and sCode == 'SMN:BP:SKILL') or
                 (table.find(pets.SmnBPMagical,petaction.Name) ~= nil and sCode == 'SMN:BP:MAG') or
                 (table.find(pets.SmnBPAccuracy,petaction.Name) ~= nil and sCode == 'SMN:BP:ACC') or
                 (table.find(pets.SmnBPHybrid,petaction.Name) ~= nil and sCode == 'SMN:BP:HYBRID') or
                 (table.find(pets.SmnBPPhysical,petaction.Name) ~= nil and sCode == 'SMN:BP:PHYS'));
    elseif sCode == 'SMN:AVATAR' then
        -- Equip if the pet being summoned is an avatar, but not an elemental spirit
        if spell ~= nil and spell.Name ~= nil then
            bGood = (table.find(gVars.tSpellGroupings['avatars'],string.lower(spell.Name)) ~= nil);
        else
            bGood = false;
        end
    elseif sCode == 'SMN:SPIRIT' then
        -- Equip if the pet being summoned is an elemental spirit
        if spell ~= nil and spell.Name ~= nil then
            bGood = (table.find(gVars.tSpellGroupings['spirits'],string.lower(spell.Name)) ~= nil);
        else
            bGood = false;
        end
    elseif sCode == 'SMN:SUMMONS' then
        -- Equip if the pet being summoned is an avatar or elemental spirit
        if spell ~= nil and spell.Name ~= nil then
            bGood = (table.find(gVars.tSpellGroupings['avatars'],string.lower(spell.Name)) ~= nil or
                     table.find(gVars.tSpellGroupings['spirits'],string.lower(spell.Name)) ~= nil);
        else
            bGood = false;
        end
    elseif string.find(sCode,'SMN:SUMMONS:') ~= nil then
        -- Does the summoner's pet name contain the passed substring
        local n = string.sub(sCode,13,-1);
         if spell ~= nil and spell.Name ~= nil then
             local o = string.gsub(string.upper(spell.Name)));
             bGood = (string.find(o,n) ~= nil(;
         else
             bGood = false;
        end
    elseif string.find(sCode,'SMN:BP:') ~= nil then
        -- Does the summoner's blood pact contain the passed blood pact substring
        if (petAction ~= nil and petAction.Name ~= nil) then
            local x = string.gsub(string.lower(string.sub(sCode,8,-1)),' ','_');
            local y = string.gsub(string.lower(petaction.Name),' ','_');
            bGood = (string.find(y,x) ~= nil);
        else
            bGood = false;
        end
    elseif sCode == 'SMN:PET' then
        --Is there a summoner's pet out
        if pet == nil then
            bGood =  false;
        else
            bGood = (pets.fSummonerPet(pet) ~= nil);
        end
    elseif sCode == 'SMN:PET:AVATAR' then
        -- Is the player's pet an elemental spirit
        bGood = (pet ~= nil and pet.Name ~= nil and table.find(gVars.tSpellGrouping['avatars'],string.lower(pet.Name)) ~= nil);
    elseif sCode == 'SMN:PET:SPIRIT' then
        -- Is the player's pet an elemental spirit
        bGood = (pet ~= nil and pet.Name ~= nil and table.find(gVars.tSpellGrouping['spirits'],string.lower(pet.Name)) ~= nil);
    elseif sCode == 'SMN:PETMW' then
        -- Does the summoner's pet's element matches the weather's element
        local ele = pets.fElementByPetName(pet.Name);
        if ele ~= nil then
            bGood = (string.find(ele,string.lower(environ.RawWeather)) ~= nil);
        else
            bGood = false;
        end
    elseif sCode == 'SMN:PETMD' then
        -- Does the summoner's pet's element matches the day's element
        local ele = pets.fElementByPetName(pet.Name);
        if ele == nil then
            bGood = false;
        else
            bGood = (ele == string.lower(environ.DayElement));
        end
    end

    if bGood ~= nil and bNot == true and bErr == false then
        -- Special case: //NOT_SMN:PET returns false if no pet exists. Otherwise, just
        -- flip the results. This also catches all other inversions
        if sCode == 'SMN:PET' and pet == nil then
            bGood = false;
        else
            bGood = not bGood;
        end
    end

    return bGood,smsg;
end     -- inline.fCheckInlinePet

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
--]]

function inline.fCheckInlineSlot(sCode,sSlot,ts)
    local bGood = nil;
    local smsg = nil;
    local bNot = false;
    local bErr = false;
    local suSlot;
    local sValidSlots = nil;

    if bSubset == nil then
        bSubset = false;
    end

    sCode = string.upper(sCode);
    suSlot = string.upper(sSlot);

    local i = string.find(sCode,'NOT_');
    if i ~= nil and i == 1 then
        bNot = true;
        sCode = string.sub(sCode,5,-1);         -- Remove the NOT_
    end

    if string.find(sCode,'EMPTY:1') ~= nil or string.find(sCode,'EMPTY:2') then
        local sNum = string.sub(sCode,-1,-1);
        if string.find('EARS,EAR1,EAR2,RINGS,RING1,RING2',suSlot) == nil then
            smsg = 'Warning: Invalid slot designated: ' .. sCode;
            bGood = false;
            bErr = true;
        elseif string.find(suSlot,'EAR') ~= nil then
            bGood = (ts['Ear'..sNum] == nil or ts['Ear'..sNum] == '');
        elseif string.find(suSlot,'RING') ~= nil then
            bGood = (ts['Ring'..sNum] == nil or ts['Ring'..sNum] == '');
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
    end

    if bGood ~= nil and smsg == nil and bNot == true and bErr == false then
        bGood = not bGood;
    end
    return bGood,smsg;
end     -- inline.fCheckInlineSlot

--[[
    fCheckInlineSongs checks to see if the song being cast matches the code definition or
    song category.

    Parameter
        sCode       coded condition to be checked

    Returned
        bGood       was the coded condition met? T/F/nil
        smsg        if an error message occurs, will contain the error message
--]]

function inline.fCheckInlineSongs(sCode)
    local song = gData.GetAction();
    local bGood = nil;
    local bNot = false;
    local smsg = nil;
    local bErr = false;
    local tSongTypes = {    -- List of songs by type
        ['aubade'] =    { 'fowl aubade' },
        ['ballad'] =    { 'mage\'s ballad','mage\'s ballad ii' },
        ['capriccio'] = { 'gold capriccio' },
        ['carol'] =     { 'light carol','earth carol','water carol','wind carol','fire carol',
                          'ice carol','lightning carol','dark carol' },
        ['elegy'] =     { 'battlefield elegy','carnage elegy' },
        ['etude'] =     { 'enchanting etude','spirited etude','learned etude','quick etude',
                          'vivacious etude','dexterous etude','sinewy etude','bewitching etude',
                          'logical etude','sage etude','swift etude','vital etude','uncanny etude',
                          'herculean etude' },
        ['fantasia'] =  { 'shining fantasia' },
        ['finale'] =    { 'magical finale' },
        ['gavotte'] =   { 'goblin gavotte' },
        ['hymnus'] =    { 'goddess\'s hymnus' },
        ['lullaby'] =   { 'foe lullaby', 'horde lullaby' },
        ['madrigal'] =  { 'sword madrigal', 'blade madrigal' },
        ['mambo'] =     { 'sheepfoe mambo','dragonfoe mambo' },
        ['march'] =     { 'advancing march','victory march' },
        ['mazurka'] =   { 'raptor mazurka','chocobo mazurka' },
        ['minne'] =     { 'knight\'s minne','knight\'s minne ii','knight\'s minne iii',
                          'knight\'s minne iv' },
        ['minuet'] =    { 'valor minuet','valor minuet ii','valor minuet iii','valor minuet iv' },
        ['operetta'] =  { 'scop\'s operetta','puppet\'s operetta' },
        ['paeon'] =     { 'army\'s paeon','army\'s paeon ii','army\'s paeon iii','army\'s paeon iv',
                          'army\'s paeon v' },
        ['pastoral'] =  { 'herb pastoral' },
        ['prelude'] =   { 'hunter\'s prelude','archer\'s prelude' },
        ['requiem'] =   { 'foe requiem','foe requiem ii','foe requiem iii','foe requiem iv',
                          'foe requiem v','foe requiem vi' },
        ['round'] =     { 'warding round' },
        ['threnody'] =  { 'light threnody','dark threnody','earth threnody','water threnody',
                          'wind threnody','fire threnody','ice threnody','lightning threnody' },
        ['virelai'] =   { 'maiden\'s virelai' }
    };

    if sCode == nil then
        return false,nil;
    end

    sCode = string.lower(sCode);

    local i = string.find(sCode,'not_');
    if i ~= nil and i == 1 then
        bNot = true;
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
        else
            smsg = 'Warning: No song identified: ' .. sCode;
            bGood = false;
            bErr = true;
        end
    elseif string.find(sCode,'songtype:') ~= nil then
        -- A songcat represents the category a song is associated with. Refer to the tSongTypes definition
        for i,j in pairs(tSongTypes) do
            if i == string.sub(sCode,10,-1) then
                bGood = (table.find(j,string.lower(song.Name)) ~= nil);
                break;
            end
        end
    end

    if bGood ~= nil and smsg == nil and bNot == true and bErr == false then
        bGood = not bGood;
    end

    return bGood,smsg;
end     -- inline.fCheckInlineSongs

--[[
    fCheckInlineOther checks the validity of the passed inline code and then determines
    if the coded condition is true.

    Parameter
        sCode       coded condition to be checked
        sGear       gear name minus codes

    Returned
        bGood       was the coded condition met? T/F/nil
        smsg        if an error message occurs, will contain the error message
--]]

function inline.fCheckInlineOther(sCode,sGear)
    local zoneId = AshitaCore:GetMemoryManager():GetParty():GetMemberZone(0);
    local spell = gData.GetAction();
    local party = gData.GetParty();
    local player = utilities.SetJob();
    local environ = gData.GetEnvironment();
    local ability = gData.GetAction();
    local bGood = nil;
    local bNot = false;
    local bErr = false;
    local bFlip = true;     -- indicates if NOT_xxx valid
    local smsg = nil;

    if sCode == nil then
        return false;
    end

    sCode = string.lower(sCode);

    local i = string.find(sCode,'not_');
    if i ~= nil and i == 1 then
        bNot = true;
        sCode = string.sub(sCode,5,-1); -- remove the "not_"
    end

    -- First check for spell name fragment
    if (spell ~= nil and spell.Name ~= nil) then
        if string.find(sCode,'spell:') ~= nil then
            bGood = (string.find(string.lower(spell.Name),string.sub(sCode,7,-1)) ~= nil);
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
        elseif sCode == 'dualwield' then
            bGood = (string.find('NIN,DNC',player.MainJob) ~= nil or string.find('NIN,DNC',player.SubJob) ~= nil);
        elseif sCode == 'inparty' then
            -- Is the player in a party?
            bGood = (party ~= nil and party.Count ~= nil and party.Count > 1);
        elseif sCode == 'own' then
            -- Is region controlled by player's nation?
            bGood = (gVars.sRegion == gVars._REGION_STATUS_OWNED);
        elseif string.find(sCode,'party:') ~= nil then
            local iNum = tonumber(string.sub(sCode,7,-1));
            bGood = (party ~= nil and party.Count ~= nil and party.Count == iNum);
        elseif string.find('mode=perp,mode=attk,mode=enmm',sCode) ~= nil then
            -- Check on smn gear's mode
            if player.MainJob == 'SMN' or player.SubJob == 'SMN' then
                -- Has to be a smn
                local x = string.sub(sCode,6,-1);
                local sMode = string.lower(utilities.fGetCycle(gVars._MODE));
                bGood = (sMode == x);
            else
                bGood = false;
            end
        elseif sCode == 'town' then
            bGood = (environ.Area ~= nil and table.find(gVars.tTownAreas['Towns'],environ.Area) ~= nil);
        elseif sCode == 'town-ak' then
            bFlip = false;
            -- Equip the appropriate national/ducal aketon if in the appropriate town
            local pNation = AshitaCore:GetMemoryManager():GetPlayer():GetNation();
            local slcGear = string.lower(sGear);

            if slcGear == 'ducal aketon' then
                bGood = (environ.Area ~= nil and
                    (table.find(gVars.tTownAreas['Windy'],environ.Area) ~= nil or
                    table.find(gVars.tTownAreas['Sandy'],environ.Area) ~= nil or
                    table.find(gVars.tTownAreas['Bastok'],environ.Area) ~= nil or
                    table.find(gVars.tTownAreas['Jeuno'],environ.Area) ~= nil)
                    );
            elseif slcGear == 'federation aketon' then
                if environ.Area ~= nil and table.find(gVars.tTownAreas['Windy'],environ.Area) ~= nil then
                    -- Equip the Windurstian national aketon if in Windurst
                    bGood = (pNation == 2);
                end
            elseif slcGear == 'republic aketon' then
                if environ.Area ~= nil and table.find(gVars.tTownAreas['Bastok'],environ.Area) == nil then
                    -- Equip the Bastokian national aketon if in Bastok
                    bGood = (pNation == 1);
                end
            elseif slcGear == 'kingdom aketon' then
                if environ.Area ~= nil and table.find(gVars.tTownAreas['Sandy'],environ.Area) == nil then
                    -- Equip the Sandorian national aketon if in San d'Oria
                    bGood = (pNation == 0);
                end
            else
                smsg = 'Warning: Invalid body piece for national aketon check: ' .. sGear;
                bGood = false;
                bErr = true;
            end
        elseif string.find(sCode,'ja:') ~= nil then
            bGood = (string.find(string.lower(ability.Name),string.sub(sCode,4,-1)) ~= nil);
        elseif string.find(sCode,'status:') ~= nil then
            bGood = ((player.Status == 'Engaged' and sCode == 'status:engaged') or
                     (player.Status == 'Resting' and sCode == 'status:resting') or
                     (player.Status == 'Idle'    and sCode == 'status:idling'));
        end
    end

    if bGood ~= nil and smsg == nil and bFlip == true and bNot == true and bErr == false then
        if sCode == 'own' then
            -- NOT_OWN is a special case. Simply flipping the result doesn't always work because
            -- Jeuno and Dynamis while N/A treat it like it's NOT_OWN.
            bGood = (gVars.sRegion == gVars._REGION_STATUS_NOT_OWNED or gVars.sRegion == gVars._REGION_STATUS_NA_NOT_OWNED);
        elseif string.find(sCode,'ja:') == nil then
            -- //ja: doesn't support the NOT_ variant. Flip if not //ja:
            bGood = not bGood;
        end
    end
    return bGood,smsg;
end     -- inline.fCheckInlineOther

--[[
    fCheckInlineJob checks the validity of the passed inline code and then determines
    if the coded condition is true.

    Parameter
        sCode       coded condition to be checked

    Returned
        bGood       was the coded condition met? T/F/nil
        smsg        if an error message occurs, will contain the error message
--]]

function inline.fCheckInlineJob(sCode)
    local player = utilities.SetJob();
    local mj = player.MainJob;
    local sj = player.SubJob;
    local smsg = nil;
    local bGood = nil;
    local bNot = false;

    if sCode == nil then
        return false;
    end

    sCode = string.upper(sCode);

    local i = string.find(sCode,'not_');
    if i ~= nil and i == 1 then
        bNot = true;
        sCode = string.sub(sCode,5,-1);     -- remove the "NOT_"
    end

    if string.find(sCode,'PJBNM:') ~= nil then
        -- Determine if any player (but not me) in the party has the specified job.
        -- Note: inversion not supported
        bGood = (utilities.fCheckPartyJob(string.sub(sCode,7,-1),true));
    else        -- These codes support negation
        if sCode == 'MMJ' then
            --  Player has a main job that can do magic
            bGood = string.find(gVars._sMagicJobs,mj);
        elseif sCode == 'MSJ' then
            --  Player has a sub job that can do magic
            bGood = string.find(gVars._sMagicJobs,sj);
        elseif string.sub(sCode,1,3) == 'MJ:' then
            -- Player's main job matches one of the listed jobs
            bGood = (string.find(string.sub(sCode,4,-1),mj) ~= nil);
        elseif string.sub(sCode,1,3) == 'SJ:' then
            -- Player's sub job matches one of the listed jobs
            bGood = (string.find(string.sub(sCode,4,-1),sj) ~= nil);
        elseif string.find(sCode,'PJB:') ~= nil then
            -- Determine if any player in the party has the specified job
            bGood = (utilities.fCheckPartyJob(string.sub(sCode,5,-1),false));
        end

        if bGood ~= nil and bNot == true then
            bGood = not bGood;
        end
    end

    return bGood,smsg;
end     -- inline.fCheckInlineJob

--[[
    fCheckInlineTime checks the validity of the passed inline code and then determines
    if the coded condition is true.

    Parameter
        sCode       coded condition to be checked

    Returned
        bGood       was the coded condition met? T/F/nil
        smsg        if an error message occurs, will contain the error message
--]]

function inline.fCheckInlineTime(sCode)
    local player = utilities.SetJob();
    local timestamp = gData.GetTimestamp();
    local bGood = nil;
    local bNot = false;
    local smsg = nil;

    if sCode == nil then
        return false;
    end

    sCode = string.upper(sCode);

    local i = string.find(sCode,'NOT_');
    if i ~= nil and i == 1 then
        bNot = true;
        sCode = string.sub(sCode,5,-1); -- remove the "not_"
    end

    if string.find(sCode,'TIME:') ~= nil then
        local tr = string.sub(sCode,6,-1);
        -- Is the time of day considered to be daytime
        bGood,smsg = utilities.fCheckTime(timestamp.hour,tr);

        if bGood ~= nil and smsg == nil and bNot == true then
            bGood = not bGood;
        end
    end

    return bGood,smsg;
end     -- inline.fCheckInlineTime

--[[
    fCheckInlineToggle checks the validity of the passed inline code and then determines
    if the coded condition is true.

    Parameters
        sCode       coded condition to be checked

    Returned
        bGood       was the coded condition met? T/F/nil
        smsg        if an error message occurs, will contain the error message
--]]

function inline.fCheckInlineToggle(sCode)
    local player = utilities.SetJob();
    local bGood = nil;
    local smsg = nil;
    local bNot = false;
    local bErr = false;

    if sCode == nil then
        return false;
    end

    sCode = string.upper(sCode);

    local i = string.find(sCode,'NOT_');
    if i ~= nil and i == 1 then
        bNot = true;
        sCode = string.sub(sCode,5,-1); -- remove the "not_"
    end

    local dt = string.find(sCode,'DT:');
    if dt ~= nil and dt == 1 then       -- Damage Taken
        if sCode == 'DT:BREATH' then
            bGood = (utilities.fGetCycle(gVars._DT) == 'Breath');
        elseif sCode == 'DT:MAGICAL' then
            bGood = (utilities.fGetCycle(gVars._DT) == 'Magical');
        elseif sCode == 'DT:PHYSICAL' then
            bGood = (utilities.fGetCycle(gVars._DT) == 'Physical');
        end
    elseif sCode == 'ACCURACY' then     -- Accuracy
        if utilities.fGetToggle(gVars._TANK) == true then
            bGood = utilities.fAccEnabled('Tank_Accuracy');
        else
            bGood = utilities.fAccEnabled('Accuracy');
        end
    elseif sCode == 'RACCURACY' then    -- Ranged Accuracy
        if utilities.fGetToggle(gVars._TANK) == true then
            bGood = (gVars.tProgressive['Tank_Ranged_Accuracy']['CurStage'] > 0)
        else
            bGood = (gVars.tProgressive['Ranged_Accuracy']['CurStage'] > 0)
        end
    elseif sCode == 'EVASION' then      -- Evasion
        -- Is 'Evasion' enabled
        bGood = (utilities.fGetToggle(gVars._EVASION) == true);
    elseif sCode == 'IDLE' then         -- Idle
        -- Is 'Idle' enabled
        bGood = (utilities.GetToggle(gVars._IDLE) == true);
    elseif sCode == 'TANK' then         -- Tank
        -- Is 'Tank' enabled
        bGood = (utilities.fGetToggle(gVars._TANK) == true);
    elseif sCode == 'MACC' then         -- Magical accuracy
        -- Is 'Macc' (Magic Accuracy) enabled
        if string.find(gVars._sMagicjobs,player.MainJob) ~= nil or
                string.find(gVars._sMagicjobs,player.SubJob) ~= nil then
            bGood = utilities.fGetToggle(gVars._MACC);
        else
            smsg = 'Warning: //[NOT_]MACC only applicable for magical jobs/subjobs: ' .. gVars._sMagicJobs;
            bGood = false;
            bErr = true;
        end
    elseif sCode == 'WSWAP' then        -- Weapon swap
        -- Is 'WSWAP' (Weapon Swap) enabled
        bGood = utilities.fGetToggle(gVars._WSWAP);
    elseif sCode == 'KITE' then         -- Kite
        -- Is 'Kite' (Kiting) enabled
        bGood = utilities.fGetToggle(gVars._KITE);
    elseif sCode == 'RIDING' then       -- Riding a "chocobo"
        bGood = utilities.fBuffed('CHOCOBO',true);
    elseif sCode == 'SPF' then          -- Show Pull Feedback
        -- Should 'Show Pull Feedback' be displayed
        bGood = utilities.fGetToggle(gVars._SPF);
    elseif sCode == 'TH' then       -- Treasure Hunter
        -- Is Treasure Hunter enabled
        bGood = utilities.fGetToggle(gVars._TH);
    elseif sCode == 'BRD:HORN' then     -- Instrument: Horn
        -- Is Bard's instrument set to a horn
        if player.MainJob == 'BRD' then
            bGood = (utilities.fGetToggle(gVars._INSTRUMENT) == 'Horn');
        else
            smsg = 'Warning: //[NOT_]BRD_HORN is only valid if you\'re a bard';
            bGood = false;
            bErr = true;
        end
    elseif sCode == 'BRD:STRING' then   -- Instrument: String
        -- Is Bard's  instrument set to a string
        if player.MainJob == 'BRD' then
            bGood = (utilities.fGetToggle(gVars._INSTRUMENT) == 'String');
        else
            smsg = 'Warning: //[NOT_]BRD_STRING is only valid if you\'re a bard';
            bGood = false;
            bErr = true;
        end
    elseif sCode == 'BST:AJUG' then     -- Automatic jug pet
        -- Is Beastmaster's 'AJUG' (automatic pet jug selection) enabled
        if player.MainJob == 'BST' then
            bGood = utilities.fGetToggle(gVars._AJUG);
        else
            smsg = 'Warning: //[NOT_]BST_AJUG is only valid if you\'re a beastmaster';
            bGood = false;
            bErr = true;
        end
    elseif sCode == 'BST:DB:BPP' then   -- BST Debuff: blind, poison, paralyze
        -- Is Beastmaster's 'DB:BPP' (debuff:blind,poison,paralyze) enabled
        if player.MainJob == 'BST' then
            bGood = (utilities.fGetToggle(gVars._DB) == 'BPP');
        else
            smsg = 'Warning: //[NOT_]BST_DB_BPP is only valid if you\'re a beastmaster';
            bGood = false;
            bErr = true;
        end
    elseif sCode == 'BST:DB:WSS' then   -- BST Debuff: weight, slow, silence
        -- Is Beastmaster's 'DB:WSS' (debuff:weight,slow,silence) enabled
        if player.MainJob == 'BST' then
            bGood = (utilities.fGetToggle(gVars._DB) == 'WSS');
        else
            smsg = 'Warning: //[NOT_]BST_DB_WSS is only valid if you\'re a beastmaster';
            bGood = false;
            bErr = true;
        end
    elseif sCode == 'SMN:SBP' then      -- Show blood pact
        -- Is Summoner's 'sBP' (Show Blood Pact) enabled
        if player.MainJob == 'SMN' or player.SubJob == 'SMN' then
            bGood = utilities.fGetToggle(gVars._SBP);
        else
            smsg = 'Warning: //[NOT_]SBP is only valid if you\'re a summoner (main or sj)';
            bGood = false;
            bErr = true;
        end
    elseif sCode == 'THF:SS' then       -- Show steal
        -- Is Thief's 'SS' (Show Steal) enabled
        if player.MainJob == 'THF' or player.SubJob == 'THF' then
            bGood = utilities.fGetToggle(gVars._SS);
        else
            smsg = 'Warning: //[NOT_]SBP is only valid if you\'re a thief';
            bGood = false;
            bErr = true;
        end
    elseif string.sub(sCode,1,2) == 'CC' then   -- Custom code
        -- Custom Code - make sure it's a known code
        local bFound = false;
        for i,j in gProfile.CustomConditionals do
            if string.upper(j['Code']) == sCode then
                bFound = true;
                break;
            end
        end

        if bFound == true then
            bGood = utilities.fGetToggle(sCode);
        else
            smsg = 'Warning: //'.. sCode .. ' is an unknown custom code'
            bGood = false;
            bErr = true;
        end
    end

    -- Assuming there's no error and the results need flipping, do so
    if bGood ~= nil and smsg == nil and bNot == true and bErr == false then
        bGood = not bGood;
    end

    return bGood,smsg;
end     -- inline.fCheckInlineToggle

--[[
    fCheckInlineWeaponType checks the validity of the passed inline code and then determines
    if the coded condition is true.

    Parameters
        sCode       coded condition to be checked
        sGear       name of gear code attached to

    Returned
        bGood       was the coded condition met? T/F/nil
        smsg        if an error message occurs, will contain the error message
--]]

function inline.fCheckInlineWeaponType(sCode,sGear)
    local bGood = nil;
    local bNot = false;

    if sCode == nil or sGear == nil then
        return false,nil;
    end

    sCode = string.upper(sCode);

    local i = string.find(sCode,'NOT_');
    if i ~= nil and i == 1 then
        bNot = true;
        sCode = string.sub(sCode,5,-1); -- remove the "not_"
    end

    if table.find(gVars.tWeaponTypes['all'],sCode) ~= nil then
        -- It's a valid weapon type
        if crossjobs.WeaponTypes[sCode] == nil then
            -- missing entries. Load up appropriately
            utilities.GetWeaponsList(sType);
        end
        bGood = (table.find(crossjobs.WeaponTypes[sCode],string.lower(sGear)) ~= nil);
    end

    -- Assuming there's no error and the results need flipping, do so
    if bGood ~= nil and bNot == true then
        bGood = not bGood;
    end

    return bGood,nil;
end     -- inline.fCheckInlineWeaponType

--[[
    fCheckInlineWeather checks the validity of the passed inline code and then determines
    if the coded condition is true.

    Parameter
        sCode       coded condition to be checked

    Returned
        bGood       was the coded condition met? T/F/nil
        smsg        if an error message occurs, will contain the error message
--]]

function inline.fCheckInlineWeather(sCode)
    local environ = gData.GetEnvironment();
    local bGood = nil;
    local bNot = false;
    local smsg = nil;

    if sCode == nil then
        return false,nil;
    end

    local i = string.find(sCode,'NOT_');
    if i ~= nil and i == 1 then
        bNot = true;
        sCode = string.sub(sCode,5,-1); -- remove the "not_"
    end

    -- Now remove the 'WTH:'
    sCode = string.sub(sCode,5,-1);

    -- Make sure passed in code is a valid weather
    local sfCode = utilities.fFormattedWord(sCode,gVars._SLOT_FA);
    if table.find(gData.Constants.Weather,sfCode) == nil then
        if sSetName ~= nil then
            smsg = 'Warning: Unknown weather specified: ' .. sfCode .. ' in ' .. sSetName;
        else
            smsg = 'Warning: Unknown weather specified: ' .. sfCode;
        end
        return false,smsg;
    else
        sCode = string.upper(sCode);
    end

    -- Can't check for equility since environ.Weather may have an 'x2' appended. Treat as a substring
    bGood = (string.find(string.upper(environ.Weather),sCode) ~= nil);

    if bGood ~= nil and bNot == true then
        bGood = not bGood;
    end

   return bGood,smsg;
end     -- inline.fCheckInlineWeather

--[[
    fCheckInlineMagicType checks the validity of the passed inline code and then determines
    if the coded magic type is what the current spell is part of.

    Parameter
        sCode       coded condition to be checked

    Returned
        bGood       was the coded condition met? T/F/nil
        smsg        if an error message occurs, will contain the error message
--]]

function inline.fCheckInlineMagicType(sCode)
    local spell = gData.GetAction();
    local ti = gData.GetTargetIndex();
    local target = gData.GetEntity(ti);
    local bGood = nil;
    local bNot = false;
    local smsg = nil;
    local tTypes = {
        'MT:BLUE','MT:DARK','MT:DIVINE','MT:ELEMENTAL','MT:ENFEEBLING','MT:ENHANCING','MT:HEALING',
        'MT:NINJUTSU','MT:OFFENSIVE_HEALING','MT:SINGING','MT:SUMMONING', 'NOT_MT:BLUE','NOT_MT:DARK',
        'NOT_MT:DIVINE','NOT_MT:ELEMENTAL','NOT_MT:ENFEEBLING','NOT_MT:ENHANCING','NOT_MT:HEALING',
        'NOT_MT:NINJUTSU','NOT_MT:OFFENSIVE_HEALING','NOT_MT:SINGING','NOT_MT:SUMMONING'
    };
    local tMagic = {
        'Blue Magic','Dark Magic','Divine Magic','Elemental Magic','Enfeebling Magic','Enhancing Magic',
        'Healing Magic','Ninjutsu','Singing','Summoning'
    };

    if sCode == nil then
        return false,nil;
    end

    sCode = string.upper(sCode);

    -- Make sure passed in code wants to check for a magic type
    if table.find(tTypes,sCode) == nil then
        if string.find(sCode,'MT:') == nil and string.find(sCode,'NOT_MT:') == nil then
            -- Not a magic type check
            return nil,nil;
        end
    end

    -- Make sure there's a spell
    if spell == nil or spell.Name == nil then
        -- This is an error since we already know the player has requested a magic type check
        smsg = 'Warning: magic type check of //' .. sCode .. ' encountered where no spell/song was cast';
        return false,smsg;
    end

    if table.find(tMagic,spell.Name) == nil then
        -- Unrecognized type of magic
        smsg = 'Warning: unrecognized type of magic - ' .. spell.Name .. ' when testing for //' .. sCode;
        return false,smsg;
    end

    -- See if NOT_ present and note it
    local i = string.find(sCode,'NOT_');
    if i ~= nil and i == 1 then
        bNot = true;
        sCode = string.sub(sCode,5,-1); -- remove the "not_"
    end

    -- See if pairing found
    if ((sCode == 'MT:BLUE' and spell.Skill == 'Blue Magic') or
        (sCode == 'MT:DARK' and spell.Skill == 'Dark Magic') or
        (sCode == 'MT:DIVINE' and spell.Skill == 'Divine Magic') or
        (sCode == 'MT:ELEMENTAL' and spell.Skill == 'Elemental Magic') or
        (sCode == 'MT:ENFEEBLING' and spell.Skill == 'Enfeebling Magic') or
        (sCode == 'MT:ENHANCING' and spell.Skill == 'Enhancing Magic') or
        (sCode == 'MT:HEALING' and spell.Skill == 'Healing Magic' and target ~= nil and target.Type ~= 'Monster') or
        (sCode == 'MT:NINJUTSU' and spell.Skill == 'Ninjutsu') or
        (sCode == 'MT:OFFENSIVE_HEALING' and spell.Skill == 'Healing Magic' and target ~= nil and target.Type == 'Monster') or
        (sCode == 'MT:SINGING' and spell.Skill == 'Singing') or
        (sCode == 'MT:SUMMONING' and spell.Skill == 'Summoning')) then
        bGood = true;
    else
        bGood = false;
    end

    if bGood ~= nil and bNot == true then
        if sCode ~= 'MT:OFFENSIVE_HEALING' then
            bGood = not bGood;
        else
            smsg = 'Warning: //MT:NOT_OFFENSIVE_HEALING isn\'s valid. Use //MT:HEALING instead';
            bGood = false;
        end
    end

    return bGood,smsg;
end     -- inline.fCheckInlineMagicType

--[[
    fCheckInlineTrackingType checks the validity of the passed inline code and then determines
    if the coded "type" matches what's set in a system setting variable.

        Parameter
            sCode       coded to be checked

        Returned
            bGood       was the coded correct? T/F/nil
            smsg        if an error message occurs, will contain the error message
--]]

function inline.fCheckInlineTrackingType(sCode)
    local smsg;
    local tWS_Codes = { 'WS_CHR','WS_DEX','WS_DEXAGI','WS_DEXCHR','WS_DEXINT',
        'WS_INT','WS_INTMND','WS_MND','WS_RANGED_AGI','WS_RANGED_STRAGI','WS_STR',
        'WS_STRAGI','WS_STRDEX','WS_STRINT','WS_STRINT_30_20','WS_STRMND',
        'WS_STRMND_30_50','WS_STRVIT','WS_SKILL','WS_HP' };

    sCode = string.upper(sCode);
    if string.find(sCode,'WS_') ~= nil then
        -- This is a weaponskill code
        if table.find(tWS_CODES,sCode) ~= nil then
            bGood = (gProfile.system_settings.WSTypeName == sCode);
        else
            smsg = 'Warning: ' .. sCode .. ' is not a valid weaponskill type';
            bGood = false;
        end
    end

    return bGood,smsg;
end     -- inline.fCheckInlineTrackingType

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
        sSetName    is the name of the gear set where the gear is being processed

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

function inline.fCheckInline(gear,sSlot,ts,bLeft,bValidate,sSetName)
    local iPos,i,suCode,sGear;
    local suCodeTbl = { };
    local bSubset;
    local smsg = nil;
    local bGood = nil;

    if gear == nil then
        return false,nil,nil;
    end

    if ts == nil then
        ts = crossjobs.Sets.CurrentGear;     -- Assume currently being built gear set
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
        return true,gear;                 -- No conditionals mean the check returns true
    end

    sSlot   = string.lower(sSlot);
    sGear   = string.sub(gear,1,iPos-1);
    bSubset = (string.lower(sGear) == 'subset');

    -- Make a table of the inline conditionals, for processing
    local sCodeString = string.sub(gear,iPos+2,-1);
    suCodeTbl = utilities.fSplitStringByDelimiter(sCodeString,'//');

    -- Now walk that table, processing the conditionals. At any time if a conditional
    -- comes back false, we're done. Conditionals are strung together and all have to
    -- be true for the results to be true.
    for _,suCode in pairs(suCodeTbl) do
        -- Start with Buffs
        bGood,smsg = inline.fCheckInlineBuff(suCode);

        if bGood == nil then
            -- Then Tracking Type
            bGood,smsg = inline.fCheckInlineTrackingType(suCode);
        end

        if bGood == nil then
            -- Next, Debuffs.
            bGood,smsg = inline.fCheckInlineDebuff(suCode);
        end

        if bGood == nil then
            -- Then Conditionals
            bGood,smsg = inline.fCheckInlineConditional(suCode,sSetName);
        end

        if bGood == nil then
            -- Then Day
            bGood,smsg = inline.fCheckInlineDay(suCode);
        end

        if bGood == nil then
            -- Then Moon
            bGood,smsg = inline.fCheckInlineMoon(suCode);
        end

        if bGood == nil then
            -- Then conditional Gear
            bGood,smsg = inline.fCheckInlineGear(suCode,sSlot,ts,sGear);
        end

        if bGood == nil then
            -- Then Slots
            bGood,smsg = inline.fCheckInlineSlot(suCode,sSlot,ts,bSubset);
        end

        if bGood == nil then
            -- Then Songs
            bGood,smsg = inline.fCheckInlineSongs(suCode);
        end

        if bGood == nil then
            -- Then Other
            bGood,smsg = inline.fCheckInlineOther(suCode,sGear);
        end

        if bGood == nil then
            -- Then Job
            bGood,smsg = inline.fCheckInlineJob(suCode);
        end

        if bGood == nil then
            -- Then Target
            bGood,smsg = inline.fCheckInlineTarget(suCode);
        end

        if bGood == nil then
            -- Then Pet
            bGood,smsg = inline.fCheckInlinePet(suCode);
        end

        if bGood == nil then
            -- Then Time
            bGood,smsg = inline.fCheckInlineTime(suCode);
        end

        if bGood == nil then
            -- Then Toggles
            bGood,smsg = inline.fCheckInlineToggle(suCode);
        end

        if bGood == nil then
            -- Then Weapon Type
            bGood,smsg = inline.fCheckInlineWeaponType(suCode,sGear);
        end

        if bGood == nil then
            -- Then Weather
            bGood,smsg = inline.fCheckInlineWeather(suCode);
        end

        if bGood == nil then
            -- Then Magic Type
            bGood,smsg = inline.fCheckInlineMagicType(suCode);
        end

        -- If an error occurred or the conditional wasn't recognized, then the results
        -- of this check function are false. If this is a validation run though, you
        -- want to keep processing. The success/failure of the inline conditional's
        -- result isn't the point, you just want to know if the conditionals are valid.

        if bGood == nil then
            smsg = 'Warning: Unrecognized Conditional: ' .. suCode;
            bGood = false;
        end

        if bGood == false and smsg ~= nil and bValidate == true then
            if sSetName ~= nil then
                smsg = smsg .. ' in ' .. sSetName;
            end
            reporting.DisplayMessage(nil,smsg);         -- !!! Need to change in future for output to file: DisplayMessage(pFile,msg,fmsg)
        end

        -- Now, if the code was in error and this was not a validation pass,
        -- processing is done and a false result must be returned. However,
        -- if this is a validation pass, you've already displayed the problem
        -- and need to continue processing the conditionals.
        if bValidate == false then
            return bGood,sGear;
        end
    end

    -- By getting to this point, the conditional(s) have to be true or the validation pass is complete
    return bGood,sGear;
end     -- inline.fCheckInline

return inline;
