local gear = {};

--[[
    This component contains all functions associated with gear

    List of routines-
        Subroutines:
            CheckForExceptions              Make sure gear that has to remain in place aren't replaced
            EquipItem                       /ei command, equips piece of gear
            EquipTheGear                    Cleans up dynamic gear set and equips the gear
            GearCheck                       Extracts all gear in all gear sets and processes
            MoveToDynamicGS                 Process gear set and place in target gear set
            ProcessGC                       Processes and coordinates the /gc invocation
            ProcessGS                       Processes the specified crafting/gathering gear set
            TallyProgressiveCaps            Determines how many stages are in progressive categories

        Functions:
            fCheckForEleGear                Determines if you have the right elemental gear
            fCheckForElementalGearByValue   Determines if an elemental gear should be equipped
            fExpandGearLine                 Expands slot's definition from subsets and references
            fGearCheckItem                  Processes and places the item in the dynamic listing
            fGetSlotDefinition              Retrieves a slot's definition from a gear set
            fHasGCBeenRun                   Determines if /gc has been run
            fParseDescription               Determines HP/MP breakdown on items
            fParseDescriptionExceptions     Determines HP/MP breakdown of specific passed items
            fSwapToStave                    Determines if swapping to a staff makes sense
            fTallyGear                      Determines if the HP/MP breakdown of the current gear
            fValidateSpecial                Determines if special condition of passed gear piece met
--]]

-- List of items that inhibit more than the obvious gear slot. Add entries as you
-- need to account for the gear you use. Please note that ears and rings are
-- not supported. Instead, you have to be explicit (eg. ring1, ring2, ear1, ear2)
gear.tMultiSlot = {
    { ['item'] = 'Vermillion Cloak', ['slot'] = 'Body', ['affected'] = 'Head' },
    { ['item'] = 'Royal Cloak', 	 ['slot'] = 'Body', ['affected'] = 'Head' },
    { ['item'] = 'Mandra. Suit',	 ['slot'] = 'Body', ['affected'] = 'Hands,Legs,Feet' },
    { ['item'] = 'Taru. Shorts',	 ['slot'] = 'Legs', ['affected'] = 'Feet' },
    { ['item'] = 'Taru. Shorts +1',  ['slot'] = 'Legs', ['affected'] = 'Feet' },
    { ['item'] = 'Tarutaru Top',	 ['slot'] = 'Body', ['affected'] = 'Hands' },
    { ['item'] = 'Tarutaru Top +1',  ['slot'] = 'Body', ['affected'] = 'Hands' },
    { ['item'] = 'Wonder Top',		 ['slot'] = 'Body', ['affected'] = 'Hands' },
    { ['item'] = 'Wonder Top +1',  	 ['slot'] = 'Body', ['affected'] = 'Hands' },
    { ['item'] = 'Goblin Suit',      ['slot'] = 'Body', ['affected'] = 'Hands,Feet' },
};

-- List of shortcut items that can be equipped with the /equipit or /ei command
gear.tEquipIt = {
    ['emp']    = { ['Name'] = 'Empress Band', ['Slot'] = 'Ring' },
    ['cha']    = { ['Name'] = 'Chariot Band', ['Slot'] = 'Ring' },
    ['empo']   = { ['Name'] = 'Emperor Band', ['Slot'] = 'Ring' },
    ['ann']    = { ['Name'] = 'Anniversary Ring', ['Slot'] = 'Ring' },
    ['dem']    = { ['Name'] = 'Dem Ring', ['Slot'] = 'Ring' },
    ['mea']    = { ['Name'] = 'Mea Ring', ['Slot'] = 'Ring' },
    ['holla']  = { ['Name'] = 'Holla Ring', ['Slot'] = 'Ring' },
    ['altep']  = { ['Name'] = 'Altep Ring', ['Slot'] = 'Ring' },
    ['yhoat']  = { ['Name'] = 'Yhoat Ring', ['Slot'] = 'Ring' },
    ['vahzl']  = { ['Name'] = 'Vahzl Ring', ['Slot'] = 'Ring' },
    ['home']   = { ['Name'] = 'Homing Ring', ['Slot'] = 'Ring' },
    ['ret']    = { ['Name'] = 'Return Ring', ['Slot'] = 'Ring' },
    ['tav']    = { ['Name'] = 'Tavnazian Ring', ['Slot'] = 'Ring' },
    ['tin']	   = { ['Name'] = 'Tinfoil Hat', ['Slot'] = 'Head' },
    ['dcl']    = { ['Name'] = 'Dcl.Grd. Ring', ['Slot'] = 'Ring' },
    ['warp']   = { ['Name'] = 'Warp Cudgel', ['Slot'] = 'Main' },
    ['trick2'] = { ['Name'] = 'Trick Staff II', ['Slot'] = 'Main' },
    ['treat2'] = { ['Name'] = 'Treat Staff II', ['Slot'] = 'Main' },
    ['purgo']  = { ['Name'] = 'Wonder Top +1', ['Slot'] = 'Body' },
    ['rre']    = { ['Name'] = 'Reraise Earring', ['Slot'] = 'Ear' },
    ['rrg']    = { ['Name'] = 'Reraise Gorget', ['Slot'] = 'Neck' },
    ['rrh']    = { ['Name'] = 'Reraise Hairpin', ['Slot'] = 'Head' },
    ['mandy']  = { ['Name'] = 'Mandra. Suit', ['Slot'] = 'Body' },
    ['gob']    = { ['Name'] = 'Goblin Suit', ['Slot'] = 'Body' },
};

-- Temporary gear definition. (Probably will be replaced.)
gear.tGearLine = {};

--[[
    fHasGCBeenRun Determines if the /gc command has been run or not
--]]
function gear.fHasGCBeenRun()

    return (gVars.bGC ~= nil and gVars.bGC == true);
end     -- gear.fHasGCBeenRun

--[[
    ProcessGC parses the passed parameters and coordinates the invocation of the /gc command

    Parameter
        args        Passed argument list

    Form: /gc [list] [file[=name]\][+] [visible|invisible]

    Note: The parameters are position independent. This means they can be in any order.
    "file" designates that the output should be written to a file in the \reports
    directory. If a name is specified, that's the name of the report file. (No name
    will generate a report name based on player's name, job, and the date.) The "+"
    indicates that if the report file already exists, the output should be appended
    to it. (The absense of a "+" will indicate an existing file should be overwritten.)
    Inclusion of a "+" without designating a file has no meaning.
--]]

function gear.ProcessGC(args)
    local bList = false;
    local bFile = false;
    local sFile;
    local fptr = nil;
    local bAppend = false;
    local bWarn = false;

    if utilities.fCheckVisibility(gVars._GC,args) == false then
        for _,j in pairs(args) do
            j = string.lower(j);
            if j ~= 'gc' then
                if j == 'list' then
                    bList = true;
                elseif (j == '+') then
                    bAppend = true;
                elseif string.find(j,'file') ~= nil then
                    bFile = true;
                    sFile,bAppend = utilities.fParseFileDesignation(j);
                    if sFile == nil then
                        bWarn = true;
                    end
                else
                    print(chat.message('Warning: Unrecognized /gc option: ' .. j));
                    bWarn = true;
                end
            end
        end

        if bWarn == true then
            print(chat.message('Info: /gc [list] [file[=name]][+]'));
            print(chat.message('Info: Please fix and resubmit'));
        else
            if sFile ~= nil then
                fptr = utilities.fOpenByFilename(sFile,bAppend);
                if fptr == nil then
                    print(chat.message('Warning: Unable to open file: ' .. sFile));
                    print(chat.message('/gc output redirected to screen'));
                end
            end

            if bList == true then
                reporting.GearCheckList(fptr);
            else
                gear.GearCheck(fptr);
                gVars.bGC = true;
            end

            if fptr ~= nil then
                if bAppend == true then
                    print(chat.message('Info: /GC output appended to '.. sFile));
                else
                    print(chat.message('Info: /GC output written to '.. sFile));
                end
                io.close(fptr);
            end
        end
    end
end     -- gear.ProcessGC

--[[
    ProcessGS processes the specified gear set

    Pararameter
        args		Passed argument list

    A general routine for any gear set.

    Note: Unlike prior versions of Luashitacast this version has an explicit listing
    for all gathering and crafting set types. As such, there's no special calls needed
    for these sets. (What was done previously was a kludge to get the right gear loaded.)
    You just need to explicitly identify the set. That means you need to know the
    appropriate acronym:

        Gathering - HELM, DIG, CLAM, and FISH
        Crafting - ALC, BONE, CLOTH, COOK, GSM, LTH, BSM, and WW

    Invocation: /gs "name" [w][l]
--]]

function gear.ProcessGS(args)
    local bOverride = false;
    local bIgnoreLocks = false;
    local sArg;

    if #args > 1 then
        for i,j in pairs(args) do
            j = string.lower(j);
            if j == 'w' then            -- indicates weaponswapping is permitted
                bOverride = true;
            elseif j == 'l' then        -- indicates locks should be ignored
                bIgnoreLocks = true;
            elseif string.find('gs,gearset',j) == nil then  -- has to be the name of the gearset
                sArg = j;
            end
        end

        if sArg ~= nil then
            local tTable = utilities.fGetTableByName(sArg);	-- Change string to table
            if tTable ~= nil then
                gear.MoveToDynamicGS(tTable,crossjobs.Sets.CurrentGear,false,sArg);
            else
                print(chat.message('Warning: Gear set not found: ' .. sArg));
                return;
            end

            gear.EquipTheGear(crossjobs.Sets.CurrentGear,bOverride,bIgnoreLocks);

            -- Lock the appropriate slots
            if gProfile.settings.bLockAll == true then
                -- predefined setting indicating to lock all slots. Tends to be used when
                -- doing gathering or crafting, but can apply to any set
                locks.LockUnlock(gVars._LOCK,'all');
            else
                locks.LockByGearSet(crossjobs.Sets.CurrentGear,nil,bDisplay)
            end

            if gProfile.settings.bConfirmation == true then
                print(chat.message('Info: Gear set ' .. sArg .. 'has been equipped'));
            end
        else
            print(chat.message('Warning: No set specified for /gearset. Command ignored.'));
        end
    else
        print(chat.message('Warning: No set specified for /gearset. Command ignored.'));
    end
end		-- gear.ProcessGS

--[[
    MoveToDynamicGS copies the gear defined in the passed set to current dynamic
    master set. Nothing is displayed, this is just a transfer routine. The
    passed set is processed and the appropriate gear piece placed into the
    dynamic master set at each evaluated slot.

    Parameters:
        tSet            the set to process
        tMaster         the destination dynamic set
        bIgnoreWSWAP    indicates if WSWAP setting should be ignored
        sSetname        identifies the name of tSet

    Note:
        The order of processing is as follows:
            - Process all subsets of the current level
            - Process all groups
            - Process the rest

        The "current level" refers to the depth in the gear set that's currently
        being processed. Normally gear sets have only a singular level, but with
        the introduction of "Group"ing, deeper levels are possible. A "Group" is
        considered it's own entity and as such is treated like a gear set within
        the gear set. So, if a group is encountered and the conditional is true,
        the process order will be run in it's entirity on that group.
--]]

function gear.MoveToDynamicGS(tSet,tMaster,bIgnoreWSWAP,sSetname)
    local player = gData.GetPlayer();
    local item = {};
    local ref = {};
    local ts = {};
    local ts1 = {};
    local ts2 = {};
    local root,sK,vRoot,stK,sRoot,tAffected;
    local bContinue,iNum,bGood,bSkip,bG;
    local sGear,bIndexed;

    if tSet == nil or tMaster == nil then       -- missing sets
        return;
    end

    -- bIgnoreWSWAP let's the invoker ignore the check on weapon swapping
    if bIgnoreWSWAP == nil then
        bIgnoreWSWAP = false;
    end

    -- Make sure player's transition between zones is complete and that /gc has been run
    if player.MainJob == nil or player.MainJob == 'NON' or gear.fHasGCBeenRun() == false then
        return;
    end

    -- Make sure that the passed in set is a table and not just a name.
    -- (This occurs when a subset is being processed.)
    if type(tSet) == 'string' then
        ts1 = utilities.fGetTableByName(tSet);
    else
        ts1 = tSet;
    end

    if ts1 == nil then
        return;
    end

    -- I've decided that the procedure for handling Subsets should be more "explicit"
    -- due to some of the complexities that Subsets can handle.

    -- First walk through the gear set looking for "subset" on the current level
    for k,v in pairs(ts1) do
        sK = string.lower(k);

        if string.find(sK,'subset') ~= nil then
            -- See if subset has an inline conditional on it and whether it's valid
            bGood,sGear = inline.fCheckInline(sK,'subset',tMaster,true,false,sSetname);
            if bGood == true then
                if type(v) == 'string' then
                    bGood,vRoot = inline.fCheckInline(v,'subset',tMaster,false,false,sSetname);
                    if bGood == true then
                        gear.MoveToDynamicGS(vRoot,tMaster,bIgnoreWSWAP,sSetname);
                    end
                else
                    bIndexed = (v[1] ~= nil);
                    -- Walk the table
                    for kk,vv in pairs(v) do
                        if type(vv) == 'string' then
                            bGood,vRoot = inline.fCheckInline(vv,'subset',tMaster,false,false,sSetname);
                            if bGood == true then
                                gear.MoveToDynamicGS(vRoot,tMaster,bIgnoreWSWAP,sSetname);
                                if bIndexed == false then
                                    break;
                                end
                            end
                        else -- Assume an unindexed table
                            for kkk,vvv in pairs(vv) do
                                bGood,vRoot = inline.fCheckInline(vvv,'subset',tMaster,false,false,sSetname);
                                if bGood == true then
                                    gear.MoveToDynamicGS(vRoot,tMaster,bIgnoreWSWAP,sSetname);
                                    break;
                                end
                            end
                        end
                    end
                end
            end
        end
    end

    -- Then walk through the gear set looking for "group"s. Unlike Subsets grouping is just
    -- a mechanism for bunching a definition together. There's no need to worry about a
    -- singular string or the difference between an indexed table and non-indexed table.
    for k,v in pairs(ts1) do
        sK = string.lower(k);

        if string.find(sK,'group') ~= nil then
            -- See if the group has an inline conditional on it and whether it's valid
            bGood,sGear = inline.fCheckInline(sK,'group',tMaster,true,false,sSetname);
            if bGood == true then
                -- Since the conditional is true, or there's no conditional, recurse on the
                -- enclosed gear set definition. (A group definition requires a table. That's
                -- what needs to be processed here.)
                gear.MoveToDynamicGS(v,tMaster,bIgnoreWSWAP,sSetname);
            end
        end
    end

    -- Lastly, walk through gear set ignoring "subset" and "group"
    for k,v in pairs(ts1) do
        bContinue = false;
        sK = string.lower(k);

        -- Make sure to ignore subsets and groups
        if string.find(sK,'subset') == nil and  string.find(sK,'group') == nil then
            local sTmp = utilities.fRemoveConditional(sK);
            -- Check for special case: Ears and Rings
            if string.find('ears,rings',sTmp) ~= nil then
                root = string.sub(sTmp,1,-2);
                iNum = 1;
                bContinue = true;
            end

            -- If the slot to be populated is one that will reset the player's TP,
            -- check to see if the swap should be done anyway
            if string.find('main,sub,range',sTmp) ~= nil then
                bSkip = not (utilities.fGetToggle(gVars._WSWAP) == true or
                             bIgnoreWSWAP == true);
            else
                bSkip = false;
            end

            -- Now, since the slot is dealt with, process the gear
            if bSkip == false then
                ts = {};
                -- Make sure the piece to be processed is a table
                if type(v) == 'table' then
                    ts = v;
                else
                    ts[k] = v;
                end

                iNum = 1;

                -- Expand out external gear set slot definition (if present)
                local tsb = {};
                if utilities.fReferenceCheck(ts) == true then
                    table.clear(gear.tGearLine);
                    if gear.fExpandGearLine(sK,ts) == true then
                        tsb = gear.tGearLine;
                    else
                        tsb = nil;	-- Erroneous inline reference: missing slot or ts
                    end
                else
                    tsb = ts;
                end

                -- Walk list of items
                for kk,vv in pairs(tsb) do
                    -- Make sure the item is noted in gear.GearDetails
                    -- and that the level, job, and accessibility is good
                    bG,ref = fGearCheckItem(sK,vv,false,nil);
                    if bG == true then
                        -- See if there's an inline conditional to be checked.
                        -- Note the need to distinguish which "ear" or "ring"
                        if bContinue then
                            stK = root .. tostring(iNum);
                        else
                            stK = sTmp;
                        end

                        bGood,vRoot = inline.fCheckInline(vv,stK,tMaster,false,false,sSetname);

                        -- If the inline check returns true, process the gear piece
                        if bGood == true then
                            if bContinue == true then
                                stK = utilities.fFormattedWord(root .. tostring(iNum),gVars._SLOT_FA);
                                tMaster[stK] = vRoot;
                                iNum = iNum + 1;
                            else
                                -- Normal single slot
                                stK = utilities.fFormattedWord(stK,gVars._SLOT_FA);
                                tMaster[stK] = vRoot;
                                break;
                            end
                        end
                    end

                    -- When iNum > 2, all special slots of "root" populated
                    if iNum > 2 then
                        break;
                    end
                end
            end
        end
    end
end     -- gear.MoveToDynamicGS

--[[
    CheckFillInEmpties determines if there's an artificial level cap involved and if any slots
    not designated in the set to equip should be set to 'empty' so that pieces not overriden
    in the current set will violate the level cap.

    Note: for some reason it unequips the wrong slot. With cap 20, back should be removed.
    Instead, earring 2 is being removed. Will revisit
--]]

function CheckFillInEmpties(tSet)
    local current = gData.GetCurrentSet();

    if gProfile.settings.PlayerCappedLevel == 0 then
        return
    else
        -- Use lock list for complete list of slots
        for i,j in ipairs(locks.tSlotLocks) do
            local slot = utilities.fFormattedWord(j['slot'],gVars._SLOT_FA);
            if (tSet[slot] == nil or tSet[slot] == '') and current[slot] ~= nil then
                local bGood,ref = fGearCheckItem(slot,current[slot],false,nil);
                if bGood == false then
                    print(i,j[slot],'--');
                    gEquip.UnequipSlot(i);
                end
            end
        end
    end
end     -- CheckFillInEmpties

--[[
    EquipTheGear makes sure that the passed gear set doesn't have an item in a slot
    that is being blocked by another item (e.g., no head gear if a vermillion cloak
    is in the body slot.) It also makes sure the It the equips the passed gear set
    isn't going to override locks unless told to ignore locks. (This function use
    to be found in MoveToCurrent which has been replaced by MoveToDynamicGS.)

    Parameters
        tSet                Name of gear set to equip
        bOverride           T/F, indicates if WSWAP is to be ignored
        bIgnoreLocks        T/F, indicates if locks are to be ignored

    Note: This version supports both locks and multislotted items
--]]

function gear.EquipTheGear(tSet,bOverride,bIgnoreLocks)
    local sSlot,bGood,bMulti,sSlots;
    local iPos,sWhich;

    if tSet == nil then
        return;
    end

    if bOverride == nil then
        bOverride = false;
    end

    if bIgnoreLocks == nil then
        bIgnoreLocks = false;
    end

    -- Deal with multislot items and locks (if told to preserve them)
    for j,k in ipairs(locks.tSlotLocks) do
        local s = utilities.fFormattedWord(k['slot'],gVars._SLOT_FA);
        if not (tSet[s] == nil or tSet[s] == '') then
            -- Item found in that slot, see if the slot is already locked
            if k['lock'] == true and bIgnoreLocks == false then
                -- Locked slot where we're not suppose to ignore locks
                -- Remove the item from the passed in set.
                tSet[s] = '';
            else
                -- No lock issue, so check for multislotted items
                for jj,kk in pairs(gear.tMultiSlot) do
                    if string.lower(tSet[s]) == string.lower(kk['item']) then
                        bGood,bMulti,sSlots = locks.fMultiSlotLockCheck(kk['item']);
                        if not bGood then
                            tSet[s] = '';
                        elseif bMulti then
                            -- The list includes the slot the item is equipped to. Remove
                            -- that and null out the affected slots.
                            while sSlots ~= nil do
                                iPos = string.find(sSlots,',');
                                if iPos ~= nil then
                                    sWhich = string.sub(sSlots,1,iPos-1);
                                    sSlots = string.sub(sSlots,iPos+1,-1);
                                else
                                    sWhich = sSlots;
                                    sSlots = nil;
                                end
                                -- Empty the affected slots
                                if sWhich ~= s then
                                    tSet[sWhich] = '';
                                end
                            end
                        end
                    end
                end
            end
        end
    end

    -- And if weapon swapping is not enabled, clear out the top line (except ammo)
    if not (utilities.fGetToggle(gVars._WSWAP) == true or bOverride == true) then
        tSet['Main']  = '';
        tSet['Sub']   = '';
        tSet['Range'] = '';
     end

--[[
    There's a funky problem that can occur on rings or ears. If the ear/ring item you're
    equipping is already equipped and the slot it's suppose to go to now is not the slot
    that it's currently equipped in, then that slot will be left empty and the item won't
    be equipped. If this is the case, then don't try to move the item. Instead, change
    which earring/ring goes where to match the position that it already occupies.
--]]

    local current = gData.GetCurrentSet();
    if current ~= nil then
        -- First, check ears
        if (tSet['Ear1'] ~= nil and current['Ear2'] ~= nil and tSet['Ear1'] == current['Ear2']) or
            (tSet['Ear2'] ~= nil and current['Ear1'] ~= nil and tSet['Ear2'] == current['Ear1']) then
            local hold = tSet['Ear1'];
        tSet['Ear1'] = tSet['Ear2'];
        tSet['Ear2'] = hold;
        end

        -- Now check rings
        if (tSet['Ring1'] ~= nil and current['Ring2'] ~= nil and tSet['Ring1'] == current['Ring2']) or
            (tSet['Ring2'] ~= nil and current['Ring1'] ~= nil and tSet['Ring2'] == current['Ring1']) then
            local hold = tSet['Ring1'];
            tSet['Ring1'] = tSet['Ring2'];
            tSet['Ring2'] = hold;
        end
    end

    gear.CheckForExceptions(tSet);
    gFunc.ForceEquipSet(tSet);
    --CheckFillInEmpties(tSet);
end			-- gear.EquipTheGear

--[[
    CheckForExceptions makes sure that pieces that must remain in place will remain
    in place before equipping new gear
--]]

function gear.CheckForExceptions(tSet)
    local msg;
    local sList = nil;
    local cgear = gData.GetEquipment();

    if utilities.fBuffed('Enchantment',true) == true then
        -- If 'High Brth. Mantle' enchantment going, keep equipped
        if cgear.Back ~= nil and tSet['Back'] ~= nil then
            if cgear.Back.Name == 'High Brth. Mantle' and tSet['Back'] ~= 'High Brth. Mantle' then
                tSet['Back'] = 'High Brth. Mantle';
                sList = 'High Breath Mantle';
            elseif cgear.Back.Name == 'Breath Mantle' and tSet['Back'] ~= 'Breath Mantle' then
                tSet['Back'] = 'Breath Mantle';
                sList = 'Breath Mantle';
            end
        end

        if cgear.Ring1 ~= nil then
            -- Albatross Ring can be on either finger. If enchant going, keep equipped
            if cear.Ring1.Name == 'Albatross Ring' and tSet['Ring1'] ~= nil and tSet['Ring1'] ~= 'Albatross Ring' then
                tSet['Ring1'] = 'Albatross Ring';
                if sList == nil then
                    sList = 'Albatross Ring';
                else
                    sList = sList .. ',' .. 'Albatross Ring';
                end
            elseif cear.Ring2.Name == 'Albatross Ring' and tSet['Ring2'] ~= nil and tSet['Ring2'] ~= 'Albatross Ring' then
                tSet['Ring2'] = 'Albatross Ring';
                if sList == nil then
                    sList = 'Albatross Ring';
                else
                    sList = sList .. ',' .. 'Albatross Ring';
                end
            end
        end

        if cgear.Main ~= nil  and tSet['Main'] ~= nil then
            -- 'High Mana Wand' and 'Mana Wand' have to be equipped if enchantment going
            if cgear.Main.Name == 'High Mana Wand' and tSet['Main'] ~= 'High Mana Wand' then
                tSet['Main'] = 'High Mana Wand';
                if sList == nil then
                    sList = 'High Mana Wand';
                else
                    sList = sList .. ',' .. 'High Mana Wand';
                end
            elseif cgear.Main.Name == 'Mana Wand' and tSet['Main'] ~= 'Mana Wand' then
                tSet['Main'] = 'Mana Wand';
                if sList == nil then
                    sList = 'Mana Wand';
                else
                    sList = sList .. ',' .. 'Mana Wand';
                end
            end
        end

        if sList ~= nil then
            msg = 'Because of enchantment, the following must be equipped: ' .. sList;
            reporting.DisplayOnce(msg,false);
        end
    end
end		-- gear.CheckForExceptions

--[[
    fParseDescription parses the passed description for the stated item
    looking for HP/HPP/MP/MPP/CHPMP/CMPHP and nation control information
    if appropriate. The record is returned.

    Parameters
        item        Name of item to parse
        sDesc       Description attached to item

    Returned:
        rec         The record structure that's tallying HP/MP details
--]]

    function fParseDescription(item,sDesc)
        local bFound,ic,sType,ipos,ival,bPct;
        local rec = {   ['own'] = { ['ctrl'] = nil, ['HP'] = 0, ['HPP'] = 0,
                        ['MP'] = 0, ['MPP'] = 0, ['cHM'] = 0, ['cMH'] = 0 },
                        ['HP'] = 0, ['HPP'] = 0, ['MP'] = 0, ['MPP'] = 0,
                        ['cHM'] = 0, ['cMH'] = 0 };

        if item == nil or sDesc == nil then
            return rec;
        end

        item = string.lower(item);

        bFound,rec = fParseDescriptionExceptions(rec,item,sDesc);
        if bFound == true then
            return rec;
        end

        -- Look for nation control settings
        if string.find(sDesc,'under own') ~= nil then
            rec['own']['ctrl'] = 'T';
        elseif string.find(sDesc,'outside own') ~= nil then
            rec['own']['ctrl'] = 'F';
        end

        -- Then conversions
        ic = string.find(sDesc,'Converts');
        if ic ~= nil then
            ival = tonumber(string.match(string.sub(sDesc,ic), "%d+"));
            if string.find(sDesc,' HP to MP') ~= nil then
                sType = 'cHM';
            else
                sType = 'cMH';
            end
            if rec['own']['ctrl'] ~= nil then
                rec['own'][sType] = ival;
            else
                rec[sType] = ival;
            end
        end

        -- See if it's HP%
        ipos = string.find(sDesc,'HP[%+%-]%d+%%');
        if ipos ~= nil then
            ival = tonumber(string.match(string.sub(sDesc,ipos+2), "%d+"));
            if string.find(string.sub(sDesc,ipos+2,ipos+2),'%-') ~= nil then
                ival = 0 - ival;
            end

            if rec['own']['ctrl'] ~= nil then
                rec['own']['HPP'] = ival;
            else
                rec['HPP'] = ival;
            end

            -- remove the "HP" from the desc so that a non-HP% might be found
            local iposp = string.find(sDesc,'%%');
            if ipos == 1 then
                sDesc = string.sub(sDesc,iposp+1,-1);
            else
                sDesc = string.sub(sDesc,1,ipos-1) .. string.sub(sDesc,iposp+1,-1);
            end
        end

        -- See if it's HP
        ipos = string.find(sDesc,'HP[%+%-]%d+');
        if ipos ~= nil then
            ival = tonumber(string.match(string.sub(sDesc,ipos+2), "%d+"));
            if string.find(string.sub(sDesc,ipos+2,ipos+2),'%-') ~= nil then
                ival = 0 - ival;
            end

            if rec['own']['ctrl'] ~= nil then
                rec['own']['HP'] = ival;
            else
                rec['HP'] = ival;
            end
        end

        -- Now use the same type of logic with MP%
        ipos = string.find(sDesc,'MP[%+%-]%d+%%');
        if ipos ~= nil then
            ival = tonumber(string.match(string.sub(sDesc,ipos+2), "%d+"));
            if string.find(string.sub(sDesc,ipos+2,ipos+2),'%-') ~= nil then
                ival = 0 - ival;
            end

            if rec['own']['ctrl'] ~= nil then
                rec['own']['MPP'] = ival;
            else
                rec['MPP'] = ival;
            end

            -- remove the "MP" from the desc so that a non-MP% might be found
            local iposp = string.find(sDesc,'%%');
            if ipos == 1 then
                sDesc = string.sub(sDesc,iposp+1,-1);
            else
                sDesc = string.sub(sDesc,1,ipos-1) .. string.sub(sDesc,iposp+1,-1);
            end
        end

        ipos = string.find(sDesc,'MP[%+%-]%d+');
        if ipos ~= nil then
            ival = tonumber(string.match(string.sub(sDesc,ipos+2), "%d+"));
            if string.find(string.sub(sDesc,ipos+2,ipos+2),'%-') ~= nil then
                ival = 0 - ival;
            end

            if rec['own']['ctrl'] ~= nil then
                rec['own']['MP'] = ival;
            else
                rec['MP'] = ival;
            end
        end
    return rec;
end		-- fParseDescription

--[[
    fParseDescriptionExceptions processes the descriptions for gear
    that requires special processing. It could have been included in
    fParseDescription, but was extracted so that function would not
    be too long.

    Parameters
        rec     The record structure that's tallying HP/MP details
        sGear   Piece of gear being analyzed
        sDesc   Piece of gear's item description

    Returned:
        T/F     Was the item found in the tracked list
        rec     The record structure that's tallying HP/MP details
--]]

function fParseDescriptionExceptions(rec,sGear,sDesc)
    local player = gData.GetPlayer();
    local environ = gData.GetEnvironment();
    local bFound = true;

    sGear = string.lower(sGear);

    if sGear == 't.m. wand +1' then
        rec['own']['ctrl'] = 'F';
        rec['own']['MP'] = 18
        rec['MP'] = 5;
    elseif sGear == 't.m. wand +2' then
        rec['own']['ctrl'] = 'F';
        rec['own']['MP'] = 20
        rec['MP'] = 5;
    elseif sGear == 'ajase beads' then
        rec['HP'] = 20;
    elseif string.find(sGear,'ryl.sqr. robe %+%d') ~= nil then
        rec['MP'] = 10;
    elseif sGear == 'sattva ring' then
        -- variable HP based on player's level
        rec['HP'] = math.floor((player.MainJobSync - 30)/15)*5 + 15;
    elseif sGear == 'tamas ring' then
        -- variable MP based on player's level
        rec['MP'] = math.floor((player.MainJobSync - 30)/15)*5 + 15;
    elseif table.find({'creek boxers +1','creek shorts +1',
        'dune boxers +1','magna shorts +1','marine boxers +1',
        'marine shorts +1','river shorts +1','woodsy boxers +1',
        'woodsy shorts +1'},sGear) ~= nil then
        if string.lower(environ.Weather) == 'sunshine' then
            rec['MP'] = 20;
        end
    elseif table.find({'custom shorts +1','custom trunks +1',
        'elder trunks +1','magna trunks +1','savage shorts +1',
        'wonder shorts +1','wonder trunks +1'},sGear) ~= nil then
        if string.lower(environ.Weather) == 'sunshine' then
            rec['HP'] = 20;
        end
    elseif sGear == 'wyvern perch' then
        -- This one will generate a false positive. The HP gain is
        -- for the Wyvern.
    elseif sGear == 'booster earring' then
        -- another player in the party (besides yourself) must be a BLU
        if utilities.fCheckPartyJob('BLU',true) == true then
            rec['HP'] = 10;
            rec['MP'] = 10;
        end
    elseif sGear == 'ese earring' then
        -- another player in the party (besides yourself) must be a MNK
        if utilities.fCheckPartyJob('MNK',true) == true then
            rec['HP'] = 20;
        end
    elseif sGear == 'multiple ring' then
        -- player level must be evenly divisible by 10
        if math.floor(player.MainJobSync/10) == player.MainJobSync/10 then
            rec['HP'] = 50;
            rec['MP'] = 20;
        end
    elseif sGear == 'diabolos\'s ring' then
        if string.lower(environ.Day) == 'darksday' then
            rec['MPP'] = -15;
        end
    elseif sGear == 'earth ring' then
        if string.lower(environ.Day) == 'earthsday' then
            rec['HPP'] = -15;
        end
    elseif sGear == 'fire ring' then
        if string.lower(environ.Day) == 'firesday' then
            rec['HPP'] = -15;
        end
    elseif sGear == 'ice ring' then
        if string.lower(environ.Day) == 'iceday' then
            rec['MPP'] = -15;
        end
    elseif sGear == 'lightning ring' then
        if string.lower(environ.Day) == 'lightningsday' then
            rec['HPP'] = -15;
        end
    elseif sGear == 'water ring' then
        if string.lower(environ.Day) == 'watersday' then
            rec['MPP'] = -15;
        end
    elseif sGear == 'wind\'s ring' then
        if string.lower(environ.Day) == 'windsday' then
            rec['HPP'] = -15;
        end
    elseif sGear == 'storm mantle' then
        -- for now, the assault aspect, is assumed to be true
        rec['HP'] = 105;
    elseif sGear == 'cougar pendant' then
        -- for now, the assault aspect, is assumed to be true
        rec['HP'] = 230;
    elseif sGear == 'storm earring' then
        -- for now, the assault aspect, is assumed to be true
        rec['MP'] = 15;
    elseif sGear == 'variable ring' then
        -- for now, the garrison aspect, is assumed to be true
        rec['MP'] = 28;
    else
        bFound = false;
    end

    return bFound,rec;
end		-- fParseDescriptionExceptions

--[[
    fTallyGear tallies up all the MP and HP manipulations on the gear
    currently equipped (and the passed gear, minus what is already in
    that slot), both visible and invisible.

        sGear   Piece of gear being analyzed
        sSlot   The affected slot

    Returned:
        rec     Structure containing the breakdown of the tally
--]]

function fTallyGear(sGear,sSlot)
    local cur = gData.GetEquipment();
    local sPiece,lcii,sVis,bGood;
    local item = {};
    local ref = {};
    local rec = { 		-- define tracking structure
        ['visible'] = {
            ['MP'] = 0, ['MPP'] = 0, ['HP'] = 0, ['HPP'] = 0,
            ['cHM'] = 0, ['cMH'] = 0
        },
        ['invisible'] = {
            ['MP'] = 0, ['MPP'] = 0, ['HP'] = 0, ['HPP'] = 0,
            ['cHM'] = 0, ['cMH'] = 0
        }
    };

    if sGear == nil or sSlot == nil then
        return nil;
    end

    sGear = string.lower(sGear);
    sSlot = string.lower(sSlot);

    -- loop through the current gear, tallying up totals
    for ii,jj in pairs(cur) do
        -- when dealing with rings and earrings, use the grouping mechanism.
        lcii = string.lower(ii);
        if string.find('ring1,ring2',lcii) ~= nil then
            lcii = 'rings';
        end
        if string.find('ear1,ear2',lcii) ~= nil then
            lcii = 'ears';
        end

        -- There's a special case. If the slot from the currently equipped
        -- gear matches the slot of the passed gear piece, use the passed
        -- in piece. Using the currently equipped one could result in an
        -- erroneous equipment of the gear piece.
        if lcii == sSlot then
            sPiece = sGear;
        else
            sPiece = string.lower(jj.Name);
        end

        if gcinclude.GearDetails[lcii][sPiece] ~= nil and
           gcinclude.GearDetails[lcii][sPiece]['valid'] == true then
            item = fParseDescription(sPiece,gcinclude.GearDetails[lcii][sPiece]['desc']);

            -- Now tally the parsed description, divided between visible and invisible,
            -- accordingly
            if gcinclude.GearDetails[lcii]['vis'] == true then
                sVis = 'visible';
            else
                sVis = 'invisible';
            end

            rec[sVis]['MP'] = rec[sVis]['MP'] + item['MP'];
            rec[sVis]['MPP'] = rec[sVis]['MPP'] + item['MPP'];
            rec[sVis]['HP'] = rec[sVis]['HP'] + item['HP'];
            rec[sVis]['HPP'] = rec[sVis]['HPP'] + item['HPP'];
            rec[sVis]['cHM'] = rec[sVis]['cHM'] + item['cHM'];
            rec[sVis]['cMH'] = rec[sVis]['cMH'] + item['cMH'];

            local bOwn = (utilities.fGetCycle(gVars._REGION) == gVars._REGION_OWNED);

            if (item['own']['ctrl'] == 'T' and bOwn == true) or
               (item['own']['ctrl'] == 'F' and bOwn == false) then
                rec[sVis]['MP'] = rec[sVis]['MP'] + item['own']['MP'];
                rec[sVis]['MPP'] = rec[sVis]['MPP'] + item['own']['MPP'];
                rec[sVis]['HP'] = rec[sVis]['HP'] + item['own']['HP'];
                rec[sVis]['HPP'] = rec[sVis]['HPP'] + item['own']['HPP'];
                rec[sVis]['cHM'] = rec[sVis]['cHM'] + item['own']['cHM'];
                rec[sVis]['cMH'] = rec[sVis]['cMH'] + item['own']['cMH'];
            end
        end
    end

    return rec;
end		-- fTallyGear

--[[
    fValidateSpecial determines if the passed gear's special settings are true

    Parameters
        sSlot       The slot the piece of gear will be placed into
        sGear       The name of the piece of gear being tested

    Returned
        Are the special conditions met
--]]

function gear.fValidateSpecial(sSlot,sGear)
    local player = gData.GetPlayer();
    local rec = {};
    local gear;
    local bGood = false;

    if sSlot == nil then
        return false;
    end

    if sGear == nil then
        return false;
    end

    gear = sGear;
    sGear = string.lower(sGear);
    sSlot = string.lower(sSlot);

    rec = fTallyGear(sGear,sSlot);
    if rec == nil then
        return false;
    end

    -- Do specific calculations based on the name of the piece of gear
    if sGear == 'uggalepih pendant' then
        -- Condition: MP% < 51. MAB bonus. Only visible gear, ignore all
        -- "Convert HP to MP" check outright first
        if player.MPP < 51 then
            return true;
        else
            if rec['visible'] == nil then
                return false;
            else
                local iMP = player.MP - rec['visible']['cHM'];
                local imMP = player.MaxMP - rec['invisible']['MP'] -
                rec['invisible']['cMH'] - rec['invisible']['cHM'];
                local iaMP = player.MaxMP * (rec['invisible']['MPP'] * 0.01);
                bGood = ((iMP/(imMP - iaMP))*100 < 51);
            end
        end
    elseif sGear == 'parade gorget' then
        -- Make sure player needs to have mp added
        if player.MPP >= gProfile.settings.MPPTolerance then
            return false;
        end

        -- Now, check condition: HP% >= 85. Adds "Refresh". Only visible gear
        -- Let's see if the invisible gear will make a difference
        local iHP = player.MaxHP - rec['invisible']['HP'] - rec['invisible']['cMH'];
        local iaHP = math.floor(rec['invisible']['HP'] * (rec['invisible']['HPP'] * 0.01));
        bGood = ((player.HP/(iHP - iaHP))*100 >= 85);
    elseif sGear == 'sorcerer\'s ring' then
        -- Condition: HP% < 76 and TP% < 100.
        -- Ignore HP+ (flat and percent) and Convert HP to MP/MP to HP gear.

        -- Check outright first
        if player.HPP < 76 and player.TP/10 < 100 then
            return true;
        else
            if rec['visible'] == nil then
                return false;
            else
                local fHP   = rec['visible']['HP'] + rec['invisible']['HP'];
                local fCH_M = (rec['visible']['cHM'] + rec['invisible']['cHM']) -
                        (rec['visible']['cMH'] + rec['invisible']['cMH']);
                local fHPP  = rec['visible']['HPP'] + rec['invisible']['HPP'];
                local tHP   = player.HP - fHP - fCH_M;
                local nHP   = tHP - (tHP * (fHPP * 0.01));

                if ((nHP/player.MaxHP) * 100) < 76 and player.TP/10 < 100 then
                    return true;
                end
            end
        end
    elseif string.find('drake ring,shinobi ring,minstrel\'s ring',sGear) ~= nil then
        if player.HPP <= 75 and player.TP/10 < 100 then
            return true;
        end
    else
        print(chat.header('fValidateSpecial'):append(chat.message('Warning: No special code exists for ' .. gear .. '. Ignoring piece.')));
    end

    return bGood;
end     -- gear.fValidateSpecial

--[[
    fGearCheckItem processes the specific item sent to it and where appropriate, populates
    gear.GearDetails

    Parameters
        sSlot   - Name of the slot
        sName   - Name of the item to check
        bAccess - True = return accessibility, False = check job, access, and level
        gsname  - Name of thee gear set that the item is from

    Returned
        bAccessibility  T/F, is the item accessible
        ref             Reference to the item in tGearDetails

    Future: Just found out to empty a slot you designate 'empty' for the piece of gear.
        Obviously 'empty' will match any level and there's no gear check involved. Have
        to address this exception.

        ** revise re: gs defs for SMG **
--]]

function fGearCheckItem(sSlot,sName,bAccess,gsname)
    local player = gData.GetPlayer();
    local bJob,bAccessible,bSlot
    local lgsname = nil;
    local iPos;
    local item = {};
    local tOwned = {};
    local sCodes = nil;
    local tJobMask = { ['None'] = 0x0, ['WAR'] = 0x2, ['MNK'] = 0x4, ['WHM'] = 0x8, ['BLM'] = 0x10, ['RDM'] = 0x20, ['THF'] = 0x40, ['PLD'] = 0x80, ['DRK'] = 0x100,
        ['BST'] = 0x200, ['BRD'] = 0x400, ['RNG'] = 0x800, ['SAM'] = 0x1000, ['NIN'] = 0x2000, ['DRG'] = 0x4000, ['SMN'] = 0x8000, ['BLU'] = 0x10000, ['COR'] = 0x20000,
        ['PUP'] = 0x40000, ['DNC'] = 0x80000, ['SCH'] = 0x100000, ['GEO'] = 0x200000, ['RUN'] = 0x400000, ['MON'] = 0x800000, ['JOB24'] = 0x1000000,
        ['JOB25'] = 0x2000000, ['JOB26'] = 0x4000000, ['JOB27'] = 0x8000000, ['JOB28'] = 0x10000000, ['JOB29'] = 0x20000000,['JOB30'] = 0x30000000,
        ['JOB31'] = 0x80000000, ['Alljobs'] = 0x007FFFFE };

    -- Required fields
    if sSlot == nil or sName == nil then
        return false,nil;
    end

    -- Make sure the slot and name have had their conditionals removed and are in lowercase
    sSlot = string.lower(utilities.fRemoveConditional(sSlot));
    sName = string.lower(utilities.fRemoveConditional(sName));

    -- Subsets, groups, and inline reference dgear.GearDetailsefinitions are skipped
    if sSlot == 'subset' or sSlot == 'group' or string.find(sName,'::') ~= nil then
        return false,nil;
    end

    if gsname ~= nil then
        lgsname = string.lower(gsname);
    end

    -- Make sure "downloading data" is not in transition
    if player.MainJob == nil or player.MainJob == 'NON' then
        return false,nil;
    end

    -- Assume full check if absent
    if bAccess == nil then
        bAccess = false;
    end

    -- Make sure all ear and ring variants represented by the generic category
    if string.find('ears,ear1,ear2',sSlot) ~= nil then
        sSlot = 'ears';
    elseif string.find('rings,ring1,ring2',sSlot) ~= nil then
        sSlot = 'rings';
    end

    if gear.fHasGCBeenRun() == false then
        -- Since /gc has not happened, create the record
        item = AshitaCore:GetResourceManager():GetItemByName(sName,2);
        if item ~= nil then
            bJob = (bit.band(item.Jobs,tJobMask[player.MainJob]) == tJobMask[player.MainJob]) or
                (bit.band(item.Jobs,tJobMask['Alljobs']) == tJobMask['Alljobs']);
            tOwned = utilities.fCheckItemOwned(item);
            bSlot = utilities.fSlotMatch(sSlot,item.Slots);
            bAccessible = (tOwned['own'] == true and tOwned['accessible'] == true);

            -- Save item w/details
            gVars.tGearDetails[sSlot][sName] = {
                ['id']		   = item.Id;
                ['valid']	   = true,
                ['slot']	   = bSlot,
                ['level']	   = item.Level,
                ['job']        = bJob,
                ['own']		   = tOwned['own'],
                ['accessible'] = bAccessible,
                ['porter']	   = tOwned['porter'],
                ['claim']	   = tOwned['claim'],
                ['locations']  = tOwned['locations'],
                ['desc'] 	   = item.Description[1],
                };

            if bSlot == false then
                gVars.tGearDetails[sSlot][sName]['valid'] = false;
                return false,gVars.tGearDetails[sSlot][sName];
            end

            if bAccessible then
                gVars.tGearDetails[sSlot]['acc'] = gVars.tGearDetails[sSlot]['acc'] + 1;
            end
        else
            gVars.tGearDetails[sSlot][sName] = { ['valid'] = false };
        end
    end

    -- If it still doesn't exist, return that state
    if gVars.tGearDetails[sSlot][sName] == nil then
        return false,nil;
    else
        gVars.tGearDetails[sSlot]['num'] = gVars.tGearDetails[sSlot]['num'] + 1;

        -- See if a record is needed for the gearset tracking table

        -- gsName can be nil if routine called from a list rather than a gear set. Skip it
        if gsName ~= nil then
            if gVars.tGearsetDetails[gsName][sSlot] ==  nil or gVars.tGearsetDetails[gsName][sSlot]['items'] == nil then
                gVars.tGearsetDetails[gsName][sSlot]['items'][1] = gVars.tGearDetails[sSlot][sName];
            else
                if utilities.fIsGearsetDetailsFound(gVars.tGearDetails[sSlot][sName],gVars.tGearsetDetails[gsName][sSlot]['items']) == false then
                    -- Add a new gear piece to the set's list for the current slot
                    gVars.tGearsetDetails[gsName][sSlot]['items'][#gVars.tGearsetDetails+1] = gVars.tGearDetails[sSlot][sName];
                end
            end
        end

        -- Now return the appropriate details
        if bAccess == true then
            return (gVars.tGearDetails[sSlot][sName]['accessible'] == true),gVars.tGearDetails[sSlot][sName];
        else
            return (gVars.tGearDetails[sSlot][sName]['job'] == true and
                    gVars.tGearDetails[sSlot][sName]['accessible'] == true and
                    gVars.tGearDetails[sSlot][sName]['level'] <= utilities.fGetLevel(false)),
                    gVars.tGearDetails[sSlot][sName];
        end
    end
end	-- fGearCheckItem

--[[
    GearCheckGS is part of GearCheck. It processes the passed in gear set definition for untracked gear. It
    has been split out from GearCheck so that it can be recursed for "groups". It extracts all pieces of gear
    from the passed in gear set and checks to make sure each piece is known in the master list.

    Parameter
        sName   The name of the gear set
        gs      The gear set's definition
        ct      Item counter
--]]

function GearCheckGS(sName,gs,ct)
    local ts = {};
    local ref = {};
    local iCnt = ct;
    local ljj;


    for jj,kk in pairs(gs) do
        ts = {};
        -- Entries can be a table or a string. Make either case a table
        if type(kk) == 'table' then
            ts = kk;
        else
            ts[1] = kk;
        end

        -- Because slots can have inline conditionals on them, we need to strip off
        -- said conditionals before we can check if the slot (left side) is correct
        ljj = string.lower(utilities.fRemoveConditional(jj));

        if table.find(gVars.tSlotNames['full'],ljj) == nil then
            print(chat.message('Warning: Invalid slot name - ' .. jj .. ' in ' .. sName));
        else
            -- Now, make sure we're not dealing with a subset or group
            -- I think here's where the split needs to occur. Also, group processing
            -- should be handled on it's own since you have to burrow.
            if ljj == 'group' then
                -- Recurse on the group's definition to process the gear
                iCnt = GearCheckGS(sName,ts,iCnt);
            elseif ljj ~= 'subset' then
                -- Since neither a group nor a subset, process the definition
                -- Walk the list of gear
                for ss,tt in pairs(ts) do
                    bGood,ref = fGearCheckItem(jj,tt,false,sName);

                    if ref ~= nil then
                        if ref['valid'] == false and ref['slot'] == nil then
                            print(chat.message('Warning: Invalid piece of gear - ' .. tt .. ' in ' .. sName));
                        elseif ref['slot'] == false then
                            print(chat.message('Warning: Invalid slot: ' .. jj .. ', gear - ' .. tt .. ' in ' .. sName));
                        end
                    end
                    iCnt = iCnt + 1;
                    utilities.ProcessedTally('sets',iCnt,50);
                end
            end
        end
    end
    return iCnt;
end     -- GearCheckGS

--[[
    GearCheck is a coordinating routine that searches and extracts all the pieces of gear from all the
    gear sets in the appropriate job and crossjobs luas.

    Parameter
        fp      File pointer to where output should go or nil
--]]

function gear.GearCheck(fp)
	local player = gData.GetPlayer();
    local tTarget = { gProfile.Sets, crossjobs.Sets };
    local ts = {};
    local ref = {};
    local iCnt = 0;
    local bGood,s;

    -- Since this might be a reprocess, zero out the tallies
    for i,j in pairs(gVars.tGearDetails) do
        j['num'] = 0;
    end

    -- Start with storage slips
    print(chat.message('Info: Starting to scan for storage slips'));
    slips.FindSlips();
    s = slips.fDisplaySlips(false);
    if s == nil then
        s = 'None';
    end
    print(chat.message('Info: Found slips: ' .. s));

    -- then claim slips
    print(chat.message('Info: Starting to scan for claim slips'));
    print(chat.message('Info: Found claim slips: ' .. slips.fFindClaimSlips()));

    -- next is EquipIt items
    print(chat.message('Info: Starting to scan EquipIt shortcut items'));
    for s,t in pairs(gear.tEquipIt) do
        local sSlot = t['Slot'];
        if string.find('Ring,Ear',sSlot) ~= nil then
            sSlot = sSlot .. 's';
        end

        bGood,ref = fGearCheckItem(sSlot,t['Name'],false,nil);
        if ref ~= nil and ref['valid'] == false then
            print(chat.message('Warning: Invalid EquipIt gear piece - ' .. t['Name'] .. ': ' .. s));
        end
    end

    -- next is pet food since any job can equip it
    print(chat.message('Info: Starting to scan Pet Food items'));
    for s,t in pairs(pets.tPetFood) do
        bGood,ref = fGearCheckItem('ammo',t['name'],false,nil);
        if ref ~= nil and ref['valid'] == false then
            print(chat.message('Warning: Invalid Pet Food - ' .. t['Name'] .. ': ' .. s));
        end
    end

    -- next is jug pets, but only BST can equip them
    if player.MainJob == 'BST' then
        pets.FavoredJugPets();   -- Make sure "favored" entries updated
        print(chat.message('Info: Starting to scan Jug Pets'));
        for s,t in pairs(pets.tJugPets) do
            bGood,ref = fGearCheckItem('ammo',s,false,nil);
            if ref ~= nil and ref['valid'] == false then
                print(chat.message('Warning: Invalid Jug Pet - ' .. s .. ': ' .. s));
            end
        end
    end

    -- now loop through the job file and crossjobs
    for s,t in pairs(tTarget) do
        if t == gProfile.Sets then
            print(chat.message('Info: Starting to scan the Job file'));
        else
            print(chat.message('Info: Starting to scan crossjobs'));
        end

        -- Loop the gear sets
        for j,k in pairs(t) do
            -- Process if not either 'CurrentGear' or 'Progressive'. CurrentGear
            -- is a composite from other gear sets and Progressive has a
            -- complelely different structure, it will be processed elsewhere
            if string.find('CurrentGear,Progressive',j) == nil then
                iCnt = GearCheckGS(j,k,iCnt);
            elseif j == 'Progressive' then
                -- Loop on type of progressive set. Note that conditionals on the slot designation are not supported
                for ij,ik in pairs(k) do
                    -- Loop on the progressive stages
                    for jj,jk in ipairs(ik) do
                        -- Loop on the line elements
                        for kj,kk in pairs(jk) do
                            ts = {};
                            -- Entries can be a table or a string. Make either case a table
                            if type(kk) == 'table' then
                                ts = kk;
                            else
                                ts[1] = kk;
                            end

                            if table.find(gVars.tSlotNames['progressive'],string.lower(kj)) == nil then
                                print(chat.message('Warning: Invalid slot name - ' .. kj .. ' in Progressive ' .. ij));
                            else
                                -- Process the list of gear
                                for ss,tt in pairs(ts) do
                                    bGood,ref = fGearCheckItem(kj,tt,false,j);
                                    if ref ~= nil then
                                        if ref['valid'] == false and ref['slot'] == nil then
                                            print(chat.message('Warning: Invalid piece of gear - ' .. tt .. ' in Progressive:' .. ij .. ', Stage: ' .. tostring(jj) .. ', Slot: ' .. ss));
                                        elseif ref['slot'] == false then
                                            print(chat.message('Warning: Invalid slot: ' .. ss .. ', gear - ' .. tt .. ' in Progressive:' .. ij));
                                        end
                                    end
                                    iCnt = iCnt +1;
                                    utilities.ProcessedTally('sets',iCnt,50);
                                end
                            end
                        end
                        iCnt = iCnt +1;
                        utilities.ProcessedTally('sets',iCnt,50);
                    end
                end
            end
        end
    end

    print(chat.message('Info: Starting to scan \'special\''));
    for i,j in pairs(gVars.tElemental_gear) do
        if i == 'staff' then
            for ii,jj in pairs(j) do
                if string.find(gVars._AllElements,ii) ~= nil then
                    bGood,jj['NQ']['Ref'] = fGearCheckItem('main',jj['NQ']['Name'],false,nil);
                    bGood,jj['HQ']['Ref'] = fGearCheckItem('main',jj['HQ']['Name'],false,nil);
                    iCnt = iCnt + 2;
                end
                utilities.ProcessedTally('sets',iCnt,50);
            end
        elseif i == 'obi' or i == 'gorget' then
            for ii,jj in pairs(j) do
                if string.find(gVars._AllElements,ii) ~= nil then
                    if i == 'obi' then
                        bGood,jj['Ref'] = fGearCheckItem('waist',jj['Name'],false,nil);
                    else
                        bGood,jj['Ref'] = fGearCheckItem('neck',jj['Name'],false,nil);
                    end
                    iCnt = iCnt + 1;
                    utilities.ProcessedTally('sets',iCnt,50);
                end
            end
        end
    end

    print(chat.message('Info: Scan completed'));
    gVars.bGC = true;
    print(chat.message(' '));
    reporting.GearCheckList();
end		-- gear.GearCheck

--[[
    TallyProgressiveCaps determines how many stages are defined in the
    Progressive entries structure: accuracy, tank accuracy, ranged
    accuracy, and tank ranged accuracy.
--]]

function gear.TallyProgressiveCaps()
    local macc = 0;
    local mtacc = 0;
    local mracc = 0;
    local mtracc = 0;

    if gProfile.Sets.Progressive ~= nil then
        if gProfile.Sets.Progressive['Accuracy'] ~= nil then
            macc = #gProfile.Sets.Progressive['Accuracy'];
        end

        if gProfile.Sets.Progressive['Tank_Accuracy'] ~= nil then
            mtacc = #gProfile.Sets.Progressive['Tank_Accuracy'];
        else
            mtacc = macc;	-- If tank_accuracy missing, use accuracy
        end

        if gProfile.Sets.Progressive['Ranged_Accuracy'] ~= nil then
            mracc = #gProfile.Sets.Progressive['Ranged_Accuracy'];
        end

        if gProfile.Sets.Progressive['Tank_Ranged_Accuracy'] ~= nil then
            mtracc = #gProfile.Sets.Progressive['Tank_Ranged_Accuracy'];
        else
            mtracc = mracc;	-- If tank_ranged_accuracy missing, use ranged_accuracy
        end
    end

    gVars.tProgressive['Accuracy']['MaxStage'] = macc;
    gVars.tProgressive['Tank_Accuracy']['MaxStage'] = mtacc;
    gVars.tProgressive['Ranged_Accuracy']['MaxStage'] = mracc;
    gVars.tProgressive['Tank_Ranged_Accuracy']['MaxStage'] = mtracc;
end		-- gear.TallyProgressiveCaps

--[[
    fCheckForElementalGearByValue is a generalized routine that searches to see
    if the targetted elemental gear should be equipped (assuming you own the
    piece and it's accessible.)

    Parameters
        sWhat		type of elemental gear to check: staff,obi,gorget
        sWhich		which associated list to check: Affinity,Summons,MEacc,eleWS
        sElement	the key to match in the appropriate list

    return
        Record of the item,element
--]]

function gear.fCheckForElementalGearByValue(sWhat,sWhich,sElement)
    local player = gData.GetPlayer();
    local sRoot,bGood,sTarget;

    -- Make sure locks won't block equipping the item
    if sWhat == 'staff' and (locks.fIsSlotLocked('main') or locks.fIsSlotLocked('sub')) then -- staff
        return nil,nil;
    elseif sWhat == 'obi' and locks.fIsSlotLocked('waist') then -- obi
        return nil,nil;
    elseif locks.fIsSlotLocked('neck') then -- gorget
        return nil,nil;
    end

    -- What's searched for is sometimes a "root" and other times an "as-is"
    if string.find('Affinity,MEacc',sWhich) ~= nil then
        sRoot = utilities.fGetRoot(sElement);
    elseif string.find('Summons,eleWS,SongAffinity',sWhich) ~= nil then
        sRoot = string.lower(sElement);
    else
        print(chat.message('Warning: Unknown field to search: ' ..sWhich));
        return nil,nil;
    end

    -- Determine target slot
    if sWhat == 'obi' then
        sTarget = 'waist';
    elseif sWhat == 'gorget' then
        sTarget = 'neck';
    else
        sTarget = 'main';
    end

    -- Then determine which gear is the appropriate one
    for i,j in pairs(gVars.tElemental_gear[sWhat]) do
        -- Looking for elemental entries. Ignore the rest
        if string.find(gVars._AllElements,i) ~= nil then
            -- Look for a match in the associated field
            if sWhat == 'staff' then
                if table.find(gVars.tElemental_gear[sWhat][i][sWhich],sRoot) ~= nil then
                    -- Make sure the link to the dynamic table is in place
                    bGood,gVars.tElemental_gear[sWhat][i]['HQ']['Ref'] =
                        fGearCheckItem(sTarget,gVars.tElemental_gear[sWhat][i]['HQ']['Name'],false,nil);
                    bGood,gVars.tElemental_gear[sWhat][i]['NQ']['Ref'] =
                        fGearCheckItem(sTarget,gVars.tElemental_gear[sWhat][i]['NQ']['Name'],false,nil);
                    -- Make sure ref in place before checking accessibility
                    if gVars.tElemental_gear[sWhat][i]['HQ']['Ref'] ~= nil and
                       gVars.tElemental_gear[sWhat][i]['HQ']['Ref']['accessible'] == true then
                        return gVars.tElemental_gear[sWhat][i]['HQ']['Name'],i;
                    elseif gVars.tElemental_gear[sWhat][i]['NQ']['Ref'] ~= nil and
                       gVars.tElemental_gear[sWhat][i]['NQ']['Ref']['accessible'] == true then
                        return gVars.tElemental_gear[sWhat][i]['NQ']['Name'],i;
                    else
                        return nil,nil;
                    end
                end
            elseif sWhat == 'obi' or sWhat == 'gorget' then
                if table.find(gVars.tElemental_gear[sWhat][i][sWhich],sRoot) ~= nil then
                    bGood,gVars.tElemental_gear[sWhat][i]['Ref'] =
                        fGearCheckItem(sTarget,gVars.tElemental_gear[sWhat][i]['Name'],false,nil);
                end

                -- Then determine if there's an obi or gorget that matches
                if gVars.tElemental_gear[sWhat][i]['Ref'] ~= nil and
                        gVars.tElemental_gear[sWhat][i]['Ref']['accessible'] == true then
                    return gVars.tElemental_gear[sWhat][i]['Name'],i;
                end
            end
        end
    end

    -- Since we got here, either the search string wasn't found in the appropriate
    -- area or it was found, but the player doesn't have the item or it's inaccessible.
    return nil,nil;
end		-- gear.fCheckForElementalGearByValue

--[[
    fSwapToStave determines if swapping your weapon out for one of the elemental staves makes
    sense and does it for you while remembering what weapon/offhand you had equipped.

    Parameters
        sStave      Staff name
        noSave      Save gear swap?
        cs          Gear set to equip item into
--]]

function gear.fSwapToStave(sStave,noSave,cs)
    local ew = gData.GetEquipment();
    local player = gData.GetPlayer();
    local msg = nil;
    local sGear;
    local eWeap = nil;
    local eOff = nil;

    -- This is needed for a timing issue
    if sStave == nil then
        return;
    end

    -- Make sure that auto staves enabled and that locks will not prevent equipping a staff
    -- Remember: both "main" and "sub" locks will cause a block
    if gProfile.settings.bAutoStaveSwapping == false then
        msg = 'due to auto-swapping turned off!';
    elseif locks.fIsSlotLocked('main') == true or locks.fIsSlotLocked('sub') == true then
        msg = 'due to lock(s)!'
    end

    if msg ~= nil then
       msg = 'Warning: Unable to swap to a ' .. sStave .. ' ' .. msg;
       reporting.DisplayOnce(msg,false);
        return;
    end

    -- Now, process the stave swap
    if ew['Main'] ~= nil then
        eWeap = ew['Main'].Name;
    end

    if ew['Sub'] ~= nil then
        eOff = ew['Sub'].Name;
    end;

    if utilities.fGetToggle(gVars._WSWAP) == true then
        -- See if a current weapon is the one of the targetted staves
        if not (eWeap == nil or (eWeap ~= nil and string.lower(eWeap) == string.lower(sStave))) then
            -- save the weapon so it can be equipped again
            if eWeap ~= gVars.weapon and noSave == false then
                gVars.weapon = eWeap;
                gVars.offhand = eOff;
            end
        end

        -- Check versus level of player.
        if player.MainJobSync >= gVars.tElemental_gear['staff']['level'] then
            cs['Main'] = sStave;
        else
            msg = 'Warning: Unable to swap to a ' .. sStave .. ' due to level!';
            reporting.DisplayOnce(msg,false);
        end
    end
end		-- gear.fSwapToStave

--[[
    EquipItem processes the passed arguments and equips the specified item
    (whether by coded entry or name) into the appropriate equipment slot,
    then locks the appropriate slot

    Parameter
        args    List of arguments for the call

    Invocation: /ei code|item name [slot]

    Note: Coded entries know the slot they go to. If specifying an item, you
    must also specify the slot.
--]]

function gear.EquipItem(args)
    local iName,iSlot,ref,msg;
    local bMulti,sSlots,bGood;

    if #args > 1 then
        -- see if the item specified is a code
        for k,v in pairs(gear.tEquipIt) do
            if string.lower(k) == string.lower(args[2]) then
                iName = v['Name'];
                iSlot = v['Slot'];
                break;
            end
        end

        -- if it wasn't a code, the item should be explicitly identified and the slot
        if iName == nil then
            iName = args[2];
            if #args > 2 then
                if string.find('ears,rings',args[3]) ~= nil then
                    args[3] = string.sub(args[3],1,-2);
                end
                iSlot = args[3];
            else
                print(chat.message('Warning: incomplete /equipit command: /equipit code|name slot. Command ignored.'));
                return;
            end
        end

        -- First check that it's a valid item and it's accessible
        bGood,ref = fGearCheckItem(iSlot,iName,false,nil);
        if not bGood then
            if ref ~= nil then
                if ref['valid'] == false then
                    print(chat.message('Warning: Invalid piece of gear specified - ' .. iName));
                elseif ref['accessible'] == false and ref['locations'] ~= nil then
                    print(chat.message('Warning: Specified gear inaccessible - ' .. iName .. ': ' .. ref['locations']));
                elseif ref['job'] == false then
                    print(chat.message('Warning: Specified gear not usable by your job - ' .. iName));
                else
                    print(chat.message('Warning: Specified gear\'s level too high - ' .. iName .. ': ' .. tostring(ref['level'])));
                end
            else
                print(chat.message('Warning: Either parameters missing, data downloading, or gear record not created.'));
                return;
            end
        end

        -- Now, see if this item is a multislot item.
        bGood,bMulti,sSlots = locks.fMultiSlotLockCheck(iName);
        if not bGood then
            -- There's a lock blocking the equipping of this item. Let the
            -- User know.
            print(chat.message('Warning: Unable to equip ' .. iName .. ' due to locks!'));
            return;
        else
            -- If item is not a multislot item, then make sure the item
            -- slot is set.
            if sSlots == nil then
                sSlots = iSlot;
            end
        end

        -- ring and ear need a slot appended to it. Just assume "1"
        if not bMulti and string.find('ring,ear',string.lower(iSlot)) ~= nil then
            iSlot = iSlot .. '1';
            sSlots = iSlot;
        end

        -- Make sure the slot is formatted right (assuming it's just a case issue)
        -- Note that if the item is multislotted, it is already formatted correctly
        iSlot = string.upper(string.sub(iSlot,1,1)) .. string.lower(string.sub(iSlot,2));

        -- Now try and load the item
        gFunc.ForceEquip(iSlot,iName);
        locks.LockUnlock('lock',sSlots);
        if gProfile.settings.bConfirmation == true then
            print(chat.message('Info: ' .. iName .. ' has been equipped'));
        end
    else
        print(chat.message('Info: List of /equipit codes and items:'));

        for i,j in pairs(gear.tEquipIt) do
            print(chat.message(string.format('%-s - %s',i,j['Name'])));
        end
    end
end		-- gear.EquipItem

--[[
    fExpandGearLine takes the passed in line from a gear set and copies
    it to the global tGearLine array, ignoring subsets and groups. If it
    encounters an inline gear line reference, assuming that the attached
    (if present) inline conditional is true, it will call itself again with
    that reference so that a single, complete set of gear can be processed
    from the calling routine.

    Parameters
        sSlot       Slot name where gear is to be equipped
        ts          Gear list/gear
        sc          Dynamically carried code
        tHold       Dynamic definition carried over (for nested references)
--]]

function gear.fExpandGearLine(sSlot,ts,sc,tHold)
    local iPos,sval,sCode;
    local t = {};

    if sSlot == nil or ts == nil then
        return false;
    end

    if tHold == nil then
        tHold = gear.tGearLine;
    end

    if type(ts) == 'string' then
        t[1] = ts;
    else
        t = ts;
    end

    for i,j in pairs(t) do
        iPos = string.find(j,'::');
        if iPos ~= nil then
            -- Found an inline reference
            sval = string.sub(j,1,iPos-1);
            -- Check for missing slot name, assume same as passed slot
            if iPos + 2 >= string.length(j) then
                s = sSlot;
            else
                s = string.sub(j,iPos+2,-1);
            end

            -- Check for valid conditional or lack of conditional
            bGood,x = inline.fCheckInline(sval,s,tss,false,false);
            if bGood == true then
                -- Since good, remove (if present) the conditional
                iPos = string.find(s,'//');
                if iPos ~= nil then
                    sCode = string.sub(s,iPos,-1);
                    s = string.sub(s,1,iPos-1);
                else
                    sCode = nil;
                end

                -- Attach any carried inline code
                if sc ~= nil then
                    if sCode == nil then
                        sCode = sc;
                    else
                        sCode = sCode .. sc;
                    end
                end

                -- Now recurse this newly found inline reference
                x = utilities.fGetTableByName(sval);
                -- Make sure slot name formatted correctly
                s = utilities.fFormattedWord(s,gVars._SLOT_FA);
                -- Now find the slot definition
                local sDef = gear.fGetSlotDefinition(x,s);
                if sDef ~= nil then
                    bGood = gear.fExpandGearLine(s,sDef,sCode);
                end
                -- Result is ignored since bad inline has no effect
                -- on the global tGearLine array and good result is
                -- already tallied.
            end
        else
            -- Treat the item as-is
            local iCtr = #tHold + 1;
            tHold[iCtr] = j
            if sc ~= nil then
                tHold[iCtr] = tHold[iCtr] .. sc;
            end
            iCtr = iCtr + 1;
        end
    end
    return true;
end		-- gear.fExpandGearLine

--[[
    fGetSlotDefinition walks the specified gear set looking for the passed in slot
    definition. Note that subsets are ignored. The first found definition is returned.
    (It's possible to have multiple definitions for a slot, but as long as there's
    attached conditionals, only one should be valid. If there are multiple valid
    entries, then the source set is erroneous.)

    Parameters
        ts      the gear set to search
        sSlot   the slot name

    Returned
        sDef    the slot definition or nil if not found
--]]

function gear.fGetSlotDefinition(ts,sSlot)
    local sDef = {};
    local bGood,x,si,sl;

    bGood,x = utilities.fValidSlots(sSlot);
    if ts == nil or bGood == nil or bGood == false then
        return nil;
    end

    sl = string.lower(sSlot);

    -- Now, walk the table looking for the passed slot
    for i,j in pairs(ts) do
        si = string.lower(i);
        if string.find(si,'subset') == nil then
            -- Subsets are ignored, so looking for individual slots and groupings
            if string.find(si,'group') ~= nil then
                -- Ok, a group. Need to see if the attached conditional is true
                bGood,x = inline.fCheckInline(i,sSlot)
                if bGood == true then
                    sDef = gear.fGetSlotDefinition(j,sSlot);
                    if sDef ~= nil then
                        return sDef;
                    end
                end
            else
                -- Process the individual lines
                if string.find(si,sl) ~= nil then
                    -- Check to see if attached conditional (if present) is true
                    bGood,x = inline.fCheckInline(i,sSlot)
                    if bGood == true then
                        return j;
                    end
                end

            end

        end
    end

    -- If we get to here, the slot definition wasn't found
    return nil;
end     -- gear.fGetSlotDefinition

--[[
	FractionalSet is similar to FractionalAccuracy in that is equips part of a
	predefined set, but it's not based on accuracy. Instead, it's based on a
	list of slots. (Note that only names are supported and not slot numbers.)
	It creates a temporary set based on the specified slots and equips it.
--]]

function gear.FractionalSet(hs,sSlots)
	local i,t;
	local tAcc = {};
	local ts = {};
	local bGood,vRoot;
	local bFound = false;
	local bSubset = false;

	if hs == nil or sSlots == nil then
		return;
	end

	sSlots = string.lower(sSlots);

	if type(hs) == 'string' then
		ts = utilities.fGetTableByName(hs);
	else
		ts = hs;
	end

	for j,k in pairs(ts) do
		t = string.lower(j)
		if t == 'subset' then
			bSubset = true;
		else
			-- Since ears and rings are pseudo slots, if specified, make
			-- sure to match with the actual slot names
			if string.find(sSlots,'ear') ~= nil and
				(t == 'ears' or t == 'ear1' or t == 'ear2') then
					tAcc[j] = k;
			elseif string.find(sSlots,'ring') ~= nil and
				(t == 'rings' or t == 'ring1' or t == 'ring2') then
					tAcc[j] = k;
			-- at this point it's an exact match
			elseif string.find(sSlots,t) ~= nil then
				tAcc[j] = k;
			end
			if bFound == false then
				-- This indicates there was a match copied and that the
				-- temporary set will need to be moved to current
				bFound = (tAcc[j] ~= nil);
			end
		end
	end

	if bFound == true then
		gear.MoveToDynamicGS(tAcc,gProfile.Sets.CurrentGear,false,nil);
	else
		if bSubset == true then
			for j,k in pairs(ts) do
				t = string.lower(j)
				if t == 'subset' then
					for ji,ki in ipairs(k) do
						if type(ki) == 'table' then
							ts = ki;
						else
							ts[j] = ki;
						end

						-- Then determine the appropriate set to load
						for kk,vv in pairs(ts) do
							bGood,vRoot = inline.fCheckInline(vv,'subset');
							if bGood == true then
								gear.FractionalSet(vRoot,sSlots)
								break;
							end
						end
					end
				end
			end
		end
	end
end	-- gear.FractionalSet

--[[
    fCheckForEleGear determines if the player has the piece of elemental gear
    indicated by type and if it is accessible

    Parameters
        sType           Type of elemental gear (staff, obi or gorget)
        sElement        Elemental type to match

    Returned
        Reference to the piece of gear
--]]

function gear.fCheckForEleGear(sType,sElement)
    local player = gData.GetPlayer();
    local bGood,slot;

    -- Make sure player job defined and download not transitioning
    if player.MainJob == 'NON' then
        return nil;
    end

    -- Then check the level of the player vs the elemental piece of gear
    if player.MainJobSync < gVars.tElemental_gear[sType]['level'] then
        return nil;
    end

    -- The links for the dynamic table will be there if /gc was run. If not,
    -- then all elemental gear's ['Ref'] will be nil and skipped.

    -- Now process the reference accordingly. For staff, check for HQ before
    -- looking at NQ
    if sType == 'staff' then
        if gVars.tElemental_gear[sType][sElement]['HQ']['Ref'] ~= nil and
           gVars.tElemental_gear[sType][sElement]['HQ']['Ref']['accessible'] == true then
            return gVars.tElemental_gear[sType][sElement]['HQ']['Name'];
        elseif gVars.tElemental_gear[sType][sElement]['NQ']['Ref'] ~= nil and
           gVars.tElemental_gear[sType][sElement]['NQ']['Ref']['accessible'] == true then
            return gVars.tElemental_gear[sType][sElement]['NQ']['Name'];
        else
            return nil;
        end
    else
        -- Obi and Gorget have the same structure, so handle the same way
        if gVars.tElemental_gear[sType][sElement]['Ref'] ~= nil and
           gVars.tElemental_gear[sType][sElement]['Ref']['accessible'] == true	then
            return gVars.tElemental_gear[sType][sElement]['Name'];
        else
            return nil;
        end
    end
    return nil;
end		-- gear.fCheckForEleGear

--[[
    fGetAccStage retrieves the specified value based on the specified type of accuracy.

    Parameters
            sWhich          Stage abbreviation: Acc, TAcc, Racc, TRacc
            sType           Which value to return: CUR or MAX

    Return
        The requested value
--]]

function gear.fGetAccStage(sWhich,sType)
    local ssType;

    if sWhich == nil then
        sWhich = 'Acc';
    end

    if sType == nil or sType == '' then
        sType = 'CUR';
    end

    if string.find('Acc,TAcc,RAcc,TRAcc',sWhich) ~= nil then
        if sType == 'CUR' then
            ssType = 'CurStage';
        else
            ssType = 'MaxStage';
        end

        for i,j in pairs(gVars.tProgressive) do
            if j['Abbr'] == sWhich then
                return j[ssType];
            end
        end
    end

    return nil;
end     -- gear.fGetAccStage

return gear;
