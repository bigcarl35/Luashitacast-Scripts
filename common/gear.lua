local gear = T{};

local utilities = require('common.utilities');
local crossjobs = require('common.crossjobs');
local reporting = require('common.reporting');
local inline = require('common.inline');
local gear = require('common.gear');
local slips = require('common.slips');
local locks = require('common.locks');

--[[
    This component contains all functions associated with gear

    List of routines-
        Subroutines:
			BuildTrackingTable		     	Builds tracking table for delayed release
            CheckForExceptions              Make sure gear that has to remain in place aren't replaced
            EquipItem                       /ei command, equips piece of gear
            EquipTheGear                    Cleans up dynamic gear set and equips the gear
            GearCheck                       Extracts all gear in all gear sets and processes
            MoveToDynamicGS                 Process gear set and place in target gear set
            ProcessGS                       Processes the specified crafting/gathering gear set

        Functions:
            fCheckForEleGear                Determines if you have the right elemental gear
            fCheckForElementalGearByValue   Determines if an elemental gear should be equipped
            fExpandGearLine                 Expands slot's definition from subsets and references
            fGearCheckItem                  Processes and places the item in the dynamic listing
            fGetSlotDefinition              Retrieves a slot's definition from a gear set
            fSwapToStave                    Determines if swapping to a staff makes sense
            local fTallyProgressiveCaps     Determines how many stages are in progressive categories
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

-- This structure will be dynamically populated by the fGearCheck function.
-- The slots will have a set structure providing details about every gear
-- piece in the job file/crossjobs so that when checking for the piece of
-- gear, the details that would require looking up item details will already
-- be known, thus avoiding excessive server requests.
gear.tGearDetails = {
    ['main']  = { ['num'] = 0, ['acc'] = 0, ['vis'] = true, {} },
    ['sub']   = { ['num'] = 0, ['acc'] = 0, ['vis'] = true, {} },
    ['range'] = { ['num'] = 0, ['acc'] = 0, ['vis'] = true, {} },
    ['ammo']  = { ['num'] = 0, ['acc'] = 0, ['vis'] = true, {} },
    ['head']  = { ['num'] = 0, ['acc'] = 0, ['vis'] = true, {} },
    ['neck']  = { ['num'] = 0, ['acc'] = 0, ['vis'] = false, {} },
    ['ears']  = { ['num'] = 0, ['acc'] = 0, ['vis'] = false, {} },
    ['body']  = { ['num'] = 0, ['acc'] = 0, ['vis'] = true, {} },
    ['hands'] = { ['num'] = 0, ['acc'] = 0, ['vis'] = true, {} },
    ['rings'] = { ['num'] = 0, ['acc'] = 0, ['vis'] = false, {} },
    ['back']  = { ['num'] = 0, ['acc'] = 0, ['vis'] = false, {} },
    ['waist'] = { ['num'] = 0, ['acc'] = 0, ['vis'] = false, {} },
    ['legs']  = { ['num'] = 0, ['acc'] = 0, ['vis'] = true, {} },
    ['feet']  = { ['num'] = 0, ['acc'] = 0, ['vis'] = true, {} }
};

-- Structure that holds tallied information about the different types of the
-- progressive structure
gear.Progressive =  {
    ['Accuracy'] = { ['MaxStage'] = 0, ['CurStage'] = 0, ['Abbr'] = 'Acc' },
    ['Tank_Accuracy'] = { ['MaxStage'] = 0, ['CurStage'] = 0, ['Abbr'] = 'TAcc' },
    ['Ranged_Accuracy'] = { ['MaxStage'] = 0, ['CurStage'] = 0, ['Abbr'] = 'RAcc' },
    ['Tank_Ranged_Accuracy'] = { ['MaxStage'] = 0, ['CurStage'] = 0, ['Abbr'] = 'TRAcc' }
};

-- Temporary holding variables for the main hand and off hand weapons
gear.weapon = nil;
gear.offhand = nil;

-- Indicates if /GC has been run or not
gear.bGC = false;

-- Master table for tracking gear that has a delay on when it's taken off
gear.TrackedGearWithDelay = {};

-- Temporary gear definition
gear.tGearLine = {};

local _sTrack = '//TRACK';

--[[
    BuildTrackingTable populates the master tracking table with the information from the
    player's job file so that the master table is prepared to support delayed release of
    certain gear. This function is invoked on the end of the /gc command.
--]]

function gear.BuildTrackingTable()

    if gProfile.TrackedGear == nil or #gProfile.TrackedGear == 0 then
        -- Player has not defined any gear to track
        return;
    end

    if #gear.TrackedGearWithDelay == 0 then
        for i,j in ipairs(gProfile.TrackedGear) do
            gear.TrackedGearWithDelay[i] = {
                ['item'] = j['item'],       -- Item name
                ['slot'] = j['slot'],       -- Main slot of item, not affected slots
                ['ref'] = gear.tGearDetails[j['slot']][j['item']],	-- reference to tGearDetails
                ['delay'] = j['delay'],		-- Delay in seconds
                ['primed'] = false,			-- Is item equipped?
                ['started'] = false,		-- Has countdown started
                ['end_time'] = nil,			-- When is hold done?
            };
        end
    end
end		-- gear.BuildTrackingTable

--[[
    ProcessGS processes the specified crafting/gathering Gear Set

    Pararameter
        args		Passed arguement list
--]]

function gear.ProcessGS(args)

    if #args > 1 then
        local sArg = string.upper(args[2]);
        local sTmp = ',' .. crossjobs.Crafting_Types .. ',';
        local sTmp2 = ',' .. crossjobs.Gathering_Types .. ',';
        if string.find(sTmp,sArg) ~= nil or string.find(sTmp2,sArg) ~= nil then
            -- gather or crafting set
            if string.find(sTmp,sArg) then
                -- Crafting set
                crossjobs.Craft = sArg;
                gear.MoveToDynamicGS(crossjobs.Sets.Crafting,crossjobs.Sets.CurrentGear,false,'Crafting');
            else
                -- Gather set
                crossjobs.Gather = sArg;
                gear.MoveToDynamicGS(crossjobs.Sets.Gathering,crossjobs.Sets.CurrentGear,false,'Gathering');
            end
        else
            local tTable = utilities.fGetTableByName(sArg);	-- Change string to table
            if tTable ~= nil then
                gear.MoveToDynamicGS(tTable,crossjobs.Sets.CurrentGear,false,sArg);
            else
                print(chat.message('Warning: Gear set not found: ' .. sArg));
                return;
            end
        end

        gear.EquipTheGear(crossjobs.sets.CurrentGear,true);
        locks.LockByGearSet(crossjobs.sets.CurrentGear,nil,false,bIgnoreWSWAP,bDisplay)
    else
        print(chat.message('Error: No set specified for /gearset. Command ignored.'));
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

    -- Make sure player's transition between zones is complete
    if player.MainJob == nil or player.MainJob == 'NON' then
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
                -- Since the conditional is true, recurse on the the enclosed gear set
                -- definition. (A group definition requires a table. That's what needs
                -- to be processed here.)
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
                bSkip = not (utilities.fGetToggle('WSwap') == true or
                             crossjobs.settings.bWSOverride == true or
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
                    bG,ref = fGearCheckItem(sK,vv,false);
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
                                stK = root .. tostring(iNum);
                                tMaster[stK] = vRoot;
                                iNum = iNum + 1;
                            else
                                if iNum == 1 then
                                    stK = root .. tostring(iNum+1);
                                    tMaster[stK] = vRoot;
                                end
                                iNum = 3;	-- This forces the pairing to kick out
                            end
                        else
                            -- Normal single slot
                            tMaster[stK] = vRoot;
                            break;
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
    EquipTheGear makes sure that the passed gear set doesn't have an item in a slot
    that is being blocked by another item (e.g., no head gear if a vermillion cloak
    is in the body slot.) It also makes sure the It the equips the passed gear set
    isn't going to override locks unless told to ignore locks. (This function use
    to be found in MoveToCurrent which has been replaced by MoveToDynamicGS.)

    Parameters
        tSet                Name of gear set to equip
        bOverride           T/F, indicates if WSWAP is to be ignored
        bIgnoreLocks        T/F, indicates if locks arre to be ignored

        !!! add lock handler !!!
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

    -- Then deal with the multislot items
    for j,k in pairs(gear.tMultiSlot) do
        if tSet[k['slot']] ~= nil and
            string.lower(tSet[k['slot']]) == string.lower(k['item']) then
            bGood,bMulti,sSlots = locks.fMultiSlotLockCheck(k['item']);

            if not bGood then
                tSet[k['slot']] = '';
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
                    if sWhich ~= k['slot'] then
                        tSet[sWhich] = '';
                    end
                end
            end
        end
    end

    -- And if weapon swapping is not enabled, clear out the top line (except ammo)
    if not (utilities.fGetToggle('WSwap') == true or
            crossjobs.settings.bWSOverride == true or bOverride == true) then
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
    fValidateSpecial determines if the passed gear's special settings are true

    Parameters
        sSlot       The slot the piece of gear will be placed into
        sGear       The name of the piece of gear being tested

    Returned
        Are the special conditions met

        !!!
--]]

function gear.fValidateSpecial(sSlot,sGear)

end     -- gear.fValidateSpecial

--[[
    fTrackingDefine makes sure that the passed definition exists in the tracking table

    Parameters
        bProfile    Profile or cross-job set
        sSet        Name of set the piece is from
        sGear       Name of gear piece to track
        iSec        How many seconds should the hold be for
--]]

!!!
-- Syntax is wrong here

function gear.fTrackingDefine(bProfile,sSet,sSlot,sGear,iSec)
    if bProfile == nil then
        bProfile = true;
    end
    if sSet == nil or sGear == nil then
        return;
    else
        if iSec == nil then
            iSec = 20;
        end
        if gear.TrackingTable[bProfile][sSet][sSlot][sGear]['duration'] == nil then
            gear.TrackingTable[bProfile][sSet][sSlot][sGear]['duration'] = iSec;
            gear.TrackingTable[bProfile][sSet][sSlot][sGear]['expiry'] = nil;
            gear.TrackingTable[bProfile][sSet][sSlot][sGear]['primed'] = false;
        end
    end
end     -- gear.fTrackingDefine

--[[
    fGearCheckItem processes the specific item sent to it and where appropriate, populates
    gear.GearDetails

    Parameters
        sSlot   - Name of the slot
        sName   - Name of the item to check
        bAccess - True = return accessibility, False = check job, access, and level

    Returned
        bAccessibility  T/F, is the item accessible
        ref             Reference to the item in tGearDetails
--]]

function fGearCheckItem(sSlot,sName,bAccess)
    local player = gData.GetPlayer();
    local bJob,bAccessible,bSlot;
    local iPos;
    local item = {};
    local tOwned = {};
    local sCodes = nil;

    -- Required fields
    if sSlot == nil or sName == nil then
        return false,nil;
    end

    -- Subsets, groups, and inline reference definitions are skipped
    if string.find('subset,group',string.lower(sSlot)) ~= nil or string.find(sName,'::') ~= nil then
        return false,nil;
    end

    -- Make sure "downloading data" is not in transition
    if player.MainJob == nil or player.MainJob == 'NON' then
        return false,nil;
    end

    -- Assume full check if absent
    if bAccess == nil then
        bAccess = false;
    end

    sSlot = string.lower(sSlot);
    sName = string.lower(sName);

    -- Make sure all ear and ring variants represented by the generic category
    if string.find('ears,ear1,ear2',sSlot) ~= nil then
        sSlot = 'ears';
    elseif string.find('rings,ring1,ring2',sSlot) ~= nil then
        sSlot = 'rings';
    end

    -- Then remove any inline conditionals
    iPos = string.find(sName,'//');
    if iPos ~= nil then
        sCodes = string.sub(sName,iPos,-1);
        sName = string.sub(sName,1,iPos-1);
    end

    local bExist = (gear.tGearDetails[sSlot][sName] ~= nil); -- Note if there is an existing record

    if gear.bGC == false then
        -- Since /gc has not happened, create the record
        item = AshitaCore:GetResourceManager():GetItemByName(sName,2);
        if item ~= nil then
            bJob = (bit.band(item.Jobs,utilities.JobMask[player.MainJob]) == utilities.JobMask[player.MainJob]) or
                (bit.band(item.Jobs,utilities.JobMask['Alljobs']) == utilities.JobMask['Alljobs']);
            tOwned = utilities.fCheckItemOwned(item);
            bSlot = utilities.fSlotMatch(sSlot,item.Slots);
            bAccessible = (tOwned['own'] == true and tOwned['accessible'] == true);

            -- Save item w/details
            gear.tGearDetails[sSlot][sName] = {
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
                ['desc'] 	   = item.Description[1]
                };
            if bSlot == false then
                gear.tGearDetails[sSlot][sName]['valid'] = false;
            end
            if not bExist then
                gear.tGearDetails[sSlot]['num'] = gear.tGearDetails[sSlot]['num'] + 1;
                if bAccessible then
                    gear.tGearDetails[sSlot]['acc'] = gear.tGearDetails[sSlot]['acc'] + 1;
                end
            end
        else
            -- This is an erroneous item
            gear.tGearDetails[sSlot][sName] = { ['valid'] = false };
            return false,gear.tGearDetails[sSlot][sName];
        end
    end

    -- If it still doesn't exist, return that state
    if gear.tGearDetails[sSlot][sName] == nil then
        return false,nil;
    else
        if bAccess == true then
            return (gear.tGearDetails[sSlot][sName]['accessible'] == true),gear.tGearDetails[sSlot][sName];
        else
            return (gear.tGearDetails[sSlot][sName]['job'] == true and
                    gear.tGearDetails[sSlot][sName]['accessible'] == true and
                    gear.tGearDetails[sSlot][sName]['level'] <= utilities.fGetLevel(false),
                    gear.tGearDetails[sSlot][sName];
        end
    end
end	-- fGearCheckItem

--[[
    GearCheck is a coordinating routine that searches and extracts all the pieces of gear from all the
    gear sets in the appropriate job and crossjobsr luas
--]]

function gear.GearCheck()
	local player = gData.GetPlayer();
    local tTarget = { gProfile.Sets, crossjobs.Sets };
    local ts = {};
    local ref = {};
    local iCnt = 0;
    local bGood,s;


    -- Tallying stage counts from the Progressive structure for Progressive Accuracy, Tank Accuracy,
    -- Ranged Accuracy, and Tank Ranged Accuracy.
    local macc,mtacc,mracc,mtracc = fTallyProgressiveCaps();
    gear.Progressive['Accuracy']['MaxStage'] = macc;
    gear.Progressive['Tank_Accuracy']['MaxStage'] = mtacc;
    gear.Progressive['Ranged_Accuracy']['MaxStage'] = mracc;
    gear.Progressive['Ranged_Tank_Accuracy']['MaxStage'] = mtracc;

    -- Now start with storage slips
    print(chat.message('Info: Starting to scan for storage slips'));
    slips.FindSlips();
    s = slips.fDisplaySlips(false);
    if s == nil then
        s = 'None';
    end
    print(chat.message('Info: Found slips: ' .. s));

    -- then claim slips
    print(chat.message('Info: Starting to scan for claim slips'));
    print(chat.message('Info: Found claim slips: ' .. slips.fFindClaimSlips());

    -- next is EquipIt items
    print(chat.message('Info: Starting to scan EquipIt shortcut items'));
    for s,t in pairs(gear.tEquipIt) do
        local sSlot = t['Slot'];
        if string.find('Ring,Ear',sSlot) ~= nil then
            sSlot = sSlot .. 's';
        end
        bGood,ref = fGearCheckItem(sSlot,t['Name'],false);
        if ref ~= nil and ref['valid'] == false then
            print(chat.message('Warning: Invalid EquipIt gear piece - ' .. t['Name'] .. ': ' .. s));
        end
    end

    -- next is pet food since any job can equip it
    print(chat.message('Info: Starting to scan Pet Food items'));
    for s,t in pairs(gear.tPetFood) do
        bGood,ref = fGearCheckItem('ammo',t['name'],false);
        if ref ~= nil and ref['valid'] == false then
            print(chat.message('Warning: Invalid Pet Food - ' .. t['Name'] .. ': ' .. s));
        end
    end

    -- next is jug pets, but only BST can equip them
    if player.MainJob == 'BST' then
        pets.FavoredJugPets();   -- Make sure "favored" entries updated
        print(chat.message('Info: Starting to scan Jug Pets')));
        for s,t in pairs(pets.tJugPets) do
            bGood,ref = fGearCheckItem('ammo',s,false);
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
            local fj = utilities.fFormattedWord(j,utilities._SLOT_FA);    -- Make sure formatted correctly
            -- Process if not either 'CurrentGear' or 'Progressive'. CurrentGear
            -- is a composite from other gear sets and Progressive has a
            -- complelely different structure, it will be processed elsewhere
            if table.find({'CurrentGear','Progressive'},fj) == nil then
                -- Loop the gear set slots
                for jj,kk in pairs(k) do
                    ts = {};
                    -- Entries can be a table or a string. Make either case a table
                    if type(kk) == 'table' then
                        ts = kk;
                    else
                        ts[1] = kk;
                    end

                    if table.find(utilities.SlotNames,string.lower(jj)) == nil then
                        print(chat.message('Warning: Invalid slot name - ' .. jj .. ' in ' .. j));
                    else
                        -- Now walk the list of gear
                        for ss,tt in pairs(ts) do
                            bGood,ref = fGearCheckItem(jj,tt,false);
                            if ref ~= nil then
                                if ref['valid'] == false and ref['slot'] == nil then
                                    print(chat.message('Warning: Invalid piece of gear - ' .. tt .. ' in ' .. j));
                                elseif ref['slot'] == false then
                                    print(chat.message('Warning: Invalid slot: ' .. jj .. ', gear - ' .. tt .. ' in ' .. j));
                                end
                            end
                            iCnt = iCnt +1;
                            utilities.ProcessedTally('sets',iCnt,50);
                        end
                    end
                end
            elseif fj == 'Progressive' then
                -- Loop on type of progressive set
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

                            if table.find(utilities.SlotNames,string.lower(kj)) == nil then
                                print(chat.message('Warning: Invalid slot name - ' .. kj .. ' in Progressive ' .. ij));
                            else
                                -- Process the list of gear
                                for ss,tt in pairs(ts) do
                                    bGood,ref = fGearCheckItem(kj,tt,false);
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
    for i,j in pairs(utilities.tElemental_gear) do
        if i == 'staff' then
            for ii,jj in pairs(j) do
                if string.find(crossjobs._AllElements,ii) ~= nil then
                    bGood,jj['NQ']['Ref'] = fGearCheckItem('main',jj['NQ']['Name'],false);
                    bGood,jj['HQ']['Ref'] = fGearCheckItem('main',jj['HQ']['Name'],false);
                    iCnt = iCnt + 2;
                end
                utilities.ProcessedTally('sets',iCnt,50);
            end
        elseif i == 'obi' or i == 'gorget' then
            for ii,jj in pairs(j) do
                if string.find(crossjobs._AllElements,ii) ~= nil then
                    if i == 'obi' then
                        bGood,jj['Ref'] = fGearCheckItem('waist',jj['Name'],false);
                    else
                        bGood,jj['Ref'] = fGearCheckItem('neck',jj['Name'],false);
                    end
                    iCnt = iCnt + 1;
                    utilities.ProcessedTally('sets',iCnt,50);
                end
            end
        end
    end

    print(chat.message('Info: Scan completed'));
    print(chat.message(' ')));
    reporting.GearCheckList();
    -- Establish the master tracking table
    gear.BuildTrackingTable();
end		-- gear.GearCheck

--[[
    TallyProgressiveCaps determines how many stages are defined in the
    Progressive entries structure: accuracy, tank accuracy, ranged
    accuracy, and tank ranged accuracy.
--]]

function fTallyProgressiveCaps()
    local macc = 0;
    local mtacc = 0;
    local mracc = 0;
    local mtracc = 0;

    if gProfile.Sets.Progressive ~= nil then
        if gProfile.Sets.Progressive['Accuracy'] ~= nil then
            for i,j in pairs(gProfile.Sets.Progressive['Accuracy']) do
                macc = macc + 1;
            end
        end

        if gProfile.Sets.Progressive['Tank_Accuracy'] ~= nil then
            for i,j in pairs(gProfile.Sets.Progressive['Tank_Accuracy']) do
                mtacc = mtacc + 1;
            end
        else
            mtacc = macc;	-- If tank_accuracy missing, use accuracy
        end

        if gProfile.Sets.Progressive['Ranged_Accuracy'] ~= nil then
            for i,j in pairs(gProfile.Sets.Progressive['Ranged_Accuracy']) do
                mracc = mracc + 1;
            end
        end

        if gProfile.Sets.Progressive['Tank_Ranged_Accuracy'] ~= nil then
            for i,j in pairs(gProfile.Sets.Progressive['Tank_Ranged_Accuracy']) do
                mtracc = mtracc + 1;
            end
        else
            mtracc = mracc;	-- If tank_ranged_accuracy missing, use ranged_accuracy
        end
    end
    return macc,mtacc,mracc,mtracc;
end		-- fTallyProgressiveCaps

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
    for i,j in pairs(gear.tElemental_gear[sWhat]) do
        -- Looking for elemental entries. Ignore the rest
        if string.find(crossjobs._AllElements,i) ~= nil then
            -- Look for a match in the associated field
            if sWhat == 'staff' then
                if table.find(gear.tElemental_gear[sWhat][i][sWhich],sRoot) ~= nil then
                    -- Make sure the link to the dynamic table is in place
                    bGood,gear.tElemental_gear[sWhat][i]['HQ']['Ref'] =
                        fGearCheckItem(sTarget,gear.tElemental_gear[sWhat][i]['HQ']['Name'],false);
                    bGood,gear.tElemental_gear[sWhat][i]['NQ']['Ref'] =
                        fGearCheckItem(sTarget,gear.tElemental_gear[sWhat][i]['NQ']['Name'],false);
                    -- Make sure ref in place before checking accessibility
                    if gear.tElemental_gear[sWhat][i]['HQ']['Ref'] ~= nil and
                       gear.tElemental_gear[sWhat][i]['HQ']['Ref']['accessible'] == true then
                        return gear.tElemental_gear[sWhat][i]['HQ']['Name'],i;
                    elseif gear.tElemental_gear[sWhat][i]['NQ']['Ref'] ~= nil and
                       gear.tElemental_gear[sWhat][i]['NQ']['Ref']['accessible'] == true then
                        return gear.tElemental_gear[sWhat][i]['NQ']['Name'],i;
                    else
                        return nil,nil;
                    end
                end
            elseif sWhat == 'obi' or sWhat == 'gorget' then
                if table.find(gear.tElemental_gear[sWhat][i][sWhich],sRoot) ~= nil then
                    bGood,gear.tElemental_gear[sWhat][i]['Ref'] =
                        fGearCheckItem(sTarget,gear.tElemental_gear[sWhat][i]['Name'],false);
                end

                -- Then determine if there's an obi or gorget that matches
                if gear.tElemental_gear[sWhat][i]['Ref'] ~= nil and
                        gear.tElemental_gear[sWhat][i]['Ref']['accessible'] == true then
                    return gear.tElemental_gear[sWhat][i]['Name'],i;
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
    if crossjobs.settings.bAutoStaveSwapping == false then
        msg = 'due to auto-swapping turned off!';
    elseif utilities.fIsLocked('main') == true or utilities.fIsLocked('sub') == true then
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

    if (utilities.fGetToggle('WSwap') or crossjobs.settings.bWSOverride == true) then
        -- See if a current weapon is the one of the targetted staves
        if not (eWeap == nil or (eWeap ~= nil and string.lower(eWeap) == string.lower(sStave))) then
            -- save the weapon so it can be equipped again
            if eWeap ~= gear.weapon and noSave == false and crossjobs.settings.bWSOverride == false then
                gear.weapon = eWeap;
                gear.offhand = eOff;
            end
        end

        -- Check versus level of player.
        if player.MainJobSync >= utilities.tElemental_gear['staff']['level'] then
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
        bGood,ref = fGearCheckItem(iSlot,iName,false);
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
        locks.fGetLockedList(true);
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
                s = utilities.fFormattedWord(s,utilities._SLOT_FA);
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
    local sDef {};
    local bGood,x,si,sl;

    bGood,x = utilities.fValidSlots(sSlot);
    if ts == nil or bGood == nil or bGood = false then
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
    if player.MainJobSync < utilities.tElemental_gear[sType]['level'] then
        return nil;
    end

    -- The links for the dynamic table will be there if /gc was run. If not,
    -- then all elemental gear's ['Ref'] will be nil and skipped.

    -- Now process the reference accordingly. For staff, check for HQ before
    -- looking at NQ
    if sType == 'staff' then
        if utilities.tElemental_gear[sType][sElement]['HQ']['Ref'] ~= nil and
           utilities.tElemental_gear[sType][sElement]['HQ']['Ref']['accessible'] == true then
            return utilities.tElemental_gear[sType][sElement]['HQ']['Name'];
        elseif utilities.tElemental_gear[sType][sElement]['NQ']['Ref'] ~= nil and
           utilities.tElemental_gear[sType][sElement]['NQ']['Ref']['accessible'] == true then
            return utilities.tElemental_gear[sType][sElement]['NQ']['Name'];
        else
            return nil;
        end
    else
        -- Obi and Gorget have the same structure, so handle the same way
        if utilities.tElemental_gear[sType][sElement]['Ref'] ~= nil and
           utilities.tElemental_gear[sType][sElement]['Ref']['accessible'] == true	then
            return utilities.tElemental_gear[sType][sElement]['Name'];
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

    if sType == nil or sType == then
        sType = 'CUR';
    end

    if string.find('Acc,TAcc,RAcc,TRAcc',sWhich) ~= nil then
        if sType == 'CUR' then
            ssType = 'CurStage';
        else
            ssType = 'MaxStage';
        end

        for i,j in pairs(gear.Progressive) do
            if j['Abbr'] == sWhich then
                return j[ssType];
            end
        end
    end

    return nil;
end     -- gear.fGetAccStage
