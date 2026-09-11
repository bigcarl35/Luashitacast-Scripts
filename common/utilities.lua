local utilities = {};

--[[
    This component contains functions that are of general use to any of the othe luashitacast compenents.

    List of routines-
        Subroutines:
            bRangeOrThrowing        Makes sure player has appropriate weapon to pull with
            ClearAliasAll           Unregisters all luashitacast commands from alias list
            ClearAliasCC            Unregisters all custom conditional alias commands
            ClearSet                Empties the passed gear set
            CopyDisplaybarSettings  Makes a copy of the player's displaybar settings in case of reset
            GetWeaponsList          Imports the list of all weapons of the type passed in
            Initialize              Defines initial settings for luashitacast
            OpenByFilename          Opens passed file for append (or generates new name and opens)
            ProcessedTally          Notification system for every 'n' entries
            PullTarget              Pulls character's target and announces to party
            Reminder                Reminder function to nag player to /gc
            SetAliasCC              Registers all custom conditional commands
            SetAliasAll             Registers all luashitacast commands
            SetJob                  Determines if main job is NON and sets it from Profile

            AdvanceCycle            Advance the setting of a specific cycle
            AdvanceToggle           Advance the setting of a specific toggle
            CreateCycle             Create a dynamic cycle
            CreateToggle            Create a dynamic toggle (on/off)
            SetToggle               Set a specific value to a toggle
            ToggleState             Toggles on/off feedback mechanism

        Functions:
            fBit                    2^(n-1) resultant
            fHasBit                 Determines if bit set in value
            fAccEnabled             Determines if any accuracy stage has been set
            fBuffed                 Determines if passed buff is on character
            fCheckDisplayFieldValidity  Determines if the passed value is valid and if so returns the appropriate index
            fCheckItemOwned         Determines if character owns piece of gear
            fCheckMagicJob          Determines if character has a magic job/subjob
            fCheckObiDW             Determines if Day/weather element advantageous for obi
            fCheckPartyJob          Is a member of your party a certain job?
            fCheckRegionControl     Determines if player's nation controls region
            fCheckTime              Determines if passed time matches keyword
            fCheckTimeList          Determines if one of the past time keywords is valid
            fCheckWSBailout         Determines range to target would fail Weapon Skill
            fFindCommand            Determines which toggle the passed command matches
            fFormattedWord          Capitalization routine for passed in word
            fGetAllGearSetNames     Creates and returns a list of all gear sets
            fGetCCDescription       Returns the question associated with the passed code
            fGetLevel               Determines gear level cap for player
            fGetMobType             Determines if the target is of the passed type
            fGetPartyCount          Determines how many characters in party
            fGetRoot                Retrieves the "base" of the passed in spell/song
            fGetTableByName         Returns the gear set associated with name
            fIs_busy                Determines if player is busy (fishing,gathering,crafting)
            fIsGearsetDetailsFound  Is the passed in gear details record found in the passed in list
            fIsVisible              Determines if the visibility is true
            fIsDisplaybarSettingValid Determines if the visibility setting is valid
            fJobInParty             Determines if a party member has the passed in job
            fLtrim                  Trims leading spaces from the passed in string
            fMagicalJob             Determines if the player's main job or subjob is magical
            fMagicalMainJob         Determines if the player's main job can do magic
            fMagicalSubjob          Determines if the player's subjob can do magic
            fNewFileName            Generates a new report file name
            fOpenByFilename         Opens the passed in file for writing/appending
            fParseFileDesignation   Determines the file name from the passed file designation
            fReferenceCheck         Determines if any gear is reference to another set's slot
            fRemoveConditional      Removes inline conditionals from string
            fSetColorText           Colors text for displaying on screen
            fSlotMatch              Determines if item can be loaded into slot
            fSplitStringByDelimiter Splits apart the delimnited string into a table
            fTargetId               Returns the target ID (hex) of player's target
            fToggleExists           Determines if the passed in toggle exists
            fTranslateWhichSlot     Determines if passed in slot valid
            fTrim                   Trims leading and trailing spaces from a string
            fValidSlots             Determines if passed in slot list is valid
            fWhichWSStat            Determines which stat(s) are emphasized in the weapon skill
            fGetCycle                Get a specific cycle's value
            fGetToggle               Get a specific toggle's value
            fSetCycle                Set a specific cycle's value
--]]

-- Timer base time used in the /GC nag reminder
utilities.basetime = os.time();

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

    gVars.Cycles[name] = newCycle;
end		-- utilities.CreateCycle

--[[
    fGetCycle returns the currently selected value of the cycle

    Parameter
        Name        Name of the cycle

    Returned
        Current value of the cycle
--]]

function utilities.fGetCycle(name)
    local ctable = gVars.Cycles[name];

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
    local ctable = gVars.Cycles[name];

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
    local ctable = gVars.Cycles[name];

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
    fCycleExists determines if there's a cyclee by the passed in name

    Parameter
        sName       Name of cycle

    Returned
        T/F, does the toggle exist
--]]

function utilities.fCycleExists(sName)
    local ctable = gVars.Cycles[sName];

    return (type(ctable) == 'table');
end     -- utilities.fToggleExists

--[[
    CreateToggle creates a binary variable that can be turrned on or off

    Parameters
        name        Name of the toggle
        default     Value of the toggle
--]]

function utilities.CreateToggle(name, default)

    gVars.Toggles[name] = default;
end		-- utilities.CreateToggle

--[[
    fGetToggle returns the name of the current setting of the passed toggle variable

    Parameter
        Name        Name of the toggle

    Returned
        Value of the toggle
--]]

function utilities.fGetToggle(name)

    if gVars.Toggles[name] ~= nil then
        return gVars.Toggles[name];
    else
        return false;
    end
end		-- utilities.fGetToggle

--[[
    fToggleExists determines if there's a toggle by the passed in name

    Parameter
        sName       Name of toggle

    Returned
        T/F, does the toggle exist
--]]

function utilities.fToggleExists(sName)

    return (type(gVars.Toggles[sName]) == 'boolean');
end     -- utilities.fToggleExists

--[[
    AdvanceToggle just flips the binary setting of the passed toggle variable

    Parameter
        Name        Name of toggle
--]]

function utilities.AdvanceToggle(name)

    if utilities.fToggleExist(name) == false then
        return;
    elseif gVars.Toggles[name] then
        gVars.Toggles[name] = false;
    else
        gVars.Toggles[name] = true;
    end
end		-- utilities.AdvanceToggle

--[[
    SetToggle explicitly sets the value of the passed binary variable

    Parameters
        Name        Name of toggle
        Val         Value to set toggle to
--]]

function utilities.SetToggle(name,val)

    if (type(gVars.Toggles[name]) ~= 'boolean' or type(val) ~= 'boolean') then
        return;
    else
        gVars.Toggles[name] = val;
    end
end		-- utilities.SetToggle

--[[
    Reminder is a simple routine used for displaying a nag message for the player
    to run /GC. Initially it displayes 15 seconds after logging in, but from then on
    it will display every 5 minutes until /GC is run. (Gear swapping does not occur
    until this command is run.)

    Note that the player can disable the reminder by setting the Enabled setting in
    the settings.Reminder structure to false. This does not mean that /gc can be
    skipped. Rather, it just turns off the reminder itself without resolving the need
    to run /gc.
--]]

function utilities.Reminder()
    local iTestVal = gProfile.settings.Reminder.MinBasetime;
    local iNow = os.time();

    -- Skip reminder if /gc has been run or the reminder has been disabled
    if gear.fHasGCBeenRun() == true or
        (gProfile.settings.Reminder.Enabled ~= nil and gProfile.settings.Reminder.Enabled == false) then
        return;
    end

    if gProfile.system_settings.bGCReminder == true then
        -- Since reminder already shown once, change the wait
        -- interval from 15 seconds to 5 minutes
        iTestVal = gProfile.settings.Reminder.MaxBasetime;
    end

    if os.difftime(iNow,utilities.basetime) >= iTestVal then
        print(chat.message('************'));
        if iTestVal == gProfile.settings.Reminder.MinBasetime then
            print(chat.message('FYI: Remember to do a /gc once \'data download\' finishes'));
        else
            print(chat.message('FYI: Remember to do a /gc'));
        end
        print(chat.message('************'));
        gProfile.system_settings.Reminder.bGCReminder = true;
        -- Change the base to current so that comparison is from now forward
        utilities.basetime = iNow;
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
        print(chat.message(string.format('%d %s processed...',iCnt,sWhat)));
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
        if sStyle == gVars._SLOT_FA then
            sTmp = string.upper(string.sub(sWord,1,1)) .. string.lower(string.sub(sWord,2,-1));
        elseif sStyle == gVars._SLOT_LA then
            sTmp = string.lower(sWord);
        elseif sStyle == gVars._SLOT_UA then
            sTmp = string.upper(sWord);
        end
    end

    return sTmp;
end     -- utilities.fFormattedWord

--[[
    ToggleState toggles on/off a feedback mechanism for all luashitacast commands
--]]

function utilities.ToggleState(toggle, status)
    if toggle ~= nil and status ~= nil then
        print(chat.message('Info: ' .. toggle .. ' is now ' .. tostring(status)))
    end
end		-- utilities.ToggleState

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
    fCheckTimeList determines if any of the passed named time periods is valid

    Parameter
        sList       One of more named time periods delimited by commas

    Return
        T/F
--]]

function utilities.fCheckTimeList(sList)
    local timestamp = gData.GetTimestamp();
    local t = utilities.fSplitStringByDelimiter(sList,',');

    for i,j in pairs(t) do
        if utilities.fCheckTime(timestamp.hour,j) == true then
            return true;
        end
    end
    return false;
end     -- utilities.fCheckTimeList

--[[
    fGetPartyCount determines how many charaters are in your party.

    Parameter
        bAlliance   T/F Should whole alliance be processed

    Return
        #   1 if solo, but up to 6 (or 18 if alliance count wanted)
--]]

function utilities.fGetPartyCount(bAlliance)
    local party = AshitaCore:GetMemoryManager():GetParty();
    local partyCount = 0;
    local PartySize = 6;

    bAlliance = bAlliance or false;

    if bAlliance then
        PartySize = 18;
    end

    for i = 0, PartySize-1 do
        if party and party:GetMemberIsActive(i) == 1 then
            partyCount = partyCount + 1
        end
    end
    return partyCount;
end     -- utilities.fGetPartyCount

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

    local s = gVars._SLOT_LA .. ',' .. gVars._SLOT_UA .. ',' .. gVars._SLOT_FA .. ',' .. gVars._SLOT_N;
    if sFmt == nil or string.find(s,sFmt) == nil then
        sFmt = gVars._SLOT_FA;      -- Unknown or missing code, assume Upper first letter and lower rest
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
                slots[i]['Have'] = true;
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
    local sValidSlotOutputTypes = gVars._SLOT_LA .. ',' .. gVars._SLOT_UA .. ',' .. gVars._SLOT_FA .. ',' .. gVars._SLOT_N;
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
        sType = gVars._SLOT_LA;       -- assume lowercase slot name
    end

    val = string.lower(val);             -- Make sure lowercase

    for i,j in pairs(slots) do
        -- Since we don't know if the passed in value is a string or a number or a number passed in as a string...
        if (type(val) == 'string' and (j['aSlot'] == val or j['nSlot'] == tonumber(val)) or (type(val) == 'number' and j['nSlot'] == val)) then
            -- There was a match, format the result accordingly
            local rVal;
            if sType == gVars._SLOT_FA then      -- Mixed case: Upper first letter, lower rest
                rVal = j['fSlot'];
            elseif sType == gVars._SLOT_LA then  -- lowercase name
                rVal = j['aSlot'];
            elseif sType == gVars._SLOT_N then  -- lowercase name
                rVal = j['nSlot']
            elseif sType == gVars._SLOT_UA then  -- uppercase name
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
    fGetAllGearSetNames generates a table listing all the gear set names only omitting
    'Progressive' and 'CurrentGear', the former being a weird reference set and the
    latter being a dynamically populated set. All sets identified in both the job file
    and crossjobs will be specified.

    Returned:
        Table containing all of the gear sets

    Note: Some sets perceived by players aren't real sets (like HELM and FISH), these
    are pseudo subsets that are generated from a master set (in this case, Gathering.)
    Pseudo sets will not be listed, only the underlying real set.
--]]

function utilities.fGetAllGearSetNames()
    local t = {};
    local lv;

    -- First, the job file
    for k,l in pairs(gProfile.Sets) do
        lv = string.lower(k);
        if lv ~= 'progressive' then
            table.insert(t,k);
        end
    end

    -- Now the crossjobs file
    for k,l in pairs(crossjobs.Sets) do
        lv = string.lower(k);
        if lv ~= 'currentgear' then
            table.insert(t,k);
        end
    end

    return t;
end     -- utilities.fGetAllGearSetNames

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
    if string.find(sName2,'gProfile.sets.') ~= nil then
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
            if string.lower(k) == sName2 then
                return l;
            end
        end
    end

    return nil;
end     -- utilities.fGetTableByName

--[[
    fBuffed determines if the player has the buff/debuff or not. The passed buff name
    can be a substring, but make sure the that's all that is needed to uniquely identify
    the buff.

    Parameters:
        sCode               Name of the buff/debuff
        bStart      T/F     sCode must match from start?

    Returned:
        True or false if buff/debuff found

    Note: if "_" found in buff name, check for both with and without "_"
--]]

function utilities.fBuffed(sCode,bStart)
    local buffs = AshitaCore:GetMemoryManager():GetPlayer():GetBuffs();
    local pos,pos2,sCode2;

    if bStart == nil then
        bStart = false;
    end

    sCode = string.lower(sCode);
    sCode2 = string.gsub(sCode,'_',' ');
    for _, buff in pairs(buffs) do
        local buffString = AshitaCore:GetResourceManager():GetString("buffs.names", buff);

        if (buffString) then
            pos = string.find(string.lower(buffString),sCode);
            pos2 = string.find(string.lower(buffString),sCode2);
            if pos ~= nil or pos2 ~= nil then
                if (bStart == true and (pos == 1 or pos2 == 1)) or (bStart == false) then
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

    if targetIndex ~= 0 and targetIndex ~= nil then
        sTxt = nil;
        if string.find('BST,SMN,PUP',player.MainJob) ~= nil then
            if gData.GetPet() ~= nil then
                sTxt = '/pet assault <t>';
            else
                if gProfile.settings.bConfirmation == true then
                    print(chat.message('Info: No pet found, assuming a normal pull'));
                end
            end
        end

        if sTxt == nil then
            if bRangeOrThrowing() == true then
                sTxt = '/ra <t>';
            else
                if gProfile.settings.bConfirmation == true then
                    print(chat.message('Info: No ranged device equipped to pull with'));
                    return
                end
            end
        end

        if utilities.fGetToggle(gVers._SPF) == true then
            local sMsg = '/p Pulling ' .. targetEntity.Name .. ' [' .. utilities.fTargetId(targetIndex) .. ']';
            AshitaCore:GetChatManager():QueueCommand(-1, sMsg);
        end
        AshitaCore:GetChatManager():QueueCommand(-1, sTxt);
    else
        print(chat.message('Warning: Unable to pull anything, no target selected'));
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
    for i,desc in pairs(gVars.STORAGES) do
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
                    if table.find(gVars.EQUIPABLE_LIST,desc['id']) then
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
    for i,desc in pairs(slips.tClaimSlips) do
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
    for i,j in ipairs(locks.tSlotLocks) do
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

    if bActual == true or gProfile.system_settings.PlayerCappedLevel == 0 then
        -- Actual max level wanted
        return player.MainJobSync;
    else
        -- Player capped level wanted unless the sync level is lower
        if player.MainJobSync < gProfile.system_settings.PlayerCappedLevel then
            return player.MainJobSync;
        else
            return gProfile.system_settings.PlayerCappedLevel;
        end
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
    local sWeak;
    local sDay = sEnvironment.DayElement;
    local PctDay = 0;
    local PctWeather = 0;

    -- Make sure a valid element specified
    if ele == nil or string.find(gVars._AllElements,ele) == nil then
        return nil;
    end

    ele = string.lower(ele);
    sWeak = gVars.tElemental_gear['staff'][ele]['Weak'];    -- Elemental weakness tracked here

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

function SetAliasAll()

    for _, v in ipairs(gVars.AliasList) do
        AshitaCore:GetChatManager():QueueCommand(-1, '/alias /' .. v .. ' /lac fwd ' .. v);
    end
end		-- SetAliasAll

--[[
    SetAliasCC registers all custom conditional commands
--]]

function SetAliasCC()

    for _, v in ipairs(gProfile.CustomConditionals) do
        local lv = string.lower(v['code']);
        AshitaCore:GetChatManager():QueueCommand(-1, '/alias /' .. lv .. ' /lac fwd ' .. lv);
    end
end		-- SetAliasCC

--[[
    ClearAliasAll removes the luashitacast commands that were registered here
--]]

function utilities.ClearAliasAll()
    for _, v in ipairs(gVars.AliasList) do
        AshitaCore:GetChatManager():QueueCommand(-1, '/alias del /' .. v);
    end
end		-- utilities.ClearAliasAll


--[[
    ClearAliasCC removes all custiom conditional commands that were registered here
--]]

function utilities.ClearAliasCC()
    for _, v in ipairs(gProfile.CustomConditionals) do
        local lv = string.lower(v['code']);
        AshitaCore:GetChatManager():QueueCommand(-1, '/alias del /' .. lv);
    end
end		-- utilities.ClearAliasCC

--[[
    Initialize gives luashitacast it's initial settings
--]]

function utilities.Initialize()
    gear.TallyProgressiveCaps();
    displaybar.InitializeDisplayBar:once(2);
    crossjobs.SetVariables:once(2);
    SetAliasAll:once(2);
    SetAliasCC:once(2);
end		-- utilities.Initialize

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

    if gProfile.settings.WScheck == true and
       tonumber(target.Distance) > gProfile.settings.WSdistance then
        print(chat.message('Warning: Distance to mob is too far! Move closer to target'));
        bGood = false;
    elseif player.TP <= 999 then
        print(chat.message('Warning: insufficient TP to weapon skill'));
        bGood = false;
    elseif buff_manager.has('SLEPT,PETRIFIED,STUNNED,AMNESIA,CHARMED') == true then
        print(chat.message('Warning: detrimental debuff inhibiting any action'));
        bGood = false;
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

    return (string.find(gVars._sMagicjobs,player.SubJob) ~= nil);
end		-- utilities.fMagicalSubJob

--[[
    fMagicMainJob determines if the sub job can do magic

    Returned
        T/F
--]]

function utilities.fMagicalMainJob()
    local player = gData.GetPlayer();

    return (string.find(gVars._sMagicjobs,player.MainJob) ~= nil);
end		-- utilities.fMagicalMainJob

--[[
    fMagicalJob determines if the player's job or subjob is magical
--]]

function utilities.fMagicalJob()
    local player = gData.GetPlayer();

    return (string.find(gVars._sMagicJobs,player.MainJob) ~= nil or string.find(gVars._sMagicJobs,player.SubJob) ~= nil);
end     -- utilities.fMagicalJob

--[[
    NewFileName generates a new file name based on the player's character name, job, and
    the date.

    Returned
        Generated name
--]]

function utilities.fNewFileName()
    local player = gData.GetPlayer();
    local sName = string.format('%s_%s_%x.txt',string.upper(player.Name),player.MainJob,os.clock);

    return sName;
end     -- utilities.fNewFileName

--[[
    fOpenByFilename will open the passed file name (in the reports directory
    or the directory if a path provided) in the mode specified.

    Parameter
        fName       File name of the report to create/append
        bAppend     Append or overwrite mode

    Returned
        fptr        Pointer to opened file

    Note: All files are opened in the "game path"/config/addons/LuAshitacast/Reports/
    directory unless a path is explicitly identified on the file name
--]]

function utilities.fOpenByFilename(fName,bAppend)
    local fptr;
    local sPath = string.format('%sconfig\\addons\\luashitacast\\Reports', AshitaCore:GetInstallPath());
    local sTemp,sOp;

    -- See if the file name already has a path in it's name
    if string.find(fName,'\\') ~= nil or string.find(fName,'/') ~= nil then
        -- Ok, contains a path
        sTemp = fName;
    else
        -- No path, make sure that the Reports directory exists
        ashita.fs.create_directory(sPath);
        sTemp = sPath .. '\\' .. fname;
    end

    if bAppend == true then
        sOp = 'a';
    else
        sOp = 'w+';
    end

    fptr = io.open(sTemp,sOp);
    return fptr;
end     -- utilities.fOpenByFilename

-- The following functions were copied or modified from code found in:
-- https://snippets.bentasker.co.uk/posts/lua. I didn't see the need to
-- create my own versions.

--[[
    fLtrim will remove leading spaces from the passed string.

    Parameter
        s       String to have leading spaces removed

    Returned
        Trimmed string
--]]

function utilities.fLtrim(s)
    if s == nil then
        return nil;
    end

    return s:match'^%s*(.*)';
end     -- utilities.fLtrim

--[[
    fRtrim will remove trailing spaces from the passed string.

    Parameter
        s       String to have trailing spaces trimmed

    Returned
        Trimmed string
--]]

function utilities.fRtrim(s)
    if s == nil then
        return nil;
    end

    return s:match'^(.*%S)%s*$';
end     -- utilities.fRtrim

--[[
    fTrim will remove leading and trailing spaces from the passed string.

    Parameter
        s       String to have leading and trailing spaces trimmed

    Returned
        Trimmed string
--]]

function utilities.fTrim(s)
    if s == nil then
        return nil;
    end

    return s:match'^()%s*$' and '' or s:match'^%s*(.*%S)';
end     -- utilities.fTrim

--[[
    fSplitStringByDelimiter takes the passed in string and creates a table split by the
    specified delimiter. Returned is the table of split values, minus the delimiters.

    Parameter
        s       String to be split
        delim   Delimiter to split on

    Returned
        Table of split values
--]]

function utilities.fSplitStringByDelimiter(s,delim)
    local t = {}

    if delim == nil then
        delim = '//';
    end

    for substr in string.gmatch(s, "[^".. delim.. "]*") do
        if substr ~= nil and string.len(substr) > 0 then
            table.insert(t,substr);
        end
    end

    return t;
end     -- utilities.fSplitStringByDelimiter

--[[
    fAccEnabled determines if any stage of the specified accuracy type has
    been enabled.

    Returned:
        True/False
--]]
function utilities.fAccEnabled(sType)
    local bTank = utilities.fGetToggle(gVars._TANK);

    if sType == nil then
        sType = 'Accuracy';
    end

    if bTank == true then
        sType = 'Tank_' .. sType;
    end

    return(gVars.tProgressive[sType]['CurStage'] > 0);
end     -- utilities.fAccEnabled

--[[
    bRangeOrThrowing determines if the player has valid equipment in their "Range" and/or "ammo"
    slots to range attack the target.

    Returned:
        True/False

    Note: This function only checks for archery/marksmanship and throwing. It is assumed that
    pet pulling will be done somewhere else.

    Note 2: A lot is assumed about the client like mixing bolts with bows will not work. Also,
    while there are specific weapon files for bolts and arrows, I'll be checking a composite
    called ammo. So yes, it's possible you'll trick the function into assuming all is good when
    it's not, but the client will let you know.
--]]

function bRangeOrThrowing()
    local targetIndex = gData.GetTargetIndex();
    local tEntity = gData.GetEntity(targetIndex);
    local ew = gData.GetEquipment();
    local bGood = false;

    -- Make sure the player is targetting something
    if targetIndex ~= 0 and targetIndex ~= nil then
        print(chat.message('Warning: No target selected. Cancelling'));
        return false;
    end

    -- Make sure the player has "some" ranged weapon
    if ew['Range'].Name == nil and ew['Ammo'].Name == nil then
        print(chat.message('Warning: No ranged gear equipped. Cancelling'));
        return false;
    end

    -- Make sure these gear types are loaded
    utilities.GetWeaponsList('ARCHERY');
    utilities.GetWeaponsList('MARKSMANSHIP');
    utilities.GetWeaponsList('AMMO');
    utilities.GetWeaponsList('THROWING');

    -- Query each type for identification
    local b1 = (table.find(crossjobs.WeaponTypes['ARCHERY'],string.lower(ew['Range'].Name)) ~= nil);
    local b2 = (table.find(crossjobs.WeaponTypes['MARKSMANSHIP'],string.lower(ew['Range'].Name)) ~= nil);
    local b3 = (table.find(crossjobs.WeaponTypes['AMMO'],string.lower(ew['Ammo'].Name)) ~= nil);
    -- Gear for throwing can be in either slot
    local b4 = (table.find(crossjobs.WeaponTypes['THROWING'],string.lower(ew['Range'].Name)) ~= nil);
    local b5 = (table.find(crossjobs.WeaponTypes['THROWING'],string.lower(ew['Ammo'].Name)) ~= nil);

    if b1 == true or b2 == true and b3 == false then
        print(chat.message('Warning: Ranged and Ammo gear an invalid pair. Cancelling'));
        return false;
    end

    -- Archery and Ammo or marksmanship and ammo or throwing in range slot or ammo slot
    bGood = ((b1 == true and b3 == true) or (b2 == true and b3 == true) or b4 == true or b5 == true);

    return bGood;
end       -- bRangeOrThrowing

--[[
    GetWeaponsList determines if the specified weapon type has been loaded or not. If it hasn't,
    then it gets the list of all valid weapons from the appropriately names weapontype file.

    Parameter
        sType       valid type of weapons
--]]

function utilities.GetWeaponsList(sType)

    if sType == nil then
        return;
    end

    local osType = sType;
    sType = string.upper(sType);
    if crossjobs.WeaponTypes[sType] ~= nil then
        -- definition already loaded
        return;
    end

    local path = string.format('config/addons/LuAshitacast/common/WeaponTypes/%s.lua', string.lower(sType));
    if ashita.fs.exists(path) then
        local success, loadError = loadfile(path);
        if not success then
            reporting.DisplayOnce(string.format('Warning: Failed to load resource file: %s', path));
            reporting.DisplayOnce('Warning: ' .. loadError);
            return;
        end
        local result, output = pcall(success);
        if not result then
            -- unable to process the weapons type file
            reporting.DisplayOnce(string.format('Warning: Failed to call resource file: %s', path));
            reporting.DisplayOnce('Warning: ' .. loadError);
            return;
        end

        crossjobs.WeaponTypes[sType] = output.wt[sType];
    end
end     -- utilities.GetWeaponsList

--[[
    fGetMobType determines if the player's target is of the passed type. If that type is not defined,
    it will update the master list accordingly. Note: the master list is based on zone id's. Only one
    zone will be defined at a time.

    Parameter
        sType   Type of monster being checked

    Returned
        T/F, was the target of the specified type or no target selected?

    This has changed. Instead of loading a zone, check for the existance of the fam/eco
    in the global variable. If absent, load it. Then check for the specified target type.
--]]

function utilities.fGetMobType(sType)
    local curr = AshitaCore:GetMemoryManager():GetParty():GetMemberZone(0);
    local targetIndex = gData.GetTargetIndex();
    local tEntity = gData.GetEntity(targetIndex);
    local iPos = nil;
    local sRest;
    local bFamily = false;

    if tEntity == nil or tEntity.Name == nil then
        -- no target, might need to change in the future. Beneficial spells default to <me>
        return false;
    end

    iPos = string.find(sType,':');
    if iPos ~= nil then
        bFamily = (string.sub(sType,1,iPos) ~= 'fam:');
        sRest = string.sub(sType,iPos+1,-1);
    end

    if curr ~= crossjobs.CurrentZone then
        crossjobs.ZoneList = {};
        local path = string.format('%sconfig/addons/luAshitacast/common/MobDB/%u.lua', AshitaCore:GetInstallPath(), curr);
        if (ashita.fs.exists(path)) then
            local success, loadError = loadfile(path);
            if not success then
                reporting.DisplayOnce(string.format('Warning: Failed to load resource file: %s', path));
                reporting.DisplayOnce('Warning: ' .. loadError);
                return false;
            end
            local result, output = pcall(success);
            if not result then
                -- unable to process the weapons type file
                reporting.DisplayOnce(string.format('Warning: Failed to call resource file: %s', path));
                reporting.DisplayOnce('Warning: ' .. loadError);
                return false;
            end
        end
        crossjobs.CurrentZone = curr;
        crossjobs.ZoneList = output.Names;
    end

    sType = string.lower(sType);
    for i,j in pairs(crossjobs.ZoneList) do
        if (bFamily == true and string.find(j['Family'],sType) ~= nil) or
            (bFamily == false and string.find(j['Ecosystem'],sType) ~= nil) then
            return true;
        end
    end

    return false;
end     -- utilities.fGetMobType


--[[
    fValidCustomCommand determines if the passed command is a custom conditional code.
--]]

function utilities.fValidCustomCommand(cmd)
    local bValid = false;

    if cmd == nil then
        return false;
    end

    for _,j in ipairs(gProfile.CustomConditionals) do
        if string.upper(j['code']) == string.upper(cmd) then
            bValid = true;
            break;
        end
    end

    return bValid;
end		-- utilities.fValidCustomCommand

--[[
    fJobInParty determines if any player in the party is of the passed in job acronym

    Parameter
        sVal        Job to check
        bNotMe      T/F should the invoking player be considered

    Return
        T/F
--]]

function utilities.fJobInParty(sList,bNotMe)
    local party = AshitaCore:GetMemoryManager():GetParty()
    sList = sList:upper();
    bNotMe = bNotMe or false;

    -- Walk the party
    for i = 0, 5 do
        -- Skip 0 if indicated
        if bNotMe == false or (bNotMe and i > 0) then
            local member = party:GetMemberProperty(i)
            -- if an active member of the party
            if member and member.Active == 1 then
                -- Convert the job ID to its 3-letter abbreviation
                local jobAbbrev = AshitaCore:GetResourceManager():GetString("jobs.names_abbr", member.MainJob)
                if string.find(sList,jobAbbrev:upper()) ~= nil then
                    return true
                end
            end
        end
    end
    return false;
end     -- utilities.fJobInParty

--[[
    fParseFileDesignation dissects the passed file designation and returns the name of the file and
    whether the new entries should be appended or not

    Parameter
        s       string to parse

    Returned
        sFile       file name
        bAppend     should statements be appended to the file

    Form: [file[=name]\][+] without the \. (Needed to escape the ] so lua wouldn't think that it's
    a close to a block comment.)
--]]

function utilities.fParseFileDesignation(s)
    local sName = nil;
    local bAppend = false;

    s = string.lower(s);
    local iPos = string.find(s,'file');
    local iPos2 = string.find(s,'=');
    local iPos3 = string.find(s,'%+');

    if iPos == nil then     -- "file" has to exist to get into this routine. How this would happen...
        return nil,nil;
    end

    bAppend = (iPos ~= nil);

    if iPos2 ~= nil then
        -- make surre not 'file=' or file+
        if iPos2+1 < string.len(s) and utilities.fTrim(string.sub(s,iPos+1,-1)) ~= '+' then
            sName = utilities.fTrim(string.sub(s,iPos+1,-1));
        else
            -- should have just been 'file'
            sName = utilities.fNewFileName();
        end
    else
        -- just a file designation
        sName = utilities.fNewFileName()
    end

    return sName,bAppend
end     -- utilities.fParseFileDesignation

--[[
    UpdateRegionalLabel determines what the label for the region control should be set to
--]]

function utilities.UpdateRegionalLabel()
    local currentZoneID = AshitaCore:GetMemoryManager():GetParty():GetMemberZone(0);
    local currentZoneName = AshitaCore:GetResourceManager():GetString('zones.names', currentZoneID);

    -- Make sure the player's nation is known
    if crossjobs.OwnNation == -1 then
        crossjobs.OwnNation = AshitaCore:GetMemoryManager():GetPlayer():GetNation() + 1;
    end

    -- Determine if current zone in region controlled by player's nation
    for i,j in pairs(gVars.RegionControl) do
        if table.find(j['zones'],currentZoneID) ~= nil then
            if j['own'] == crossjobs.OwnNation then
                gVars.sRegion = gVars._REGION_STATUS_OWNED;
            elseif j['own'] ~= crossjobs.OwnNation and j['own'] > 0 then
                gVars.sRegion = gVars._REGION_STATUS_NOT_OWNED;
            elseif j['own'] == gVars._REGION_NA then
                if i == 'Jeuno' or i == 'Dynamis' then
                   gVars.sRegion = gVars._REGION_STATUS_NA_NOT_OWNED;
                else
                   gVars.sRegion = gVars._REGION_STATUS_NA;
                end
            else        -- Unknown
                gVars.sRegion = gVars._REGION_STATUS_UNKNOWN;
            end
        else
            -- Region not in master list
            gVars.sRegion = gVars._REGION_STATUS_UNKNOWN;
        end
    end
end     -- utilities.UpdateRegionalLabel

--[[
    fIsDisplaybarSettingValid determines if the specified field in the display bar is defined

    Parameters:
        bBar1       Is it in bar 1?
        sName       Name of setting
--]]

function utilities.fIsDisplaybarSettingValid(bBar1,sField)
    local sBar;

    if bBar1 == nil or sField == nil then
        return false;
    end

    if bBar1 == true then
        sBar = gVars._BAR1;
    else
        sBar = gVars._BAR2;
    end

    return (gProfile.settings.DisplayBar[sBar] ~= nil and gProfile.settings.DisplayBar[sBar][sField] ~= nil);
end     -- utilities.fIsDisplaybarSettingValid

--[[
    fIsVisible determines if the passed setting name's visibility is true

    Parameters:
        bBar1       Is it in bar 1?
        sName       Name of setting

    Returned:
        T/F         Is it visible
--]]

function utilities.fIsVisible(sBar1,sName)
    local sBar;

    if bBar1 == true then
        sBar = gVars._BAR1;
    else
        sBar = gVars._BAR2;
    end

    return (gProfile.settings.DisplayBar[sBar][sName]);
end     -- utilities.fIsVisible


--[[
    CopyDisplaybarSettings makes a copy of the displaybar settings found in the job file. This is needed if the
    player wants to reset the displaybar back to how it was before interactive modifications.
--]]

function utilities.CopyDisplaybarSettings()

    for i,j in pairs(gProfile.settings.Displaybar) do
        gVars.Displaybar[i] = j;
    end
end     -- utilities.CopyDisplaybarSettings

--[[
    fCheckMagicJob determines if the player's character can cast magic
--]]

function utilities.fCheckMagicJob()
    local player = gData.GetPlayer();

    if string.find(gVars._sMagicjobs,player.MainJob) ~= nil or
        string.find(gVars._sMagicjobs,player.SubJob) ~= nil then
        return true;
    else
        return false;
    end
    return false;
end     -- fCheckMagicJob

--[[
    fIsGearsetDetailsFound determines if the passed in gear item exists in the passed in gearset item list for the
    in common slot.

    Parameter:
        sName       Name of item
        gsRec       Reference to the list of gear for the in common slot

    Returned:
        T/F         Was the record found
--]]

function utilities.fIsGearsetDetailsFound(sName,gsRec)

    for _,j in pairs(gsRec) do
        if j == sName then
            return true;
        end
    end

    return false;
end     -- utilities.fIsGearsetDetailsFound

--[[
    fGetCCDescription takes the passed custom conditional code and returns the associated question

    Parameter:
        ccc     Custom conditional code

    Returned:
        Associated question for the passed code
--]]

function utilities.fGetCCDescription(ccc)
    local uc = string.upper(ccc);

    for i=1,#gProfile.CustomConditionals,1 do
        if uc == gProfile.CustomConditionals[i]['code'] then
            return gProfile.CustomConditionals[i]['question'];
        end
    end

    return('Not found');
end     -- utilities.fGetCCDescription

--[[

    fFindCommand determines if the passed command is valid and returns the appropriately formatted
    index for the command

    Parameter:
        sCmd        Command to check

    Returned:
        toggle index
--]]

function utilities.fFindCommand(sCmd)

    if sCmd == nil then
        return nil;
    else
        sCmd = string.lower(sCmd);
    end

    for i,j in pairs(gVars.Toggles) do
        if string.lower(i) == sCmd then
            return i;
        end
    end

    return nil;
end

--[[
    fCheckDisplayFieldValidity determines if the passed field name is a valid field name in the displaybar and
    returns the actual index if a match is found.

    Parameter:
        s   name of field to look for

    Returned:
        field index name or nil
--]]
function utilities.fCheckDisplayFieldValidity(s)

    if s == nil then
        return nil;
    end

    -- Walk through the DisplayBar settings looking for a match

    for i,j in pairs(gProfile.settings.DisplayBar) do
        if j['tag'] ~= nil and j == s then
            return i;
        end
    end

    return nil;
end

--[[
    fWhichWSStat determines which known stat(s) are associated with the passed in weaponskill and
    returns the appropriate reference name.

    Parameter
        Name        Name of the weaponskill

    Return
        weaponskill Type
--]]

function utilities.fWhichWSStat(name)

    if name == nil then then
        return nil,'unknown';
    end

    name = string.lower(name);
    for i,j in pairs(gVars.tWeaponSkills) do
        if table.find(j,name) ~= nil then
            return i,name;
        end
    end
    return nil,name;
end     -- utilities.fWhichWSStat

--[[
    SetJob determines if NON is returned and sets the main job based on the system setting
    found in the job file

    Return
        Player's main job
--]]

function utilities.SetJob()
    local player = gData.GetPlayer();

    if (player.MainJob == nil or player.MainJob == 'NON') and gProfile.system_settings.job ~= nil then
        player.Mainjob = gProfile.system_settings.job;
    end

    return player;
end     -- utilities.SetJob

--[[
    is_busy determines if the player is doing some action like fishing, gathering, or crafting.

    Parameter
        bNot    Should the result be inverted

    Return
        True if they are, false if not

    This is missing buff definition. function should go in the buff_manager file
--]]

function utilities.fIsBusy(bNot)
    local player = AshitaCore:GetMemoryManager():GetPlayer()
    local bBusy = false;

    if not player then return false end

    if bNot == nil then
        bNot = false
    end

    -- 1. Check Character Status (4 = clamming/event lock, 5 = Crafting, 45 = Fishing)
    local status = player:GetStatus()
    if status == 4 or status == 5 or status == 45 then
        bBusy = true;
    end

    -- 2. Check for gathering buff lock (254 = Logging/Mining/Harvesting)
    if bBusy == false and buffs and buffs.has(254) then
        bBusy = true;
    end

    if bNot then
        bBusy = not bBusy;
    end

    return bBusy;
end     -- utilities.fIsBusy

return utilities;
