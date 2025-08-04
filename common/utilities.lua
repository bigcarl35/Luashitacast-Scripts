local utilities = {};

version = {
    ['author']	= 'Paiine',
    ['name']	= 'Luashitacast (Karma)',
    ['version']	= '2.0.alpha'
};

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
local utilities._SLOT_LA = 'LA';     -- lowercase slot name
local utilities._SLOT_UA = 'UA';     -- uppercase slot name
local utilities._SLOT_N  = 'N';      -- numeric
local utilities._SLOT_FA = 'FA';     -- formatted output: first letter uppercase, rest lowercase

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
    local sValidSlotInputTypes = utilities._SLOT_LA .. ',' .. utilities._SLOT_UA .. ',' .. utilities._SLOT_N;
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

    if sType == nil or string.find(sValidSlotInputTypes,sType) == nil then
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
            elseif sType == utilities._SLOT_UA then  -- uppercase name
                rVal = string.upper(j['aSlot']);
            else                                    -- numeric
                rVal = j['nSlot'];
            end
            return rVal;
        end
    end

    -- If you got here it wasn't found
    print(chat.header('fTranslateWhichSlot'):append(chat.message('Unrecognized slot: ' .. val .. 'Skipping...')));
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
    fGetTableByName returns the gear set that is associated with the set name passed to it.
    It does this by walking the Sets (either gProfile.Sets or gcinclude.Sets)

    Returned: gearset/nil
--]]

function utilities.fGetTableByName(sName)
    local s,s2;
    local sName2;

    sName2 = string.lower(sName);
    s = string.find(sName2,'gcinclude');
    if s == nil then
        for k,l in pairs(gProfile.Sets) do
            if string.lower(k) == sName2 then
                return l;
            end
        end
    else
        s2 = string.sub(sName2,s+2,-1);
    end

    if s2 == nil then
        s2 = sName2;
    end

    for k,l in pairs(gcinclude.Sets) do
        if string.lower(k) == s2 then
            return l;
        end
    end

    return nil;
end     -- utilities.fGetTableByName

--[[
    fMakeConditionalTable takes the passed, // delimited list and
    returns the individual codes in a table

    Parameters:
        sList       // delimited list of inline conditionals

    Returned:
        Table of the split apart inline conditionals, all uppercase
--]]

function utilities.fMakeConditionalTable(sList)
    local tTbl = {};
    local iPos;

    iPos = 1;		-- Assume start at first position
    while iPos ~= nil do
        -- Look for the next //
        iPos = string.find(string.sub(sList,3,-1),'//');
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

