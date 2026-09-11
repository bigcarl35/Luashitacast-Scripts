local buff_manager = {};

-- The master list is a combination of buffs and debuffs. Separation is in the eyes of the player, but since there's no overlap
-- in id's, a combined list is fine
local MASTER_LIST = {
    ['ADDLED'] = 21,           ['AFTERMATH'] = 274,   ['AMNESIA'] = 16,         ['ANCIENT_CIRCLE'] = 118, ['ARCANE_CIRCLE'] = 75,
    ['BANED'] = 30,            ['BARAERO'] = 102,     ['BARBLIND'] = 109,       ['BARBLIZZARD'] = 101,    ['BARFIRE'] = 100,
    ['BARPARALYZE'] = 108,     ['BARPETRIFY'] = 111,  ['BARPOISON'] = 107,      ['BARSILENCE'] = 110,     ['BARSLEEP'] = 106,
    ['BARSTONE'] = 103,        ['BARTHUNDER'] = 104,  ['BARVIRUS'] = 112,       ['BARWATER'] = 105,       ['BLINDED'] = 4,
    ['BOUND'] = 11,            ['BUSTED'] = 309,      ['CHARMED'] = 14,         ['CHOCOBO'] = 252,        ['COVER'] = 114,
    ['CURSED'] = 9,            ['DIA'] = 134,         ['DISEASED'] = 8,         ['DOOMED'] = 15,          ['ENAERO'] = 96,
    ['ENBLIZZARD'] = 95,       ['ENCUMBERED'] = 177,  ['ENCHANTMENT'] = 162,    ['ENDARK'] = 288,         ['ENFIRE'] = 94,
    ['ENLIGHT'] = 274,         ['ENSTONE'] = 97,      ['ENTHUNDER'] = 98,       ['ENWATER'] = 105,        ['FLED'] = 32,
    ['HASTE'] = 33,            ['HOLY_CIRCLE'] = 74,  ['IMPAIRED'] = 261,       ['INVISIBLE'] = 69,       ['KO'] = 0,
    ['LVL_RESTRICTION'] = 143, ['LVL_SYNC'] = 269,    ['MEDICATED'] = 155,      ['MUDDLED'] = 473,        ['MUTED'] = 29,
    ['PARALYZED'] = 4,         ['PETRIFIED'] = 7,     ['PLAGUED'] = 31,         ['POISONED'] = 3,         ['REPRISAL'] = 403,
    ['RERAISE'] = 113,         ['SANCTION'] = 256,    ['SHINING_RUBY'] = 154,   ['SIGNET'] = 253,         ['SILENCED'] = 6,
    ['SJ_RESTRICTION'] = 157,  ['SLEPT'] = 2,         ['SLOWED'] = 13,          ['SNEAK_ATTACK'] = 65,    ['STUNNED'] = 10,
    ['TERRIFIED'] = 28,        ['TRICK_ATTACK'] = 87, ['WARDING_CIRCLE'] = 117, ['WEAKNENED'] = 1,        ['WEIGHTED'] = 12,
    ['YONIN'] = 420,           ['DEDICATION'] = 249,  ['CHAIN_AFFINITY'] = 164, ['BURST_AFFINITY'] = 165
};

-- Some references refer to a list
local GROUPED_LIST = {
    ['BUFF'] = {
                MASTER_LIST['AFTERMATH'],MASTER_LIST['ANCIENT_CIRCLE'],MASTER_LIST['ARCANE_CIRCLE'],MASTER_LIST['BARAERO'],
                MASTER_LIST['BARBLIND'],MASTER_LIST['BARBLIZZARD'],MASTER_LIST['BARFIRE'],MASTER_LIST['BARPARALYZE'],
                MASTER_LIST['BARPETRIFY'],MASTER_LIST['BARPOISON'],MASTER_LIST['BARSILENCE'],MASTER_LIST['BARSLEEP'],
                MASTER_LIST['BARSTONE'],MASTER_LIST['BARTHUNDER'],MASTER_LIST['BARVIRUS'],MASTER_LIST['BARWATER'],
                MASTER_LIST['CHOCOBO'],MASTER_LIST['COVER'],MASTER_LIST['ENAERO'],MASTER_LIST['ENBLIZZARD'],
                MASTER_LIST['ENCHANTMENT'],MASTER_LIST['ENDARK'],MASTER_LIST['ENFIRE'],MASTER_LIST['ENLIGHT'],
                MASTER_LIST['ENSTONE'],MASTER_LIST['ENTHUNDER'],MASTER_LIST['ENWATER'],MASTER_LIST['FLED'],
                MASTER_LIST['HASTE'],MASTER_LIST['HOLY_CIRCLE'],MASTER_LIST['INVISIBLE'],MASTER_LIST['REPRISAL'],
                MASTER_LIST['RERAISE'],MASTER_LIST['SANCTION'],MASTER_LIST['SHINING_RUBY'],MASTER_LIST['SIGNET'],
                MASTER_LIST['SNEAK_ATTACK'],MASTER_LIST['TRICK_ATTACK'],MASTER_LIST['WARDING_CIRCLE'],MASTER_LIST['YONIN'],
                MASTER_LIST['DEDICATION'],MASTER_LIST['CHAIN_AFFINITY'],MASTER_LIST['BURST_AFFINITY']
               },
    ['DEBUFF'] = {
                MASTER_LIST['ADDLED'],MASTER_LIST['AMNESIA'],MASTER_LIST['BANED'],MASTER_LIST['BLINDED'],
                MASTER_LIST['BOUND'],MASTER_LIST['BUSTED'],MASTER_LIST['CHARMED'],MASTER_LIST['CURSED'],
                MASTER_LIST['DIA'],MASTER_LIST['DISEASED'],MASTER_LIST['DOOMED'],MASTER_LIST['ENCUMBERED'],
                MASTER_LIST['IMPAIRED'],MASTER_LIST['KO'],MASTER_LIST['LVL_RESTRICTION'],MASTER_LIST['LVL_SYNC'],
                MASTER_LIST['MEDICATED'],MASTER_LIST['MUDDLED'],MASTER_LIST['MUTED'],MASTER_LIST['PARALYZED'],
                MASTER_LIST['PETRIFIED'],MASTER_LIST['PLAGUED'],MASTER_LIST['POISONED'],MASTER_LIST['SILENCED'],
                MASTER_LIST['SJ_RESTRICTION'],MASTER_LIST['SLEPT'],MASTER_LIST['SLOWED'],MASTER_LIST['STUNNED'],
                MASTER_LIST['TERRIFIED'],MASTER_LIST['WEAKNENED'],MASTER_LIST['WEIGHTED']
                 },
    ['BARANY'] = {
                MASTER_LIST['BARAERO'], MASTER_LIST['BARBLIND'],MASTER_LIST['BARBLIZZARD'],MASTER_LIST['BARFIRE'],
                MASTER_LIST['BARPARALYZE'],MASTER_LIST['BARPETRIFY'],MASTER_LIST['BARPOISON'],MASTER_LIST['BARSILENCE'],
                MASTER_LIST['BARSLEEP'],MASTER_LIST['BARSTONE'],MASTER_LIST['BARTHUNDER'],MASTER_LIST['BARVIRUS'],
                MASTER_LIST['BARWATER']
                },
    ['BARELEMENTAL'] = {
                MASTER_LIST['BARAERO'],MASTER_LIST['BARBLIZZARD'],MASTER_LIST['BARFIRE'],MASTER_LIST['BARSTONE'],
                MASTER_LIST['BARTHUNDER'],MASTER_LIST['BARWATER']
                },
    ['BARSTATUS'] = {
                MASTER_LIST['BARBLIND'],MASTER_LIST['BARPARALYZE'],MASTER_LIST['BARPETRIFY'],MASTER_LIST['BARPOISON'],
                MASTER_LIST['BARSILENCE'],MASTER_LIST['BARSLEEP'],MASTER_LIST['BARVIRUS']
                },
    ['ENANY'] = {
                MASTER_LIST['ENAERO'],MASTER_LIST['ENBLIZZARD'],MASTER_LIST['ENDARK'],MASTER_LIST['ENFIRE'],
                MASTER_LIST['ENLIGHT'],MASTER_LIST['ENSTONE'],MASTER_LIST['ENTHUNDER'],MASTER_LIST['ENWATER']
                },
    ['UTSUSEMI'] = { 446,445,444,66 },  -- Utsusemi, shadows 4, 3, 2, 1
    ['IMAGERY'] =  { 235,236,237,238,239,240,241,242,243 }, -- Fishing, Woodworking, Smithing, Goldsmithing, Clothcraft, Leathercraft, Bonecraft, Alchemy, Cooking
    ['CONTROL'] = { MASTER_LIST['SIGNET'], MASTER_LIST['SANCTION'] },
};

local active_buffs = {};   -- table of currently active buff ids

--[[
    update_buff_list queries the player's buff list and saves the ids of any
    found into active_buffs
--]]

local function update_buff_list()
    local player = AshitaCore:GetMemoryManager():GetPlayer()
    if not player then return end

    active_buffs = {};

    for i = 0, 31 do
        local buff_id = player:GetBuff(i)
        if buff_id and buff_id ~= -1 and buff_id ~= 255 then
            active_buffs[buff_id] = true;
        end
    end
end     -- update_buff_list

--[[
    Parse_input takes the passed input, coverts it into a table of IDs.
    Input can be a number, a list of numbers (comma delimited), a name,
    or a mixture of names and numbers.

    Parameter
        input   either a number, list of numbers, name, or list of names

    Returned
        a table containing all the buff ids
--]]

local function parse_input(input)
    local tIDs = {};

    -- Parse comma delimited list of numbers and keywords
    for element in string.gmatch(input, "([^,]+)") do
        element = utilities.fTrim(element); -- Clean spaces

        local numeric_value = tonumber(element);
        if numeric_value then
            -- Since a number found, insert the number as a string or number
            table.insert(tIDs, numeric_value);
        else
            element = element:upper();  -- Index has to be uppercase
            -- since a string, look for match im MASTER_LIST
            local mapped_value = MASTER_LIST[element];
            if mapped_value then
                table.insert(tIDs, mapped_value);
            else
                -- Not in MASTER_LIST, how about GROUPED_LIST
                mapped_value = GROUPED_LIST[element];
                if mapped_value then
                    -- Append the values
                    for i = 1, #mapped_value do
                        table.insert(tIDs, mapped_value[i])
                    end
                else
                    reporting.DisplayOnce('Warning: Unrecognized buff reference: ' .. element .. ' in buff_manager.parse_input. Skipping',false);
                end
            end
        end
    end
    return tIDs;
end     -- parse_input

--[[
    "has" determines if the any of the passed buffs are active

    Parameter
        input   list of buffs/keywords comma delimited
        bAll    Do all buffs have to be present?
        bNot    Is inverted results wanted?

    Return
        T/F
--]]

function buff_manager.has(input,bAll,bNot)

    if input == nil then
        return false;
    end

    bAll = bAll or false;
    bNot = bNot or false;

    -- Refresh list whether something there or not
    update_buff_list();

    -- Edge case: Player has no buffs
    if active_buffs == nil or next(active_buffs) == nil then
        return false ~= bNot;
    end

    -- Track matches
    local matches_found = 0
    local total_required = 0

    -- Parse list of ids
    local tInput = parse_input(input);

    -- Count total required and see how many are active
    for _, v in pairs(tInput) do
        total_required = total_required + 1
        if active_buffs[v] ~= nil then
            matches_found = matches_found + 1
        end
    end

    -- Determine the normal result based on bAll flag
    local base_result = false
    if bAll then
        -- All requested buffs must be found
        base_result = (matches_found == total_required);
    else
        -- At least one requested buff must be found
        base_result = (matches_found > 0);
    end

    return (base_result ~= bNot);
end     -- buff_manager.has

--[[
    other_than behaves similarly to "has" except that the listed buffs/keywords
    are exceptions.

    Parameter
        input   list of buffs/keywords comma delimited

    Return
        T/F
--]]

function buff_manager.other_than(input)

    if input == nil then
        return false;
    end

    -- Refresh list whether something there or not
    update_buff_list();

    -- Edge case: Player has no buffs
    if active_buffs == nil or next(active_buffs) == nil then
        -- Even though excluded buffs not present, at least one other buff needed for true
        return false;
    end

    -- Parse list of ids
    local tInput = parse_input(input);

    -- Walk list of buffs looking for exceptions
    for i,j in active_buffs do
        if table.find(tInput,i) == nil then
            return true;    -- Found a buff not in exception list
        end
    end

    return false;
return buff_manager;
