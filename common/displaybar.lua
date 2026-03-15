local displaybar = {};

local fonts = require('fonts');

--[[
    This component contains all routines that deal with the display bar

    List of routines-
        Subroutines:
            SetAccCur               Sets the appropriate tracking current stage
            Unload                  Unloads the font objects and registered events
            UpdateBarStatic         Updates the static portion of the display bar's variables

        Functions:
            local fAccuracyDisplay  Returns the colorized display of the appropriate accuracy
            fColor                  Formats passed string with passed color keyword
--]]

-- List of know color codes by keyword for displaying to the screen
local tkwEle = {	-- cOORRGGBB where OO is opacity, RR red, GG green, BB blue
    { ['kw'] = 'firesday',		 ['color'] = '|cFFFF0000|' }, -- red
    { ['kw'] = 'earthsday',		 ['color'] = '|cFFC19A6B|' }, -- camel
    { ['kw'] = 'watersday',		 ['color'] = '|cFF1F51FF|' }, -- neon blue
    { ['kw'] = 'windsday',		 ['color'] = '|cFF4CBB17|' }, -- kelly green
    { ['kw'] = 'iceday',		 ['color'] = '|cFF00FFFF|' }, -- aqua
    { ['kw'] = 'lightningday',   ['color'] = '|cE1C16EFF|' }, -- "light purple"
    { ['kw'] = 'lightsday', 	 ['color'] = '|cFFFFFFFF|' }, -- white
    { ['kw'] = 'darksday', 		 ['color'] = '|cFF71797E|' }, -- steel gray
    { ['kw'] = 'clear', 		 ['color'] = '|cFFA7C7E7|' }, -- pastel blue
    { ['kw'] = 'sunshine', 		 ['color'] = '|cFFFFEA00|' }, -- bright yellow
    { ['kw'] = 'clouds', 		 ['color'] = '|cFFFFFDD0|' }, -- cream
    { ['kw'] = 'fog',	 		 ['color'] = '|cFFB2BEB5|' }, -- ash gray
    { ['kw'] = 'fire', 			 ['color'] = '|cFFFF0000|' }, -- red
    { ['kw'] = 'fire x2', 		 ['color'] = '|cFFFF0000|' }, -- red
    { ['kw'] = 'water', 		 ['color'] = '|cFF1F51FF|' }, -- neon blue
    { ['kw'] = 'water x2', 		 ['color'] = '|cFF1F51FF|' }, -- neon blue
    { ['kw'] = 'earth', 		 ['color'] = '|cFFC19A6B|' }, -- camel
    { ['kw'] = 'earth x2', 		 ['color'] = '|cFFC19A6B|' }, -- camel
    { ['kw'] = 'wind',	 		 ['color'] = '|cFF4CBB17|' }, -- kelly green
    { ['kw'] = 'wind x2', 		 ['color'] = '|cFF4CBB17|' }, -- kelly green
    { ['kw'] = 'ice',	 		 ['color'] = '|cFF00FFFF|' }, -- aqua
    { ['kw'] = 'ice x2', 		 ['color'] = '|cFF00FFFF|' }, -- aqua
    { ['kw'] = 'thunder', 		 ['color'] = '|cE1C16EFF|' }, -- "light purple"
    { ['kw'] = 'thunder x2',	 ['color'] = '|cE1C16EFF|' }, -- "light purple"
    { ['kw'] = 'light', 		 ['color'] = '|cFFFFFFFF|' }, -- white
    { ['kw'] = 'light x2', 		 ['color'] = '|cFFFFFFFF|' }, -- white
    { ['kw'] = 'dark',	 		 ['color'] = '|cFF71797E|' }, -- steel gray
    { ['kw'] = 'dark x2', 		 ['color'] = '|cFF71797E|' }, -- steel gray
    { ['kw'] = 'full moon',		 ['color'] = '|cFFFFFFFF|' }, -- white
    { ['kw'] = 'waning gibbous', ['color'] = '|cFFE5E4E2|' }, -- platinum
    { ['kw'] = 'last quarter', 	 ['color'] = '|cFFC0C0C0|' }, -- silver
    { ['kw'] = 'waning crescent',['color'] = '|cFF848884|' }, -- smoke
    { ['kw'] = 'new moon', 		 ['color'] = '|cFF71797E|' }, -- steel gray
    { ['kw'] = 'waxing crescent',['color'] = '|cFF848884|' }, -- smoke
    { ['kw'] = 'first quarter',  ['color'] = '|cFFC0C0C0|' }, -- silver
    { ['kw'] = 'waxing gibbous', ['color'] = '|cFFE5E4E2|' }, -- platinum
    { ['kw'] = 'green',			 ['color'] = '|cFF00FF00|' }, -- green
    { ['kw'] = 'red',			 ['color'] = '|cFFFF0000|' }, -- red
    { ['kw'] = 'yellow', 		 ['color'] = '|cFFFFEA00|' }, -- bright yellow
};

-- Local variables used for display purposes
local MainLV = 0;
local SubLV = 0;
local Zone = ' ';
local Main = 'FOO';
local Sub = 'BAR';

-- Generic font settings for display
local fontSettings = {
    visible = true,
    font_family = 'Arial',
    font_height = 14,
    color = 0xFFFFFFFF,			-- White
    position_x = 325,
    position_y = 0,
    background = T{
        visible = true,
        color = 0xFF000000,		-- Black
    }
};

--[[
    UpdateBarStatic updates the current identifying aspects of the display bar data (specifically the
    player specific information)
--]]

function displaybar.UpdateBarStatic()
    local player = AshitaCore:GetMemoryManager():GetPlayer();
    local pEntity = AshitaCore:GetMemoryManager():GetEntity();
    local myIndex = AshitaCore:GetMemoryManager():GetParty():GetMemberTargetIndex(0);
    local currentZoneID = AshitaCore:GetMemoryManager():GetParty():GetMemberZone(0);
    local currentZoneName = AshitaCore:GetResourceManager():GetString('zones.names', currentZoneID);
    local MID = player:GetMainJob();
    local SID = player:GetSubJob();

    MainLV	= player:GetMainJobLevel();
    SubLV	= player:GetSubJobLevel();
    Main	= AshitaCore:GetResourceManager():GetString("jobs.names_abbr", MID);
    Sub		= AshitaCore:GetResourceManager():GetString("jobs.names_abbr", SID);
    Zone    = currentZoneName;
end		-- displaybar.UpdateBarStatic

--[[
    Unload removes the objects and commands created by the display bar code
--]]

function displaybar.Unload()

    -- Remove any dynamic objects
    if (displaybar.FontObject ~= nil) then
        displaybar.FontObject:destroy();
    end

    -- Unregister the displaybar and the toggle command for turning the display off
    ashita.events.unregister('d3d_present', 'displaybar_present_cb');
    ashita.events.unregister('command', 'displaybar_cb');
end		-- displaybar.Unload

--[[
    fWhichJobBar determines which record matches the passed in toggle

    Parameter
        s       Toggle name

    Returned
        The matching row pointer or nil
--]]

function fWhichJobBar(s)
    local bFound = false;

    for i,j in ipairs(JobBar) do
        if string.upper(j['Code']) == string.upper(s) then
            bFound = true;
            return j;
        end
    end

    if bFound == false then
        return nil;
    end
end     -- fWhichJobBar

--[[
    fColor will return the colorized string according to the keyword's color. All
    colors are denoted in hex

    Parameters
        skw         color keyword
        sMsg        message to be colorized

    Returned
        Formatted output
--]]

function displaybar.fColor(skw,sMsg)
    local sEnd = '|r';
    local sfColor;
    local sColor = nil;

    if skw == nil then
        return ' ';
    end

    skw = string.lower(skw);
    for i,j in pairs(tkwEle) do
        if j['kw'] == skw then
            sColor = j['color'];
        break;
        end
    end

    if sColor == nil then
        sfColor = sMsg;
    else
        sfColor = sColor .. sMsg .. sEnd;
    end
    return sfColor;
end		-- displaybar.fColor

--[[
    fAccuracyDisplay generates the accuracy listing highlighting what is currently enabled
    based on the passed sType. Returned is the generated colorized string.
--]]

function fAccuracyDisplay(sType)
    local msg;
    local which = {};

    if sType == nil then
        return "";
    end

    for i,j in pairs(gVars.tProgressive) do
        if string.lower(sType) == string.lower(j['Abbr']) then
            which = j;
        break;
        end
    end

    if which == nil then
        return "";
    else
        if which['MaxStage'] == 0 then
            msg = ' ';
        else
            for i=1,which['MaxStage'],1 do
                if i <= which['CurStage'] then
                    if msg == nil then
                        msg = displaybar.fColor('green',tostring(i))
                    else
                        msg = msg .. displaybar.fColor('green',',' .. tostring(i))
                    end
                else
                    if msg == nil then
                        msg = displaybar.fColor('red',tostring(i))
                    else
                        msg = msg .. displaybar.fColor('red',',' .. tostring(i))
                    end
                end
            end
        end
    end
    return msg;
end		-- fAccuracyDisplay

--[[
    fColorizedEntry displays the label based on the passed in value in the display bar
--]]

function fColorizedEntry(lbl,val)
    local s;
    if lbl == nil or val == nil then
        return;
    end

    if val == true then
        s = displaybar.fColor('green',lbl) .. ' ';
    else
        s = displaybar.fColor('red',lbl) .. ' ';
    end
    return s;
end     -- fColorizedEntry

--[[
    InitializeDisplayBar creates the display bar and assigns it to an event. It is
    only run once.
--]]

function displaybar.InitializeDisplayBar()
    local player = gData.GetPlayer();
    local cmn = { [1] = gVars._WSWAP, [2] = gVars._KITE , [3] = gVars._TH, [4] = gVars._TANK ,
                  [5] = gVars._IDLE, [6] = gVars._EVASION , [7] = gVars._SPF , [8] = gVars._GSWAP
    };
    local cc;

    displaybar.UpdateBarStatic();
    displaybar.FontObject = fonts.new(fontSettings);

    ashita.events.register('d3d_present', 'displaybar_present_cb', function ()
        local display = 'X:';

        -- Now process the tool bar
        if gProfile.settings.DisplayBar[gVars._JOB] ~= nil and gProfile.settings.DisplayBar[gVars._JOB]['visible'] == true then
            display = display .. MainLV .. Main .. '/' .. SubLV .. Sub .. ' ';
        end

        if gProfile.settings.DisplayBar[gVars._CAP] ~= nil and gProfile.settings.DisplayBar[gVars._CAP]['visible'] == true then
            display = display .. '| L.Cap: ';
            if gProfile.settings.PlayerCappedLevel > 0 then
                local stmp = tostring(gProfile.settings.PlayerCappedLevel);
                display = display .. fColorizedEntry(stmp,false) .. ' | ';
            else
                display = display .. fColorizedEntry('None',true) .. ' | ';
            end
        end

        if gProfile.settings.DisplayBar[gVars._GC] ~= nil and gProfile.settings.DisplayBar[gVars._GC]['visible'] == true then
            display = display .. fColorizedEntry('GC',gear.fHasGCBeenRun()) .. '| ';
        end

        -- Display the common (cross jobs) toggles
        for k,v in ipairs(cmn) do
            if gProfile.settings.DisplayBar[v] ~= nil and gProfile.settings.DisplayBar[v]['visible'] == true then
                display = display .. fColorizedEntry(v,utilities.fGetToggle(v)) .. ' ';
            end
        end

        -- Now the job or class specific toggles. Note that the logic for limiting what is displayed use to be
        -- a copy of what's in crossjobs.SetVariables(). This is no longer the case. Variables will be displayed
        -- in the displaybar based on the gProfile.settings.DisplayBar definitions in the job file. Thus, the
        -- player determines what is displayed and what is not displayed.
        if gProfile.settings.DisplayBar[gVars._MACC] ~= nil and gProfile.settings.DisplayBar[gVars._MACC]['visible'] == true then
            display = display .. fColorizedEntry(gVars._MACC,utilities.fGetToggle(gVars._MACC)) .. ' '; -- /MACC
        end

        if gProfile.settings.DisplayBar[gVars._SS] ~= nil and gProfile.settings.DisplayBar[gVars._SS]['visible'] == true then
            display = display .. fColorizedEntry(gVars._SS,utilities.fGetToggle(gVars._SS)) .. ' ';     -- /SS
        end

        if gProfile.settings.DisplayBar[gVars._AJUG] ~= nil and gProfile.settings.DisplayBar[gVars._AJUG]['visible'] == true then
            display = display .. fColorizedEntry(gVars._AJUG,utilities.fGetToggle(gVars._AJUG)) .. ' '; -- /AJug
        end

        if gProfile.settings.DisplayBar[gVars._DB] ~= nil and gProfile.settings.DisplayBar[gVars._DB]['visible'] == true then
            display = display .. 'DB: ' .. fColorizedEntry(utilities.fGetCycle(gVars._DB),true) .. ' '; -- /DB
        end

        if gProfile.settings.DisplayBar[gVars._SBP] ~= nil and gProfile.settings.DisplayBar[gVars._SBP]['visible'] == true then
            display = display .. fColorizedEntry(gVars._SBP,utilities.fGetToggle(gVars._SBP)) .. ' ';   -- /sBP
        end

        if gProfile.settings.DisplayBar[gVars._INSTRUMENT] ~= nil and gProfile.settings.DisplayBar[gVars._INSTRUMENT]['visible'] == true then
            display = display .. 'Instrument: ' .. fColorizedEntry(utilities.fGetCycle(gVars._INSTRUMENT),true) .. ' ';  -- /horn or /string
        end

        if gProfile.settings.DisplayBar[gVars._MODE] ~= nil and gProfile.settings.DisplayBar[gVars._MODE]['visible'] == true or
           gProfile.settings.DisplayBar[gVars._DT] ~= nil and gProfile.settings.DisplayBar[gVars._DT]['visible'] == true or
           gProfile.settings.DisplayBar[gVars._REGION] ~= nil and gProfile.settings.DisplayBar[gVars._REGION]['visible'] == true then
            display = display .. '| ';
        end

        if gProfile.settings.DisplayBar[gVars._MODE] ~= nil and gProfile.settings.DisplayBar[gVars._MODE]['visible'] == true then
            display = display .. 'Mode: ' .. fColorizedEntry(utilities.fGetCycle(gVars._MODE),true) .. ' ';  -- /Mode
        end

        -- and the last two all-job cycles
        if gProfile.settings.DisplayBar[gVars._DT] ~= nil and gProfile.settings.DisplayBar[gVars._DT]['visible'] == true then
            display = display .. 'DT: ' .. fColorizedEntry(utilities.fGetCycle(gVars._DT),true) .. ' ';    -- /dt
        end

        if gProfile.settings.DisplayBar[gVars._REGION] ~= nil and gProfile.settings.DisplayBar[gVars._REGION]['visible'] == true then
            local srColor;
            if gVars.sRegion == gVars._REGION_STATUS_MUST_ZONE then
                srColor = 'yellow';
            elseif gVars.sRegion == gVars._REGION_STATUS_OWNED then
                srColor = 'green';
            else
                srColor = 'red';
            end
            display = display .. 'Region: ' .. displaybar.fColor(srColor,gVars.sRegion) .. ' ';
        end

        -- Accuracy
        if gProfile.settings.DisplayBar[gVars._ACC] ~= nil and gProfile.settings.DisplayBar[gVars._ACC]['visible'] == true then
            if utilities.fGetToggle(gVars._TANK) == true then
                display = display .. '| Acc: ' .. fAccuracyDisplay('TAcc') .. ' ';
                if gProfile.settings.DisplayBar[gVars._RACC] ~= nil and gProfile.settings.DisplayBar[gVars._RACC]['visible'] == true then
                    display = display .. 'RAcc: ' .. fAccuracyDisplay('TRAcc') .. ' ';
                end
            else
                display = display .. '| Acc: ' .. fAccuracyDisplay('Acc') .. ' ';
                if gProfile.settings.DisplayBar[gVars._RACC] ~= nil and gProfile.settings.DisplayBar[gVars._RACC]['visible'] == true then
                    display = display .. 'RAcc: ' .. fAccuracyDisplay('RAcc') .. ' ';
                end
            end
        end

        -- Custom Conditionals
        if gProfile.settings.DisplayBar[gVars._CC] ~= nil and gProfile.settings.DisplayBar[gVars._CC]['visible'] == true then
            display = display .. '| CC: ';
            for i,j in ipairs(gProfile.CustomConditionals) do
                cc = utilities.fGetToggle(j['code']);
                if i > 1 then
                    display = display .. ',';
                end
                if cc == true then
                    display = display .. displaybar.fColor('green',i);
                else
                    display = display .. displaybar.fColor('red',i);
                end
            end
        end

        -- Locks
        if gProfile.settings.DisplayBar[gVars._LOCKS] ~= nil and gProfile.settings.DisplayBar[gVars._LOCKS]['visible'] == true then
            local s = locks.fCompactLocks();
            if s ~= 'None' then
                display = display .. ' | Locks: ' .. displaybar.fColor('red',s);
            else
                display = display .. ' | Locks: ' .. displaybar.fColor('green',s);
            end
        end

        local env = gData.GetEnvironment();
        if gProfile.settings.DisplayBar[gVars._DAY] ~= nil and gProfile.settings.DisplayBar[gVars._DAY]['visible'] == true then
            display = display .. ' | ' .. displaybar.fColor(env.Day,env.Day) .. ' ';
        end
        if gProfile.settings.DisplayBar[gVars._TIME] ~= nil and gProfile.settings.DisplayBar[gVars._TIME]['visible'] == true then
            display = display .. string.format('| %02d:%02d ',env.Timestamp.hour,env.Timestamp.minute) .. ' ';
        end
        if gProfile.settings.DisplayBar[gVars._MOON] ~= nil and gProfile.settings.DisplayBar[gVars._MOON]['visible'] == true then
            display = display .. string.format('| %d%% %s ',env.MoonPercent,displaybar.fColor(env.MoonPhase,env.MoonPhase)) .. ' ';
        end
        if gProfile.settings.DisplayBar[gVars._WEATHER] ~= nil and gProfile.settings.DisplayBar[gVars._WEATHER]['visible'] == true then
            display = display .. '| ' .. displaybar.fColor(env.RawWeather,env.RawWeather) .. ' ';
        end
        if gProfile.settings.DisplayBar[gVars._ZONE] ~= nil and gProfile.settings.DisplayBar[gVars._ZONE]['visible'] == true then
            display = display .. '| ' .. Zone;
        end

        displaybar.FontObject.text = display;
    end);
end		-- displaybar.InitializeDisplayBar

--[[
    SetAccCur sets the current stage level for the specified type of
    accuracy. The passed in value is checked versus the maximum to
    make sure the stage is valid.

    Parameters
        sType       Type of accuracy
        val         Value to set accuracy settiing to
--]]

function displaybar.SetAccCur(sType,val)
    if sType == nil then
        sType = 'Acc';
    end

    if val == nil then
        val = 0;
    elseif type(val) == 'string' then
        val = tonumber(val);
    end

    if sType == 'Acc' then
        if val < 0 or val > gVars.tProgressive['Accuracy']['MaxStage'] then
            val = 0;
        end
        gVars.tProgressive['Accuracy']['CurStage'] = val;
    elseif sType == 'TAcc' then
        if val < 0 or val > gVars.tProgressive['Tank_Accuracy']['MaxStage'] then
            val = 0;
        end
        gVars.tProgressive['Tank_Accuracy']['CurStage'] = val;
    elseif sType == 'RAcc' then
        if val < 0 or val > gVars.tProgressive['Ranged_Accuracy']['MaxStage'] then
            val = 0;
        end
        gVars.tProgressive['Ranged_Accuracy']['CurStage'] = val;
    elseif sType == 'TRAcc' then
        if val < 0 or val > gVars.tProgressive['Tank_Ranged_Accuracy']['MaxStage'] then
            val = 0;
        end
        gVars.tProgressive['Tank_Ranged_Accuracy']['CurStage'] = val;
    end
end		-- displaybar.SetAccCur

--[[
    displaybar_cb registers the command so that the display bar can be turned on or off
--]]

ashita.events.register('command', 'displaybar_cb', function (e)
    local args = e.command:args()

    if #args == 0 or args[1] ~= '/displaybar' then
        return
    end

    e.blocked = true

    if #args == 1 then
        displaybar.FontObject.visible = not displaybar.FontObject.visible;
    end
end);

return displaybar;
