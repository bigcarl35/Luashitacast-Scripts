local displaybar = {};

local fonts = require('fonts');

--[[
    This component contains all routines that deal with the display bar

    List of routines
        Subroutines:
            ResetVisibility         Resets the display bar's element's visibility to what it was at start
            SetAccCur               Sets the appropriate tracking current stage
            SetVisibility           Sets the visibility of the display bar's elements
            Unload                  Unloads the font objects and registered events
            UpdateBarStatic         Updates the static portion of the display bar's variables

        Functions:
            fAccuracyDisplay        Returns the colorized display of the appropriate accuracy
            fColor                  Formats passed string with passed color keyword
            fDbarFieldCheck         Formats color for passed field for each display bar
            fDbarFormatDisplays     Formats passed info into text for display bars
            fHTMLEmphasis           Formats passed string with character emphasis
            fGetColorRecord         Returns the record wanted from tkwEle
            fHTMLColor              Formats passed string with passed color keyword into html
--]]

-- List of know color codes by keyword for displaying to the screen
local tkwEle = {	-- cOORRGGBB where OO is opacity, RR red, GG green, BB blue
    { ['kw'] = 'firesday',		 ['color'] = '|cFFFF0000|', ['html'] = 'rgba(255,0,0,1)' },         -- red
    { ['kw'] = 'earthsday',		 ['color'] = '|cFFC19A6B|', ['html'] = 'rgba(193,154,107,1)' },     -- camel
    { ['kw'] = 'watersday',		 ['color'] = '|cFF1F51FF|', ['html'] = 'rgba(31,245,255,1)' },      -- neon blue
    { ['kw'] = 'windsday',		 ['color'] = '|cFF4CBB17|', ['html'] = 'rgba(76,187,23,1)' },       -- kelly green
    { ['kw'] = 'iceday',		 ['color'] = '|cFF00FFFF|', ['html'] = 'rgba(0,255,255,1)'},        -- aqua
    { ['kw'] = 'lightningday',   ['color'] = '|cE1C16EFF|', ['html'] = 'rgba(193,110,247,0.9)' },   -- light purple
    { ['kw'] = 'lightsday', 	 ['color'] = '|cFFFFFFFF|', ['html'] = 'rgba(255,255,247,1)' },     -- white
    { ['kw'] = 'darksday', 		 ['color'] = '|cFF71797E|', ['html'] = 'rgba(113,121,126,1)' },     -- steel gray
    { ['kw'] = 'clear', 		 ['color'] = '|cFFA7C7E7|', ['html'] = 'rgba(167,199,231,1)' },     -- pastel blue
    { ['kw'] = 'sunshine', 		 ['color'] = '|cFFFFEA00|', ['html'] = 'rgba(255,234,0,1)' },       -- bright yellow
    { ['kw'] = 'clouds', 		 ['color'] = '|cFFFFFDD0|', ['html'] = 'rgba(255,253,208,1)' },     -- cream
    { ['kw'] = 'fog',	 		 ['color'] = '|cFFB2BEB5|', ['html'] = 'rgba(178,190,181,1)' },     -- ash gray
    { ['kw'] = 'fire', 			 ['color'] = '|cFFFF0000|', ['html'] = 'rgba(255,0,0,1)' },         -- red
    { ['kw'] = 'fire x2', 		 ['color'] = '|cFFFF0000|', ['html'] = 'rgba(255,0,0,1)' },         -- red
    { ['kw'] = 'water', 		 ['color'] = '|cFF1F51FF|', ['html'] = 'rgba(31,245,255,1)' },      -- neon blue
    { ['kw'] = 'water x2', 		 ['color'] = '|cFF1F51FF|', ['html'] = 'rgba(31,245,255,1)' },      -- neon blue
    { ['kw'] = 'earth', 		 ['color'] = '|cFFC19A6B|', ['html'] = 'rgba(193,154,107,1)' },     -- camel
    { ['kw'] = 'earth x2', 		 ['color'] = '|cFFC19A6B|', ['html'] = 'rgba(193,154,107,1)' },     -- camel
    { ['kw'] = 'wind',	 		 ['color'] = '|cFF4CBB17|', ['html'] = 'rgba(76,187,23,1)' },       -- kelly green
    { ['kw'] = 'wind x2', 		 ['color'] = '|cFF4CBB17|', ['html'] = 'rgba(76,187,23,1)' },       -- kelly green
    { ['kw'] = 'ice',	 		 ['color'] = '|cFF00FFFF|', ['html'] = 'rgba(0,255,255,1)' },       -- aqua
    { ['kw'] = 'ice x2', 		 ['color'] = '|cFF00FFFF|', ['html'] = 'rgba(0,255,255,1)' },       -- aqua
    { ['kw'] = 'thunder', 		 ['color'] = '|cE1C16EFF|', ['html'] = 'rgba(193,110,247,0.9)' },   -- light purple
    { ['kw'] = 'thunder x2',	 ['color'] = '|cE1C16EFF|', ['html'] = 'rgba(193,110,247,0.9)' },   -- light purple
    { ['kw'] = 'light', 		 ['color'] = '|cFFFFFFFF|', ['html'] = 'rgba(255,255,247,1)' },     -- white
    { ['kw'] = 'light x2', 		 ['color'] = '|cFFFFFFFF|', ['html'] = 'rgba(255,255,247,1)' },     -- white
    { ['kw'] = 'dark',	 		 ['color'] = '|cFF71797E|', ['html'] = 'rgba(113,121,126,1)' },     -- steel gray
    { ['kw'] = 'dark x2', 		 ['color'] = '|cFF71797E|', ['html'] = 'rgba(113,121,126,1)' },     -- steel gray
    { ['kw'] = 'full moon',		 ['color'] = '|cFFFFFFFF|', ['html'] = 'rgba(255,255,255,1)' },     -- white
    { ['kw'] = 'waning gibbous', ['color'] = '|cFFE5E4E2|', ['html'] = 'rgba(229,228,226,1)' },     -- platinum
    { ['kw'] = 'last quarter', 	 ['color'] = '|cFFC0C0C0|', ['html'] = 'rgba(192,192,192,1)' },     -- silver
    { ['kw'] = 'waning crescent',['color'] = '|cFF848884|', ['html'] = 'rgba(132,136,132,1)' },     -- smoke
    { ['kw'] = 'new moon', 		 ['color'] = '|cFF71797E|', ['html'] = 'rgba(113,121,126,1)' },     -- steel gray
    { ['kw'] = 'waxing crescent',['color'] = '|cFF848884|', ['html'] = 'rgba(132,136,132,1)' },     -- smoke
    { ['kw'] = 'first quarter',  ['color'] = '|cFFC0C0C0|', ['html'] = 'rgba(192,192,192,1)' },     -- silver
    { ['kw'] = 'waxing gibbous', ['color'] = '|cFFE5E4E2|', ['html'] = 'rgba(229,228,226,1)' },     -- platinum
    { ['kw'] = 'green',			 ['color'] = '|cFF00FF00|', ['html'] = 'rgba(0,255,0,1)' },         -- green
    { ['kw'] = 'red',			 ['color'] = '|cFFFF0000|', ['html'] = 'rgb(255,0,0)' },            -- red
    { ['kw'] = 'yellow', 		 ['color'] = '|cFFFFEA00|', ['html'] = 'rgba(255,234,0,1)' },       -- bright yellow
    { ['kw'] = 'lightgray', 	 ['color'] = '|cFFB2BEB5|', ['html'] = 'rgba(178,190,181,1)' },     -- light gray
    { ['kw'] = 'blue',		     ['color'] = '|cFF1F51FF|', ['html'] = 'rgba(31,245,255,1)' },      -- neon blue
    { ['kw'] = 'gray',	 		 ['color'] = '|cFF71797E|', ['html'] = 'rgba(113,121,126,1)' },     -- steel gray
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

    if (displaybar.FontObject2 ~= nil) then
        displaybar.FontObject2:destroy();
    end
    -- Unregister the displaybar and the toggle command for turning the display off
    ashita.events.unregister('d3d_present', 'displaybar_present_cb');
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
    fHTMLColor is similar to fColor except it returns the style tagged colorization of the
    passed string. All colors are found in the tkwEle structure and addressed via rgba.

    Parameters
        skw     identifying keyword for color
        sMsg    String to be colorized

    Returned
        Correctly formatted HTML string
--]]

function displaybar.fHTMLColor(skw,sMsg)
    local sStyle;

    if sMsg == nil then
        return '<br>';
    end

    if skw == nil then
        return sMsg;
    end

    skw = string.lower(skw);
    for i,j in pairs(tkwEle) do
        if j['kw'] == skw then
            return '<span style=color: ' .. j['html'] .. ';>' .. sMsg .. '</span>';
        end
    end

    -- Unrecognized color, return the string 'as is'
    return sMsg;
end     -- displaybar.fHTMLColor

--[[
    fHTMLEmphasis applies a character emphasis to the passed message string like bold or underline.

    Parameters
        sType   Type of character emphasis desired
        sMsg    String to emphasize

    Returned
        Correctly formatted HTML string
--]]

function displaybar.fHTMLEmphasis(sType,sMsg)

    if sType == nil then
        if sMsg == nil then
            return ' ';
        else
            return sMsg;
        end
    end

    if sType == gVars._HTML_BOLD then
        return '<b>' .. sMsg .. '</b>';
    elseif sType == gVars._HTML_UNDERLINE then
        return '<u>' .. sMsg .. '</u>';
    elseif sType == gVars._HTML_STRIKE then
        return '<s>' .. sMsg .. '</s>';
    else
        return sMsg;
    end        -- Note: macc toggle will not exist if neither the main job nor subjob can use magic, but the player might have
    -- indicated they wanted MACC displayed, regardless. Magic accuracy, bar
end     -- displaybar.fHTMLEmphasis

--[[
    fAccuracyDisplay generates the accuracy listing highlighting what is currently enabled
    based on the passed sType. Returned is the generated colorized string.

    Parameter
        sType   Type of accuracy: ACC or RACC

    Returned
        Correctly formatted colorized HTML string
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

    Parameters
        lbl      String to colorize
        bVal     T/F, true meaning make green, false make red

    Returned
        Correctly formatted displaybar string
--]]

function fColorizedEntry(lbl,bVal)
    local s;

    if bVal == nil then
        -- This means that the underlying variable is undefined. Use gray
        s = displaybar.fColor('gray',lbl) .. ' ';
    elseif bVal == true then
        s = displaybar.fColor('green',lbl) .. ' ';
    else
        s = displaybar.fColor('red',lbl) .. ' ';
    end
    return s;
end     -- fColorizedEntry

--[[
    fDbarFieldCheck sees if the two display bars should have the passed field included. Returned is
    the two values (if any).

    Parameter
        sField      Field to check
        bToggle     Is it a toggle?

    Returned
        Display bar 1's value if appropriate
        Display bar 2's value if appropriate
--]]

function fDbarFieldCheck(sField,bToggle)
    local d1,d2;
    local bExist;


    if sField == nil or bToggle == nil then
        return nil,nil;
    end

    -- Sees if the underlying variable for the passed field exists
    if bToggle == true then
        bExist = utilities.fToggleExists(sField);
    else
        bExist = utilities.fCycleExists(sField);
    end

    -- Determine if the passed field for dbar 1 is valid
    if utilities.fIsDisplaybarSettingValid(true,sField) == true then
        if bExist == true then
            -- The underlying variable exists
            if utilities.fIsVisible(true,sField) == true then
                -- And it is intended to be visible
                d1 = fColorizedEntry(sField,utilities.fGetToggle(sField));
            end
        else
            -- The underlying variable does not exists
            if utilities.fIsVisible(true,sField) == true then
                -- Since visible, change color because of the disconnect
                d1 = fColorizedEntry(sField,nil);
            end
        end
    end

    -- Now repeat for the second display bar
    -- Determine if the passed field for dbar 2 is valid
    if utilities.fIsDisplaybarSettingValid(false,sField) == true then
        if bExist == true then
            -- The underlying variable exists
            if utilities.fIsVisible(false,sField) == true then
                -- And it is intended to be visible
                d2 = fColorizedEntry(sField,utilities.fGetToggle(sField));
            end
        else
            -- The underlying variable does not exists
            if utilities.fIsVisible(false,sField) == true then
                -- Since visible, change color because of the disconnect
                d2 = fColorizedEntry(sField,nil);
            end
        end
    end

    return d1,d2;
end     -- fDbarFieldCheck

--[[
    fDbarFormatDisplays takes the passed information and adds the appropriate values to the
    two display bar texts.

    Parameters
        d1          New bar 1's value
        d2          New bar 2's value
        cur1        Current display bar 1
        cur2        Current display bar 2
        sTag        Prefix for the value
--]]

function fDbarFormatDisplays(d1,d2,cur1,cur2,sTag)
    local out1,out2;

    if d1 ~= nil then
        -- Value to add to display bar 1
        if cur1 ~= nil then
            -- Display bar already has info
            if sTag ~= nil then
                -- There's a prefix tag
                out1 = cur1 .. sTag .. d1 .. ' ';
            else
                -- No prefix tag
                out1 = cur1 .. d1 .. ' ';
            end
        else
            -- Display bar is empty
            if sTag ~= nil then
                -- There's a prefix tag
                out1 = sTag .. d1 .. ' ';
            else
                -- No prefix tag
                out1 = d1 .. ' ';
            end
        end
    else
        -- No value to add, just return current value
        out1 = cur1;
    end

    if d2 ~= nil then
        -- Value to add to display bar 1
        if cur2 ~= nil then
            -- Display bar already has info
            if sTag ~= nil then
                -- There's a prefix tag
                out2 = cur2 .. sTag .. d2 .. ' ';
            else
                -- No prefix tag
                out2 = cur2 .. d2 .. ' ';
            end
        else
            -- Display bar is empty
            if sTag ~= nil then
                -- There's a prefix tag
                out2 = sTag .. d2 .. ' ';
            else
                -- No prefix tag
                out2 = d2 .. ' ';
            end
        end
    end
end     -- fDbarFormatDisplays

--[[
    InitializeDisplayBar creates the display bar(s) and assigns "them" to an event. It is
    only run once.

    Note: some of the fields that can be displayed in a display bar are job specific. The logic for whether the
    underlying variable exists or not occurs when the variables are created. As to the display bars, what is
    displayed depends on the DisplayBar settings found in the job file, the only check that is done to the
    underlying variables are when they are toggles, the existance of the variable is checked.
--]]

function displaybar.InitializeDisplayBar()
    local player = gData.GetPlayer();
    local cmn = { [1] = gVars._WSWAP, [2] = gVars._KITE , [3] = gVars._TH, [4] = gVars._TANK ,
                  [5] = gVars._IDLE, [6] = gVars._EVASION , [7] = gVars._SPF , [8] = gVars._SGS,
                  [9] = gVars._GSWAP
    };

    displaybar.UpdateBarStatic();

    -- Even if not used, create both bars. This is needed if the player makes a bar
    -- visible via the /dbar command. The font creation code will only be run once,
    -- so the object better be there to display.
    displaybar.FontObject = fonts.new(fontSettings);
    displaybar.FontObject2 = fonts.new(fontSettings);

    -- Define the location of the display bars and whether visible. Start with display bar 1.
    -- Whether the bar is visible or not, the location must be set.
    if utilities.fIsDisplaybarSettingValid(true,gVars._POS_X) == true then
        displaybar.FontObject.position_x = gProfile.settings.DisplayBar[gVars._BAR1][gVars._POS_X];
    end

    if utilities.fIsDisplaybarSettingValid(true,gVars._POS_Y) == true then
        displaybar.FontObject.position_y = gProfile.settings.DisplayBar[gVars._BAR1][gVars._POS_Y];
    end

    if utilities.fIsDisplaybarSettingValid(true,gVars._VISIBLE) == true then
        displaybar.FontObject.visible = gProfile.settings.DisplayBar[gVars._BAR1][gVars._VISIBLE];
        displaybar.FontObject.background.visible = gProfile.settings.DisplayBar[gVars._BAR1][gVars._VISIBLE];
    end

    -- Now repeat for display bar 2
    if utilities.fIsDisplaybarSettingValid(false,gVars._POS_X) == true then
        displaybar.FontObject2.position_x = gProfile.settings.DisplayBar[gVars._BAR2][gVars._POS_X];
    end

    if utilities.fIsDisplaybarSettingValid(false,gVars._POS_Y) == true then
        displaybar.FontObject2.position_y = gProfile.settings.DisplayBar[gVars._BAR2][gVars._POS_Y];
    end

    if utilities.fIsDisplaybarSettingValid(false,gVars._VISIBLE) == true then
        displaybar.FontObject2.visible = gProfile.settings.DisplayBar[gVars._BAR2][gVars._VISIBLE];
        displaybar.FontObject2.background.visible = gProfile.settings.DisplayBar[gVars._BAR2][gVars._VISIBLE];
    end

    ashita.events.register('d3d_present', 'displaybar_present_cb', function ()
        local display,display2,d1,d2;
        local sAcc,sRAcc;
        local stmp;

        -- Display bar 1's prefix
        if utilities.fIsDisplaybarSettingValid(true,gVars._PREFIX) == true then
            display = gProfile.settings.DisplayBar[gVars._BAR1][gVars._PREFIX];
        else
            display = ' ';  -- Makes it easier to concatenate to
        end

        -- Display bar 2's prefix
        if utilities.fIsDisplaybarSettingValid(false,gVars._PREFIX) == true then
            display2 = gProfile.settings.DisplayBar[gVars._BAR2][gVars._PREFIX];
        else
            display2 = ' ';  -- Makes it easier to concatenate to
        end

        -- Because any field can be in either bar, you need to check each individually. Walk through
        -- all of the fields here. Note: even if the bar is invisible, define it like it is visible.
        -- Visibility is turned on or off on the font object directly.

        -- Job, bar 1
        if utilities.fIsDisplaybarSettingValid(true,gVars._JOB) == true and utilities.fIsVisible(true,gVars._JOB) == true then
            display = display .. MainLV .. Main .. '/' .. SubLV .. Sub .. ' | ';
        end

        -- Job, bar 2
        if utilities.fIsDisplaybarSettingValid(false,gVars._JOB) == true and utilities.fIsVisible(false,gVars._JOB) == true then
            display2 = display2 .. MainLV .. Main .. '/' .. SubLV .. Sub .. ' | ';
        end

        stmp = tostring(gProfile.system_settings.PlayerCappedLevel);
        -- Level capping, bar 1
        if utilities.fIsDisplaybarSettingValid(true,gVars._CAP) == true and utilities.fIsVisible(true,gVars._CAP) == true then
            display = display .. 'L.Cap: ';
            if gProfile.system_settings.PlayerCappedLevel > 0 then
                display = display .. fColorizedEntry(stmp,false) .. ' | ';
            else
                display = display .. fColorizedEntry('None',true) .. ' | ';
            end
        end

        -- Level capping, bar 2
        if utilities.fIsDisplaybarSettingValid(false,gVars._CAP) == true and utilities.fIsVisible(false,gVars._CAP) == true then
            display2 = display2 .. 'L.Cap: ';
            if gProfile.system_settings.PlayerCappedLevel > 0 then
                display2 = display2 .. fColorizedEntry(stmp,false) .. ' | ';
            else
                display2 = display2 .. fColorizedEntry('None',true) .. ' | ';
            end
        end

        -- Has GC been run, bar 1
        if utilities.fIsDisplaybarSettingValid(true,gVars._GC) == true and utilities.fIsVisible(true,gVars._GC) == true then
            display = display .. fColorizedEntry('GC',gear.fHasGCBeenRun()) .. '| ';
        end

        -- Has GC been run, bar 2
        if utilities.fIsDisplaybarSettingValid(false,gVars._GC) == true and utilities.fIsVisible(false,gVars._GC) == true then
            display2 = display2 .. fColorizedEntry('GC',gear.fHasGCBeenRun()) .. '| ';
        end

        -- Display the common (cross jobs) toggles
        for k,v in ipairs(cmn) do
            d1,d2 = fDbarFieldCheck(v,true);
            display,display2 = fDbarFormatDisplays(d1,d2,display,display2,nil);
        end

        -- Now, the job or class specific fields

        -- /MAcc, magical accurracy
        d1,d2 = fDbarFieldCheck(gVars._MACC,true);
        display,display2 = fDbarFormatDisplays(d1,d2,display,display2,nil);

        -- /SS, Show Steal, THF or /THF only
        d1,d2 = fDbarFieldCheck(gVars._SS,true);
        display,display2 = fDbarFormatDisplays(d1,d2,display,display2,nil);

        -- /SBP, Show Blood Pact, SMN or /SMN only
        d1,d2 = fDbarFieldCheck(gVars._SBP,true);
        display,display2 = fDbarFormatDisplays(d1,d2,display,display2,nil);

        -- /HPPLUS, HPPlus set equipped prior to healing breath. DRG/ only
        d1,d2 = fDbarFieldCheck(gVars._HPPLUS,true);
        display,display2 = fDbarFormatDisplays(d1,d2,display,display2,nil);

        -- /AJUG, Automatic Jug, BST only
        d1,d2 = fDbarFieldCheck(gVars._AJUG,true);
        display,display2 = fDbarFormatDisplays(d1,d2,display,display2,nil);

        -- /DB, Debuff pet, BST or /BST only
        d1,d2 = fDbarFieldCheck(gVars._DB,false);
        display,display2 = fDbarFormatDisplays(d1,d2,display,display2,'DB: ');

        -- /INSTRUMENT, string or horn, BRD only
        d1,d2 = fDbarFieldCheck(gVars._INSTRUMENT,false);
        display,display2 = fDbarFormatDisplays(d1,d2,display,display2,'Instrument: ');

        -- Display bar 1 conditional separator
        if (utilities.fIsDisplaybarSettingValid(true,gVars._MODE) and utilities.fIsVisible(true,gVars._MODE) == true) or
            (utilities.fIsDisplaybarSettingValid(true,gVars._DT) and utilities.fIsVisible(true,gVars._DT) == true) or
            (utilities.fIsDisplaybarSettingValid(true,gVars._REGION) and utilities.fIsVisible(true,gVars._REGION) == true)  then
            display = display .. '| ';
        end

        -- Display bar 2 conditional separator
        if (utilities.fIsDisplaybarSettingValid(false,gVars._MODE) and utilities.fIsVisible(false,gVars._MODE) == true) or
            (utilities.fIsDisplaybarSettingValid(false,gVars._DT) and utilities.fIsVisible(false,gVars._DT) == true) or
            (utilities.fIsDisplaybarSettingValid(false,gVars._REGION) and utilities.fIsVisible(false,gVars._REGION) == true)  then
            display2 = display2 .. '| ';
        end

        -- /MODE, Mode, SMN or /smn only
        d1,d2 = fDbarFieldCheck(gVars._MODE,false);
        display,display2 = fDbarFormatDisplays(d1,d2,display,display2,'Mode: ');

        -- /DT, Damage Taken
        d1,d2 = fDbarFieldCheck(gVars._DT,false);
        display,display2 = fDbarFormatDisplays(d1,d2,display,display2,'DT: ');

        local srColor;
        if gVars.sRegion == gVars._REGION_STATUS_MUST_ZONE then
            srColor = 'yellow';
        elseif gVars.sRegion == gVars._REGION_STATUS_OWNED then
            srColor = 'green';
        else
            srColor = 'red';
        end

        -- Region, display bar 1
        if utilities.fIsDisplaybarSettingValid(true,gVars._REGION) == true and utilities.fIsVisible(true,gVars._REGION) == true then
            display = display .. 'Region: ' .. displaybar.fColor(srColor,gVars.sRegion) .. ' ';
        end

        -- Region, display bar 2
        if utilities.fIsDisplaybarSettingValid(false,gVars._REGION) == true and utilities.fIsVisible(false,gVars._REGION) == true then
            display2 = display2 .. 'Region: ' .. displaybar.fColor(srColor,gVars.sRegion) .. ' ';
        end

        -- Acc/RAcc reference
        if utilities.fGetToggle(gVars._TANK) == true then
            sAcc = 'TAcc';
            sRAcc = 'TRAcc';
        else
            sAcc = 'Acc'
            sRAcc = 'RAcc';
        end

        -- ACC/RACC, Accuracy/Ranged accuracy, display bar 1
        if utilities.fIsDisplaybarSettingValid(true,gVars._ACC) == true and utilities.fIsVisible(true,gVars._ACC) == true then
            display = display .. '| Acc: ' .. fAccuracyDisplay(sAcc) .. ' ';
            if utilities.fIsDisplaybarSettingValid(true,gVars._RACC) == true and utilities.fIsVisible(true,gVars._RACC) == true then
               display = display .. 'RAcc: ' .. fAccuracyDisplay(sRAcc) .. ' ';
            end
        elseif utilities.fIsDisplaybarSettingValid(false,gVars._RACC) == true and utilities.fIsVisible(false,gVars._RACC) == true then
            display = display .. '| RAcc: ' .. fAccuracyDisplay(sRAcc) .. ' ';
        end

        -- ACC/RACC, Accuracy/Ranged accuracy, display bar 2
        if utilities.fIsDisplaybarSettingValid(false,gVars._ACC) == true and utilities.fIsVisible(false,gVars._ACC) == true then
            display2 = display2 .. '| Acc: ' .. fAccuracyDisplay(sAcc) .. ' ';
            if utilities.fIsDisplaybarSettingValid(false,gVars._RACC) == true and utilities.fIsVisible(false,gVars._RACC) == true then
                display2 = display2 .. 'RAcc: ' .. fAccuracyDisplay(sRAcc) .. ' ';
            end
        elseif utilities.fIsDisplaybarSettingValid(false,gVars._RACC) == true and utilities.fIsVisible(false,gVars._RACC) == true then
            display2 = display2 .. '| RAcc: ' .. fAccuracyDisplay(sRAcc) .. ' ';
        end

        -- Define Custom Conditionals list
        if (utilities.fIsDisplaybarSettingValid(true,gVars._CC) == true and utilities.fIsVisible(true,gVars._CC) == true) or
            utilities.fIsDisplaybarSettingValid(false,gVars._CC) == true and utilities.fIsVisible(false,gVars._CC) == true then
            stmp = '| CC: ';

            if gProfile.CustomConditionals ~= nil then
                for i,j in ipairs(gProfile.CustomConditionals) do
                    if i > 1 then
                        stmp = stmp .. ',';
                    end
                    if utilities.fGetToggle(j['code']) == true then
                        stmp = stmp .. displaybar.fColor('green',i);
                    else
                        stmp = stmp .. displaybar.fColor('red',i);
                    end
                end
            end

            -- CC, Custom Conditionals, display bar 1
            if utilities.fIsDisplaybarSettingValid(true,gVars._CC) == true and utilities.fIsVisible(true,gVars._CC) == true then
                display = display .. stmp;
            end

            -- CC, Custom Conditionals, display bar 2
            if utilities.fIsDisplaybarSettingValid(false,gVars._CC) == true and utilities.fIsVisible(false,gVars._CC) == true then
                display2 = display2 .. stmp;
            end
        end

        -- Locks
        if (utilities.fIsDisplaybarSettingValid(true,gVars._LOCKS) == true and utilities.fIsVisible(true,gVars._LOCKS) == true) or
            (utilities.fIsDisplaybarSettingValid(false,gVars._LOCKS) == true and utilities.fIsVisible(false,gVars._LOCKS) == true) then
            local s = locks.fCompactLocks();
            if s ~= 'None' then
                stmp = ' | Locks: ' .. displaybar.fColor('red',s);
            else
                stmp = ' | Locks: ' .. displaybar.fColor('green',s);
            end

            -- Locks, display bar 1
            if utilities.fIsDisplaybarSettingValid(true,gVars._LOCKS) == true and utilities.fIsVisible(true,gVars._LOCKS) == true then
                display = display .. stmp;
            end

            -- Locks, display bar 2
            if utilities.fIsDisplaybarSettingValid(false,gVars._LOCKS) == true and utilities.fIsVisible(false,gVars._LOCKS) == true then
                display2 = display2 .. stmp;
            end
        end

        local env = gData.GetEnvironment();
        stmp = displaybar.fColor(env.Day,env.Day);
        -- Day, display bar 1
        if utilities.fIsDisplaybarSettingValid(true,gVars._DAY) == true and utilities.fIsVisible(true,gVars._DAY) == true then
            display = display .. ' | ' .. stmp .. ' ';
        end

        -- Day, display bar 2
        if utilities.fIsDisplaybarSettingValid(false,gVars._DAY) == true and utilities.fIsVisible(false,gVars._DAY) == true then
            display2 = display2 .. ' | ' .. stmp .. ' ';
        end

        stmp = string.format('| %02d:%02d ',env.Timestamp.hour,env.Timestamp.minute);
        -- Time, display bar 1
        if utilities.fIsDisplaybarSettingValid(true,gVars._TIME) == true and utilities.fIsVisible(true,gVars._TIME) == true then
            display = display .. stmp .. ' ';
        end

        -- Time, display bar 2
        if utilities.fIsDisplaybarSettingValid(false,gVars._TIME) == true and utilities.fIsVisible(false,gVars._TIME) == true then
            display2 = display2 .. stmp .. ' ';
        end

        stmp = string.format('| %d%% %s ',env.MoonPercent,displaybar.fColor(env.MoonPhase,env.MoonPhase));
        -- Moon, display bar 1
        if utilities.fIsDisplaybarSettingValid(true,gVars._MOON) == true and utilities.fIsVisible(true,gVars._MOON) == true then
            display = display .. stmp .. ' ';
        end

        -- Moon, display bar 2
        if utilities.fIsDisplaybarSettingValid(false,gVars._MOON) == true and utilities.fIsVisible(false,gVars._MOON) == true then
            display2 = display2 .. stmp .. ' ';
        end

        stmp = displaybar.fColor(env.RawWeather,env.RawWeather);
        -- Weather, display bar 1
        if utilities.fIsDisplaybarSettingValid(true,gVars._WEATHER) == true and utilities.fIsVisible(true,gVars._WEATHER) == true then
            display = display .. '| ' .. stmp .. ' ';
        end

        -- Weather, display bar 2
        if utilities.fIsDisplaybarSettingValid(false,gVars._WEATHER) == true and utilities.fIsVisible(false,gVars._WEATHER) == true then
            display2 = display2 .. '| ' .. stmp .. ' ';
        end

        -- Zone, display bar 1
        if utilities.fIsDisplaybarSettingValid(true,gVars._ZONE) == true and utilities.fIsVisible(true,gVars._ZONE) == true then
            display = display .. '| ' .. Zone;
        end

        -- Zone, display bar 1
        if utilities.fIsDisplaybarSettingValid(false,gVars._ZONE) == true and utilities.fIsVisible(false,gVars._ZONE) == true then
            display2 = display2 .. '| ' .. Zone;
        end

        displaybar.FontObject.text = utilities.fLtrim(display);
        displaybar.FontObject2.text = utilities.fLtrim(display2);
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
    ResetVisibility restores the stored visibility settings for the display bar back
    to the way it was when they were first loaded for the current job
--]]

function displaybar.ResetVisibility()
    -- The only thing that needs to be restored is the visibility setting. All other entries, while
    -- copied, are not modified anywhere in this program.

    for i,j in pairs(gVars.DisplayBar) do
        if gProfile.settings.Displaybar[i] ~= nil then
            gProfile.settings.Displaybar[i]['visible'] = j['visible'];
        end
    end
end     -- displaybar.ResetVisibility

--[[
    SetVisibility changes the visibility settings for the components of the display bar based on
    the passed in information.

    Parameters:
        sList   Comma delimited list of components to be affected
        bVis    T/F, should the result be visible or invisible
--]]

function displaybar.SetVisibility(sList,bVis)
    local sIndex,sExceptions;
    local t = {};

    if sList == 'all' or sList == 'none' then
        sExceptions = gVars._POS_X .. ',' .. gVars._POS_Y .. ',' .. gVars._VISIBLE;
        for i,_ in pairs(gProfile.settings.DisplayBar) do
            if string.find(sExceptions,i) == nil then
                gProfile.settings.DisplayBar[i]['visible'] = (sList == 'all');
            end
        end
    else
        -- Need to walk the comma delimited list
        t = utilities.fSplitStringByDelimiter(sList,',');
        for _,j in pairs(t) do
            -- make sure it's a valid reference
            sIndex = utilities.fCheckDisplayFieldValidity(j);
            if sIndex ~= nil then
                gProfile.settings.DisplayBar[sIndex]['visible'] = (bVis == true);
            else
                print(chat.message('Info: Unrecognized DisplayBar field - ' ..j));
            end
        end
    end
end     -- displaybar.SetVisibility

--[[
    GetColorRecord finds the passed keyword and returns the matching record
--]]

function displaybar.fGetColorRecord(sColor)
    local t = {};

    if sColor == nil then
        return nil;
    end

    sColor = string.lower(sColor);

    for i,j in pairs(tkwEle) do
        if j['kw'] == sColor then
            return j;
        end
    end

    return nil;
end     -- display.fGetColorRecord

return displaybar;
