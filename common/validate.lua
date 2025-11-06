local validate = T{};

local displaybar = require('common.displaybar');
local utilities = require('common.utilities');
local reporting = require('common.reporting');

--[[
	This component contains all functions that are associated with the /val command used to validate gear sets.
	They are intended to help the player to insure the designated (or all) gear sets contain valid: conditional
	inline codes, subset references, and inline references. Further, it makes sure that the "sidedness" of the
	codes are correct (what should be on the left side, slot name/group or subset, or right side, item/subset
	name is correct). Lastly, like /gc, it checks for the validity of the item.

	Syntax:
            /val [gs=*|name,name,name] [file=*|name]

        Just /val will validate all gear sets, both in the current job file and in the crossjobs common file.
        "/val gs=*" is equivalent to just /val. If you wish to validate specific gear sets, use:

            /val gs=name,[name,name,...]

        The "file" option redirects the output to a file. "file" alone or "file=*" will have luashitacast
        name the file based on the date. If it already exists, the results of the validation run will be
        appended. If the player gives the file a name, that will be used instead. This file will exist in
        the /Reporting subdirectory.

	List of routines-
		Subroutines:
			FlowControl			Finds the specified gear set's conditionals and invokes the validation
			ValidateGear		Determines if the inline conditionals are known and used correctly

		Functions:
			HandleValidation	Coordinates the validation process
--]]

-- List of all valid inline conditional codes with associated settings: bNot,bSubset,bLeft,bRight
-- Note: for conditionals like SPELL: and SONG:, there's no way to check if the spell/song is
-- valid since what can be specified is a substring. Have to assume correct.
local tCodeMaster = {
	-- all true
	['at'] = { 'AFTERMATH','ANCIENT_CIRCLE','ARCANE_CIRCLE','COVER','FLEE','HOLY_CIRCLE','REPRISAL','SAMBA','SHINING_RUBY','SPIKE','UTSUSEMI','WARD_CIRCLE','YONIN',
			   'BARANY','BARELEMENTAL','BARSTATUS','BARAERO','BARBLIZZARD','BARFIRE','BARSTONE','BARTHUNDER','BARWATER','BARSLEEP','BARPOISON','BARPARALYZE','BARBLIND',
			   'BARVIRUS','BARPETRIFY','ENANY','ENAERO','ENBLIZZARD','ENDARK','ENFIRE','ENLIGHT','ENSTONE','ENTHUNDER','ENWATER','MT:BLUE','MT:DARK','MT:DIVINE',
			   'MT:ELEMENTAL','MT:ENFEEBLING','MT:ENHANCING','MT:HEALING','MT:NINJUTSU','MT:OFFENSIVE_HEALING','MT:SINGING','MT:SUMMONING','CR:ANY','CR:ALC','CR:BONE',
			   'CR:BSM','CR:CLOTH','CR:COOK','CR:GSM','CR:LTH','CR:WW','GA:ANY','GA:HELM','GA:DIG','GA:CLAM','GA:FISH','DARKSDAY','EARTHSDAY','FIRESDAY','ICEDAY',
			   'LIGHTNINGDAY','LIGHTSDAY','WATERSDAY','WINDSDAY','DEBUFFED','ADDLED','AMNESIA','BANED','BLINDED','BOUND','BUSTED','CHARMED','CURSED','DISEASED','DOOMED',
			   'ENCUMBERED','IMPAIRED','KO','MEDICATED','MUTED','PARALYZED','PETRIFIED','PLAGUED','POISONED','SJ_RESTRICTION','SILENCED','SLEPT','STUNNED','TERRIFIED',
			   'WEAKENED','WEIGHTED','FULLMOON','NEWMOON','GIBBOUS','QUARTERMOON','CRESCENT','INPARTY','OWN','SPELLCAT:BARELEMENTAL','SPELLCAT:BARSTATUS','TOWN',
			   'TOWN-AK','MODE:PERP','MODE:ATK','STATUS:ENGAGED','STATUS:RESTING','STATUS:IDLING','CAROL','ELEGY','ETUDE','FINALE','HYMNUS','LULLABY','MADRIGAL',
			   'MAMBA','MARCH','MAZURKA','MINNE','MINUET','PAEON','PRELUDE','REQUIEM','THRENODY','VIRALEI','TRUE','BST:PET_ATTACK','BST:PET_MATT','BST:PET_MACC',
			   'PET','PETF','PETFNPF','SMN:BP:PHYS','SMN:BP:MAG','SMN:BP:SKILL','SMN:BP:ACC','SMN:BP:HYBRID','SMN:PET','SMN:SUMMONS','SMN:PETMD','SMN:PETMW',
			   'SMN:SPIRIT:ES','SMN:SPIRIT:EP','MSJ','SJBLM','SJBLU','SJBRD','SJBST','SJCOR','SJDNC','SJDRK','SJGEO','SJMNK','SJNIN','SJPLD','SJPUP','SJRDM','SJRNG',
			   'SJRUN','SJSAM','SJSMN','SJTHF','SJWAR','SJWHM','PJBLM','PJBLU','PJBRD','PJBST','PJCOR','PJDNC','PJDRK','PJGEO','PJMNK','PJNIN','PJPLD','PJPUP','PJRDM',
			   'PJRNG','PJRUN','PJSAM','PJSMN','PJTHF','PJWAR','PJWHM','PJBLM_NOT_ME','PJBLU_NOT_ME','PJBRD_NOT_ME','PJBST_NOT_ME','PJCOR_NOT_ME','PJDNC_NOT_ME',
			   'PJDRK_NOT_ME','PJGEO_NOT_ME','PJMNK_NOT_ME','PJNIN_NOT_ME','PJPLD_NOT_ME','PJPUP_NOT_ME','PJRDM_NOT_ME','PJRNG_NOT_ME','PJRUN_NOT_ME','PJSAM_NOT_ME',
			   'PJSMN_NOT_ME','PJTHF_NOT_ME','PJWAR_NOT_ME','PJWHM_NOT_ME','AMORPH','AQUAN','ME','TIME:DAYTIME','TIME:DUSK2DAWN','TIME:NIGHTTIME','TIME:DAWN','TIME:DAY',
			   'TIME:DUSK','TIME:EVENING','TIME:DEADOFNIGHT','DT:BREATH','DT:MAGICAL','DT:PHYSICAL','EVASION','IDLE','TANK','MACC','WSWAP','KITE','SPF','ARCHERY',
			   'AXE','CLUB','DAGGER','GAXE','GKATANA','GSWORD','H2H','KATANA','MARKSMANSHIP','POLEARM','SCYTHE','STAVE','SWORD','THROWING','SHIELD','WTH:CLEAR',
			   'WTH:CLOUDS','WTH:DARK','WTH:EARTH','WTH:FIRE','WTH:FOG','WTH:ICE','WTH:LIGHT','WTH:SUNSHINE','WTH:THUNDER','WTH:WATER','WTH:WIND','WTH:DAY','SPELL:',
			   'SONG:'
			},
	-- not subset
	['ns'] = { 'EMPTY','EMPTY:1','EMPTY:2' },
	-- only right
	['or'] = { 'SPECIAL','TRACK' },
	-- conditionals
	['cond'] = { 'HP.','HPP.','MP.','MPP.','TP.','TPP.','LVL.'.'PARTY.','PETHPP.' }

};

--[[
	HandleValidation coordinates the validation process. It handles the file designation and sets up the
	looping for what gear sets should be processed.

	Parameter
		args		Invocation arguments
--]]

function validate.HandleValidation (args)
end		-- validate.HandleValidation

--[[
	FlowControl unravels a gear set so that ValidateGear will only be processing conditionals. It does this
	by walking the gear set, determining where conditionals are found and then relaying that information to
	ValidateGear. If a group is found or a subset with an array (either indexed or not), FlowControl is
	invoked again to unravel that table.

	Parameter
		gs		Pointer to the gear set to process
		pFile	Pointer to where the output should go. (Carried info)

	Note: /val [gs=[*|name,name,...]] [file=[*|name]]

--]]

function FlowControl(gs,pFile)
	local bLeft,bRight,iPos,sTmp;
	local tGs={};

	if gs == nil then
		return
	end

	-- Walk the gear set
	for i,j in pairs(gs) do
		-- Loop on Left, then Right side
		for k=1,2,1 do
			bLeft = (k == 1);
			bRight = not bLeft;

			if bLeft == true then
				-- Look for a inline conditional
				iPos = string.find(i,'//');
				if iPos ~= nil then
					sTmp = string.upper(string.sub(i,iPos,-1));
					tGs = utilities.fMakeConditionalTable(sTmp);
					ValidateGearSet(tGs,i,bLeft,bRight,pFile);
				end
			else
				if type(j) == 'table' then
					FlowControl(j,pFile);
				else
					iPos = string.find(i,'//');
					if iPos ~= nil then
						sTmp = string.upper(string.sub(i,iPos,-1));
						tGs = utilities.fMakeConditionalTable(sTmp);
						ValidateGearSet(tGs,j,bLeft,bRight,pFile);
					end
				end
			end
		end
	end
end		-- FlowControl

--[[
	ValidateGear takes the passed conditional list and checks to see if it's a known code. Further, it
	determines if it has been used correctly. It does not check to see if the logic is correct though.
	The output is either written to the screen or the designated file, whichever has been requested.

	Parameters
		tGs			Table containing the inline conditionals
		lines		The input line that contained the conditionals
		bLeft		T/F, is it from the left side
		bRight		T/F, is it from the right side
		pfile		File pointer. If nil, write to screen. Otherwise write to file
--]]

function ValidateGear(tGs,line,bLeft,bRight,pFile)
	local bNot,jj;
	local bFound;
	local iPos,iPos2,iPos3;
	local sOut,sfOut;

	if tGs == nil then
		return
	end

	local sline = string.upper(line);

	-- Walk the conditional codes list
	for _,j in pairs(tGs) do
		bFound = false;
		-- See if code negated
		if string.find(j,'NOT_') ~= nil then
			bNot = true;
			jj = string.sub(j,5,-1);
		end

		-- First check for codes that can be used anywhere
		if table.find(tCodeMaster['at'],j) ~= nil or
		   (bNot == true and table.find(tCodeMaster['at'],jj)) then
			-- Valid code, valid usage
			bFound = true;
		elseif table.find(tCodeMaster['ns'],j) ~= nil or
		   (bNot == true and table.find(tCodeMaster['ns'],jj)) then
		   	-- Then see if code valid anywhere but on a Subset
			if string.find(sline,'SUBSET') ~= nil or string.find(sline,'GROUP') ~= nil then
				-- Code used in wrong context
				sOut  = 'Invalid use of ' .. displaybar.fColor('red','//'.. j) ..', it cannot be used on a Subset command';
				sfOut = 'Invalid use of //' ..j .. ', it cannot be used on a Subset command';
				reporting.DisplayMessage(pFile,sOut,sfOut);
			end
			bFound = true;
		elseif table.find(tCodeMaster['or',j]) ~= nil then
			bFound = true;
			if bRight == false then
				-- Code used on wrong side
				sOut  = displaybar.fColor('red','//'.. j) ..'can only be used on the right side.';
				sfOut = '//' ..j .. ' can only be used on the right side.';
				reporting.DisplayMessage(pFile,sOut,sfOut);
			end

			if string.find(j,'SUBSET') ~= nil or string.find(j,'GROUP') ~= nil then
				-- Code cannot be used on Subsets or Groups
				sOut  = displaybar.fColor('red','//'.. j) ..' cannot be used on a Subset or Group command.';
				sfOut = '//' ..j .. ' cannot be used on a Subset or Group command.';
				reporting.DisplayMessage(pFile,sOut,sfOut);
			end
		elseif bNot == true and table.find(tCodeMaster['or'],jj) then
			bFound = true;
			-- Inverted code is not supported
			sOut  = displaybar.fColor('red','//'.. j) ..', is not supported.';
			sfOut = '//' ..j .. ', is not supported.';
			reporting.DisplayMessage(pFile,sOut,sfOut);
		else
			iPos = string.find(j,'LVLDIV:');
			if iPos ~= nil then
				-- True for LVLDIV and NOT_LVLDIV. Just need to check the number
				bFound = true;
				local val = tonumber(string.sub(j,iPos+7,-1));
				if val <= 0 or val > 75 then
					-- Invalid level divider
					sOut  = displaybar.fColor('red','//'.. j) ..' level divisor out of range: 1 - 75.';
					sfOut = '//' ..j .. ' level divisor out of range: 1 - 75.';
					reporting.DisplayMessage(pFile,sOut,sfOut);
				end
			end

			iPos = string.find(j,'IF:');
			if bFound == false and iPos ~= nil then
				bFound = true;
				if string.find(sline,'SUBSET') ~= nil or string.find(sline,'GROUP') ~= nil then
					-- Conditional Inline not supported for Subset
					sOut  = displaybar.fColor('red','//'.. j) ..' cannot be used on a Subset or Group.';
					sfOut = '//' ..j .. ' cannot be used on a Subset or Group.';
					reporting.DisplayMessage(pFile,sOut,sfOut);
				end
			end

			iPos = string.find(j,'IF-');
			if bFound == false and iPos ~= nil then
				bFound = true;
				-- Need to see if the slot is valid
				iPos2 = string.find(j,':');
				if iPos2 ~= nil then
					local sSlot = string.sub(j,iPos+3,iPos2-1);
					local bValid = utilities.fValidSlots(sSlot,utilities._SLOT_UA);
					if bValid == false then
						sOut  = displaybar.fColor('red','//'.. j) ..' contains an unrecognized slot name.';
						sfOut = '//' ..j .. ' contains an unrecognized slot name.';
						reporting.DisplayMessage(pFile,sOut,sfOut);
					end
				else
					-- Ill-formed conditional
					sOut  = displaybar.fColor('red','//'.. j) ..' is not formatted correctly. Should be //[NOT_]IF-slot:';
					sfOut = '//' ..j .. '  is not formatted correctly. Should be //[NOT_]IF-slot:';
					reporting.DisplayMessage(pFile,sOut,sfOut);
				end
			end

			iPos = string.find(j,'SPELL:');
			iPos2 = string.find(j,'SONG:');
			iPos3 = string.find(j,'PETNAME:');
			if bFound == false and (iPos ~= nil or iPos2 ~= nil or iPos3 ~= nil) then
				-- Spell, Song or {et Name}. No easy way to check since qualifier can be a substring.
				bFound = true;
			end

			if bFound = false;
				sOut = displaybar.fColor('red','Warning: Unrecognized conditional: //' .. j);
				sfOut = 'Warning: Unrecognized conditional: //' .. j);
				reporting.DisplayMessage(pFile,sOut,sfOut);
			end
		end
	end
end		-- ValidateGearSet



