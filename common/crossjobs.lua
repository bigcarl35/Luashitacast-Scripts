local crossjobs = {};

crossjobs.sets = {

--[[
	There are currently eight crafts: alchemy, bonecraft, clothcraft, cooking, goldsmithing, leathercraft,
	smithing, and woodworking. It's possible that a player will have gear for more than one craft. There's
	only one Crafting gear set, so you need to qualify each piece with what type of crafting the piece is
	used for. (Ex: Body = 'Weaver\'s Apron//CR:CLOTH).

	Please note that Crafting sets ignore the /WSWAP setting.
--]]

	['Crafting'] = {
	},

--[[
	There are seven gathering types: harvesting, excavtion, logging, and mining which are grouped in the H.E.L.M.
	set. The other three types of gathering: digging, clamming and fishing, have their own gear.

	Please note that Gathering sets ignore the /WSWAP setting.
--]]

	['Gathering'] = {
		Range = 'Lu Shang\'s F. Rod//GA:FISH',
		Ammo  = 'Sinking Minnow//GA:FISH',
		Body  = { 'Field Tunica//GA:HELM', 'Choc. Jack Coat//GA:DIG', 'Tarutaru Top +1//GA:CLAM', 'Angler\'s Tunica//GA:FISH' },
		Hands = { 'Field Gloves//GA:HELM', 'Fsh. Gloves//GA:FISH' },
		Legs  = { 'Field Hose//GA:HELM', 'Taru. Shorts +1//GA:CLAM', 'Fisherman\'s Hose//GA:FISH' },
		Feet  = { 'Field Boots//GA:HELM', 'Waders//GA:FISH' },
	},

--[[
	The Sneaky set is equipped and the slots are locked. It's a set intended to equip gear
	to help the player sneak around.
--]]

	['Sneaky'] = {
		Hands = 'Dream Mittens +1',
		Feet  = 'Dream Boots +1',
	},

--[[
	The dispense set is used to equip items that have an ability to daily dispense items.
	They're grouped here as a convenience. Note that since Sub is specified, something has
	to be specified in Main. If the Job file sees an empty Main, it assigns a default
	weapon. This was needed to get around a strange bug that sometimes occurred when
	entering a level capped zone.
--]]

	['Dispense'] = {
		Head = 'Dream Hat +1',
		Main = 'Dream Bell',
		Sub  = 'Hatchling Shield',
	},

--[[
	The following set is used to dynamically create a gear set to be displayed once rather
	than in a piecemeal manner. This set is to be left empty by the player  Please do not
	modify it.
--]]

	['CurrentGear'] = { },
};
