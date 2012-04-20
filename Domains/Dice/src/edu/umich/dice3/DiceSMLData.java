package edu.umich.dice3;

import sml.Identifier;
import sml.WMElement;

public class DiceSMLData {

	public final Identifier idState;
	public final Identifier idPlayers;
	public final Identifier idAffordances;
	public final WMElement idHistory;
	public final WMElement idRounds;
	
	public DiceSMLData(Identifier idState, Identifier idPlayers, Identifier idAffordances, WMElement idHistory, WMElement idRounds) {
		this.idState = idState;
		this.idPlayers = idPlayers;
		this.idAffordances = idAffordances;
		this.idHistory = idHistory;
		this.idRounds = idRounds;
	}
}
