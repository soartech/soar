package edu.umich.dice3;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import sml.Identifier;
import sml.WMElement;
import edu.umich.dice3.gamestate.Bid;
import edu.umich.dice3.gamestate.DiceGameState;
import edu.umich.dice3.gamestate.Die;
import edu.umich.dice3.gamestate.HistoryItem;
import edu.umich.dice3.gamestate.Player;
import edu.umich.dice3.gamestate.HistoryItem.Type;

public class DiceSMLModule
{
    public static DiceSMLData GameStateToWM(DiceGameState game, Identifier inputLink, int me)
    {

        Identifier idState = null;
        Identifier idPlayers = null;
        Identifier idAffordances = null;
        WMElement idHistory = null;
        WMElement idRounds = null;

        idState = inputLink.CreateIdWME("state");
        idPlayers = inputLink.CreateIdWME("players");
        idAffordances = inputLink.CreateIdWME("affordances");

        Player mePlayer = game.getPlayer(me);

        /*********
         * State *
         *********/

        idState.CreateStringWME("special", (game.isInSpecialRules() ? "true" : "false"));
        idState.CreateStringWME("inprogress", (game.inProgress() ? "true" : "false"));

        Map<Integer, Identifier> playerMap = new HashMap<Integer, Identifier>();
        int victorId = -1;
        if (game.someoneWonGame())
        {
            victorId = game.getWinner().getId();
        }

        idPlayers.CreateStringWME("mystatus", game.getPlaterStatus(me));

        for (Player player : game.getPlayers())
        {
            Identifier playerId = idPlayers.CreateIdWME("player");
            playerId.CreateIntWME("id", player.getId());
            playerId.CreateStringWME("name", player.getName());
            playerId.CreateStringWME("exists", "" + !player.getLost());
            Identifier cup = playerId.CreateIdWME("cup");
            List<Die> hiddenDice = player.getHiddenDice();
            if (mePlayer == player)
            {
                if (!hiddenDice.isEmpty())
                {
                    cup.CreateIntWME("count", hiddenDice.size());
                    for (Die hiddenDie : hiddenDice)
                    {
                        Identifier die = cup.CreateIdWME("die");
                        die.CreateIntWME("face", hiddenDie.getValue());
                    }
                }
                else
                {
                    cup.CreateIntWME("count", 0);
                }
                Identifier cupTotals = cup.CreateIdWME("totals");
                int[] cupCounts = Die.countDice(hiddenDice);
                for (int i = 0; i < cupCounts.length; ++i)
                {
                    cupTotals.CreateIntWME("" + (i + 1), cupCounts[i]);
                }
            }
            else
            {
                cup.CreateIntWME("count", hiddenDice.size());
            }
            Identifier pushed = playerId.CreateIdWME("pushed");
            List<Die> pushedDice = player.getPushedDice();

            if (!pushedDice.isEmpty())
            {
                pushed.CreateIntWME("count", pushedDice.size());
                for (Die pushedDie : pushedDice)
                {
                    Identifier die = pushed.CreateIdWME("die");
                    die.CreateIntWME("face", pushedDie.getValue());
                }
            }
            else
            {
                pushed.CreateIntWME("count", 0);
            }

            Identifier pushedTotals = pushed.CreateIdWME("totals");
            int[] pushedCounts = Die.countDice(pushedDice);

            for (int i = 0; i < pushedCounts.length; ++i)
            {
                pushedTotals.CreateIntWME("" + (i + 1), pushedCounts[i]);
            }

            if (mePlayer == player)
            {
                idPlayers.CreateSharedIdWME("me", playerId);
            }

            if (game.getCurrentPlayer() == player)
            {
                idPlayers.CreateSharedIdWME("current", playerId);
            }

            if (victorId == player.getId())
            {
                idPlayers.CreateSharedIdWME("victor", playerId);
            }

            playerMap.put(player.getId(), playerId);
        }

        /***************
         * Affordances *
         ***************/

        Identifier bid = idAffordances.CreateIdWME("action");
        bid.CreateStringWME("name", "bid");
        bid.CreateStringWME("available", "" + mePlayer.canBid());

        Identifier challenge = idAffordances.CreateIdWME("action");
        challenge.CreateStringWME("name", "challenge");

        boolean canChallengeBid = mePlayer.canChallengeBid();
        boolean canChallengePass = mePlayer.canChallengeLastPass();

        if (!canChallengeBid && !canChallengePass)
        {
            challenge.CreateStringWME("available", "false");
        }
        else
        {
            challenge.CreateStringWME("available", "true");
        }
        if (canChallengeBid)
        {
            int target = game.getPreviousBid().getPlayerId();
            challenge.CreateSharedIdWME("target", playerMap.get(target));
        }
        if (canChallengePass)
        {
            int target = game.getLastPassPlayerId();
            challenge.CreateSharedIdWME("target", playerMap.get(target));
            target = game.getSecondLastPassPlayerId();
            if (target != -1 && target != mePlayer.getId())
            {
                challenge.CreateSharedIdWME("target", playerMap.get(target));
            }
        }

        Identifier exact = idAffordances.CreateIdWME("action");
        exact.CreateStringWME("name", "exact");
        exact.CreateStringWME("available", "" + mePlayer.canExact());

        Identifier pass = idAffordances.CreateIdWME("action");
        pass.CreateStringWME("name", "pass");
        pass.CreateStringWME("available", "" + mePlayer.canPass());

        Identifier push = idAffordances.CreateIdWME("action");
        push.CreateStringWME("name", "push");
        push.CreateStringWME("available", "" + mePlayer.canPush());

        Identifier accept = idAffordances.CreateIdWME("action");
        accept.CreateStringWME("name", "accept");
        accept.CreateStringWME("available", "" + mePlayer.canAccept());

        /***********
         * History *
         ***********/

        List<HistoryItem> history = game.getHistory();
        int roundLength = history.size();

        if (roundLength == 0)
        {
            idHistory = inputLink.CreateStringWME("history", "nil");
            idState.CreateStringWME("last-bid", "nil");
        }
        else
        {
            idHistory = inputLink.CreateIdWME("history");
            Identifier prev = idHistory.ConvertToIdentifier();
            Identifier lastBid = null;

            for (int i = roundLength - 1; i >= 0; --i)
            {
                HistoryItem item = history.get(i);

                int historyPlayerId = item.getPlayer().getId();
                if (playerMap.containsKey(historyPlayerId))
                {
                    prev.CreateSharedIdWME("player", playerMap.get(historyPlayerId));
                }

                prev.CreateStringWME("action", item.getType().getName());

                if ((lastBid == null) && item.getType() == Type.BID)
                {
                    lastBid = idState.CreateSharedIdWME("last-bid", prev);
                }

                if (item.getType() == Type.BID)
                {
                    Bid itemBid = item.getBid();
                    prev.CreateIntWME("multiplier", itemBid.getCount());
                    prev.CreateIntWME("face", itemBid.getRank());
                }
                else if (item.getType() == Type.CHALLENGE_BID || item.getType() == Type.CHALLENGE_PASS)
                {
                    int challangeTarget = item.getValue();
                    if (playerMap.containsKey(challangeTarget))
                    {
                        prev.CreateSharedIdWME("target", playerMap.get(challangeTarget));
                    }

                    prev.CreateStringWME("result", item.getResult() == 1 ? "success" : "failure");

                }
                else if (item.getType() == Type.EXACT)
                {
                    prev.CreateStringWME("result", item.getResult() == 1 ? "success" : "failure");
                }

                if (i == 0)
                {
                    prev.CreateStringWME("next", "nil");
                }
                else
                {
                    prev = prev.CreateIdWME("next");
                }
            }

            if (lastBid == null)
            {
                idState.CreateStringWME("last-bid", "nil");
            }
        }

        /**********
         * Rounds *
         **********/

        List<ArrayList<HistoryItem>> rounds = game.getRoundHistory();
        int numRounds = rounds.size();

        if (numRounds == 0)
        {
            idRounds = inputLink.CreateStringWME("rounds", "nil");
        }
        else
        {
            idRounds = inputLink.CreateIdWME("rounds");
            Identifier prev = idRounds.ConvertToIdentifier();

            for (int i = numRounds - 1; i >= 0; --i)
            {
                prev.CreateIntWME("id", i);
                ArrayList<HistoryItem> round = rounds.get(i);
                HistoryItem roundEnd = round.get(round.size() - 1);

                int playerId = roundEnd.getPlayer().getId();
                if (playerMap.containsKey(playerId))
                {
                    prev.CreateSharedIdWME("player", playerMap.get(playerId));
                }

                prev.CreateStringWME("action", roundEnd.getType().getName());

                if (roundEnd.getType() == Type.CHALLENGE_BID || roundEnd.getType() == Type.CHALLENGE_PASS)
                {
                    int challengeValue = roundEnd.getValue();
                    if (playerMap.containsKey(challengeValue))
                    {
                        prev.CreateSharedIdWME("target", playerMap.get(challengeValue));
                    }
                    prev.CreateStringWME("result", roundEnd.getResult() == 1 ? "success" : "failure");
                }
                else if (roundEnd.getType() == Type.EXACT)
                {
                    prev.CreateStringWME("result", roundEnd.getResult() == 1 ? "success" : "failure");
                }

                if (i == 0)
                {
                    prev.CreateStringWME("next", "nil");
                }
                else
                {
                    prev = prev.CreateIdWME("next");
                }
            }
        }

        return new DiceSMLData(idState, idPlayers, idAffordances, idHistory, idRounds);
    }
}
