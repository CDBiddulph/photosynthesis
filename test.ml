(* Test plan: In general, anything falling under the operation of the
   Game module (which handles game logic regarding the state of the
   board and the 2-4 players) was tested via OUnit tests, while the Gui
   module (which handles rendering the game representation to the
   screen) and the Ui module (which combines the Gui and Game modules by
   responding to player inputs) were tested manually.

   More specifically, the Board, Cell, Flow, Gui, HexMap, HexUtil,
   Plant, PlantInventory, Player, PlayerId, and Store modules are used
   by the Game class, so they were tested directly or indirectly via
   OUnit testing. However, we only directly tested the important
   functions of Board, Cell, Game, HexMap, Plant, and Player. Generally,
   any other modules were very simple data-storage modules (such as
   PlayerId) or their functionality was used by simple functions in
   higher-level modules which were tested (such as Store and
   PlantInventory, whose functionality was indirectly tested in the
   tests for Player). Black box testing was used for most functions,
   especially those with complex logic that is not quickly tested by
   playing the game (i.e. the Game module's system for awarding scoring
   points, which only updates on the relatively rare occasion that a
   player harvests a Large tree). Glass box/Bisect testing was also used
   as a check to make sure that the black box tests cover important
   functions.

   This test plan is sufficient because the Gui module re-renders every
   time a player makes a keystroke, so it is fast and easy to see when
   the GUI does something incorrect. There are not really rare and
   unusual events for the GUI, since it always performs a similar
   process of updating its layers, flattening them, and displaying them
   to the screen. On the other hand, events in the game logic can be
   much more rare and have much less immediately-visible effects, so
   black box testing is better for that. *)
let _ =
  TestGame.test;
  TestHexmap.test;
  TestPlayer.test;
  TestBoard.test
