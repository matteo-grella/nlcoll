------------------------------------------------------------------------------
--                               N L C O L L
--  N a t u r a l   L a n g u a g e   C o m p o n e n t   C o l l e c t i o n
--
--                       Copyright 2014 M. Grella
--
--  This is free software; you can redistribute it and/or modify it under
--  terms of the GNU General Public License as published by the Free Software
--  Foundation; either version 2, or (at your option) any later version.
--  This software is distributed in the hope that it will be useful, but WITH
--  OUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
--  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License
--  for more details. Free Software Foundation, 59 Temple Place - Suite
--  330, Boston, MA 02111-1307, USA.
--
--  As a special exception, if other files instantiate generics from this
--  unit, or you link this unit with other files to produce an executable,
--  this unit does not by itself cause the resulting executable to be
--  covered by the GNU General Public License. This exception does not
--  however invalidate any other reasons why the executable file might be
--  covered by the GNU Public License.
--
------------------------------------------------------------------------------


pragma License (Modified_GPL);

with Ada.Text_IO; use Ada.Text_IO;
with ARColl.Strings.Unbounded; use ARColl.Strings.Unbounded;

with NLColl.Corpus.Treebank; use NLColl.Corpus.Treebank;
with ARColl.Numerics.C;
with ARColl; use ARColl;

package body NLColl.Transition_Systems.Easy_First.Test is

    procedure Print_Current_State
      (Sentence             : in Sentence_Type;
       State                : in State_Type;
       Gold_Dependency_Tree : in Dependency_Tree_Type;
       Show_Gold            : in Boolean := False) is
    begin
        New_Line (Standard_Error);

        if Show_Gold then
            New_Line (Standard_Error);
            Put_Line (Standard_Error, "-- SENTENCE (GOLD)");

            NLColl.Corpus.Treebank.To_CoNLL
              (Sentence        => Sentence,
               Dependency_Tree => Gold_Dependency_Tree);
        end if;

        New_Line (Standard_Error);
        Put_Line (Standard_Error, "-- SENTENCE");

        NLColl.Corpus.Treebank.To_CoNLL
          (Sentence        => Sentence,
           Dependency_Tree => State.Dependency_Tree);

        New_Line;
        Put_Line (Standard_Error, "-- STATE");
        Print_State (State);
    end Print_Current_State;

    procedure Test_Transition_Sequence
      (Sentence             : in     Sentence_Type;
       Gold_Dependency_Tree : in     Dependency_Tree_Type;
       Verbose              : in     Boolean := False) is

        State : State_Type;
    begin

        State_Initialize
          (Sentence       => Sentence,
           State          => State);

        Loop_State : while not Is_Final_State (State) loop

            if Verbose then
                New_Line;
                Put_Line (Standard_Error, "---------------------");
            end if;

            if Verbose then
                Print_Current_State
                  (Sentence             => Sentence,
                   State                => State,
                   Gold_Dependency_Tree => Gold_Dependency_Tree,
                   Show_Gold            => True);
            end if;

            declare
                Next_Gold_Action          : Action_Type;
            begin

                Next_Gold_Action := Get_Next_Random_Correct_Action
                  (State                => State,
                   Gold_Dependency_Tree => Gold_Dependency_Tree);

                if Verbose then
                    New_Line;
                    Put_Line (Standard_Error, "[Next_Gold_Action]");
                    Put_Line(To_String(Next_Gold_Action));
                end if;

                Perform_Action
                  (State  => State,
                   Action => Next_Gold_Action);

                if Verbose then
                    Print_Current_State
                      (Sentence             => Sentence,
                       State                => State,
                       Gold_Dependency_Tree => Gold_Dependency_Tree,
                       Show_Gold            => True);
                end if;

            exception

                when others =>

                    New_Line (Standard_Error);

                    To_CoNLL
                      (Sentence        => Sentence,
                       Dependency_Tree => Gold_Dependency_Tree);

                    New_Line (Standard_Error);

                    To_CoNLL
                      (Sentence        => Sentence,
                       Dependency_Tree => State.Dependency_Tree);

                    New_Line (Standard_Error);

                    Print_State (State);

                    raise;
            end;

        end loop Loop_State;

        if Verbose then
            Print_Current_State
              (Sentence             => Sentence,
               State                => State,
               Gold_Dependency_Tree => Gold_Dependency_Tree,
               Show_Gold            => True);
        end if;

        if not Is_Tree (State.Dependency_Tree) then

            Print_Current_State
              (Sentence             => Sentence,
               State                => State,
               Gold_Dependency_Tree => Gold_Dependency_Tree,
               Show_Gold            => True);

            raise Parser_State_Error
              with "Anomaly: Resulting Dependency_Tree is not a Tree";
        end if;


        if State.Dependency_Tree /= Gold_Dependency_Tree then

            Print_Current_State
              (Sentence             => Sentence,
               State                => State,
               Gold_Dependency_Tree => Gold_Dependency_Tree,
               Show_Gold            => True);

            raise Parser_State_Error
              with "Anomaly: Dependency_Tree differs from the Gold_Dependency_Tree";
        end if;

        if False then

            To_CoNLL
              (Sentence        => Sentence,
               Dependency_Tree => State.Dependency_Tree);

            New_Line (Standard_Error);

        end if;

        State_Finalize (State);

    end Test_Transition_Sequence;

    function Get_Next_Correct_Action
      (State                : in State_Type;
       Gold_Dependency_Tree : in Dependency_Tree_Type) return Action_Type is

        Possible_Actions : constant Action_Type_Vectors.Vector
          := Get_Possible_Actions(State);
    begin

        for Action of Possible_Actions loop

            --Put_Line(To_String(Action));

            if Is_Correct
              (State                => State,
               Gold_Dependency_Tree => Gold_Dependency_Tree,
               Action               => Action) then

                return Action;

            end if;

        end loop;

        raise Parser_State_Error with "No Correct Action Found";
    end Get_Next_Correct_Action;

    function Get_Next_Random_Correct_Action
      (State                : in State_Type;
       Gold_Dependency_Tree : in Dependency_Tree_Type) return Action_Type is

        Possible_Actions : constant Action_Type_Vectors.Vector
          := Get_Possible_Actions(State);

        Possible_Correct_Actions : Action_Type_Vectors.Vector;
    begin

        for Action of Possible_Actions loop
            --Put_Line(To_String(Action));

            if Is_Correct
              (State                => State,
               Gold_Dependency_Tree => Gold_Dependency_Tree,
               Action               => Action) then

                Possible_Correct_Actions.Append(Action);
            end if;

        end loop;

        --- Choose Random

        if not Possible_Correct_Actions.Is_Empty then
            return Possible_Correct_Actions.Element
              (Index_Type(Float'Floor(Float (ARColl.Numerics.C.DRand48)
               * Float (Possible_Correct_Actions.Length)))
               + Possible_Correct_Actions.First_Index);
        else
            raise Parser_State_Error with "No Correct Action Found";
        end if;

    end Get_Next_Random_Correct_Action;


end NLColl.Transition_Systems.Easy_First.Test;
