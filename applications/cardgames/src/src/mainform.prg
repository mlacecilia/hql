/****************************************************************************
** $HQL_BEGIN_LICENSE$
**
** Copyright (C) 2020 by Luigi Ferraris
**
** This file is part of HQL project
**
** HQL is free software: you can redistribute it and/or modify
** it under the terms of the GNU General Public License as published by
** the Free Software Foundation, either version 3 of the License, or
** (at your option) any later version.
**
** HQL is distributed in the hope that it will be useful,
** but WITHOUT ANY WARRANTY; without even the implied warranty of
** MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
** GNU General Public License for more details.
**
** You should have received a copy of the GNU General Public License
** along with HQL.  If not, see <http://www.gnu.org/licenses/>.
**
** $HQL_END_LICENSE$
****************************************************************************/
#include "hbclass.ch"
#include "hqlinclude.ch"
#include "cardgames.ch"

/*!

 \brief returns g_mainForm object
 \param(IN) ...
 \return g_mainForm

*/
FUNCTION mainForm( ... )
RETURN g_mainForm():new( ... )

/*!

 \brief define class

*/
CLASS g_mainForm STATIC

   EXPORTED:
   METHOD init
   METHOD activate

   PROTECTED:

   HIDDEN:
   VAR oWindow                            INIT NIL
   METHOD __createWin
   METHOD __cleaner
   METHOD __onFormActivate
   METHOD __onFormClose
   METHOD __run_hilo
   METHOD __run_eleven
   METHOD __run_showCards

ENDCLASS

/*!

 \brief initialize object instance
 \param(IN) ...
 \return self

*/
METHOD init( ... ) CLASS g_mainForm
   ::__createWin( ... )
RETURN self

/*!

 \brief activate form
 \param(IN)
 \return self

*/
METHOD activate() CLASS g_mainForm
   ::oWindow:hqlActivate()
RETURN self

// ==================== PROTECTED section ====================

// ==================== HIDDEN section ====================

/*!

 \brief [HIDDEN] create form
 \param(IN)
 \return NIL

*/
METHOD __createWin( ... ) CLASS g_mainForm
   LOCAL oSize

   WITH OBJECT ::oWindow := hqlMainWindow( "MAINWIN" )
      :setWindowTitle("HQL Card Games")
      :setWindowIcon( QIcon( ":pgmico" ) )
      :setCentralWidget( hqlWidget( /*name*/, ::oWindow ) )
      :hqlOnActivate( { || ::__onFormActivate() } )
      :hqlOnClose( { || ::__onFormClose() } )

      WITH OBJECT hqlMenuBar( "mainmenu" )
         WITH OBJECT :hqlAddMenu( /*name*/ )
            :hqlCaption( "&File" )
            WITH OBJECT :hqlAddAction( "exit" )
               :setText( "e&Xit" )
               :setIcon( QIcon( ":/hqlres/exit" ) )
               :setShortcut( QKeySequence( "Alt+X" ) )
               :hqlOnTriggered( { || ::oWindow:close() } )
            END WITH
         END WITH

         WITH OBJECT :hqlAddMenu( /*name*/ )
            :hqlCaption( "&Games" )
            WITH OBJECT :hqlAddAction( "hilo" )
               :hqlCaption( "&HiLo" )
               :setShortcut( QKeySequence( "Alt+H" ) )
               :hqlOnTriggered( { || ::__run_hilo() } )
            END WITH
            WITH OBJECT :hqlAddAction( "eleven" )
               :hqlCaption( "&Eleven" )
               :setShortcut( QKeySequence( "Alt+E" ) )
               :hqlOnTriggered( { || ::__run_eleven() } )
            END WITH
         END WITH

         WITH OBJECT :hqlAddMenu( /*name*/ )
            :hqlCaption( "&Tools" )
            WITH OBJECT :hqlAddAction( "showCards" )
               :setText( "&Cards" )
               :setShortcut( QKeySequence( "Alt+C" ) )
               :hqlOnTriggered( { || ::__run_showCards() } )
            END WITH
            WITH OBJECT :hqlAddAction()
               :setText( "&Tester" )
               :hqlOnTriggered( { || UDFtest() } )
            END WITH
         END WITH

      END WITH

   END WITH

   oSize := HqlQDesktop:availableGeometry():size()
   ::oWindow:resize( oSize:width()*0.9,  oSize:height()*0.9 )

   ::oWindow:setStyleSheet( "#MAINWIN {background-image: url(:pgmhome); background-repeat: no-repeat; background-position: center;}")

RETURN NIL

/*!

 \brief [HIDDEN] destroyer
 \param(IN)
 \return NIL

*/
METHOD __cleaner() CLASS g_mainForm
   ::oWindow := NIL
RETURN NIL

/*!

 \brief [HIDDEN] action when activated
 \param(IN)
 \return NIL

*/
METHOD __onFormActivate() CLASS g_mainForm
RETURN NIL

/*!

 \brief [HIDDEN] action when closed
 \param(IN)
 \return NIL

*/
METHOD __onFormClose() CLASS g_mainForm
   ::__cleaner()
RETURN NIL

METHOD __run_showCards() CLASS g_mainForm
   LOCAL oPgm := showCards( ::oWindow )
   oPgm:activate()
RETURN NIL

METHOD __run_hilo() CLASS g_mainForm
   LOCAL oPgm := gHilo( ::oWindow )
   oPgm:activate()
RETURN NIL

METHOD __run_eleven() CLASS g_mainForm
   LOCAL oPgm := gEleven( ::oWindow )
   oPgm:activate()
RETURN NIL

////////////////////////////////////////  ====  \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\


STATIC PROCEDURE UDFtest()

   UDFcardTest()

   UDFdeckTest()

   UDFplayerTest()

   UDFsortTest()

RETURN

STATIC PROCEDURE UDFsortTest()
   LOCAL oDeck, nBack, nSuit, nRank, oCard
   hql_Trace( "SORT_TEST_START" )

   oDeck := gDeck()
   FOR nBack := BACK_1 TO BACK_2
      FOR nSuit := SUIT_MIN TO SUIT_MAX   /* SUIT_HEARTS, SUIT_DIAMONDS, SUIT_CLUBS, SUIT_SPADES */
         FOR nRank := RANK_MIN TO RANK_MAX   /* RANK_ACE, RANK_2, ...., RANK_KING */
            oDeck:append( gCard( nSuit, nRank, nBack, /*value*/ ) )
         NEXT
      NEXT
   NEXT

   oDeck:shuffle()
   hql_Trace( "after shuffle" )
   FOR EACH oCard IN oDeck
      UDFdbgCard(oCard, "", 3 )
   NEXT

   oDeck:sort()
   hql_Trace( "after sort" )
   FOR EACH oCard IN oDeck
      UDFdbgCard(oCard, "", 3 )
   NEXT

   hql_Trace( "SORT_TEST_END" )
RETURN

STATIC PROCEDURE UDFplayerTest()
   LOCAL oPlayer
   LOCAL oDeck, nSuit, nRank, nR
   hql_Trace( "PLAYER_TEST_START" )

   oDeck := gDeck()
   FOR nSuit := SUIT_MIN TO SUIT_MAX   /* SUIT_HEARTS, SUIT_DIAMONDS, SUIT_CLUBS, SUIT_SPADES */
      FOR nRank := RANK_MIN TO RANK_MAX   /* RANK_ACE, RANK_2, ...., RANK_KING */
         oDeck:append( gCard( nSuit, nRank, /*back*/, /*value*/ ) )
      NEXT
   NEXT
   oDeck:shuffle()

   oPlayer := gPlayer( /*name*/ )
   oPlayer:setName( "luigi" )
   /* oPlayer:setScore( num) */
   /* oPlayer:addScore( num = 0 ) */
   UDFdbgPlayer(oPlayer, "", 0 )

   nR := oDeck:choose()
   oPlayer:append( oDeck:takeAt( nR ) )
   nR := oDeck:choose()
   oPlayer:append( oDeck:takeAt( nR ) )
   UDFdbgPlayer(oPlayer, "gets 2 cards", 0 )

   hql_Trace( "PLAYER_TEST_END" )
RETURN

STATIC PROCEDURE UDFdeckTest()
   LOCAL oDeck, nSuit, nRank, oCard, nR
   hql_Trace( "DECK_TEST_START" )

   oDeck := gDeck()
   FOR nSuit := SUIT_MIN TO SUIT_MAX   /* SUIT_HEARTS, SUIT_DIAMONDS, SUIT_CLUBS, SUIT_SPADES */
      FOR nRank := RANK_MIN TO RANK_MAX   /* RANK_ACE, RANK_2, ...., RANK_KING */
         oDeck:append( gCard( nSuit, nRank, /*back*/, /*value*/ ) )
      NEXT
   NEXT

   hql_Trace( "show cards BEFORE shuffle " + hb_NtoS(oDeck:size()) )
   FOR EACH oCard IN oDeck
      UDFdbgCard(oCard, "", 3 )
   NEXT

   hql_Trace( "show cards AFTER shuffle " + hb_NtoS(oDeck:size()) )
   oDeck:shuffle( /*ntimes = 3*/ )
   FOR EACH oCard IN oDeck
      UDFdbgCard(oCard, "", 3 )
   NEXT

   hql_Trace( "choose card start" )
   WHILE ( oDeck:size() > 0 )
      nR := oDeck:choose()
      /* oCard := oDeck:at(nR)  get from deck */
      /* oCard := oDeck:takeAt(nR)  remove from deck */
      UDFdbgCard( oDeck:takeAt(nR), "", 3 )
   END

   hql_Trace( "DECK_TEST_END" )
RETURN

STATIC PROCEDURE UDFcardTest()
   LOCAL oCard
   hql_Trace( "CARD_TEST_START" )

   oCard := gCard( /*suit*/, /*rank*/, /*back*/, /*value*/ )
   UDFdbgCard(oCard, "null card", 0 )

   oCard := gCard( SUIT_SPADES, RANK_9, /*back*/, /*value*/ )
   UDFdbgCard(oCard, "SPADES / 9", 0 )

   hql_Trace( "CARD_TEST_END" )
RETURN

STATIC PROCEDURE UDFdbgCard(oobj, ctext, ntab )
   ctext := hb_DefaultValue(ctext, "" )
   ntab := hb_DefaultValue(ntab, 0 )
   IF ( LEN(ctext) > 0 )
      hql_Trace( SPACE(ntab) + ctext )
      ntab += 3
   ENDIF
   hql_Trace( SPACE(ntab) + ;
                      "isValid: " + hb_ValToExp(oobj:isValid()) + " " + ;
                      "isFaceUp: " + hb_ValToExp(oobj:isFaceUp()) + " " + ;
                      "suit: " + STRZERO(oobj:suit(), 1, 0) + " " + ;
                      "rank: " + STRZERO(oobj:rank(), 2, 0) + " " + ;
                      "value: " + STRZERO(oobj:value(), 4, 0) + " " + ;
                      "back: " + STRZERO(oobj:back(), 1, 0) + " " + ;
                      "weight: " + STRZERO(oobj:weight(), 4, 0) + " " + ;
                      "frontImage: " + oobj:frontImage() + " " + ;
                      "backImage: " + oobj:backImage() + " " + ;
                      "imageShown: " + oobj:imageShown() )
RETURN

STATIC PROCEDURE UDFdbgPlayer(oobj, ctext, ntab )
   LOCAL oCard
   ctext := hb_DefaultValue(ctext, "" )
   ntab := hb_DefaultValue(ntab, 0 )
   IF ( LEN(ctext) > 0 )
      hql_Trace( SPACE(ntab) + ctext )
      ntab += 3
   ENDIF
   hql_Trace( SPACE(ntab) + ;
                      "name: " + oobj:name() + " " + ;
                      "score: " + hb_NtoS(oobj:score()) + " " + ;
                      "cards: " + hb_NtoS(oobj:size())  )
   IF ( oobj:size() > 0 )
      FOR EACH oCard IN oobj
         UDFdbgCard(oCard, /*ctext*/, ntab+3 )
      NEXT
   ENDIF
RETURN
