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

 \brief Returns a new g_hilo object instance

*/
FUNCTION gHilo( ... )
RETURN g_hilo():new( ... )

/*!

 \brief define g_hilo class

*/
CLASS g_hilo STATIC

   EXPORTED:
   METHOD init
   METHOD activate

   PROTECTED:
   VAR oWindow                            INIT NIL
   VAR oDeck                              INIT NIL
   VAR oDealer                            INIT NIL
   VAR oDeckCard                          INIT NIL
   VAR oDealerCard                        INIT NIL
   VAR oDealerScore                       INIT NIL
   VAR oPlayer                            INIT NIL
   VAR oPlayerCard                        INIT NIL
   VAR oPlayerScore                       INIT NIL
   VAR oHiButton                          INIT NIL
   VAR oLoButton                          INIT NIL
   VAR oEmptyCard                         INIT NIL
   VAR oCounter                           INIT NIL
   METHOD __cleaner
   METHOD __createWin
   METHOD __onFormActivate
   METHOD __onFormClose

   METHOD __createEmptyCard
   METHOD __createDeck
   METHOD __createPlayers
   METHOD __makeRoom_start
   METHOD __makeRoom_cards
   METHOD __makeRoom_buttons
   METHOD __startGame
   METHOD __getDealerCard
   METHOD __getPlayerCard

   HIDDEN:

ENDCLASS

/*!

 \brief initialize object instance for given [name], [Qt flags]
 \param(IN) string, ...
 \return self

*/
METHOD init( ... ) CLASS g_hilo
   ::__createDeck()
   ::__createPlayers()
   ::__createEmptyCard()

   ::__createWin( ... )
RETURN self

/*!

 \brief activate form
 \param(IN)
 \return self

*/
METHOD activate() CLASS g_hilo
   ::oWindow:hqlActivate()
RETURN self

// ==================== PROTECTED section ====================

/*!

 \brief [PROTECTED] destroyer
 \param(IN)
 \return NIL

*/
METHOD __cleaner() CLASS g_hilo
   ::oDealerCard := NIL
   ::oDealerScore := NIL
   ::oDeckCard := NIL
   ::oPlayerCard := NIL
   ::oPlayerScore := NIL
   ::oHiButton := NIL
   ::oLoButton := NIL
   ::oCounter := NIL
   ::oWindow := NIL
RETURN NIL

/*!

 \brief [PROTECTED] create form
 \param(IN)
 \return NIL

*/
METHOD __createWin( oParent ) CLASS g_hilo
   LOCAL oSize
   LOCAL cBackColor := hqlSconfig():get( "carpet", "#0A6C03" )
   LOCAL cWinName := hqlFw:getAutoName()

   WITH OBJECT ::oWindow := hqlChildWindow( cWinName, oParent )
      :setWindowTitle("High Low solitaire")
      :setWindowIcon( QIcon( ":pgmico" ) )
      :setCentralWidget( hqlWidget( /*name*/, ::oWindow ) )
      :centralWidget():setLayout( hqlVBoxLayout() )
      /* :setStyleSheet( '#HILOWIN { background-color: #0A6C03; }' ) */
      :setStyleSheet( '#' + cWinName + ' { background-color: ' + cBackColor + '; }' )
      :hqlOnActivate( { || ::__onFormActivate() } )
      :hqlOnClose( { || ::__onFormClose() } )
   END WITH

   ::__makeRoom_start()
   ::__makeRoom_buttons()
   ::__makeRoom_cards()

   // resize form to 90% of parent size
   oSize := oParent:size()
   ::oWindow:resize( oSize:width()*0.9,  oSize:height()*0.9 )

RETURN NIL

/*!

 \brief [PROTECTED] action when activated
 \param(IN)
 \return NIL

*/
METHOD __onFormActivate() CLASS g_hilo
RETURN NIL

/*!

 \brief [PROTECTED] action when closed
 \param(IN)
 \return NIL

*/
METHOD __onFormClose() CLASS g_hilo
   ::__cleaner()
RETURN NIL

////////////////////////////////////////  ====  \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

/*!

 \brief [PROTECTED] create deck
 \param(IN)
 \return NIL

*/
METHOD __createDeck() CLASS g_hilo
   LOCAL nSuit, nRank
   ::oDeck := gDeck()
   FOR nSuit := SUIT_MIN TO SUIT_MAX
      FOR nRank := RANK_MIN TO RANK_MAX
         ::oDeck:append( gCard( nSuit, nRank, /*back = BACK_1*/, nRank ) )
      NEXT
   NEXT
   ::oDeck:shuffle()
RETURN NIL

/*!

 \brief [PROTECTED] create an empty card
 \param(IN)
 \return NIL

*/
METHOD __createEmptyCard() CLASS g_hilo
   ::oEmptyCard := gCard( -1, -1, /*back = BACK_1, /*value >= 0*/ )
RETURN NIL

/*!

 \brief [PROTECTED] create deck
 \param(IN)
 \return NIL

*/
METHOD __createPlayers() CLASS g_hilo
   ::oDealer := gPlayer( "Dealer" )
   ::oPlayer := gPlayer( "You" )
RETURN NIL

/*!

 \brief [PROTECTED] make room for start button
 \param(IN)
 \return NIL

*/
METHOD __makeRoom_start() CLASS g_hilo
   LOCAL oHlay, oFont

   oFont := hqlQApplication:font()
   oFont:setPointSize( 24 )

   oHlay := hqlHBoxLayout()
   oHlay:setContentsMargins( 0, 0, 0, 0 )
   oHlay:addStretch()

   WITH OBJECT hqlPushButton("STBTN", ::oWindow)
      :hqlAddMeToLayout( oHlay )
      :setFont( oFont )
      :setStyleSheet( '#STBTN { background-color: #0000D1; color: #FFFFFF; }' )
      :setText( "Start" )
      :hqlOnClicked( { || ::__startGame() } )
   END WITH

   oHlay:addStretch()

   ::oWindow:centralWidget():layout():addLayout( oHlay )

RETURN NIL

/*!

 \brief [PROTECTED] make room for cards and scores
 \param(IN)
 \return NIL

*/
METHOD __makeRoom_cards() CLASS g_hilo
   LOCAL oGlay
   LOCAL oSpacer := QSpacerItem( 20, 8, QSizePolicy_MinimumExpanding, QSizePolicy_Preferred )
   LOCAL oFont

   oFont := hqlQApplication:font()
   oFont:setPointSize( 24 )

   oGlay := hqlGridLayout()
   oGlay:setContentsMargins( 0, 0, 0, 0 )

   // labels widget
   WITH OBJECT hqlLabel(/*name*/, ::oWindow:centralWidget() )
      :hqlAddMeToLayout( oGlay, 0, 1, Qt_AlignCenter )
      :setFont( oFont )
      :setStyleSheet( "color: yellow;" )
      :setText( "DEALER" )
   END WITH

   WITH OBJECT hqlLabel(/*name*/, ::oWindow:centralWidget() )
      :hqlAddMeToLayout( oGlay, 0, 3, Qt_AlignCenter )
      :setFont( oFont )
      :setStyleSheet( "color: yellow;" )
      :setText( "DECK" )
   END WITH

   WITH OBJECT hqlLabel(/*name*/, ::oWindow:centralWidget() )
      :hqlAddMeToLayout( oGlay, 0, 5, Qt_AlignCenter )
      :setFont( oFont )
      :setStyleSheet( "color: yellow;" )
      :setText( "YOU" )
   END WITH

   // card widgets
   oGlay:addItem( oSpacer, 1, 0, 1, 1, 0 )    /* to keep padding left */

   WITH OBJECT ::oDealerCard := cardWidget(/*name*/, ::oWindow:centralWidget() )
      :setMinimumSize( 200, 306 )
      :hqlAddMeToLayout( oGlay, 1, 1 )
      :setCard( ::oEmptyCard )
   END WITH

   oGlay:addItem( oSpacer, 1, 2, 1, 1, 0 )
   WITH OBJECT ::oDeckCard := cardWidget(/*name*/, ::oWindow:centralWidget() )
      :setMinimumSize( 200, 306 )
      :hqlAddMeToLayout( oGlay, 1, 3 )
      :setCard( ::oEmptyCard )
   END WITH

   oGlay:addItem( oSpacer, 1, 4, 1, 1, 0 )
   WITH OBJECT ::oPlayerCard := cardWidget(/*name*/, ::oWindow:centralWidget() )
      :setMinimumSize( 200, 306 )
      :hqlAddMeToLayout( oGlay, 1, 5 )
      :setCard( ::oEmptyCard )
   END WITH
   oGlay:addItem( oSpacer, 1, 6, 1, 1, 0 )   /* to keep padding right */

   // score widgets
   WITH OBJECT ::oDealerScore := hqlLcdNumber(/*name*/, ::oWindow:centralWidget() )
      :hqlAddMeToLayout( oGlay, 3, 1, 0 )
      :setSmallDecimalPoint( .F. )
      /* :setNumDigits( 7 ) */
      /* :setSegmentStyle( QLCDNumber_Flat ) */
      /* :setAutoFillBackground( .T. ) */
   END WITH

   WITH OBJECT ::oCounter := hqlLcdNumber(/*name*/, ::oWindow:centralWidget() )
      :hqlAddMeToLayout( oGlay, 3, 3, 0 )
      :setSmallDecimalPoint( .F. )
      /* :setNumDigits( 7 ) */
      /* :setSegmentStyle( QLCDNumber_Flat ) */
      /* :setAutoFillBackground( .T. ) */
   END WITH

   WITH OBJECT ::oPlayerScore := hqlLcdNumber(/*name*/, ::oWindow:centralWidget() )
      :hqlAddMeToLayout( oGlay, 3, 5, 0 )
      :setSmallDecimalPoint( .F. )
      /* :setNumDigits( 7 ) */
      /* :setSegmentStyle( QLCDNumber_Flat ) */
      /* :setAutoFillBackground( .T. ) */
   END WITH

   ::oWindow:centralWidget():layout():addLayout( oGlay )

RETURN NIL

/*!

 \brief [PROTECTED] make room for buttons
 \param(IN)
 \return NIL

*/
METHOD __makeRoom_buttons() CLASS g_hilo
   LOCAL oHlay
   LOCAL oSpacer := QSpacerItem( 200, 8, QSizePolicy_Preferred, QSizePolicy_Preferred )
   LOCAL oFont

   oFont := hqlQApplication:font()
   oFont:setPointSize( 24 )

   oHlay := hqlHBoxLayout()
   oHlay:setContentsMargins( 0, 0, 0, 0 )

   oHlay:addStretch()

   WITH OBJECT ::oHiButton := hqlPushButton("HIBTN", ::oWindow:centralWidget())
      :hqlAddMeToLayout( oHlay )
      :setFont( oFont )
      :setStyleSheet( '#HIBTN:enabled { background-color: #0000D1; color: #FFFFFF; } #HIBTN:disabled { background-color: #FFFFFF; color: #0000D1; }' )
      :setText( "HI" )
      :setEnabled( .F. )
      :hqlOnClicked( { || ::__getPlayerCard("hi") } )
   END WITH

   oHlay:addSpacerItem( oSpacer )

   WITH OBJECT ::oLoButton := hqlPushButton("LOBTN", ::oWindow:centralWidget())
      :hqlAddMeToLayout( oHlay )
      :setFont( oFont )
      :setStyleSheet( '#LOBTN:enabled { background-color: #0000D1; color: #FFFFFF; } #LOBTN:disabled { background-color: #FFFFFF; color: #0000D1; }' )
      :setText( "LO" )
      :setEnabled( .F. )
      :hqlOnClicked( { || ::__getPlayerCard("lo") } )
   END WITH

   oHlay:addStretch()

   ::oWindow:centralWidget():layout():addLayout( oHlay )

RETURN NIL

////////////////////////////////////////  ====  \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

METHOD __startGame() CLASS g_hilo

   ::__createDeck()

   ::oHiButton:setEnabled( .F. )
   ::oLoButton:setEnabled( .F. )

   ::oPlayerCard:setCard( ::oEmptyCard )
   ::oDealerCard:setCard( ::oEmptyCard )

   ::oDealer:clear()
   ::oPlayer:clear()

   ::oDealerScore:display( ::oDealer:score() )
   ::oPlayerScore:display( ::oPlayer:score() )
   ::oCounter:display( ::oDeck:size() )

   ::__getDealerCard()

RETURN NIL

METHOD __getDealerCard() CLASS g_hilo
   LOCAL nCard := ::oDeck:choose()
   LOCAL oCard := ::oDeck:takeAt( nCard )

   oCard:setFace( FACE_FRONT )
   ::oDealerCard:setCard( oCard )
   ::oHiButton:setEnabled( .T. )
   ::oLoButton:setEnabled( .T. )

RETURN NIL

METHOD __getPlayerCard( cWhat ) CLASS g_hilo
   LOCAL nCard := ::oDeck:choose()
   LOCAL oCard := ::oDeck:takeAt( nCard )
   LOCAL nWinner := 0   // 0==none, 1=dealer, 2=player
   LOCAL nDealerCardValue, nPlayerCardValue

   oCard:setFace( FACE_FRONT )
   ::oPlayerCard:setCard( oCard )

   nDealerCardValue := ::oDealerCard:card:value()
   nPlayerCardValue := ::oPlayerCard:card:value()

   IF ( LOWER(cWhat) == "hi" )
      IF ( nPlayerCardValue > nDealerCardValue )
         nWinner := 2
      ELSEIF ( nPlayerCardValue < nDealerCardValue )
         nWinner := 1
      ENDIF
   ELSE  /*lo*/
      IF ( nPlayerCardValue < nDealerCardValue )
         nWinner := 2
      ELSEIF ( nPlayerCardValue > nDealerCardValue )
         nWinner := 1
      ENDIF
   ENDIF

   IF ( nWinner == 2 )
      hql_MsgStop( ::oPlayer:name() + " is the winner" )
      ::oPlayer:addScore( nPlayerCardValue )
   ELSEIF ( nWinner == 1 )
      hql_MsgStop( ::oDealer:name() + " is the winner" )
      ::oDealer:addScore( nDealerCardValue )
   ENDIF

   ::oDealerScore:display( ::oDealer:score() )
   ::oPlayerScore:display( ::oPlayer:score() )

   ::oHiButton:setEnabled( .F. )
   ::oLoButton:setEnabled( .F. )

   ::oPlayerCard:setCard( ::oEmptyCard )
   ::oDealerCard:setCard( ::oEmptyCard )

   ::oCounter:display( ::oDeck:size() )

   IF ( ::oDeck:size() >= 2 )
      ::__getDealerCard()
   ENDIF

RETURN NIL

// ==================== HIDDEN section ====================
