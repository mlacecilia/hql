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

 \brief Returns a new g_eleven object instance
   rules http://www.solitariconlecarte.it/gioca_on_line14.htm

*/
FUNCTION gEleven( ... )
RETURN g_eleven():new( ... )

/*!

 \brief define g_eleven class

*/
CLASS g_eleven STATIC

   EXPORTED:
   METHOD init
   METHOD activate

   PROTECTED:
   VAR oWindow                            INIT NIL
   VAR oDeck                              INIT NIL
   VAR oEmptyCard                         INIT NIL
   METHOD __cleaner
   METHOD __createWin
   METHOD __onFormActivate
   METHOD __onFormClose

   METHOD __createDeck
   METHOD __createEmptyCard
   METHOD __makeRoom_buttons
   METHOD __makeRoom_cards
   METHOD __makeRoom_score
   METHOD __makeRoom_start
   METHOD __startGame
   METHOD __setScore

   HIDDEN:

ENDCLASS

/*!

 \brief initialize object instance for given [name], [Qt flags]
 \param(IN) string, ...
 \return self

*/
METHOD init( ... ) CLASS g_eleven
   ::__createDeck()
   ::__createEmptyCard()

   ::__createWin( ... )
RETURN self

/*!

 \brief activate form
 \param(IN)
 \return self

*/
METHOD activate() CLASS g_eleven
   ::oWindow:hqlActivate()
RETURN self

// ==================== PROTECTED section ====================

/*!

 \brief [PROTECTED] destroyer
 \param(IN)
 \return NIL

*/
METHOD __cleaner() CLASS g_eleven
   ::oWindow := NIL
RETURN NIL

/*!

 \brief [PROTECTED] create form
 \param(IN)
 \return NIL

*/
METHOD __createWin( oParent ) CLASS g_eleven
   LOCAL oSize
   LOCAL cBackColor := hqlSconfig():get( "carpet", "#0A6C03" )
   LOCAL oVspacer := QSpacerItem( 0, 0, QSizePolicy_Minimum, QSizePolicy_Expanding )
   LOCAL cWinName := hqlFw:getAutoName()

   WITH OBJECT ::oWindow := hqlChildWindow( cWinName, oParent )
      :setWindowTitle("Eleven solitaire")
      :setWindowIcon( QIcon( ":pgmico" ) )
      :setCentralWidget( hqlWidget( /*name*/, ::oWindow ) )
      :centralWidget():setLayout( hqlVBoxLayout() )
      /* :setStyleSheet( '#ELEVENWIN { background-color: #0A6C03; }' ) */
      :setStyleSheet( '#' + cWinName + ' { background-color: ' + cBackColor + '; }' )
      :hqlOnActivate( { || ::__onFormActivate() } )
      :hqlOnClose( { || ::__onFormClose() } )
   END WITH

   ::oWindow:centralWidget():layout():addItem( oVspacer )
   ::__makeRoom_start()
   ::oWindow:centralWidget():layout():addItem( oVspacer )
   ::__makeRoom_cards()
   ::oWindow:centralWidget():layout():addItem( oVspacer )
   ::__makeRoom_buttons()
   ::oWindow:centralWidget():layout():addItem( oVspacer )
   ::__makeRoom_score()
   ::oWindow:centralWidget():layout():addItem( oVspacer )

   // resize form to 90% of parent size
   oSize := oParent:size()
   ::oWindow:resize( oSize:width()*0.9,  oSize:height()*0.9 )

RETURN NIL

/*!

 \brief [PROTECTED] action when activated
 \param(IN)
 \return NIL

*/
METHOD __onFormActivate() CLASS g_eleven
RETURN NIL

/*!

 \brief [PROTECTED] action when closed
 \param(IN)
 \return NIL

*/
METHOD __onFormClose() CLASS g_eleven
   ::__cleaner()
RETURN NIL

////////////////////////////////////////  ====  \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

/*!

 \brief [PROTECTED] create deck
 \param(IN)
 \return NIL

*/
METHOD __createDeck() CLASS g_eleven
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
METHOD __createEmptyCard() CLASS g_eleven
   ::oEmptyCard := gCard( -1, -1, /*back = BACK_1, /*value >= 0*/ )
RETURN NIL

/*!

 \brief [PROTECTED] make room for buttons
 \param(IN)
 \return NIL

*/
METHOD __makeRoom_buttons() CLASS g_eleven
   LOCAL oHlay, oFont
   LOCAL oHspacer := QSpacerItem( 0, 0, QSizePolicy_Expanding, QSizePolicy_Minimum )

   oFont := hqlQApplication:font()
   oFont:setPointSize( 24 )

   oHlay := hqlHBoxLayout()
   oHlay:setContentsMargins( 0, 0, 0, 0 )

   oHlay:addItem( oHspacer )
   WITH OBJECT hqlPushButton("HIBTN", ::oWindow:centralWidget())
      :hqlAddMeToLayout( oHlay )
      :setFont( oFont )
      :setStyleSheet( '#HIBTN:enabled { background-color: #0000D1; color: #FFFFFF; } #HIBTN:disabled { background-color: #FFFFFF; color: #0000D1; }' )
      :setText( "Accept" )
      :setEnabled( .F. )
      :hqlOnClicked( { || ::__setScore() } )
   END WITH
   oHlay:addItem( oHspacer )

   ::oWindow:centralWidget():layout():addLayout( oHlay )

RETURN NIL

/*!

 \brief [PROTECTED] make room for cards and scores
 \param(IN)
 \return NIL

*/
METHOD __makeRoom_cards() CLASS g_eleven
   LOCAL oHlay
   LOCAL oHspacer := QSpacerItem( 0, 0, QSizePolicy_Expanding, QSizePolicy_Minimum )
   LOCAL oSpacer := QSpacerItem( 80, 0, QSizePolicy_Preferred, QSizePolicy_Minimum )

   /* 1st row */
   oHlay := hqlHBoxLayout()
   oHlay:setContentsMargins( 0, 0, 0, 0 )
   oHlay:addItem( oHspacer )
   WITH OBJECT cardWidget("card1x1", ::oWindow:centralWidget() )
      :setMinimumSize( 79, 120 )
      :hqlAddMeToLayout( oHlay )
      :setCard( ::oEmptyCard )
   END WITH
   oHlay:addItem( oSpacer )
   WITH OBJECT cardWidget("card1x2", ::oWindow:centralWidget() )
      :setMinimumSize( 79, 120 )
      :hqlAddMeToLayout( oHlay )
      :setCard( ::oEmptyCard )
   END WITH
   oHlay:addItem( oSpacer )
   WITH OBJECT cardWidget("card1x3", ::oWindow:centralWidget() )
      :setMinimumSize( 79, 120 )
      :hqlAddMeToLayout( oHlay )
      :setCard( ::oEmptyCard )
   END WITH
   oHlay:addItem( oHspacer )
   ::oWindow:centralWidget():layout():addLayout( oHlay )

   /* 2nd row */
   oHlay := hqlHBoxLayout()
   oHlay:setContentsMargins( 0, 0, 0, 0 )
   oHlay:addItem( oHspacer )
   WITH OBJECT cardWidget("card2x1", ::oWindow:centralWidget() )
      :setMinimumSize( 79, 120 )
      :hqlAddMeToLayout( oHlay )
      :setCard( ::oEmptyCard )
   END WITH
   oHlay:addItem( oSpacer )
   WITH OBJECT cardWidget("card2x2", ::oWindow:centralWidget() )
      :setMinimumSize( 79, 120 )
      :hqlAddMeToLayout( oHlay )
      :setCard( ::oEmptyCard )
   END WITH
   oHlay:addItem( oSpacer )
   WITH OBJECT cardWidget("card2x3", ::oWindow:centralWidget() )
      :setMinimumSize( 79, 120 )
      :hqlAddMeToLayout( oHlay )
      :setCard( ::oEmptyCard )
   END WITH
   oHlay:addItem( oHspacer )
   ::oWindow:centralWidget():layout():addLayout( oHlay )

   /* 3rd row */
   oHlay := hqlHBoxLayout()
   oHlay:setContentsMargins( 0, 0, 0, 0 )
   oHlay:addItem( oHspacer )
   WITH OBJECT cardWidget("card3x1", ::oWindow:centralWidget() )
      :setMinimumSize( 79, 120 )
      :hqlAddMeToLayout( oHlay )
      :setCard( ::oEmptyCard )
   END WITH
   oHlay:addItem( oSpacer )
   WITH OBJECT cardWidget("card3x2", ::oWindow:centralWidget() )
      :setMinimumSize( 79, 120 )
      :hqlAddMeToLayout( oHlay )
      :setCard( ::oEmptyCard )
   END WITH
   oHlay:addItem( oSpacer )
   WITH OBJECT cardWidget("card3x3", ::oWindow:centralWidget() )
      :setMinimumSize( 79, 120 )
      :hqlAddMeToLayout( oHlay )
      :setCard( ::oEmptyCard )
   END WITH
   oHlay:addItem( oHspacer )

   ::oWindow:centralWidget():layout():addLayout( oHlay )

RETURN NIL

/*!

 \brief [PROTECTED] make room for score
 \param(IN)
 \return NIL

*/
METHOD __makeRoom_score() CLASS g_eleven
   LOCAL oHlay
   LOCAL oHspacer := QSpacerItem( 0, 0, QSizePolicy_Expanding, QSizePolicy_Minimum )

   oHlay := hqlHBoxLayout()
   oHlay:setContentsMargins( 0, 0, 0, 0 )

   oHlay:addItem( oHspacer )
   WITH OBJECT hqlLcdNumber("SCORE", ::oWindow:centralWidget() )
      :hqlAddMeToLayout( oHlay )
      :setSmallDecimalPoint( .F. )
   END WITH
   oHlay:addItem( oHspacer )

   ::oWindow:centralWidget():layout():addLayout( oHlay )

RETURN NIL

/*!

 \brief [PROTECTED] make room for start button
 \param(IN)
 \return NIL

*/
METHOD __makeRoom_start() CLASS g_eleven
   LOCAL oHlay, oFont
   LOCAL oHspacer := QSpacerItem( 0, 0, QSizePolicy_Expanding, QSizePolicy_Minimum )

   oFont := hqlQApplication:font()
   oFont:setPointSize( 24 )

   oHlay := hqlHBoxLayout()
   oHlay:setContentsMargins( 0, 0, 0, 0 )

   oHlay:addItem( oHspacer )
   WITH OBJECT hqlPushButton("STBTN", ::oWindow)
      :hqlAddMeToLayout( oHlay )
      :setFont( oFont )
      :setStyleSheet( '#STBTN { background-color: #0000D1; color: #FFFFFF; }' )
      :setText( "Start" )
      :hqlOnClicked( { || ::__startGame() } )
   END WITH
   oHlay:addItem( oHspacer )

   ::oWindow:centralWidget():layout():addLayout( oHlay )

RETURN NIL

////////////////////////////////////////  ====  \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

METHOD __startGame() CLASS g_eleven
   LOCAL oCard

   ::__createDeck()

   /* 1st row */
   oCard := ::oDeck:takeAt( ::oDeck:choose() )
   oCard:setFace( FACE_FRONT )
   ::oWindow:card1x1:setCard( oCard )
   ::oWindow:card1x1:setCardChecked( .F. )

   oCard := ::oDeck:takeAt( ::oDeck:choose() )
   oCard:setFace( FACE_FRONT )
   ::oWindow:card1x2:setCard( oCard )
   ::oWindow:card1x2:setCardChecked( .F. )

   oCard := ::oDeck:takeAt( ::oDeck:choose() )
   oCard:setFace( FACE_FRONT )
   ::oWindow:card1x3:setCard( oCard )
   ::oWindow:card1x3:setCardChecked( .F. )

   /* 2nd row */
   oCard := ::oDeck:takeAt( ::oDeck:choose() )
   oCard:setFace( FACE_FRONT )
   ::oWindow:card2x1:setCard( oCard )
   ::oWindow:card2x1:setCardChecked( .F. )

   oCard := ::oDeck:takeAt( ::oDeck:choose() )
   oCard:setFace( FACE_FRONT )
   ::oWindow:card2x2:setCard( oCard )
   ::oWindow:card2x2:setCardChecked( .F. )

   oCard := ::oDeck:takeAt( ::oDeck:choose() )
   oCard:setFace( FACE_FRONT )
   ::oWindow:card2x3:setCard( oCard )
   ::oWindow:card2x3:setCardChecked( .F. )

   /* 3rd row */
   oCard := ::oDeck:takeAt( ::oDeck:choose() )
   oCard:setFace( FACE_FRONT )
   ::oWindow:card3x1:setCard( oCard )
   ::oWindow:card3x1:setCardChecked( .F. )

   oCard := ::oDeck:takeAt( ::oDeck:choose() )
   oCard:setFace( FACE_FRONT )
   ::oWindow:card3x2:setCard( oCard )
   ::oWindow:card3x2:setCardChecked( .F. )

   oCard := ::oDeck:takeAt( ::oDeck:choose() )
   oCard:setFace( FACE_FRONT )
   ::oWindow:card3x3:setCard( oCard )
   ::oWindow:card3x3:setCardChecked( .F. )

   ::oWindow:score:display( 0 )
   ::oWindow:hibtn:setEnabled( .T. )

RETURN NIL

METHOD __setScore() CLASS g_eleven
   LOCAL nScore := 0, nCards := 0, lLose, oCard

   ::oWindow:hibtn:setEnabled( .F. )

   /* 1st row */
   IF ( ::oWindow:card1x1:isCardChecked() )
      nScore += ::oWindow:card1x1:card:value()
      nCards += 1
   ENDIF
   IF ( ::oWindow:card1x2:isCardChecked() )
      nScore += ::oWindow:card1x2:card:value()
      nCards += 1
   ENDIF
   IF ( ::oWindow:card1x3:isCardChecked() )
      nScore += ::oWindow:card1x3:card:value()
      nCards += 1
   ENDIF

   /* 2nd row */
   IF ( ::oWindow:card2x1:isCardChecked() )
      nScore += ::oWindow:card2x1:card:value()
      nCards += 1
   ENDIF
   IF ( ::oWindow:card2x2:isCardChecked() )
      nScore += ::oWindow:card2x2:card:value()
      nCards += 1
   ENDIF
   IF ( ::oWindow:card2x3:isCardChecked() )
      nScore += ::oWindow:card2x3:card:value()
      nCards += 1
   ENDIF

   /* 3rd row */
   IF ( ::oWindow:card3x1:isCardChecked() )
      nScore += ::oWindow:card3x1:card:value()
      nCards += 1
   ENDIF
   IF ( ::oWindow:card3x2:isCardChecked() )
      nScore += ::oWindow:card3x2:card:value()
      nCards += 1
   ENDIF
   IF ( ::oWindow:card3x3:isCardChecked() )
      nScore += ::oWindow:card3x3:card:value()
      nCards += 1
   ENDIF

   IF ( nScore == 11 .OR. nScore == 36 )
      hql_MsgStop( "You won" )
      lLose := .F.
   ELSE
      hql_MsgStop( "Invalid selection" )
      lLose := .T.
   ENDIF

   IF ( lLose )
      /* 1st row */
      ::oWindow:card1x1:setCardChecked( .F. )
      ::oWindow:card1x2:setCardChecked( .F. )
      ::oWindow:card1x3:setCardChecked( .F. )
      /* 2nd row */
      ::oWindow:card2x1:setCardChecked( .F. )
      ::oWindow:card2x2:setCardChecked( .F. )
      ::oWindow:card2x3:setCardChecked( .F. )
      /* 3rd row */
      ::oWindow:card3x1:setCardChecked( .F. )
      ::oWindow:card3x2:setCardChecked( .F. )
      ::oWindow:card3x3:setCardChecked( .F. )
      ::oWindow:hibtn:setEnabled( .T. )
      RETURN NIL
   ENDIF

   /* deck finished */
   IF ( ::oDeck:size() - nCards < 1 )
      hql_MsgStop( "Start a new game" )
      RETURN NIL
   ENDIF

   nScore += ::oWindow:score:value()
   ::oWindow:score:display( nScore )

   /* 1st row */
   IF ( ::oWindow:card1x1:isCardChecked() )
      oCard := ::oDeck:takeAt( ::oDeck:choose() )
      oCard:setFace( FACE_FRONT )
      ::oWindow:card1x1:setCardChecked( .F. )
      ::oWindow:card1x1:setCard( oCard )
   ENDIF
   IF ( ::oWindow:card1x2:isCardChecked() )
      oCard := ::oDeck:takeAt( ::oDeck:choose() )
      oCard:setFace( FACE_FRONT )
      ::oWindow:card1x2:setCardChecked( .F. )
      ::oWindow:card1x2:setCard( oCard )
   ENDIF
   IF ( ::oWindow:card1x3:isCardChecked() )
      oCard := ::oDeck:takeAt( ::oDeck:choose() )
      oCard:setFace( FACE_FRONT )
      ::oWindow:card1x3:setCardChecked( .F. )
      ::oWindow:card1x3:setCard( oCard )
   ENDIF

   /* 2nd row */
   IF ( ::oWindow:card2x1:isCardChecked() )
      oCard := ::oDeck:takeAt( ::oDeck:choose() )
      oCard:setFace( FACE_FRONT )
      ::oWindow:card2x1:setCardChecked( .F. )
      ::oWindow:card2x1:setCard( oCard )
   ENDIF
   IF ( ::oWindow:card2x2:isCardChecked() )
      oCard := ::oDeck:takeAt( ::oDeck:choose() )
      oCard:setFace( FACE_FRONT )
      ::oWindow:card2x2:setCardChecked( .F. )
      ::oWindow:card2x2:setCard( oCard )
   ENDIF
   IF ( ::oWindow:card2x3:isCardChecked() )
      oCard := ::oDeck:takeAt( ::oDeck:choose() )
      oCard:setFace( FACE_FRONT )
      ::oWindow:card2x3:setCardChecked( .F. )
      ::oWindow:card2x3:setCard( oCard )
   ENDIF

   /* 3rd row */
   IF ( ::oWindow:card3x1:isCardChecked() )
      oCard := ::oDeck:takeAt( ::oDeck:choose() )
      oCard:setFace( FACE_FRONT )
      ::oWindow:card3x1:setCardChecked( .F. )
      ::oWindow:card3x1:setCard( oCard )
   ENDIF
   IF ( ::oWindow:card3x2:isCardChecked() )
      oCard := ::oDeck:takeAt( ::oDeck:choose() )
      oCard:setFace( FACE_FRONT )
      ::oWindow:card3x2:setCardChecked( .F. )
      ::oWindow:card3x2:setCard( oCard )
   ENDIF
   IF ( ::oWindow:card3x3:isCardChecked() )
      oCard := ::oDeck:takeAt( ::oDeck:choose() )
      oCard:setFace( FACE_FRONT )
      ::oWindow:card3x3:setCardChecked( .F. )
      ::oWindow:card3x3:setCard( oCard )
   ENDIF
   ::oWindow:hibtn:setEnabled( .T. )

RETURN NIL

/*
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
*/
