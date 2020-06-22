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

 \brief Returns a new g_showCards object instance

*/
FUNCTION showCards( ... )
RETURN g_showCards():new( ... )

/*!

 \brief define g_showCards class

*/
CLASS g_showCards STATIC

   EXPORTED:
   METHOD init
   METHOD activate

   PROTECTED:
   VAR oWindow                            INIT NIL
   VAR oSuit                              INIT NIL
   VAR oRank                              INIT NIL
   VAR oFace                              INIT NIL
   VAR oCardWidget                        INIT NIL
   VAR oDeck                              INIT NIL
   METHOD __cleaner
   METHOD __createDeck
   METHOD __createWin
   METHOD __onFormActivate
   METHOD __onFormClose
   METHOD __setCard

   HIDDEN:

ENDCLASS

/*!

 \brief initialize object instance for given [name], [Qt flags]
 \param(IN) string, ...
 \return self

*/
METHOD init( ... ) CLASS g_showCards
   ::__createDeck()
   ::__createWin( ... )
RETURN self

/*!

 \brief activate form
 \param(IN)
 \return self

*/
METHOD activate() CLASS g_showCards
   ::oWindow:hqlActivate()
RETURN self

// ==================== PROTECTED section ====================

/*!

 \brief [PROTECTED] destroyer
 \param(IN)
 \return NIL

*/
METHOD __cleaner() CLASS g_showCards
   ::oWindow := NIL
   ::oSuit := NIL
   ::oRank := NIL
   ::oFace := NIL
   ::oCardWidget := NIL
RETURN NIL

/*!

 \brief [PROTECTED] create deck
 \param(IN)
 \return NIL

*/
METHOD __createDeck() CLASS g_showCards
   LOCAL nSuit, nRank
   ::oDeck := gDeck()
   FOR nSuit := SUIT_MIN TO SUIT_MAX
      FOR nRank := RANK_MIN TO RANK_MAX
         ::oDeck:append( gCard( nSuit, nRank, /*back = BACK_1, /*value >= 0*/ ) )
      NEXT
   NEXT
RETURN NIL

/*!

 \brief [PROTECTED] create form
 \param(IN)
 \return NIL

*/
METHOD __createWin( oParent ) CLASS g_showCards
   LOCAL oSize, oMlayout, oHlay
   LOCAL cBackColor := hqlSconfig():get( "carpet", "#0A6C03" )

   WITH OBJECT ::oWindow := hqlChildWindow( "THISFORM", oParent )
      :setWindowTitle("Show cards")
      :setWindowIcon( QIcon( ":pgmico" ) )
      :setCentralWidget( hqlWidget( /*name*/, ::oWindow ) )
      :centralWidget():setLayout( hqlVBoxLayout() )
      /* :setStyleSheet( "#THISFORM {background-color: #0A6C03;}" ) */
      :setStyleSheet( '#THISFORM { background-color: ' + cBackColor + '; }' )
      :hqlOnActivate( { || ::__onFormActivate() } )
      :hqlOnClose( { || ::__onFormClose() } )
   END WITH


   WITH OBJECT ::oWindow:centralWidget()
      oMlayout := :layout()

      // make room for suit, rank, face widgets
      oHlay := hqlHBoxLayout()
      oHlay:setContentsMargins( 0, 0, 0, 0 )

      WITH OBJECT hqlLabel(/*name*/)
         :hqlAddMeToLayout( oHlay )
         :setText( "Suit" )
         :setStyleSheet( "color: yellow;" )
      END WITH
      WITH OBJECT ::oSuit := hqlComboBox(/*name*/)
         :hqlAddMeToLayout( oHlay )
         :hqlAddRow( "Heart",   SUIT_HEARTS,   NIL )
         :hqlAddRow( "Diamond", SUIT_DIAMONDS, NIL )
         :hqlAddRow( "Club",    SUIT_CLUBS,    NIL )
         :hqlAddRow( "Spade",   SUIT_SPADES,   NIL )
         :hqlOnCurrentIndexChanged( { || ::__setCard() } )
      END WITH

      oHlay:addSpacing( 20 )
      WITH OBJECT hqlLabel(/*name*/ )
         :hqlAddMeToLayout( oHlay )
         :setText( "Rank" )
         :setStyleSheet( "color: yellow;" )
      END WITH
      WITH OBJECT ::oRank := hqlSpinbox(/*name*/)
         :hqlAddMeToLayout( oHlay )
         :setRange( RANK_MIN, RANK_MAX )
         :setValue( RANK_MIN )
         :hqlOnValueChanged( { || ::__setCard() } )
      END WITH

      oHlay:addSpacing( 20 )
      WITH OBJECT hqlLabel(/*name*/)
         :hqlAddMeToLayout( oHlay )
         :setText( "Face" )
         :setStyleSheet( "color: yellow;" )
      END WITH
      WITH OBJECT ::oFace := hqlComboBox(/*name*/)
         :hqlAddMeToLayout( oHlay )
         :hqlAddRow( "Front", FACE_FRONT, NIL )
         :hqlAddRow( "Back", FACE_BACK, NIL )
         :hqlOnCurrentIndexChanged( { || ::__setCard() } )
      END WITH
      oHlay:addStretch()
      oMlayout:addLayout( oHlay )

      oMlayout:addSpacing( 20 )

      // make room for card widget
      oHlay := hqlHBoxLayout()
      oHlay:setContentsMargins( 0, 0, 0, 0 )
      oHlay:addStretch()
      ::oCardWidget := cardWidget(/*name*/)
      ::oCardWidget:hqlAddMeToLayout( oHlay )
      /* ::oCardWidget:setSizePolicy( QSizePolicy_Fixed, QSizePolicy_Fixed ) */
      /* ::oCardWidget:setMinimumSize( 300, 458 ) */
      /* ::oCardWidget:setMaximumSize( 300, 458 ) */
      oHlay:addStretch()
      /* oMlayout:addLayout( oHlay, hb_BitOr(Qt_AlignVCenter,Qt_AlignLeft) ) */
      oMlayout:addLayout( oHlay )
      oMlayout:addStretch()

   END WITH /* centalWidget */

   ::__setCard()

   // resize form to 90% of parent size
   oSize := oParent:size()
   ::oWindow:resize( oSize:width()*0.9,  oSize:height()*0.9 )

RETURN NIL

/*!

 \brief [PROTECTED] action when activated
 \param(IN)
 \return NIL

*/
METHOD __onFormActivate() CLASS g_showCards
RETURN NIL

/*!

 \brief [PROTECTED] action when closed
 \param(IN)
 \return NIL

*/
METHOD __onFormClose() CLASS g_showCards
   ::__cleaner()
RETURN NIL

/*!

 \brief [PROTECTED] set card
 \param(IN)
 \return NIL

*/
METHOD __setCard() CLASS g_showCards
   LOCAL nSuit := ::oSuit:currentData():toInt() /* ::oSuit:currentIndex() + 1 /* +1 to align Qt/Hql style to Harbour */
   LOCAL nRank := ::oRank:value()   /* is a spinBox */
   LOCAL nFace := ::oFace:currentData():toInt() /*IIF( ::oFace:currentIndex() == 0, FACE_FRONT, FACE_BACK )  /* is a comboBox */
   LOCAL nAt := ::oDeck:find( nSuit, nRank )
   LOCAL oCard := ::oDeck:at(nAt):setFace( nFace )

   ::oCardWidget:setCard( oCard )
   //::oCardWidget:updateCard()

RETURN NIL

// ==================== HIDDEN section ====================
