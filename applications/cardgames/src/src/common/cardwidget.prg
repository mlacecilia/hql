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

 \brief Returns a new g_cardWidget object instance

*/
FUNCTION cardWidget( ... )
RETURN g_cardWidget():new( ... )

/*!

 \brief define g_cardWidget class

*/
CLASS g_cardWidget INHERIT hql_label

   EXPORTED:
   METHOD init
   METHOD hqlOnLeftButtonPressed
   METHOD hqlOnRightButtonPressed
   METHOD card                            INLINE ::oCard   // only to get card info; do not use to change values
   METHOD isCardChecked                   INLINE ::lCardChecked
   METHOD removeCard
   METHOD setCard
   METHOD setCardChecked
   METHOD updateCard

   PROTECTED:
   VAR bHqlLeftButtonPressed              INIT NIL
   VAR bHqlRightButtonPressed             INIT NIL
   VAR oCard                              INIT NIL
   VAR lCardChecked                       INIT .F.
   VAR nPadding                           INIT 4
   METHOD __hqlCleaner
   METHOD __hqlConnect
   METHOD __cardSetBackGround
   METHOD __makePixmap
   METHOD __nullPixMap
   SIGNAL __hql_QMouseButtonPress
   SIGNAL __hql_QResize

   HIDDEN:

ENDCLASS

/*!

 \brief initialize object instance for given [name], [Qt flags]
 \param(IN) string, ...
 \return self

*/
METHOD init( ... ) CLASS g_cardWidget
   ::oCard := gCard( /*suit*/, /*rank*/, /*back*/, /*value*/ )  /* an invalid card by default */
   ::hql_label:init( ... )
   /* ::setScaledContents( .T. )  when resize event used this must be false */
   ::setAutoFillBackground( .T. )
   ::setAlignment( Qt_AlignCenter )
   ::__cardSetBackGround()
RETURN self

/*!

 \brief set signal/event block
 \param(IN) block
 \return Self

*/
METHOD hqlOnLeftButtonPressed( arg1 ) CLASS g_cardWidget
   IF ( PCOUNT() == 1 .AND. ( hb_IsEvalItem(arg1 ) .OR. arg1 == NIL ) )
      ::bHqlLeftButtonPressed := arg1
   ENDIF
RETURN self

/*!

 \brief set signal/event block
 \param(IN) block
 \return Self

*/
METHOD hqlOnRightButtonPressed( arg1 ) CLASS g_cardWidget
   IF ( PCOUNT() == 1 .AND. ( hb_IsEvalItem(arg1 ) .OR. arg1 == NIL ) )
      ::bHqlRightButtonPressed := arg1
   ENDIF
RETURN self

/*!

 \brief remove and returns current card
 \param(IN)
 \return object

*/
METHOD removeCard() CLASS g_cardWidget
   LOCAL oCard := ::oCard
   ::oCard := gCard( /*suit*/, /*rank*/, /*back*/, /*value*/ )  /* an invalid card by default */
   ::setPixmap( ::__makePixmap( /*oSize*/ ) )
RETURN oCard

/*!

 \brief set card
 \param(IN) object
 \return self

*/
METHOD setCard( oCard ) CLASS g_cardWidget
   IF ( hb_IsObject(oCard) .AND. oCard:isDerivedFrom("g_card") )
      ::oCard := oCard
   ENDIF
   ::setPixmap( ::__makePixmap( /*oSize*/ ) )
RETURN self

METHOD setCardChecked( arg1 ) CLASS g_cardWidget
   IF ( hb_IsLogical(arg1) .AND. arg1 != ::lCardChecked )
      ::lCardChecked := arg1
      ::__cardSetBackGround()
   ENDIF
RETURN self

/*!

 \brief show card
 \param(IN)
 \return self

*/
METHOD updateCard() CLASS g_cardWidget
   ::setPixmap( ::__makePixmap( /*oSize*/ ) )
RETURN self

// ==================== PROTECTED section ====================

/*
 \brief [PROTECTED] Object cleaner
 \param(IN)
 \return NIL

*/
METHOD __hqlCleaner() CLASS g_cardWidget
   ::bHqlLeftButtonPressed := NIL
   ::bHqlRightButtonPressed := NIL
   ::hql_label:__hqlCleaner()
RETURN NIL

/*!

 \brief [PROTECTD] Connect signal
 \param(IN)
 \return bool

*/
METHOD __hqlConnect() CLASS g_cardWidget
   ::hql_label:__hqlConnect()
   ::connect( QEvent_Resize, { |oEvent| ::__hql_QResize(oEvent) } )
   ::connect( QEvent_MouseButtonPress , { |oEvent| ::__hql_QMouseButtonPress(oEvent) } )
RETURN .T.

METHOD __cardSetBackGround() CLASS g_cardWidget
   LOCAL oPalette := ::palette()

   IF ( ::lCardChecked )
      /* oPalette:setColor( QPalette_Window, QColor( 255, 102, 0 ) ) */
      oPalette:setColor( QPalette_Window, QColor( hqlSconfig():get( "checked", "#FF6600" ) ) )
   ELSE
      /* oPalette:setColor( QPalette_Window, QColor( 10, 108, 3 ) )  /* #0A6C03 */
      oPalette:setColor( QPalette_Window, QColor( hqlSconfig():get( "carpet", "#0A6C03" ) ) )
   ENDIF

   ::setPalette( oPalette )
   ::update()
RETURN NIL

/*!

 \brief [PROTECTD] returns pixmap based on current card and scaled to fit label
 \param(IN)
 \return pixmap

*/
METHOD __makePixmap( oSize ) CLASS g_cardWidget
   LOCAL cSource := ::oCard:imageShown()
   LOCAL pixmap := QPixmap()
   LOCAL nW, nH

   IF ( oSize == NIL )
      oSize := ::size()
   ENDIF

   nW := oSize:width() - ( ::nPadding * 2 )
   nH := oSize:height() - ( ::nPadding * 2 )
   oSize := QSize( nW, nH )

   IF ( pixmap:load( cSource ) )
      // ok
   ELSE
      pixmap := ::__nullPixmap()
   ENDIF

RETURN ( pixmap:scaled( oSize, Qt_KeepAspectRatio, Qt_SmoothTransformation ) )

/*!

 \brief it returns a NULL pixmap
 \param[in] none
 \return QPixmap

*/
METHOD __nullPixmap() CLASS g_cardWidget
   /* LOCAL oImage := QImage( 0, 0, QImage_Format_ARGB32 ) */
   /* oImage:fill( qRgba(0,0,0,0) ) not supported by HbQt */
   /* oImage:fill( QColor( 0, 0, 0, 0 ) ) */
RETURN QPixmap():fromImage( QImage( 0, 0, QImage_Format_ARGB32 ) )

// ==================== SLOTS/EVENTS section ====================

/*!

 \brief [PROTECTED] signal/event handler
   false ==> .F. means don't stop event hanlder, else .T. STOP see Harbour Changelog
 \param(IN) ...
 \return bool

*/
SIGNAL __hql_QMouseButtonPress(oEvent) CLASS g_cardWidget
   LOCAL nButton := oEvent:button()
   LOCAL lStopEvents := .F.

   SWITCH nButton
   CASE Qt_LeftButton
      ::lCardChecked := !::lCardChecked
      ::__cardSetBackGround()
      IF hb_IsEvalItem( ::bHqlLeftButtonPressed )
         EVAL( ::bHqlLeftButtonPressed, nButton, Self )  // Self always as last
      ENDIF
      lStopEvents := .T.
      EXIT

   CASE Qt_RightButton
      ::lCardChecked := .F.
      ::__cardSetBackGround()
      IF hb_IsEvalItem( ::bHqlRightButtonPressed )
         EVAL( ::bHqlRightButtonPressed, nButton, Self )  // Self always as last
      END IF
      lStopEvents := .T.
      EXIT

   CASE Qt_MidButton
      /*hb_trace( HB_TR_ALWAYS, "mid-PRESS" ) */
      EXIT

   ENDSWITCH
RETURN lStopEvents

/*!

 \brief [PROTECTED] signal/event handler
   false ==> .F. means don't stop event hanlder, else .T. STOP see Harbour Changelog
 \param(IN) ...
 \return bool

*/
SIGNAL __hql_QResize(oEvent) CLASS g_cardWidget
   ::__makePixmap( oEvent:size() )
RETURN .F.

// ==================== HIDDEN section ====================
