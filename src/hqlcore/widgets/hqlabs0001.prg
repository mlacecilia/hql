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
#include "hqlhbqt.ch"

/*!

 \brief define hql_abs0001 class; basically FOR ANY object is QWidget derived

*/
CLASS hql_abs0001 INHERIT hql_abs0000

   EXPORTED:
   METHOD hqlAddMeAsWidget
   METHOD hqlAddMeToLayout
   METHOD hqlAlignMeToLayout
   METHOD hqlCaption                      SETGET
   METHOD hqlEnabled                      SETGET
   METHOD hqlFontBold                     SETGET
   METHOD hqlFontFamily                   SETGET
   METHOD hqlFontItalic                   SETGET
   METHOD hqlFontSize                     SETGET
   METHOD hqlRunAsWindow
   METHOD hqlSetCentralWidgetOf
   METHOD hqlSetColorPalette
   METHOD hqlSetFocus
   METHOD hqlSetMeAsWidgetOf
   METHOD hqlTabstop                      SETGET
   METHOD hqlValue                        SETGET

   PROTECTED:
   METHOD __hqlCleaner
   METHOD __hqlValueGet
   METHOD __hqlValueSet

   HIDDEN:

ENDCLASS

/*!

 \brief helper to add me as widget IOW where object:addWidget can be used
 \param(IN) object receiver
 \return Self

*/
METHOD hqlAddMeAsWidget( oObject ) CLASS hql_abs0001
   IF ( ::isDerivedFrom("QWidget") .AND. __objHasMsg(oObject, "addWidget") )
      oObject:addWidget( self )
   ENDIF
RETURN Self

/*!

 \brief  add itself to a layout
 \param(IN) object receiver, ... arguments
 \return Self

   PAY ATTENTION to the number of parameters for every Qt object; only QLayout need only one: the widget to be added
      so do not alter IF sequence or at least keep QLayout test as the last.
      - QGridLayout inherits QLayout; QGridLayout:addWidget( oWidget, ... ) see Qt docs for params
      - QHBoxLayout, QVBoxLayout inherits QBoxLayout
      - QBoxLayout inherits QLayout; QBoxLayout:addWidget( oWidget, ... ) see Qt docs for params
      - QLayout:addWidget( oWidget ) see Qt docs for params

      WARNING!!
      - QFormLayout inherits QLayout but QFormLayout:addWidget() it seems not admitted. to be checked

*/
METHOD hqlAddMeToLayout( oObject, ... ) CLASS hql_abs0001
   IF ( oObject:isDerivedFrom("QGridLayout") .OR. oObject:isDerivedFrom("QBoxLayout") )
      oObject:addWidget( Self, ... )
   ELSEIF ( oObject:isDerivedFrom("QLayout") )
      oObject:addWidget( Self )
   ENDIF
RETURN Self

/*!

 \brief align my self when I'm inserted into layout
 \param(IN) object receiver, numeric alignment
 \return Self
      TODO: verify if we need to check oObject classname (isDerivedFrom) to prevent error

*/
METHOD hqlAlignMeToLayout( oObject, nAlignment ) CLASS hql_abs0001
   IF ( oObject:isDerivedFrom("QLayout") .AND. hb_IsNumeric(nAlignment) )
      oObject:setAlignment( Self, nAlignment )
   ENDIF
RETURN Self

/*!

 \brief set/get generic text
 \param(IN) [string]
 \return string

*/
METHOD hqlCaption( arg1 ) CLASS hql_abs0001
   IF ( hb_IsString(arg1) )
      ::setText( arg1 )
   ENDIF
RETURN ::text()

/*!

 \brief set/get enabled status
 \param(IN) [boolean]
 \return boolean

*/
METHOD hqlEnabled( arg1 ) CLASS hql_abs0001
   IF ( hb_IsLogical(arg1) )
      ::setEnabled(arg1)
   ENDIF
RETURN ( ::isEnabled() )

/*!

 \brief set/get font-bold status
 \param(IN) [boolean]
 \return boolean

*/
METHOD hqlFontBold( arg1 ) CLASS hql_abs0001
   LOCAL oFont
   IF ( hb_IsLogical(arg1) )
      oFont := ::font()
      oFont:setBold( arg1 )
      ::setFont( oFont )
   ENDIF
RETURN ::font:bold()

/*!

 \brief set/get font-italic status
 \param(IN) [boolean]
 \return boolean

*/
METHOD hqlFontItalic( arg1 ) CLASS hql_abs0001
   LOCAL oFont
   IF ( hb_IsLogical(arg1) )
      oFont := ::font()
      oFont:setItalic( arg1 )
      ::setFont( oFont )
   ENDIF
RETURN ::font:italic()

/*!

 \brief set/get font-family
 \param(IN) [string]
 \return string

*/
METHOD hqlFontFamily( arg1 ) CLASS hql_abs0001
   LOCAL oFont
   IF ( hb_IsString(arg1) )
      oFont := ::font()
      oFont:setFamily( arg1 )
      ::setFont( oFont )
   ENDIF
RETURN ::font:family()

/*!

 \brief set/get font-size value, sign
 \param(IN) [numeric], [string]
 \return numeric

*/
METHOD hqlFontSize( arg1, arg2 ) CLASS hql_abs0001
   LOCAL oFont
   IF ( hb_IsNumeric(arg1) )
      oFont := ::font()
      IF ( hb_IsString( arg2 ) .AND. arg2 == "+" )
         oFont:setPointSize( oFont:pointSize() + arg1 )
      ELSEIF ( hb_IsString( arg2 ) .AND. arg2 == "-" )
         oFont:setPointSize( oFont:pointSize() - arg1 )
      ELSE
         oFont:setPointSize( arg1 )
      ENDIF
      ::setFont( oFont )
   ENDIF
RETURN ::font:pointSize()

/*!

 \brief helper to act widget as window.
   WARNING 1: before you need to configure widget as window eg using Qt_Window as creation argument
   WARNING 2: it connect close event to replicate *window close event
 \param[in] none
 \return NIL

*/
METHOD hqlRunAsWindow() CLASS hql_abs0001

   IF ( hql_IsDerived(Self, "QWidget") .AND. ::isWindow() )
      ::setAttribute( Qt_WA_DeleteOnClose )
      ::connect( QEvent_Close, { |oEvent| oEvent:accept(), ::hqlDestroyer() } )  //TODO check if works fine
   ENDIF

RETURN Self

/*!

 \brief helper to set me as centralwidget (QMainWindow)
 \param[in] object optionally if nil value given parent will be used
 \return Self

*/
METHOD hqlSetCentralWidgetOf( oObject ) CLASS hql_abs0001

   oObject := IIF( hb_IsObject( oObject ), oObject, ::parent() )

   IF ( hql_IsDerived(oObject, "QMainWindow") )
      oObject:setCentralWidget( Self )
   ENDIF

RETURN Self

/*!

 \brief sets currents color-palette
 \param[in] QColor or Harbour array
 \param[in] numeric color group see Qt
 \param[in] numeric color role see Qt
 \param[in] [OPTIONAL) boolean auto fill; by default false
 \return Self

*/
METHOD hqlSetColorPalette( xValue, nColorGroup, nColorRole, lAutoFill ) CLASS hql_abs0001
   LOCAL oPalette
   LOCAL oColor

   hb_Default( @lAutoFill, .F. )

   IF ( hb_IsArray( xValue ) )
      oColor := hql_ArrayToColor( xValue )
   ELSEIF ( hql_IsDerived(xValue, "QColor") )
      oColor := xValue
   ENDIF

   IF hb_IsObject( oColor )
      ::setAutoFillBackground( lAutoFill )
      oPalette := ::palette()
      oPalette:setColor( nColorGroup, nColorRole, oColor )
      ::setPalette( oPalette )
   ENDIF

RETURN Self

/*!

 \brief helper to set focus by default Qt_OtherFocusReason used
 \param[in] numeric reason
 \return Self

*/
METHOD hqlSetFocus( nReason ) CLASS hql_abs0001
   hb_Default( @nReason, Qt_OtherFocusReason )
   ::setFocus( nReason )
RETURN Self

/*!

 \brief helper to set me as widget of (es QDockWidget:setWidget(...) ) IOW where object:setWidget can be used
 \param[in] object receiver
 \return Self

*/
METHOD hqlSetMeAsWidgetOf( oObject ) CLASS hql_abs0001

   IF ( hb_IsObject( oObject ) .AND. __objHasMsg( oObject, "setWidget" ) )
      oObject:setWidget( Self )
   ENDIF

RETURN Self

/*!

 \brief sets/returns tabstop
 \param(IN) [boolean]
 \return boolean

*/
METHOD hqlTabstop( lArg1 ) CLASS hql_abs0001
   LOCAL nFocusPolicyFlags

   IF ( hb_IsLogical(lArg1) )

      nFocusPolicyFlags := ::focusPolicy()

      IF hb_BitAnd( nFocusPolicyFlags, Qt_TabFocus ) == Qt_TabFocus // it accept focus when tab pressed

         IF ! lArg1 // widget not accept focus when tab pressed
            nFocusPolicyFlags -= Qt_TabFocus
            ::setFocusPolicy( nFocusPolicyFlags )
         ENDIF

      ELSE

         IF lArg1 // widget accept focus when tab pressed
            nFocusPolicyFlags += Qt_TabFocus
            ::setFocusPolicy( nFocusPolicyFlags )
         ENDIF

      ENDIF

   ENDIF

   nFocusPolicyFlags := ::focusPolicy()

RETURN ( hb_BitAnd( nFocusPolicyFlags, Qt_TabFocus ) == Qt_TabFocus )

/*!

 \brief set/get generic text
 \param(IN) [string]
 \return string

*/
METHOD hqlValue( ... ) CLASS hql_abs0001
   IF ( PCOUNT() > 0 )
      ::__hqlValueSet( ... )
   ENDIF
RETURN ::__hqlValueGet()

// ==================== PROTECTED section ====================

/*
 \brief [PROTECTED] Object cleaner
 \param(IN)
 \return NIL

*/
METHOD __hqlCleaner() CLASS hql_abs0001
   // do something
   ::hql_abs0000:__hqlCleaner()
RETURN NIL

/*!

 \brief [PROTECTED] get text
 \param(IN) none
 \return text

*/
METHOD __hqlValueGet() CLASS hql_abs0001
RETURN ::text()

/*!

 \brief [PROTECTED] set text
 \param(IN) string
 \return NIL

*/
METHOD __hqlValueSet( arg1 ) CLASS hql_abs0001
   IF ( hb_IsString(arg1) )
      ::setText( arg1 )
   ENDIF
RETURN NIL

// ==================== SLOTS/EVENTS section ====================

// ==================== HIDDEN section ====================
