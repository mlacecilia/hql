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

 \brief Returns a new hql_toolBar object instance

*/
FUNCTION hqlToolBar( ... )
RETURN hql_toolBar():new( ... )

/*!

 \brief define hql_toolBar class

*/
CLASS hql_toolBar INHERIT hb_QToolBar, hql_abs0001

   EXPORTED:
   METHOD init
   METHOD hqlAddAction
   METHOD hqlAddButtonMenu
   METHOD hqlAddMeToLayout
   METHOD hqlAddToolButton
   METHOD hqlAddWidget
   METHOD hqlOnActionTriggered
   METHOD hqlPlace

   PROTECTED:
   VAR bHqlActionTriggered                INIT NIL
   SIGNAL __hql_QActionTriggered

   HIDDEN:

ENDCLASS

/*!

 \brief initialize object instance for given [name], [Qt flags]
 \param(IN) string, ...
 \return self

*/
METHOD init( cName, ... ) CLASS hql_toolBar

   // from Qt docs
   // When a QToolBar is not a child of a QMainWindow, it loses the ability to populate the extension pop up with widgets
   // added to the toolbar using addWidget(). Please use widget actions created
   // by inheriting QWidgetAction and implementing QWidgetAction::createWidget() instead.

   ::QToolBar:init( ... )

   IF ( !::__hqlHasParent() )
      ::__hqlNestedWithSetParent()  // perhaps ::setParent( oParent )  can creates some problems
   ENDIF

   IF ( ::__hqlHasParent() .AND. ::parent():isDerivedFrom("QMainWindow") )
      ::parent():addToolbar( Self )
   ENDIF

   ::__hqlAssignObjectName( cName )
   ::__hqlConnect()

RETURN self

/*!

 \brief helper to add an action
 \param[in] none
 \return NIL

*/
METHOD hqlAddAction( arg1 ) CLASS hql_toolBar
   LOCAL oObject

   IF ( hql_IsDerived(arg1, "QAction") )
      oObject := arg1
   ELSE
      arg1 := hb_DefaultValue(arg1, "")
      oObject := hqlAction( arg1, Self )
   ENDIF

   ::addAction( oObject )

RETURN oObject

/*!

 \brief helper to add toolButton with a hqlMenu; this method returns the toolbutton object.
   you can retrieve the menu object using ":menu()" eg
   WITH OBJECT :hqlAddButtonMenu( <cButtonName>, <cMenuName> )
      :setText( "button_text" )
      WITH OBJECT :menu()
         WITH OBJECT :hqlAddAction( <cActionName> )
            :setText( "&Quit" )
            :setIcon( QIcon( ":/hqlres/quit" ) )
            :setShortcut( QKeySequence( "Alt+Q" ) )
            :hqlOnTriggered( { || oWnd:close() } )
         END WITH
      END WITH
   END WITH

*/
METHOD hqlAddButtonMenu( arg1, arg2 ) CLASS hql_toolBar
   LOCAL oToolButton
   LOCAL oMenu

   arg1 := hb_DefaultValue(arg1, "")
   oToolButton := hqlToolButton( arg1, Self )
   oToolButton:setPopupMode( QToolButton_InstantPopup ) // most important

   arg2 := hb_DefaultValue(arg2, "")
   oMenu := hqlMenu( arg2, oToolButton )

   oToolButton:setMenu( oMenu )

   ::hqlAddWidget( oToolButton ) // read Qt note ::addWidget( oToolButton )

RETURN oToolButton

/*!

 \brief  add itself to a layout
 \param(IN) object
 \return Self

*/
METHOD hqlAddMeToLayout( oObject ) CLASS hql_toolBar
   IF ( oObject:isDerivedFrom("QLayout") )
      oObject:addWidget( Self )  // instead of oObject:setMenuBar( self )
   ENDIF
RETURN Self

/*!

 \brief helper to add a tool button
 \param[in] toolButton object | name
 \return toolbutton

*/
METHOD hqlAddToolButton( arg1 ) CLASS hql_toolBar
   LOCAL oToolButton

   IF ( hql_IsDerived(arg1, "QToolButton") )
      oToolButton := arg1
   ELSE
      arg1 := hb_DefaultValue(arg1, "")
      oToolButton := hqlToolButton( arg1, Self )
   ENDIF

   ::hqlAddWidget( oToolButton ) // read Qt note ::addWidget( oToolButton )

RETURN oToolButton

/*!

 \brief helper to add a widget
 \param[in] object QWidget derived
 \return NIL
   WARNING: note from Qt
   <<When a QToolBar is not a child of a QMainWindow, it loses the ability to populate the extension pop up with widgets added to the toolbar
      using addWidget(). Please use widget actions created by inheriting QWidgetAction and implementing QWidgetAction::createWidget() instead.>>


*/
METHOD hqlAddWidget( arg1 ) CLASS hql_toolBar
   LOCAL oBtn

   IF ( hql_IsDerived(arg1, "QWidget") )   // for the moment
      oBtn := QWidgetAction( Self )
      oBtn:setDefaultWidget( arg1 )
      ::addAction( oBtn )
   ENDIF

RETURN arg1

/*!

 \brief set signal/event block
 \param(IN) block
 \return Self

*/
METHOD hqlOnActionTriggered( arg1 ) CLASS hql_toolBar
   IF ( PCOUNT() == 1 .AND. ( hb_IsEvalItem(arg1 ) .OR. arg1 == NIL ) )
      ::bHqlActionTriggered := arg1
      IF ( hb_IsEvalItem(::bHqlActionTriggered) )
         ::connect( "actionTriggered(QAction*)" , { |oAction| ::__hql_QActionTriggered(oAction) } )
      ELSE
         ::disconnect( "actionTriggered(QAction*)" )
      ENDIF
   ENDIF
RETURN self

/*!

 \brief helper to place toolBar somewhere ONLY when parent is QMainWindow
 \param[in] none
 \return NIL

*/
METHOD hqlPlace( arg1 ) CLASS hql_toolBar
   LOCAL oParent

   // This property only makes sense if the toolbar is in a QMainWindow
   IF ( hb_IsNumeric(arg1) .AND. hql_IsDerived(::parent(), "QMainWindow") .AND. arg1 >= Qt_NoToolBarArea .AND. arg1 <= Qt_AllToolBarAreas )
      SWITCH ( hb_BitAnd( ::allowedAreas(), arg1 ) )
      CASE Qt_LeftToolBarArea
      CASE Qt_RightToolBarArea
      CASE Qt_TopToolBarArea
      CASE Qt_BottomToolBarArea
         oParent := ::parent()
         ::hide()
         ::parent:removeToolBar( Self )
         oParent:addToolBar( arg1, Self )
         ::show()
         EXIT
      ENDSWITCH
   ENDIF

RETURN Self

// ==================== PROTECTED section ====================

// ==================== SLOTS/EVENTS section ====================

/*!

 \brief [PROTECTED] signal/event handler
   false ==> .F. means don't stop event hanlder, else .T. STOP see Harbour Changelog
 \param(IN) ...
 \return bool

*/
SIGNAL __hql_QActionTriggered(oAction) CLASS hql_toolBar
   IF ( hb_IsEvalItem( ::bHqlActionTriggered ) )
      EVAL( ::bHqlActionTriggered, oAction, Self )   // Self always as last
   ENDIF
RETURN .F.

// ==================== HIDDEN section ====================
