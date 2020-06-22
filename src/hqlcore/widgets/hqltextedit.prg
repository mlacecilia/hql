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

 \brief Returns a new hql_textEdit object instance

*/
FUNCTION hqlTextEdit( ... )
RETURN hql_textEdit():new( ... )

/*!

 \brief define hql_textEdit class

*/
CLASS hql_textEdit INHERIT hb_QTextEdit, hql_abs0100

   EXPORTED:
   METHOD init
   METHOD hqlCustomMenuEnabled            INLINE   ::lHqlCustomContextMenuEnabled
   METHOD hqlSetCustomMenuEnabled

   PROTECTED:
   VAR nHqlDefaultContextPolicy           INIT NIL
   VAR lHqlCustomContextMenuEnabled       INIT .F.
   METHOD __hqlManageOptions
   SIGNAL __hql_QCustomContextMenuRequested

   HIDDEN:

ENDCLASS

/*!

 \brief initialize object instance for given [name], [Qt flags]
 \param(IN) string, ...
 \return self

*/
METHOD init( cName, ... ) CLASS hql_textEdit

   ::QTextEdit:init( ... )
   // to know the default policy; if user chhange this value somewhere...it's a problem.
   ::nHqlDefaultContextPolicy := ::contextMenuPolicy()

   IF ( !::__hqlHasParent() )
      ::__hqlNestedWithSetParent()
   ENDIF

   ::__hqlAssignObjectName( cName )
   ::__hqlConnect()

RETURN self
/*!

 \brief set/get custom context menu enabled flag
 \param[in] [OPTIONAL] boolean
 \return boolean

*/
METHOD hqlSetCustomMenuEnabled( arg1 ) CLASS hql_textEdit

   IF ( hb_IsLogical(arg1) )

      IF ( arg1 )

         ::lHqlCustomContextMenuEnabled := .T.
         ::setContextMenuPolicy( Qt_CustomContextMenu )
         ::connect( "customContextMenuRequested(QPoint)", {|oPoint| ::__hql_QCustomContextMenuRequested(oPoint) } )

      ELSE

         ::lHqlCustomEnabled := .F.
         ::setContextMenuPolicy( ::nHqlDefaultContextPolicy )
         ::disconnect( "customContextMenuRequested(QPoint)" )

      ENDIF

   ENDIF

RETURN Self

// ==================== PROTECTED section ====================

/*!

 \brief [INTERNAL] handle HQL options
   see QTextCharFormat class

*/
METHOD __hqlManageOptions( cContext ) CLASS hql_textEdit

   LOCAL oCursor
   LOCAL oCharFormat
   LOCAL oBlockFormat

   oCursor := ::textCursor()
   oCharFormat := oCursor:charFormat()
   oBlockFormat := oCursor:blockFormat()

   SWITCH cContext

   CASE "__bold__"
      IF ( oCharFormat:fontWeight() == QFont_Bold  )
         oCharFormat:setFontWeight( QFont_Normal )
      ELSE
         oCharFormat:setFontWeight( QFont_Bold )
      END IF
      EXIT

   CASE "__italic__"
      oCharFormat:setFontItalic( !(oCharFormat:fontItalic()) )
      EXIT

   CASE "__strikeout__"
      oCharFormat:setFontStrikeOut( !(oCharFormat:fontStrikeOut()) )
      EXIT

   CASE "__underline__"
      oCharFormat:setFontUnderline( !(oCharFormat:fontUnderline()) )
      EXIT

   CASE "__black__"
      oCharFormat:setForeground( QBrush( QColor( Qt_black ) ) )
      EXIT

   CASE "__red__"
      oCharFormat:setForeground( QBrush( QColor( Qt_red ) ) )
      EXIT

   CASE "__green__"
      oCharFormat:setForeground( QBrush( QColor( Qt_green ) ) )
      EXIT

   CASE "__blue__"
      oCharFormat:setForeground( QBrush( QColor( Qt_blue ) ) )
      EXIT

   CASE "__left__"
      oBlockFormat:setAlignment( Qt_AlignLeft )
      EXIT

   CASE "__right__"
      oBlockFormat:setAlignment( Qt_AlignRight )
      EXIT

   CASE "__centered__"
      oBlockFormat:setAlignment( Qt_AlignHCenter )
      EXIT

   CASE "__justified__"
      oBlockFormat:setAlignment( Qt_AlignJustify )
      EXIT

   END SWITCH

   oCursor:mergeCharFormat( oCharFormat )
   oCursor:mergeBlockFormat( oBlockFormat )

   ::mergeCurrentCharFormat( oCharFormat )

RETURN NIL

// ==================== SLOTS/EVENTS section ====================

/*!

 \brief build context menu
   see http://www.qtcentre.org/threads/35166-extend-the-standard-context-menu-of-qtextedit
 \param[in] ... based on event/signal
 \return false ==> .F. means don't stop event hanlder, else .T. STOP see Harbour Changelog

*/
SIGNAL __hql_QcustomContextMenuRequested( oPoint ) CLASS hql_textEdit

   LOCAL oContextMenu
   LOCAL oHqlCustomMenu // to keep object alive from Harbour perspective

   oContextMenu := ::createStandardContextMenu()

   oContextMenu:addSeparator()

   WITH OBJECT oHqlCustomMenu := hqlMenu( /*name*/, oContextMenu )
      :setTitle( "HQL options" )

      // __bold__ action
      WITH OBJECT :hqlAddAction( /*name*/ )
         :setText( "&Bold" )
         :setIcon( QIcon( ":/hqlres/txtbold" ) )
         :setShortcut( QKeySequence( QKeySequence_Bold ) )  // QKeySequence::New see http://doc.qt.io/qt-5/qkeysequence.html#StandardKey-enum
         :setEnabled( ::hqlHasSelection() )
         :hqlOnTriggered( { || ::__hqlManageOptions( "__bold__" ) } )
      END WITH

      // __italic__ action
      WITH OBJECT :hqlAddAction( /*name*/ )
         :setText( "&Italic" )
         :setIcon( QIcon( ":/hqlres/txtitalic" ) )
         :setShortcut( QKeySequence( QKeySequence_Italic ) )
         :setEnabled( ::hqlHasSelection() )
         :hqlOnTriggered( { || ::__hqlManageOptions( "__italic__" ) } )
      END WITH

      // __strikeout__ action
      WITH OBJECT :hqlAddAction( /*name*/ )
         :setText( "&Strikethrough" )
         :setIcon( QIcon( ":/hqlres/txtstrout" ) )
         :setShortcut( QKeySequence( QKeySequence_Save ) )
         :setEnabled( ::hqlHasSelection() )
         :hqlOnTriggered( { || ::__hqlManageOptions( "__strikeout__" ) } )
      END WITH

      // __underline__ action
      WITH OBJECT :hqlAddAction( /*name*/ )
         :setText( "&Underline" )
         :setIcon( QIcon( ":/hqlres/txtunderl" ) )
         :setShortcut( QKeySequence( QKeySequence_Underline ) )
         :setEnabled( ::hqlHasSelection() )
         :hqlOnTriggered( { || ::__hqlManageOptions( "__underline__" ) } )
      END WITH

      // __colorMenu__ menu
      WITH OBJECT :hqlAddMenu( /*name*/ )
         :setTitle( "&Colors" )

         // __black__ action
         WITH OBJECT :hqlAddAction( /*name*/ )
            :setText( "&Black" )
            :setEnabled( ::hqlHasSelection() )
            :hqlOnTriggered( { || ::__hqlManageOptions( "__black__" ) } )
         END WITH

         // __red__ action
         WITH OBJECT :hqlAddAction( /*name*/ )
            :setText( "&Red" )
            :setEnabled( ::hqlHasSelection() )
            :hqlOnTriggered( { || ::__hqlManageOptions( "__red__" ) } )
         END WITH

         // __green__ action
         WITH OBJECT :hqlAddAction( /*name*/ )
            :setText( "&Green" )
            :setEnabled( ::hqlHasSelection() )
            :hqlOnTriggered( { || ::__hqlManageOptions( "__green__" ) } )
         END WITH

         // __blue__ action
         WITH OBJECT :hqlAddAction( /*name*/ )
            :setText( "&Blue" )
            :setEnabled( ::hqlHasSelection() )
            :hqlOnTriggered( { || ::__hqlManageOptions( "__blue__" ) } )
         END WITH

      END WITH

      // __alignMenu__ menu
      WITH OBJECT :hqlAddMenu( /*name*/ )
         :setTitle( "&Alignment" )

         // __left__ action
         WITH OBJECT :hqlAddAction( /*name*/ )
            :setText( "&Left" )
            :setIcon( QIcon( ":/hqlres/txtlalign" ) )
            :setEnabled( ::hqlHasSelection() )
            :hqlOnTriggered( { || ::__hqlManageOptions( "__left__" ) } )
         END WITH

         // __right__ action
         WITH OBJECT :hqlAddAction( /*name*/ )
            :setText( "&Right" )
            :setIcon( QIcon( ":/hqlres/txtralign" ) )
            :setEnabled( ::hqlHasSelection() )
            :hqlOnTriggered( { || ::__hqlManageOptions( "__right__" ) } )
         END WITH

         // __centered__ action
         WITH OBJECT :hqlAddAction( /*name*/ )
            :setText( "&Centered" )
            :setIcon( QIcon( ":/hqlres/txtcenter" ) )
            :setEnabled( ::hqlHasSelection() )
            :hqlOnTriggered( { || ::__hqlManageOptions( "__centered__" ) } )
         END WITH

         // __justified__ action
         WITH OBJECT :hqlAddAction( /*name*/ )
            :setText( "&Justified" )
            :setIcon( QIcon( ":/hqlres/txtjust" ) )
            :setEnabled( ::hqlHasSelection() )
            :hqlOnTriggered( { || ::__hqlManageOptions( "__justified__" ) } )
         END WITH

      END WITH

   END WITH

   // add hql custom menu to current context menu
   oContextMenu:addMenu( oHqlCustomMenu )

   // exec
   oContextMenu:exec( ::mapToGlobal( oPoint ) )

RETURN .F.

// ==================== HIDDEN section ====================
