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

 \brief Returns a new hql_helpWindow object instance

*/
FUNCTION hqlHelpWindow( ... )
RETURN hql_helpWindow():new( ... )

/*!

 \brief define hql_helpWindow class

*/
CLASS hql_helpWindow INHERIT hql_childWindow

   EXPORTED:
   METHOD init
   METHOD hqlSetSearcPaths
   METHOD hqlSetUrl

   PROTECTED:
   METHOD __hqlCustomize
   ACCESS __hqlTheBrowser                 INLINE ::centralWidget()
   METHOD __hqlUpdateWindowTitle

   HIDDEN:

ENDCLASS

/*!

 \brief initialize object instance for given [name], [Qt flags]
 \param(IN) string, ...
 \return self

*/
METHOD init( oParent ) CLASS hql_helpWindow
   LOCAL cName := HqlFw:getAutoName()
   ::hql_childWindow:init( cName, oParent )
   ::__hqlCustomize()
RETURN self

/*!

 \brief show a page
 \param[in] QUrl object || string
 \return NIL

*/
METHOD hqlSetUrl( arg1 ) CLASS hql_helpWindow

   LOCAL oUrl

   IF ( hql_IsDerived(arg1, "Qurl") )
      oUrl := QUrl( arg1 )
   ELSEIF ( hb_IsString(arg1) )
      oUrl := QUrl( arg1 )
   ELSE
      oUrl := QUrl()
   ENDIF

   ::__hqlTheBrowser:setSource( oUrl )

RETURN Self

/*!

 \brief set search path
 \param[in] QStringList
 \return NIL

*/
METHOD hqlSetSearcPaths( ... ) CLASS hql_helpWindow
   ::__hqlTheBrowser:setSearchPaths( ... )
RETURN Self

// ==================== PROTECTED section ====================

/*!

 \brief customize widget
 \param(IN)
 \return NIL

*/
METHOD __hqlCustomize() CLASS hql_helpWindow

   // to avoid problems about object positioning, I choose to center on desktop
   ::hqlCentered( HQL_DESKTOPCENTER )
   ::setCentralWidget( hqlTextBrowser(/*name*/, self) )

   WITH OBJECT ::centralWidget()
      :setOpenExternalLinks( .T. )
      :hqlOnBackwardAvailable( { |lbool| ::btback:setEnabled( lbool ) } )
      :hqlOnForwardAvailable( { |lbool| ::btfor:setEnabled( lbool ) } )
      :hqlOnSourceChanged( { || ::__hqlUpdateWindowTitle() } )
   END WITH

   WITH OBJECT hqlToolBar(/*name*/, self )
      :setIconSize( QSize( 32, 32 ) )
      :setContextMenuPolicy( Qt_PreventContextMenu ) // to prevent rightClick to close

      WITH OBJECT :hqlAddToolButton( /*name*/ )
         :setIcon( QIcon( ":/hqlres/home" ) )
         :setToolTip( "Home" )
         :hqlOnClicked( { || ::__hqlTheBrowser:home() } )
      END WITH

      WITH OBJECT :hqlAddToolButton( "btback" )
         :setIcon( QIcon( ":/hqlres/goleft" ) )
         :setToolTip( "Backward" )
         :hqlOnClicked( { || ::__hqlTheBrowser:backward() } )
      END WITH

      WITH OBJECT :hqlAddToolButton( "btfor" )
         :setIcon( QIcon( ":/hqlres/goright" ) )
         :setToolTip( "Forward" )
         :hqlOnClicked( { || ::__hqlTheBrowser:forward() } )
      END WITH
   END WITH

   ::resize( 400, 300 )

RETURN NIL

/*!

 \brief set search path
 \param[in] QStringList
 \return NIL

*/
METHOD __hqlUpdateWindowTitle() CLASS hql_helpWindow
   ::setWindowTitle( "Help - " + ::__hqlTheBrowser:documentTitle() )
RETURN NIL

// ==================== SLOTS/EVENTS section ====================

// ==================== HIDDEN section ====================
