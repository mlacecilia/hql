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
#include "hqlinclude.ch"
#include "hbclass.ch"

CLASS oop004

   EXPORTED:
   METHOD init
   METHOD activate

   PROTECTED:
   VAR oWnd                               INIT NIL
   CLASSVAR s_nCount                      INIT 0
   METHOD __cleaner
   METHOD __createUi

   METHOD __call_oop003
   METHOD __call_oop004
   METHOD __call_oop005
   METHOD __call_oop006

   HIDDEN:

ENDCLASS

METHOD init( ... ) CLASS oop004
   ::__createUi( ... )
RETURN self

METHOD activate() CLASS oop004
   ::oWnd:hqlActivate()
RETURN self

METHOD __cleaner() CLASS oop004
   ::oWnd := NIL
RETURN NIL

METHOD __createUi( oParent ) CLASS oop004
   LOCAL oSize, oMlayout

   WITH OBJECT ::oWnd := hqlChildDialog(/*name*/, oParent)
      :setWindowTitle( "HQLCHILDDIALOG #"+hb_NtoS(++::s_nCount) )
      :hqlOnClose( { || ::__cleaner() } )
      :setLayout( hqlVBoxLayout() )
   END WITH
   oMlayout :=  ::oWnd:layout()

   WITH OBJECT ::oWnd
      WITH OBJECT hqlPushButton()
         :hqlAddMeToLayout( oMlayout )
         :hqlCaption( "Close all windows" )
         :hqlOnClicked( { || hqlQapplication:closeAllWindows() } )
      END WITH

      WITH OBJECT hqlPushButton()
         :hqlAddMeToLayout( oMlayout )
         :hqlCaption( "Close this window" )
         :hqlOnClicked( { || ::oWnd:hqlRelease() } )
      END WITH

      WITH OBJECT hqlPushButton()
         :hqlAddMeToLayout( oMlayout )
         :hqlCaption( "Open HQLCHILDWINDOW" )
         :hqlOnClicked( { |oFrm| oFrm := oop003():new(::oWnd), oFrm:activate() } )
         /* :hqlOnClicked( { || ::__call_oop003() } ) */
      END WITH

      WITH OBJECT hqlPushButton()
         :hqlAddMeToLayout( oMlayout )
         :hqlCaption( "Open HQLCHILDDIALOG" )
         :hqlOnClicked( { |oFrm| oFrm := oop004():new(::oWnd), oFrm:activate() } )
         /* :hqlOnClicked( { || ::__call_oop004() } ) */
      END WITH

      WITH OBJECT hqlPushButton()
         :hqlAddMeToLayout( oMlayout )
         :hqlCaption( "Open HQLMODALWINDOW (no parent)" )
         :hqlOnClicked( { |oFrm| oFrm := oop005():new(), oFrm:activate() } )
         /* :hqlOnClicked( { || ::__call_oop005() } ) */
      END WITH

      WITH OBJECT hqlPushButton()
         :hqlAddMeToLayout( oMlayout )
         :hqlCaption( "Open HQLMODALWINDOW (with parent)" )
         :hqlOnClicked( { |oFrm| oFrm := oop005():new(::oWnd), oFrm:activate() } )
         /* :hqlOnClicked( { || ::__call_oop005(::oWnd) } ) */
      END WITH

      WITH OBJECT hqlPushButton()
         :hqlAddMeToLayout( oMlayout )
         :hqlCaption( "Open HQLMODALDIALOG (no parent)" )
         :hqlOnClicked( { |oFrm| oFrm := oop006():new(), oFrm:activate() } )
         /* :hqlOnClicked( { || ::__call_oop006() } ) */
      END WITH

      WITH OBJECT hqlPushButton()
         :hqlAddMeToLayout( oMlayout )
         :hqlCaption( "Open HQLMODALDIALOG (with parent)" )
         :hqlOnClicked( { |oFrm| oFrm := oop006():new(::oWnd), oFrm:activate() } )
         /* :hqlOnClicked( { || ::__call_oop006(::oWnd) } ) */
      END WITH
   END WITH

   // trick to resize window at 90% of desktop
   oSize := oParent:size()
   oSize := QSize( oSize:width()*0.9, oSize:height()*0.9 )
   ::oWnd:resize( oSize )

RETURN NIL

METHOD __call_oop003() CLASS oop004
   LOCAL oPgm := oop003():new( ::oWnd )
   oPgm:activate()
RETURN NIL

METHOD __call_oop004() CLASS oop004
   LOCAL oPgm := oop004():new( ::oWnd )
   oPgm:activate()
RETURN NIL

METHOD __call_oop005( oparent ) CLASS oop004
   LOCAL oPgm := oop005():new( oparent )
   oPgm:activate()
RETURN NIL

METHOD __call_oop006( oparent ) CLASS oop004
   LOCAL oPgm := oop006():new( oparent )
   oPgm:activate()
RETURN NIL
