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

/*
*/
CREATE CLASS oop004

   EXPORTED:
   METHOD init
   METHOD activate

   PROTECTED:
   DATA oDialog                           INIT NIL

   HIDDEN:
   METHOD __createForm

END CLASS

/*!

 \brief initialize object instance
 \param[in] widget parent
 \return SELF

*/
METHOD init( oParent ) CLASS oop004

   ::__createForm( oParent )

RETURN Self

/*!

 \brief activate this dialog
 \param[in] none
 \return SELF

*/
METHOD activate() CLASS oop004

   ::oDialog:hqlActivate()

RETURN Self

/*!

 \brief create form dialog
 \param[in] widget parent
 \return NIL

*/
METHOD __createForm( oParent ) CLASS oop004

   LOCAL oGlayout
   LOCAL nGrow := 0

   WITH OBJECT ::oDialog := hqlChildWindow( /*name*/, oParent )
      :setWindowTitle("HQLLINEEDIT focusxxx tester")
      :resize( 600, 400 )
      :setWindowIcon( QIcon( ":/hqlres/HQL96" ) )
      :setCentralWidget( hqlWidget( /*name*/ ) )

      WITH OBJECT :centralWidget()

         WITH OBJECT oGlayout := hqlGridLayout( /*name*/ )
            :hqlSetLayoutOf( ::oDialog:centralWidget() )

            // row 0 - START
            WITH OBJECT hqlLabel( /*name*/ )
               :setMinimumSize( 200, 30 )
               :setSizePolicy( QSizePolicy( QSizePolicy_Fixed, QSizePolicy_Fixed ) )
               :setText( "free text, finished, hqlValid 12345" )
               :setAlignment( hb_BitOr( Qt_AlignRight, Qt_AlignVCenter ) )
               :hqlAddMeToLayout( oGlayout, nGrow, 0 )
               :hqlAlignMeToLayout( oGlayout, Qt_AlignRight )  // is QLayout::setAlignment(QWidget *, Qt::Alignment)
            END WITH

            WITH OBJECT hqlLineEdit( "txt_01" )
               :setMinimumSize( 200, 30 )
               :setSizePolicy( QSizePolicy( QSizePolicy_Fixed, QSizePolicy_Fixed ) )
               :setClearButtonEnabled( .T. )
               :hqlAddMeToLayout( oGlayout, nGrow, 1 )
               :hqlOnEditingFinished( { |oself| udfFinished(oself) } )
               :hqlValid( { |oself| udfShowError( oself ) } )
            END WITH

            :addItem( QSpacerItem( 100, 30, QSizePolicy_Expanding, QSizePolicy_Expanding ), 0, 2, -1, -1 )
            // row 0 - END

            ++nGrow

            // row 0 - START
            WITH OBJECT hqlLabel( /*name*/ )
               :setMinimumSize( 200, 30 )
               :setSizePolicy( QSizePolicy( QSizePolicy_Fixed, QSizePolicy_Fixed ) )
               :setText( "free text, hqlOnTabPressed or return 12345" )
               :setAlignment( hb_BitOr( Qt_AlignRight, Qt_AlignVCenter ) )
               :hqlAddMeToLayout( oGlayout, nGrow, 0 )
               :hqlAlignMeToLayout( oGlayout, Qt_AlignRight )  // is QLayout::setAlignment(QWidget *, Qt::Alignment)
            END WITH

            WITH OBJECT hqlLineEdit( "txt_02" )
               :setMinimumSize( 200, 30 )
               :setSizePolicy( QSizePolicy( QSizePolicy_Fixed, QSizePolicy_Fixed ) )
               :setClearButtonEnabled( .T. )
               :hqlAddMeToLayout( oGlayout, nGrow, 1 )
               :hqlReturnAsTab( .T. )
               :hqlOnEditingFinished( { |oself| udfFinished(oself) } )
               :hqlOnTabPressed( { |oself| udfEnabling( oself, ::oDialog ) } )
            END WITH

            :addItem( QSpacerItem( 100, 30, QSizePolicy_Expanding, QSizePolicy_Expanding ), 0, 2, -1, -1 )
            // row 0 - END

            ++nGrow

            // row +1 - START
            WITH OBJECT hqlLabel( /*name*/ )
               :setMinimumSize( 120, 30 )
               :setSizePolicy( QSizePolicy( QSizePolicy_Fixed, QSizePolicy_Fixed ) )
               :setText( "lineEdit to gain focus" )
               :setAlignment( hb_BitOr( Qt_AlignRight, Qt_AlignVCenter ) )
               :hqlAddMeToLayout( oGlayout, nGrow, 0 )
               :hqlAlignMeToLayout( oGlayout, Qt_AlignRight )  // is QLayout::setAlignment(QWidget *, Qt::Alignment)
            END WITH

            WITH OBJECT hqlLineEdit( "ledummy" )
               :setMinimumSize( 200, 30 )
               :setSizePolicy( QSizePolicy( QSizePolicy_Fixed, QSizePolicy_Fixed ) )
               :setClearButtonEnabled( .T. )
               :setEnabled( .F. )
               :hqlAddMeToLayout( oGlayout, nGrow, 1 )
            END WITH
            // row +1 - END

         END WITH

      END WITH

   END WITH

RETURN NIL

STATIC PROCEDURE udfFinished(oself)
   hb_trace( HB_TR_ALWAYS, "finished SIGNAL" + ;
                           " objectName =" + oself:objectName() + ;
                           " its value=" + oself:hqlValue() + "<<" )
RETURN

STATIC FUNCTION udfShowError( oself )

   IF oself:hqlValue() == "12345"
      RETURN .T.
   ENDIF

   MsgStop( "Errore", "window error title", "enter '12345'" )

   oSelf:hqlSetFocus()

RETURN .F.

STATIC PROCEDURE udfEnabling( oself, ownd )

   IF oself:hqlValue() == "12345"
      ownd:ledummy:setEnabled( .T. )
   ELSE
      ownd:ledummy:setEnabled( .F. )
   ENDIF

RETURN
