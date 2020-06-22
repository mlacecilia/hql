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
CREATE CLASS oop003

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
METHOD init( oParent ) CLASS oop003

   ::__createForm( oParent )

RETURN Self

/*!

 \brief activate this dialog
 \param[in] none
 \return SELF

*/
METHOD activate() CLASS oop003

   ::oDialog:hqlActivate()

RETURN Self

/*!

 \brief create form dialog
 \param[in] widget parent
 \return NIL

*/
METHOD __createForm( oParent ) CLASS oop003

   LOCAL oGlayout
   LOCAL nGrow := 0

   WITH OBJECT ::oDialog := hqlChildWindow( /*name*/, oParent )
      :setWindowTitle("HQLLINEEDIT hqlValid, hqlWhen usage")
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
               :hqlValid( { |oself| IIF( oself:hqlValue() == "12345", .T., .F. ) } )
            END WITH

            :addItem( QSpacerItem( 100, 30, QSizePolicy_Expanding, QSizePolicy_Expanding ), 0, 2, -1, -1 )
            // row 0 - END

            ++nGrow

            // row +1 - START
            WITH OBJECT hqlLabel( /*name*/ )
               :setMinimumSize( 200, 30 )
               :setSizePolicy( QSizePolicy( QSizePolicy_Fixed, QSizePolicy_Fixed ) )
               :setText( "free text, enter 'ABC' for next hqlWhen" )
               :setAlignment( hb_BitOr( Qt_AlignRight, Qt_AlignVCenter ) )
               :hqlAddMeToLayout( oGlayout, nGrow, 0 )
               :hqlAlignMeToLayout( oGlayout, Qt_AlignRight )  // is QLayout::setAlignment(QWidget *, Qt::Alignment)
            END WITH

            WITH OBJECT hqlLineEdit( "txt_02" )
               :hqlCaseLetter( HQL_ALL_UPPERCASE )
               :setMinimumSize( 200, 30 )
               :setSizePolicy( QSizePolicy( QSizePolicy_Fixed, QSizePolicy_Fixed ) )
               :setClearButtonEnabled( .T. )
               :hqlAddMeToLayout( oGlayout, nGrow, 1 )
               :hqlOnTextEdited( { |ctext,oself| udfTextEdited(ctext,oself) } )
               // :setPlaceHolderText( "enter what you want" )
            END WITH
            // row +1 - END

            ++nGrow

            // row +1 - START
            WITH OBJECT hqlLabel( /*name*/ )
               :setMinimumSize( 200, 30 )
               :setSizePolicy( QSizePolicy( QSizePolicy_Fixed, QSizePolicy_Fixed ) )
               :setText( "free text, hqlWhen" )
               :setAlignment( hb_BitOr( Qt_AlignRight, Qt_AlignVCenter ) )
               :hqlAddMeToLayout( oGlayout, nGrow, 0 )
               :hqlAlignMeToLayout( oGlayout, Qt_AlignRight )  // is QLayout::setAlignment(QWidget *, Qt::Alignment)
            END WITH

            WITH OBJECT hqlLineEdit( "txt_03" )
               :hqlCaseLetter( HQL_ALL_LOWERCASE )
               :setMinimumSize( 200, 30 )
               :setSizePolicy( QSizePolicy( QSizePolicy_Fixed, QSizePolicy_Fixed ) )
               :setClearButtonEnabled( .T. )
               :hqlAddMeToLayout( oGlayout, nGrow, 1 )
               :hqlWhen( { || IIF( ::oDialog:txt_02:hqlValue() == "ABC", .T., .F. ) } )
               :hqlOnTextEdited( { |ctext,oself| udfTextEdited(ctext,oself) } )
            END WITH
            // row +1 - END

            ++nGrow

            // row +1 - START
            WITH OBJECT hqlLabel( /*name*/ )
               :setMinimumSize( 200, 30 )
               :setSizePolicy( QSizePolicy( QSizePolicy_Fixed, QSizePolicy_Fixed ) )
               :setText( "free text, hqlvalid 192.168.001.001" )
               :setAlignment( hb_BitOr( Qt_AlignRight, Qt_AlignVCenter ) )
               :hqlAddMeToLayout( oGlayout, nGrow, 0 )
               :hqlAlignMeToLayout( oGlayout, Qt_AlignRight )  // is QLayout::setAlignment(QWidget *, Qt::Alignment)
            END WITH

            WITH OBJECT hqlLineEdit( "txt_04" )
               :setMinimumSize( 200, 30 )
               :setSizePolicy( QSizePolicy( QSizePolicy_Fixed, QSizePolicy_Fixed ) )
               :setClearButtonEnabled( .T. )
               :setInputMask( "000.000.000.000;_" )
               :hqlAddMeToLayout( oGlayout, nGrow, 1 )
               :hqlValid( { |oself| IIF( oself:hqlValue() == "192.168.001.001", .T., .F. ) } )
               :hqlOnEditingFinished( { |oself| udfFinished(oself) } )
               :hqlOnReturnPressed( { |oself| udfReturnPressed(oself) } )
            END WITH
            // row +1 - END

            ++nGrow

            // row +1 - START
            WITH OBJECT hqlLabel( /*name*/ )
               :setMinimumSize( 200, 30 )
               :setSizePolicy( QSizePolicy( QSizePolicy_Fixed, QSizePolicy_Fixed ) )
               :setText( "free text, ISO date" )
               :setAlignment( hb_BitOr( Qt_AlignRight, Qt_AlignVCenter ) )
               :hqlAddMeToLayout( oGlayout, nGrow, 0 )
               :hqlAlignMeToLayout( oGlayout, Qt_AlignRight )  // is QLayout::setAlignment(QWidget *, Qt::Alignment)
            END WITH

            WITH OBJECT hqlLineEdit( "txt_05" )
               :setMinimumSize( 200, 30 )
               :setSizePolicy( QSizePolicy( QSizePolicy_Fixed, QSizePolicy_Fixed ) )
               :setClearButtonEnabled( .T. )
               :setInputMask( "0000-00-00" )
               :hqlAddMeToLayout( oGlayout, nGrow, 1 )
               :hqlOnEditingFinished( { |oself| udfFinished(oself) } )
            END WITH
            // row +1 - END

            ++nGrow

            // row +1 - START
            WITH OBJECT hqlLabel( /*name*/ )
               :setMinimumSize( 200, 30 )
               :setSizePolicy( QSizePolicy( QSizePolicy_Fixed, QSizePolicy_Fixed ) )
               :setText( "free text, number (test)" )
               :setAlignment( hb_BitOr( Qt_AlignRight, Qt_AlignVCenter ) )
               :hqlAddMeToLayout( oGlayout, nGrow, 0 )
               :hqlAlignMeToLayout( oGlayout, Qt_AlignRight )  // is QLayout::setAlignment(QWidget *, Qt::Alignment)
            END WITH

            WITH OBJECT hqlLineEdit( "txt_06" )
               :setMinimumSize( 200, 30 )
               :setSizePolicy( QSizePolicy( QSizePolicy_Fixed, QSizePolicy_Fixed ) )
               :setClearButtonEnabled( .T. )
               :setInputMask( "#09.90" )
               :hqlAddMeToLayout( oGlayout, nGrow, 1 )
               :hqlOnEditingFinished( { |oself| udfFinished(oself) } )
            END WITH
            // row +1 - END

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

            WITH OBJECT hqlLineEdit( /*name*/ )
               :setMinimumSize( 200, 30 )
               :setSizePolicy( QSizePolicy( QSizePolicy_Fixed, QSizePolicy_Fixed ) )
               :setClearButtonEnabled( .T. )
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

/*STATIC PROCEDURE udfTextChanged(ctext,oself)
   hb_trace( HB_TR_ALWAYS, "textChanged SIGNAL" + ;
                           " objectName =" + oself:objectName() + ;
                           " text changed=" + cText + "<<" )
RETURN */

STATIC PROCEDURE udfTextEdited(ctext,oself)
   hb_trace( HB_TR_ALWAYS, "textEdited SIGNAL" + ;
                           " objectName =" + oself:objectName() + ;
                           " text edited=" + cText + "<<" )
RETURN

STATIC PROCEDURE udfReturnPressed(oself)
   hb_trace( HB_TR_ALWAYS, "returnPressed SIGNAL" + ;
                           "objectName =" + oself:objectName() + ;
                           " its value=" + oself:hqlValue() + "<<" )
RETURN
