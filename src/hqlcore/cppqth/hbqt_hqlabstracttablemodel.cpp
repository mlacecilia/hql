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
/*

   From Qt https://github.com/qt/qtbase/blob/5.9/src/corelib/itemmodels/qabstractitemmodel.cpp
    When subclassing QAbstractItemModel, at the very least you must implement
    index(), parent(), rowCount(), columnCount(), and data(). These functions
    are used in all read-only models, and form the basis of editable models.
    You can also reimplement hasChildren() to provide special behavior for
    models where the implementation of rowCount() is expensive. This makes it
    possible for models to restrict the amount of data requested by views, and
    can be used as a way to implement lazy population of model data.
    To enable editing in your model, you must also implement setData(), and
    reimplement flags() to ensure that \c ItemIsEditable is returned.  You can
    also reimplement headerData() and setHeaderData() to control the way the
    headers for your model are presented.

   It is not necessary to support every role defined in Qt::ItemDataRole.
    Depending on the type of data contained within a model, it may only be
    useful to implement the data() function to return valid information for
    some of the more common roles. Most models provide at least a textual
    representation of item data for the Qt::DisplayRole, and well-behaved
    models should also provide valid information for the Qt::ToolTipRole and
    Qt::WhatsThisRole. Supporting these roles enables models to be used with
    standard Qt views. However, for some models that handle highly-specialized
    data, it may be appropriate to provide data only for user-defined roles.
    Models that provide interfaces to resizable data structures can provide
    implementations of insertRows(), removeRows(), insertColumns(),and
    removeColumns(). When implementing these functions, it is important to
    notify any connected views about changes to the model's dimensions both
    \e before and \e after they occur:
    \list
        \li An insertRows() implementation must call beginInsertRows() \e before
           inserting new rows into the data structure, and endInsertRows()
           \e{immediately afterwards}.
        \li An insertColumns() implementation must call beginInsertColumns()
           \e before inserting new columns into the data structure, and
           endInsertColumns() \e{immediately afterwards}.
        \li A removeRows() implementation must call beginRemoveRows() \e before
           the rows are removed from the data structure, and endRemoveRows()
           \e{immediately afterwards}.
        \li A removeColumns() implementation must call beginRemoveColumns()
           \e before the columns are removed from the data structure, and
           endRemoveColumns() \e{immediately afterwards}.
    \endlist

*/
#include "hbqt.h"

#include "hbapi.h"
#include "hbapicls.h"
#include "hbapierr.h"
#include "hbapiitm.h"
#include "hbinit.h"
#include "hbstack.h"
#include "hbvm.h"

#if QT_VERSION >= 0x040500

#include "hbqt_hqlabstracttablemodel.h"

#include <QtGui/QBrush>
#include <QtGui/QColor>
#include <QtGui/QIcon>
#include <QtGui/QFont>
#include <QtGui/QPixmap>
#include <QtCore/QFlags>
#include <QtCore/QModelIndex>
#include <QtCore/QSize>
#include <QtCore/QDate>
#include <QtCore/QDateTime>
#include <QtCore/QString>
#include <QtCore/QVariant>

#if QT_VERSION <= 0x040900
#include <QtGui/QWidget>
#else
#include <QtWidgets/QWidget>
#endif

// #include <typeinfo> to use typeId( ... )

/*

   helper to convert clipper value to QVariant

*/
static QVariant hqlATM_fromClipperToQvariant( PHB_ITEM pClpValue )
{
   QVariant ret_value;

   if( !pClpValue ) {
      HB_TRACE( HB_TR_DEBUG, ( "hqlATM_fromClipperToQvariant[undefined PHB_ITEM]" ) );
      return ret_value;
   }

   switch( hb_itemType( pClpValue ) ) {
   case HB_IT_STRING:
      {
         void * pText01 = NULL;
         ret_value = hb_itemGetStrUTF8( pClpValue, &pText01, NULL );
         hb_strfree( pText01 );
         HB_TRACE( HB_TR_DEBUG, ( "hqlATM_fromClipperToQvariant[ s = %s ]", hb_itemGetCPtr( pClpValue ) ) );
         break;
      }

   case HB_IT_LOGICAL:
      {
         ret_value = hb_itemGetL( pClpValue );
         HB_TRACE( HB_TR_DEBUG, ( "hqlATM_fromClipperToQvariant[ l = %i ]", hb_itemGetL( pClpValue ) ) );
         break;
      }

   case HB_IT_DOUBLE:
      {
         ret_value = hb_itemGetND( pClpValue );
         HB_TRACE( HB_TR_DEBUG, ( "hqlATM_fromClipperToQvariant[ d = %f ]", hb_itemGetND( pClpValue ) ) );
         break;
      }

   case HB_IT_NUMERIC:
      {
         ret_value = hb_itemGetNI( pClpValue );
         HB_TRACE( HB_TR_DEBUG, ( "hqlATM_fromClipperToQvariant[ n = %i ]", hb_itemGetNI( pClpValue ) ) );
         break;
      }

   case HB_IT_OBJECT:
      {
         void * p = hbqt_get_ptr( pClpValue );
         if( hbqt_obj_isDerivedFrom( pClpValue, "QBRUSH" ) )
            ret_value = * ( ( QBrush * ) ( p ) );
         else if( hbqt_obj_isDerivedFrom( pClpValue, "QCOLOR" ) )
            ret_value = * ( ( QColor * ) ( p ) );
         else if( hbqt_obj_isDerivedFrom( pClpValue, "QICON" ) )
            ret_value = * ( ( QIcon * ) ( p ) );
         else if( hbqt_obj_isDerivedFrom( pClpValue, "QSIZE" ) )
            ret_value = * ( ( QSize * ) ( p ) );
         else if( hbqt_obj_isDerivedFrom( pClpValue, "QFONT" ) )
            ret_value = * ( ( QFont * ) ( p ) );
         else if( hbqt_obj_isDerivedFrom( pClpValue, "QPIXMAP" ) )
            ret_value = * ( ( QPixmap * ) ( p ) );
         else if( hbqt_obj_isDerivedFrom( pClpValue, "QVARIANT" ) )
            ret_value = * ( ( QVariant * ) ( p ) );
#if QT_VERSION >= 0x040900
         else if( hbqt_obj_isDerivedFrom( pClpValue, "QMODELINDEX" ) )
            ret_value = * ( ( QModelIndex * ) ( p ) );
#endif
         else
            HB_TRACE( HB_TR_DEBUG, ( "hqlTableModel_clpTOvariant[ unConverted value ]" ) );
         break;
      }
   }

   return ret_value;

}

HQLAbstractTableModel::HQLAbstractTableModel( QObject * parent ) : QAbstractTableModel( parent )
{
   HB_TRACE( HB_TR_DEBUG, ( "HQLAbstractTableModel::HQLAbstractTableModel()" ) );
}

HQLAbstractTableModel::~HQLAbstractTableModel( void )
{
   HB_TRACE( HB_TR_DEBUG, ( "HQLAbstractTableModel::~HQLAbstractTableModel()" ) );
}

/*!

   helper to create an index given row and column

*/
QModelIndex HQLAbstractTableModel::hbCreateIndex( int row, int column ) const
{
   HB_TRACE( HB_TR_DEBUG, ( "HQLAbstractTableModel::hbCreateIndex()" ) );

   if( row >= 0 && column >= 0 )
      return createIndex( row, column );

   return QModelIndex();
}

/*!

   helper to returns Qt default flags

*/
Qt::ItemFlags HQLAbstractTableModel::hbFlags( const QModelIndex &index ) const
{

   HB_TRACE( HB_TR_DEBUG, ( "HQLAbstractTableModel::hbFlags()" ) );

   Qt::ItemFlags ret_value = QAbstractTableModel::flags(index);

   HB_TRACE( HB_TR_DEBUG, ( "flags: returns=%d", (int) ret_value ) );
   return ret_value;
}

/*!

   retrieve columnCount

*/
int HQLAbstractTableModel::columnCount( const QModelIndex &parent ) const
{
   HB_TRACE( HB_TR_DEBUG, ( "HQLAbstractTableModel::columnCount()" ) );

   // parent can be omitted but I want to keep aligned function/method arguments at CLIPPER callback level

   int ret_value = 0;
   static PHB_DYNS pMessage = NULL;

   PHB_ITEM pHarbourObject;
   pHarbourObject = hbqt_bindGetHbObjectByQtObject( ( void * ) this );

   if( pHarbourObject ) {

      if( pMessage == NULL )
         pMessage = hb_dynsymGetCase( "COLUMNCOUNT" );

      if( hb_vmRequestQuery() == 0 && hb_objHasMessage( pHarbourObject, pMessage ) ) {

         if( hb_vmRequestReenter() ) {
            PHB_ITEM pHmodelIndex;
            PHB_ITEM pResult;
            pHmodelIndex = hbqt_bindGetHbObject( NULL, ( void * ) &parent, "HB_QMODELINDEX", NULL, HBQT_BIT_NONE );

            if( hb_itemType( pHmodelIndex ) && HB_IT_OBJECT ) {
               hb_objSendMsg( pHarbourObject, "COLUMNCOUNT", 1, pHmodelIndex );
               pResult = hb_param( -1, HB_IT_ANY );
               if( ( hb_itemType( pResult ) && HB_IT_INTEGER ) || ( hb_itemType( pResult ) && HB_IT_LONG ) )
                  ret_value = hb_itemGetNI( pResult );
            }
            hb_vmRequestRestore();
            hbqt_bindDestroyHbObject( pHmodelIndex );
         }
      }
   }

   hb_itemRelease( pHarbourObject );

   HB_TRACE( HB_TR_DEBUG, ( "columnCount: returns=%d", (int) ret_value ) );
   return ret_value;

}

/*!

   retrieve rowcount

*/
int HQLAbstractTableModel::rowCount( const QModelIndex &parent ) const
{
   HB_TRACE( HB_TR_DEBUG, ( "HQLAbstractTableModel::rowCount()" ) );

   // parent can be omitted but I want to keep aligned function/method arguments at CLIPPER callback level

   int ret_value = 0;
   static PHB_DYNS pMessage = NULL;

   PHB_ITEM pHarbourObject;
   pHarbourObject = hbqt_bindGetHbObjectByQtObject( ( void * ) this );

   if( pHarbourObject ) {

      if( pMessage == NULL )
         pMessage = hb_dynsymGetCase( "ROWCOUNT" );

      if( hb_vmRequestQuery() == 0 && hb_objHasMessage( pHarbourObject, pMessage ) ) {

         if( hb_vmRequestReenter() ) {
            PHB_ITEM pHmodelIndex;
            PHB_ITEM pResult;
            pHmodelIndex = hbqt_bindGetHbObject( NULL, ( void * ) &parent, "HB_QMODELINDEX", NULL, HBQT_BIT_NONE );

            if( hb_itemType( pHmodelIndex ) && HB_IT_OBJECT ) {
               hb_objSendMsg( pHarbourObject, "ROWCOUNT", 1, pHmodelIndex );
               pResult = hb_param( -1, HB_IT_ANY );
               if( ( hb_itemType( pResult ) && HB_IT_INTEGER ) || ( hb_itemType( pResult ) && HB_IT_LONG ) )
                  ret_value = hb_itemGetNI( pResult );
            }
            hb_vmRequestRestore();
            hbqt_bindDestroyHbObject( pHmodelIndex );
         }
      }
   }

   hb_itemRelease( pHarbourObject );

   HB_TRACE( HB_TR_DEBUG, ( "rowCount: returns=%d", (int) ret_value ) );
   return ret_value;
}

/*!

   retrieve flags

*/
Qt::ItemFlags HQLAbstractTableModel::flags( const QModelIndex &index ) const
{

   HB_TRACE( HB_TR_DEBUG, ( "HQLAbstractTableModel::flags()" ) );

   Qt::ItemFlags ret_value = QAbstractTableModel::flags(index);
   static PHB_DYNS pMessage = NULL;

   // this test doesn't change the right behaviour.
   if( !index.isValid() )
      return ret_value;

   PHB_ITEM pHarbourObject;
   pHarbourObject = hbqt_bindGetHbObjectByQtObject( ( void * ) this );

   if( pHarbourObject ) {

      if( pMessage == NULL )
         pMessage = hb_dynsymGetCase( "FLAGS" );

      if( hb_vmRequestQuery() == 0 && hb_objHasMessage( pHarbourObject, pMessage ) ) {

         if( hb_vmRequestReenter() ) {
            PHB_ITEM pHmodelIndex;
            PHB_ITEM pResult;
            pHmodelIndex = hbqt_bindGetHbObject( NULL, ( void * ) &index, "HB_QMODELINDEX", NULL, HBQT_BIT_NONE );
            if( hb_itemType( pHmodelIndex ) && HB_IT_OBJECT ) {
               hb_objSendMsg( pHarbourObject, "FLAGS", 1, pHmodelIndex );
               pResult = hb_param( -1, HB_IT_ANY );
               if( ( hb_itemType( pResult ) && HB_IT_INTEGER ) || ( hb_itemType( pResult ) && HB_IT_LONG ) )
                  ret_value = ( QFlags<Qt::ItemFlag> ) hb_itemGetNI( pResult );
            }
            hb_vmRequestRestore();
            hbqt_bindDestroyHbObject( pHmodelIndex );
         }
      }
   }

   hb_itemRelease( pHarbourObject );

   HB_TRACE( HB_TR_DEBUG, ( "flags: returns=%d", (int) ret_value ) );
   return ret_value;
}

/*!

   retrieve data

*/
QVariant HQLAbstractTableModel::data( const QModelIndex & index, int role ) const
{
   HB_TRACE( HB_TR_DEBUG, ( "HQLAbstractTableModel::data()" ) );

   QVariant ret_value;
   static PHB_DYNS pMessage = NULL;

   // this test doesn't change the right behaviour.
   if( !index.isValid() )
      return ret_value;

   PHB_ITEM pHarbourObject;
   pHarbourObject = hbqt_bindGetHbObjectByQtObject( ( void * ) this );

   if( pHarbourObject ) {

      if( pMessage == NULL )
         pMessage = hb_dynsymGetCase( "DATA" );

      if( hb_vmRequestQuery() == 0 && hb_objHasMessage( pHarbourObject, pMessage ) ) {

         if( hb_vmRequestReenter() ) {
            PHB_ITEM pHmodelIndex;
            PHB_ITEM pResult;
            pHmodelIndex = hbqt_bindGetHbObject( NULL, ( void * ) &index, "HB_QMODELINDEX", NULL, HBQT_BIT_NONE );

            if( hb_itemType( pHmodelIndex ) && HB_IT_OBJECT ) {
               PHB_ITEM pRole = hb_itemNew( NULL );
               hb_itemPutNI( pRole, role );
               hb_objSendMsg( pHarbourObject, "DATA", 2, pHmodelIndex, pRole );
               pResult = hb_param( -1, HB_IT_ANY );
               ret_value = hqlATM_fromClipperToQvariant( pResult );
               hb_itemRelease( pRole );
            }
            hb_vmRequestRestore();
            hbqt_bindDestroyHbObject( pHmodelIndex );
         }
      }
   }

   hb_itemRelease( pHarbourObject );

   HB_TRACE( HB_TR_DEBUG, ( "data: returns" ) );
   return ret_value;
}

/*!

   retrieve header data

*/
QVariant HQLAbstractTableModel::headerData( int section, Qt::Orientation orientation, int role ) const
{
   HB_TRACE( HB_TR_DEBUG, ( "HQLAbstractTableModel::headerData()" ) );

   QVariant ret_value;
   static PHB_DYNS pMessage = NULL;

   PHB_ITEM pHarbourObject;
   pHarbourObject = hbqt_bindGetHbObjectByQtObject( ( void * ) this );

   if( pHarbourObject ) {

      if( pMessage == NULL )
         pMessage = hb_dynsymGetCase( "HEADERDATA" );

      if( hb_vmRequestQuery() == 0 && hb_objHasMessage( pHarbourObject, pMessage ) ) {

         if( hb_vmRequestReenter() ) {
            PHB_ITEM pResult;
            PHB_ITEM pSection = hb_itemNew( NULL );
            hb_itemPutNI( pSection, section );
            PHB_ITEM pOrientation = hb_itemNew( NULL );
            hb_itemPutNI( pOrientation, orientation );
            PHB_ITEM pRole = hb_itemNew( NULL );
            hb_itemPutNI( pRole, role );
            hb_objSendMsg( pHarbourObject, "HEADERDATA", 3, pSection, pOrientation, pRole );
            pResult = hb_param( -1, HB_IT_ANY );
            ret_value = hqlATM_fromClipperToQvariant( pResult );
            // HB_TRACE( HB_TR_ALWAYS, ( "pippo[ s = %s ]", typeid(ret_value).name() ) );
            hb_itemRelease( pSection );
            hb_itemRelease( pOrientation );
            hb_itemRelease( pRole );
            hb_vmRequestRestore();
         }
      }
   }

   hb_itemRelease( pHarbourObject );

   HB_TRACE( HB_TR_DEBUG, ( "headerData: returns" ) );
   return ret_value;
}

/*!

   retrieve canFetchMore

*/
bool HQLAbstractTableModel::canFetchMore( const QModelIndex &parent ) const
{
   HB_TRACE( HB_TR_DEBUG, ( "HQLAbstractTableModel::canFetchMore()" ) );

   // parent can be omitted but I want to keep aligned function/method arguments at CLIPPER callback level

   bool ret_value = false;
   static PHB_DYNS pMessage = NULL;

   PHB_ITEM pHarbourObject;
   pHarbourObject = hbqt_bindGetHbObjectByQtObject( ( void * ) this );

   if( pHarbourObject ) {

      if( pMessage == NULL )
         pMessage = hb_dynsymGetCase( "CANFETCHMORE" );

      if( hb_vmRequestQuery() == 0 && hb_objHasMessage( pHarbourObject, pMessage ) ) {

         if( hb_vmRequestReenter() ) {
            PHB_ITEM pHmodelIndex;
            PHB_ITEM pResult;
            pHmodelIndex = hbqt_bindGetHbObject( NULL, ( void * ) &parent, "HB_QMODELINDEX", NULL, HBQT_BIT_NONE );

            if( hb_itemType( pHmodelIndex ) && HB_IT_OBJECT ) {
               hb_objSendMsg( pHarbourObject, "CANFETCHMORE", 1, pHmodelIndex );
               pResult = hb_param( -1, HB_IT_ANY );
               if( pResult && hb_itemType( pResult ) & HB_IT_LOGICAL )
                  ret_value = hb_itemGetL( pResult );
            }
            hb_vmRequestRestore();
            hbqt_bindDestroyHbObject( pHmodelIndex );
         }
      }
   }

   hb_itemRelease( pHarbourObject );

   HB_TRACE( HB_TR_DEBUG, ( "canFetchMore: returns[ l = %i ]", ret_value ) );
   return ret_value;
}

/*!

   do fetchMore

*/
void HQLAbstractTableModel::fetchMore( const QModelIndex &parent )
{
   HB_TRACE( HB_TR_DEBUG, ( "HQLAbstractTableModel::fetchMore()" ) );

   // parent can be omitted but I want to keep aligned function/method arguments at CLIPPER callback level

   static PHB_DYNS pMessage = NULL;

   PHB_ITEM pHarbourObject;
   pHarbourObject = hbqt_bindGetHbObjectByQtObject( ( void * ) this );

   if( pHarbourObject ) {

      if( pMessage == NULL )
         pMessage = hb_dynsymGetCase( "FETCHMORE" );

      if( hb_vmRequestQuery() == 0 && hb_objHasMessage( pHarbourObject, pMessage ) ) {

         if( hb_vmRequestReenter() ) {
            PHB_ITEM pHmodelIndex;
            pHmodelIndex = hbqt_bindGetHbObject( NULL, ( void * ) &parent, "HB_QMODELINDEX", NULL, HBQT_BIT_NONE );

            if( hb_itemType( pHmodelIndex ) && HB_IT_OBJECT )
               hb_objSendMsg( pHarbourObject, "FETCHMORE", 1, pHmodelIndex );

            hb_vmRequestRestore();
            hbqt_bindDestroyHbObject( pHmodelIndex );
         }
      }
   }

   hb_itemRelease( pHarbourObject );

   HB_TRACE( HB_TR_DEBUG, ( "fetchMore: executed" ) );
}

/* ================================= signals ================================= */

/*!
   reset()
*/
void HQLAbstractTableModel::reset()
{
   QAbstractTableModel::beginResetModel();
   QAbstractTableModel::endResetModel();

   emit QAbstractTableModel::layoutAboutToBeChanged();
   emit QAbstractTableModel::layoutChanged();
}


/*!
   EmitDataChanged( ... ) addon to emit dataChanged
*/
void HQLAbstractTableModel::EmitDataChanged( const QModelIndex & topLeft, const QModelIndex & bottomRight )
{
   QAbstractTableModel::dataChanged( topLeft, bottomRight );
}

/*!
   EmitHeaderDataChanged( ... ) addon to emit headerDataChanged
*/
void HQLAbstractTableModel::EmitHeaderDataChanged( Qt::Orientation orientation, int first, int last)
{
   QAbstractTableModel::headerDataChanged( orientation, first, last );
}

/*!
   EmitLayoutAboutToBeChanged() addon to emit layoutAboutToBeChanged
*/
void HQLAbstractTableModel::EmitLayoutAboutToBeChanged()
{
   QAbstractTableModel::layoutAboutToBeChanged( );
}

/*!
   EmitLayoutChanged() addon to emit layoutChanged
*/
void HQLAbstractTableModel::EmitLayoutChanged()
{
   QAbstractTableModel::layoutChanged( );
}

/*!
   beginInsertRows( ... ) addon not supplied by HbQt
*/
void HQLAbstractTableModel::beginInsertRows( const QModelIndex & parent, int first, int last )
{
   QAbstractTableModel::beginInsertRows( parent, first, last );
}

/*!
   endInsertRows() addon not supplied by HbQt
*/
void HQLAbstractTableModel::endInsertRows()
{
   QAbstractTableModel::endInsertRows();
}

/*!
   beginRemoveRows( ... ) addon not supplied by HbQt
*/
void HQLAbstractTableModel::beginRemoveRows( const QModelIndex & parent, int first, int last )
{
   QAbstractTableModel::beginRemoveRows( parent, first, last );
}

/*!
   endRemoveRows() addon not supplied by HbQt
*/
void HQLAbstractTableModel::endRemoveRows()
{
   QAbstractTableModel::endRemoveRows();
}

/*!
   beginInsertColumns( ... ) addon not supplied by HbQt
*/
void HQLAbstractTableModel::beginInsertColumns( const QModelIndex & parent, int first, int last)
{
   QAbstractTableModel::beginInsertColumns( parent, first, last );
}

/*!
   endInsertColumns() addon not supplied by HbQt
*/
void HQLAbstractTableModel::endInsertColumns()
{
   QAbstractTableModel::endInsertColumns();
}

/*!
   beginRemoveColumns( ... ) addon not supplied by HbQt
*/
void HQLAbstractTableModel::beginRemoveColumns( const QModelIndex & parent, int first, int last)
{
   QAbstractTableModel::beginRemoveColumns( parent, first, last );
}

/*!
   endRemoveColumns() addon not supplied by HbQt
*/
void HQLAbstractTableModel::endRemoveColumns()
{
   QAbstractTableModel::endRemoveColumns();
}

/*!
   beginResetModel() addon not supplied by HbQt
*/
void HQLAbstractTableModel::beginResetModel()
{
   QAbstractTableModel::beginResetModel();
}

/*!
   endResetModel() addon not supplied by HbQt
*/
void HQLAbstractTableModel::endResetModel()
{
   QAbstractTableModel::endResetModel();
}

#endif
