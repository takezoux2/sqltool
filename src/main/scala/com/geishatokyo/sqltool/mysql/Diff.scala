package com.geishatokyo.sqltool.mysql


object Diff{
  
	def diff( t1 : Table, t2 : Table) : TableDiff = {
    TableDiff(
      diffForAddColumn(t1,t2),
      diffForDeleteColiumn(t1,t2),
      diffForAlterColumn(t1,t2),
      diffForAddIndex(t1,t2),
      diffForDeleteIndex(t1,t2),
      diffForAlterIndex(t1,t2)
    )


	}

	def diffForAddColumn(t1 : Table, t2 : Table) = {
    val colNames = t2.columns.map(_.name.toLowerCase) diff t1.columns.map(_.name.toLowerCase)
    colNames.map( t2.column(_))
	}

	def diffForDeleteColiumn(t1 : Table, t2 : Table) = {
		diffForAddColumn(t2,t1)
	}

	def diffForAlterColumn(t1 : Table,t2 : Table) = {
		val d1 = t1.columns.map(_.name.toLowerCase) diff t2.columns.map(_.name.toLowerCase)
		val d2 = t2.columns.map(_.name.toLowerCase) diff t1.columns.map(_.name.toLowerCase)
    val common = t1.columns.map(_.name.toLowerCase) diff d1 diff d2
    
    val cols1 = common.map(t1.column(_))
    val cols2 = common.map(t2.column(_))

    (cols1 zip cols2).filter( {case (c1,c2) => c1 != c2})

	}

	def diffForAddIndex(t1 : Table, t2 : Table) : List[Index] = {
    val indexNames = t2.indexes.map(_.name.toLowerCase) diff t1.indexes.map(_.name.toLowerCase)

		val indexes = indexNames.map(t2.index(_))
		if(!t1.primaryKey.isDefined && t2.primaryKey.isDefined){
			t2.primaryKey.get :: indexes
		}else{
			indexes
		}
	}

	def diffForDeleteIndex(t1 : Table,t2 : Table) = {
		diffForAddIndex(t2,t1)
	}
  
  def diffForAlterIndex(t1 : Table,t2 : Table) = {
		val d1 = t1.indexes.map(_.name.toLowerCase) diff t2.indexes.map(_.name.toLowerCase)
		val d2 = t2.indexes.map(_.name.toLowerCase) diff t1.indexes.map(_.name.toLowerCase)
    val common = t1.indexes.map(_.name.toLowerCase) diff d1 diff d2
    
    val ind1 = common.map(t1.index(_))
    val ind2 = common.map(t2.index(_))

    val indexes = (ind1 zip ind2).filter( {case (c1,c2) => c1 != c2})

    if( t1.primaryKey.isDefined && t2.primaryKey.isDefined && 
    	t1.primaryKey.get != t2.primaryKey.get){
    	(t1.primaryKey.get,t2.primaryKey.get) :: indexes

    }else{
    	indexes
    }

  }



}
case class TableDiff(
		columnToAdd : List[Column],
		columnToDelete : List[Column],
		columnToAlter : List[(Column,Column)],
		indexToAdd  : List[Index],
		indexToDelete : List[Index],
		indexToAlter : List[(Index,Index)]
)








