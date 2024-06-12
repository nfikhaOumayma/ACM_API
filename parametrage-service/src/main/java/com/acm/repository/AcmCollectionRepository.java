/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.repository;

import javax.transaction.Transactional;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.querydsl.QuerydslPredicateExecutor;
import org.springframework.data.repository.CrudRepository;
import org.springframework.data.repository.PagingAndSortingRepository;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import com.acm.utils.models.AcmCollection;

/**
 * Class provides service dao for {@link AcmCollection} table.
 *
 * @author idridi
 * @since 1.1.9
 */
@Repository
public interface AcmCollectionRepository
		extends JpaRepository<AcmCollection, Long>, QuerydslPredicateExecutor<AcmCollection>,
		CrudRepository<AcmCollection, Long>, PagingAndSortingRepository<AcmCollection, Long> {

	/**
	 * Close collections.
	 *
	 * @param closedStatus the closed status
	 * @param openStatus the open status
	 * @return the int
	 */
	@Modifying(clearAutomatically = true)
	@Query("UPDATE AcmCollection SET STATUS = :closedStatus, dateLastUpdate = GETDATE()  where (FORMAT (COALESCE(dateLastUpdate, dateInsertion), 'yyyy-MM-dd') < FORMAT (GETDATE(), 'yyyy-MM-dd') ) and STATUS = :openStatus and collectionType = 'COLLECTION' ")
	@Transactional
	int closeCollections(@Param("closedStatus") Integer closedStatus,
			@Param("openStatus") Integer openStatus);

	/**
	 * Close tasks by id collections.
	 *
	 * @param statutClose the statut close
	 * @param collectionStatus the collection status
	 * @param statutNew the statut new
	 * @return the int
	 */
	@Modifying
	@Query(value = "update ACM_CALENDAR_EVENT set STATUT = :statutClose where ID_ACM_CALENDAR_EVENT in ( "
			+ " select ev.ID_ACM_CALENDAR_EVENT from ACM_COLLECTION col "
			+ " inner join ACM_CALENDAR_EVENT ev on ev.ACM_ID_COLLECTION = col.ID_ACM_COLLECTION "
			+ " where col.STATUS = :collectionStatus and ev.STATUT = :statutNew ) ",
			nativeQuery = true)
	@Transactional
	int closeTasksForClosedCollections(@Param("statutClose") String statutClose,
			@Param("collectionStatus") Integer collectionStatus,
			@Param("statutNew") String statutNew);
}
