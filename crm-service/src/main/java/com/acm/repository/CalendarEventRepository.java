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

import com.acm.utils.models.CalendarEvent;

/**
 * Class provides Repo dao for {@link CalendarEvent} table.
 *
 * @author Moez Mhiri
 * @since 0.5.0
 */
@Repository
public interface CalendarEventRepository
		extends JpaRepository<CalendarEvent, Long>, QuerydslPredicateExecutor<CalendarEvent>,
		CrudRepository<CalendarEvent, Long>, PagingAndSortingRepository<CalendarEvent, Long> {

	/**
	 * Close old task by id collection.
	 *
	 * @param statut the statut
	 * @param acmVersion the acm version
	 * @param updatedBy the updated by
	 * @param idCollection the id collection
	 * @param typeEvent the type event
	 * @return the int
	 */
	@Modifying
	@Query("UPDATE CalendarEvent SET statut = :statut, dateLastUpdate = GETDATE(), acmVersion= :acmVersion, updatedBy = :updatedBy  where dateDebut < GETDATE() and idCollection = :idCollection and typeEvent != :typeEvent")
	@Transactional
	int closeOldTaskByIdCollectionAndNotTypeEvent(@Param("statut") String statut,
			@Param("acmVersion") Integer acmVersion, @Param("updatedBy") String updatedBy,
			@Param("idCollection") Long idCollection, @Param("typeEvent") String typeEvent);

	/**
	 * Close tasks for grp of users.
	 * 
	 * @author idridi
	 * @param statut the statut
	 * @param acmVersion the acm version
	 * @param updatedBy the updated by
	 * @param idCollection the id collection
	 * @param username the username
	 * @return the int
	 */
	@Modifying
	@Query("UPDATE CalendarEvent SET statut = :statut, dateLastUpdate = GETDATE(), acmVersion= :acmVersion, updatedBy = :updatedBy  where dateDebut < GETDATE() and idCollection = :idCollection and username != :username")
	@Transactional
	int closeTasksForGrpOfUsers(@Param("statut") String statut,
			@Param("acmVersion") Integer acmVersion, @Param("updatedBy") String updatedBy,
			@Param("idCollection") Long idCollection, @Param("username") String username);

}
