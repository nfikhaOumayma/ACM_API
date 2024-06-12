/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.repository;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.querydsl.QuerydslPredicateExecutor;
import org.springframework.data.repository.CrudRepository;
import org.springframework.data.repository.PagingAndSortingRepository;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import com.acm.utils.models.AcmThirdParty;

/**
 * The Interface AcmThirdPartyRepository.
 */
@Repository
public interface AcmThirdPartyRepository extends
		JpaRepository<AcmThirdParty, Long>, CrudRepository<AcmThirdParty, Long>,
		PagingAndSortingRepository<AcmThirdParty, Long>,
		QuerydslPredicateExecutor<AcmThirdParty> {

	/**
	 * Find acm third party by id.
	 *
	 * @param id the id
	 * @return the acm third party
	 */
	@Query("select alctp from AcmThirdParty alctp where id=:id")
	AcmThirdParty findAcmThirdPartyById(@Param("id") Long id);

}
