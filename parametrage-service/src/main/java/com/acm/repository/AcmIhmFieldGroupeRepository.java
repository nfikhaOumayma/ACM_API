/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.repository;

import java.util.List;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.querydsl.QuerydslPredicateExecutor;
import org.springframework.data.repository.CrudRepository;
import org.springframework.data.repository.PagingAndSortingRepository;

import com.acm.utils.models.AcmIhmField;
import com.acm.utils.models.AcmIhmFieldGroupe;
import com.acm.utils.models.Groupe;

/**
 * {@link AcmIhmFieldGroupeRepository} class.
 *
 * @author ManelLamloum
 * @since 1.0.14
 */
public interface AcmIhmFieldGroupeRepository extends JpaRepository<AcmIhmFieldGroupe, Long>,
		QuerydslPredicateExecutor<AcmIhmFieldGroupe>, CrudRepository<AcmIhmFieldGroupe, Long>,
		PagingAndSortingRepository<AcmIhmFieldGroupe, Long> {

	/**
	 * Find byacm ihm field and group.
	 *
	 * @author ManelLamloum
	 * @param acmIhmField the acm ihm field
	 * @param groupe the groupe
	 * @return the list
	 */
	List<AcmIhmFieldGroupe> findByAcmIhmFieldAndGroup(AcmIhmField acmIhmField, Groupe groupe);

}
