/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.repository;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.querydsl.QuerydslPredicateExecutor;
import org.springframework.data.repository.CrudRepository;
import org.springframework.data.repository.PagingAndSortingRepository;
import org.springframework.stereotype.Repository;

import com.acm.utils.models.Groupe;

/**
 * {@link GroupeRepository} class.
 *
 * @author HaythemBenizid
 * @since 0.3.0
 */
@Repository
public interface GroupeRepository extends JpaRepository<Groupe, Long>, CrudRepository<Groupe, Long>,
		PagingAndSortingRepository<Groupe, Long>, QuerydslPredicateExecutor<Groupe> {

	/**
	 * find groupe by groupe code.
	 *
	 * @author MoezMoez
	 * @param code the code
	 * @return the groupe
	 */
	Groupe findByCode(String code);

}
