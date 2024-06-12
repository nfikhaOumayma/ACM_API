/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.repository;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.querydsl.QuerydslPredicateExecutor;
import org.springframework.data.repository.CrudRepository;
import org.springframework.data.repository.PagingAndSortingRepository;

import com.acm.utils.models.AcmIhmField;

/**
 * {@link AcmIhmFieldRepository} class.
 *
 * @author ManelLamloum
 * @since 1.0.14
 */
public interface AcmIhmFieldRepository
		extends JpaRepository<AcmIhmField, Long>, QuerydslPredicateExecutor<AcmIhmField>,
		CrudRepository<AcmIhmField, Long>, PagingAndSortingRepository<AcmIhmField, Long> {

}
