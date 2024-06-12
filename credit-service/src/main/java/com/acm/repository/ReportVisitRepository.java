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

import com.acm.utils.models.ReportVisit;

/**
 * Class provides Repo dao for {@link ReportVisit} table.
 *
 * @author Yesser Somai
 * @since 0.2.0
 */
@Repository
public interface ReportVisitRepository
		extends JpaRepository<ReportVisit, Long>, QuerydslPredicateExecutor<ReportVisit>,
		CrudRepository<ReportVisit, Long>, PagingAndSortingRepository<ReportVisit, Long> {

}
