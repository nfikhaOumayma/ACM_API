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
import org.springframework.stereotype.Repository;

import com.acm.utils.models.ReportSearchHistory;

/**
 * {@link ReportSearchHistoryRepository} class.
 *
 * @author HaythemBenizid
 * @since 1.0.2
 */
@Repository
public interface ReportSearchHistoryRepository extends JpaRepository<ReportSearchHistory, Long>,
		QuerydslPredicateExecutor<ReportSearchHistory>, CrudRepository<ReportSearchHistory, Long>,
		PagingAndSortingRepository<ReportSearchHistory, Long> {

	/**
	 * Find by report name and connected user and enabled.
	 * 
	 * @author HaythemBenizid
	 * @param reportName the report name
	 * @param username the username
	 * @param enabled the enabled
	 * @return the list
	 */
	List<ReportSearchHistory> findByReportNameAndUsernameAndEnabled(String reportName,
			String username, Boolean enabled);
}
