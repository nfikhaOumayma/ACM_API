package com.acm.repository;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.querydsl.QuerydslPredicateExecutor;
import org.springframework.data.repository.CrudRepository;
import org.springframework.data.repository.PagingAndSortingRepository;

import com.acm.utils.models.AcmToppedUpHistory;


public interface AcmToppedUpHistoryRepository
extends JpaRepository<AcmToppedUpHistory, Long>, QuerydslPredicateExecutor<AcmToppedUpHistory>,
CrudRepository<AcmToppedUpHistory, Long>, PagingAndSortingRepository<AcmToppedUpHistory, Long> {
	
}
