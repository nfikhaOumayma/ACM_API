package com.acm.repository;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.querydsl.QuerydslPredicateExecutor;
import org.springframework.data.repository.CrudRepository;
import org.springframework.data.repository.PagingAndSortingRepository;

import com.acm.utils.models.AcmCreditLine;

public interface AcmCreditLineRepository
extends JpaRepository<AcmCreditLine, Long>, QuerydslPredicateExecutor<AcmCreditLine>,
CrudRepository<AcmCreditLine, Long>, PagingAndSortingRepository<AcmCreditLine, Long> {
	
}
