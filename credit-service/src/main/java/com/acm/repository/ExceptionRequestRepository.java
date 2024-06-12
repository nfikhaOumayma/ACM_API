package com.acm.repository;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.querydsl.QuerydslPredicateExecutor;
import org.springframework.data.repository.CrudRepository;
import org.springframework.data.repository.PagingAndSortingRepository;
import org.springframework.stereotype.Repository;

import com.acm.utils.models.ExceptionRequest;

@Repository
public interface ExceptionRequestRepository
		extends JpaRepository<ExceptionRequest, Long>, QuerydslPredicateExecutor<ExceptionRequest>,
		CrudRepository<ExceptionRequest, Long>, PagingAndSortingRepository<ExceptionRequest, Long> {

}
