package com.acm.repository;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.querydsl.QuerydslPredicateExecutor;
import org.springframework.data.repository.CrudRepository;
import org.springframework.data.repository.PagingAndSortingRepository;
import org.springframework.stereotype.Repository;

import com.acm.utils.models.AcmTemplateSMS;

@Repository
public interface AcmTemplateSMSRepository extends JpaRepository<AcmTemplateSMS, Long>,
		CrudRepository<AcmTemplateSMS, Long>, PagingAndSortingRepository<AcmTemplateSMS, Long>,
		QuerydslPredicateExecutor<AcmTemplateSMS> {

}
