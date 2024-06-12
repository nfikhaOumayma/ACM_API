package com.acm.repository;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.querydsl.QuerydslPredicateExecutor;
import org.springframework.data.repository.CrudRepository;
import org.springframework.data.repository.PagingAndSortingRepository;
import org.springframework.stereotype.Repository;

import com.acm.utils.models.ChargeFees;

/**
 * The Interface ChargeFeesRepository.
 */
@Repository
public interface ChargeFeesRepository
		extends JpaRepository<ChargeFees, Long>, QuerydslPredicateExecutor<ChargeFees>,
		CrudRepository<ChargeFees, Long>, PagingAndSortingRepository<ChargeFees, Long> {

}
