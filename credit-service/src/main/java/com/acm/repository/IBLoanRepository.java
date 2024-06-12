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

import com.acm.utils.models.IBLoan;

/**
 * Class provides service dao for {@link IBLoan} table.
 *
 * @author MoezMhiri
 * @since 1.0.3
 */
@Repository
public interface IBLoanRepository
		extends JpaRepository<IBLoan, Long>, QuerydslPredicateExecutor<IBLoan>,
		CrudRepository<IBLoan, Long>, PagingAndSortingRepository<IBLoan, Long> {

	/**
	 * Count by statut and enabled and branch ID in given list.
	 *
	 * @author HaythemBenizid
	 * @param statut the statut
	 * @param enbled the enbled
	 * @param branchIds the branch id
	 * @param owners the owners
	 * @param statuts the statuts
	 * @return the list
	 */
	long countByStatutAndEnabledAndBranchIDInOrOwnerInAndStatut(Integer statut, Boolean enbled,
			List<Integer> branchIds, List<String> owners, Integer statuts);

	/**
	 * Count by product id and enabled.
	 * 
	 * @author HaythemBenizid
	 * @param productId the product id
	 * @param enbled the enbled
	 * @return the long
	 */
	long countByProductIdAndEnabled(Integer productId, Boolean enbled);
}
