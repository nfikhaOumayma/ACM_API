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

import com.acm.utils.models.ThirdPartyHistorique;

/**
 * Class provides service dao for {@link ThirdPartyHistorique} table.
 *
 * @author HaythemBenizid
 * @since 1.0.11
 */
@Repository
public interface ThirdPartyHistoriqueRepository extends JpaRepository<ThirdPartyHistorique, Long>,
		QuerydslPredicateExecutor<ThirdPartyHistorique>, CrudRepository<ThirdPartyHistorique, Long>,
		PagingAndSortingRepository<ThirdPartyHistorique, Long> {

	/**
	 * Find by category and id loan and id customer guarantor.
	 *
	 * @author HaythemBenizid
	 * @param category the category
	 * @param idLoan the id loan
	 * @param idCustomerGuarantor the id customer guarantor
	 * @return the list
	 */
	List<ThirdPartyHistorique> findByCategoryAndIdLoanAndIdCustomerGuarantor(String category,
			Long idLoan, Long idCustomerGuarantor);

	/**
	 * Find by category and id loan and id customer.
	 * 
	 * @author HaythemBenizid
	 * @param category the category
	 * @param idLoan the id loan
	 * @param idCustomer the id customer
	 * @return the list
	 */
	List<ThirdPartyHistorique> findByCategoryAndIdLoanAndIdCustomer(String category, Long idLoan,
			Long idCustomer);

	/**
	 * Find by id loan.
	 *
	 * @param idLoan the id loan
	 * @return the third party historique
	 */
	List<ThirdPartyHistorique> findByIdLoan(Long idLoan);

	/**
	 * Find by search query id and category.
	 *
	 * @param searchQueryId the search query id
	 * @param category the category
	 * @return the third party historique
	 */
	ThirdPartyHistorique findBySearchQueryIdAndCategory(Long searchQueryId, String category);

	/**
	 * Find by customer reis id and category.
	 *
	 * @param customerIdReis the customer id reis
	 * @param category the category
	 * @return the third party historique
	 */
	ThirdPartyHistorique findByCustomerReisIdAndCategory(Long customerIdReis, String category);

	/**
	 * Find by loan id and category order by id desc.
	 *
	 * @param loanId the loan id
	 * @param category the category
	 * @return the list
	 */
	List<ThirdPartyHistorique> findByIdLoanAndCategoryOrderByIdDesc(Long loanId, String category);
}
