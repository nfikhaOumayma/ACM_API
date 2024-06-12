/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.repository;

import java.math.BigDecimal;
import java.util.List;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.querydsl.QuerydslPredicateExecutor;
import org.springframework.data.repository.CrudRepository;
import org.springframework.data.repository.PagingAndSortingRepository;
import org.springframework.stereotype.Repository;

import com.acm.utils.models.AcmMezaCard;
import com.acm.utils.models.Customer;

/**
 * {@link AcmMezaCardRepository} class.
 *
 * @author YesserSomai
 * @since 1.0.6
 */
@Repository
public interface AcmMezaCardRepository
		extends JpaRepository<AcmMezaCard, Long>, QuerydslPredicateExecutor<AcmMezaCard>,
		CrudRepository<AcmMezaCard, Long>, PagingAndSortingRepository<AcmMezaCard, Long> {

	/**
	 * Find first by branch ID and status order by CardNumber.
	 *
	 * @author ManelLamloum
	 * @param branchID the branch ID
	 * @param status the status
	 * @param acmMezaCardBanList the acm meza card ban list
	 * @return the acm meza card
	 */
	AcmMezaCard findFirstByBranchIDAndStatusAndCardNumberNotInOrderByCardNumber(Long branchID,
			String status, List<String> acmMezaCardBanList);

	/**
	 * Find first by merchant ID and card number.
	 * 
	 * @author HaythemBenizid
	 * @param merchantID the merchant ID
	 * @param cardNumber the card number
	 * @return the acm meza card
	 */
	AcmMezaCard findFirstByMerchantIDAndCardNumber(BigDecimal merchantID, String cardNumber);

	/**
	 * Find by customer ID in given List and status.
	 *
	 * @author HaythemBenizid
	 * @param customerIDs the customer Ids
	 * @param status the status
	 * @return the list
	 */
	List<AcmMezaCard> findByCustomerInAndStatus(List<Customer> customerIDs, String status);

}
