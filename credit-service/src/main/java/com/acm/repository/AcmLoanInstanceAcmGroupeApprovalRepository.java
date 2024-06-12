/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.repository;

import java.util.List;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.querydsl.QuerydslPredicateExecutor;
import org.springframework.data.repository.CrudRepository;
import org.springframework.data.repository.PagingAndSortingRepository;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import com.acm.utils.models.AcmLoanInstanceAcmGroupeApproval;
import com.acm.utils.models.LoanInstance;

/**
 * The Interface AcmLoanInstanceGroupe_AssociationRepository.
 */
@Repository
public interface AcmLoanInstanceAcmGroupeApprovalRepository
		extends JpaRepository<AcmLoanInstanceAcmGroupeApproval, Long>,
		CrudRepository<AcmLoanInstanceAcmGroupeApproval, Long>,
		PagingAndSortingRepository<AcmLoanInstanceAcmGroupeApproval, Long>,
		QuerydslPredicateExecutor<AcmLoanInstanceAcmGroupeApproval> {

	/**
	 * Find by loan instance.
	 *
	 * @param loanInstance the loan instance
	 * @return the list
	 */
	List<AcmLoanInstanceAcmGroupeApproval> findByLoanInstance(LoanInstance loanInstance);

	/**
	 * Find by validation.
	 *
	 * @param validation the validation
	 * @return the list
	 */
	List<AcmLoanInstanceAcmGroupeApproval> findByValidation(Boolean validation);

	/**
	 * Find unassainged approval by loan.
	 *
	 * @param groupeCode the groupe code
	 * @return the list
	 */
	@Query(value = "select distinct loanInstance.loan.id from AcmLoanInstanceAcmGroupeApproval where loanInstance.loan.etapeWorkflow = loanInstance.code "
			+ " and owner is null and groupeCode = :groupeCode")
	List<Long> findUnassaingedApprovalLoansByGroup(@Param("groupeCode") String groupeCode);

	/**
	 * Find unassainged approval assoc by group and etape wf.
	 *
	 * @param groupeCode the groupe code
	 * @param loanId the loan id
	 * @return the list
	 */
	@Query(value = "Select aa From AcmLoanInstanceAcmGroupeApproval aa where owner is null and groupeCode = :groupeCode and loanInstance.loan.etapeWorkflow = loanInstance.code and loanInstance.loan.id = :loanId ")
	List<AcmLoanInstanceAcmGroupeApproval> findUnassaingedApprovalAssocByGroupAndEtapeWf(
			@Param("groupeCode") String groupeCode, @Param("loanId") Long loanId);

	/**
	 * Find unassainged approval loans by owner.
	 *
	 * @param connectedLogin the connected login
	 * @return the list
	 */
	@Query(value = "select distinct loanInstance.loan.id from AcmLoanInstanceAcmGroupeApproval where loanInstance.loan.etapeWorkflow = loanInstance.code "
			+ " and owner is not null and owner = :connectedLogin and validation = false ")
	List<Long> findAssaingedApprovalLoansByOwner(@Param("connectedLogin") String connectedLogin);

}
