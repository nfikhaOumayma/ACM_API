/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.repository;

import java.math.BigInteger;
import java.util.List;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.querydsl.QuerydslPredicateExecutor;
import org.springframework.data.repository.CrudRepository;
import org.springframework.data.repository.PagingAndSortingRepository;
import org.springframework.stereotype.Repository;

import com.acm.utils.models.Customer;
import com.acm.utils.models.Loan;

/**
 * Class provides service dao for {@link Loan} table.
 *
 * @author HaythemBenizid
 * @since 0.1.0
 */
@Repository
public interface LoanRepository extends JpaRepository<Loan, Long>, QuerydslPredicateExecutor<Loan>,
		CrudRepository<Loan, Long>, PagingAndSortingRepository<Loan, Long> {

	/**
	 * Count by statut.
	 *
	 * @author HaythemBenizid
	 * @param statut the statut
	 * @param enbled the enbled
	 * @param owners the owners
	 * @return the long
	 */
	long countByStatutAndEnabledAndOwnerIn(Integer statut, Boolean enbled, List<String> owners);

	/**
	 * Count by owner.
	 *
	 * @author HaythemBenizid
	 * @param owner the owner
	 * @param enbled the enbled
	 * @return the long
	 */
	long countByOwnerAndEnabled(String owner, Boolean enbled);

	/**
	 * Find by id loan extern.
	 *
	 * @author HaythemBenizid
	 * @param idLoanExtern the id loan extern
	 * @param enbled the enbled
	 * @return the list
	 */
	List<Loan> findByIdLoanExternAndEnabled(Long idLoanExtern, Boolean enbled);

	/**
	 * Find by id ib loan.
	 *
	 * @param idIbLoan the id ib loan
	 * @return the list
	 */
	List<Loan> findByIdIbLoan(Long idIbLoan);

	/**
	 * Find by id account extern (CUACCOUNTID in ABACUS) and enabled.
	 * 
	 * @author HaythemBenizid
	 * @param idLoanExtern the id loan extern
	 * @param enbled the enbled
	 * @return the list
	 */
	List<Loan> findByIdAccountExternAndEnabled(Long idLoanExtern, Boolean enbled);

	/**
	 * Find by parent id (loan Group) and enabled.
	 * 
	 * @author HaythemBenizid
	 * @param parentId the parent id
	 * @param enbled the enbled
	 * @return the list
	 */
	List<Loan> findByParentIdAndEnabled(Long parentId, Boolean enbled);

	/**
	 * Find by customer and enabled.
	 * 
	 * @author HaythemBenizid
	 * @param customer the customer
	 * @param enbled the enbled
	 * @return the list
	 */
	List<Loan> findByCustomerAndEnabled(Customer customer, Boolean enbled);

	/**
	 * Find by statut workflow and enabled.
	 * 
	 * @author HaythemBenizid
	 * @param statutWorkflow the statut workflow
	 * @param enbled the enbled
	 * @return the list
	 */
	List<Loan> findByStatutWorkflowAndEnabled(Integer statutWorkflow, Boolean enbled);

	/**
	 * Find child by parent id.
	 *
	 * @author Ines Dridi
	 * @param parentId the parent id
	 * @param enbled the enbled
	 * @return the list
	 */
	List<Loan> findChildByParentIdAndEnabled(Long parentId, Boolean enbled);

	/**
	 * Analytics : Count All Applied Loans by enabled and branch ID in given list and list Owner.
	 * 
	 * @author HaythemBenizid
	 * @param enbled the enbled
	 * @param branchId the branch id
	 * @param owners the owners
	 * @return the long
	 */
	long countByEnabledAndBranchIDInOrOwnerIn(Boolean enbled, List<Integer> branchId,
			List<String> owners);

	/**
	 * Analytics : Count by statutWorkflow in given list and enabled and branch ID in given list and
	 * list Owner.
	 * 
	 * @author HaythemBenizid
	 * @param listStatutWorkflow the list statut workflow
	 * @param enbled the enbled
	 * @param branchId the branch id
	 * @param owners the owners
	 * @param statutWorkflows the statut workflows
	 * @return the long
	 */
	long countByStatutWorkflowInAndEnabledAndBranchIDInOrOwnerInAndStatutWorkflowIn(
			List<Integer> listStatutWorkflow, Boolean enbled, List<Integer> branchId,
			List<String> owners, List<Integer> statutWorkflows);

	/**
	 * Count by product id and enabled.
	 * 
	 * @author HaythemBenizid
	 * @param productId the product id
	 * @param enbled the enbled
	 * @return the long
	 */
	long countByProductIdAndEnabled(Integer productId, Boolean enbled);

	/**
	 * Find by id loan extern in and statut workflow not in.
	 *
	 * @author YesserSomai
	 * @param idLoanExtern the id loan extern
	 * @param statutWorkflows the statut workflows
	 * @return the list
	 */
	List<Loan> findByIdLoanExternInAndStatutWorkflowNotIn(List<Long> idLoanExtern,
			List<Integer> statutWorkflows);

	/**
	 * Find by customer and statut workflow and enabled.
	 *
	 * @author moezMhiri
	 * @param customer the customer
	 * @param statutWorkflow the statut workflow
	 * @param enabled the enabled
	 * @return the list
	 */
	List<Loan> findByCustomerAndStatutWorkflowAndEnabled(Customer customer, Integer statutWorkflow,
			Boolean enabled);

	/**
	 * Find by owner in and enabled.
	 * 
	 * @author Ines Dridi
	 * @param owners the owners
	 * @param enbled the enbled
	 * @return the list
	 */
	List<Loan> findByOwnerInAndEnabled(List<String> owners, Boolean enbled);

	/**
	 * Find by account number extern and enabled.
	 * 
	 * @author idridi
	 * @param accountNumber the account number
	 * @param enabled the enabled
	 * @return the list
	 */
	List<Loan> findByAccountNumberExternAndEnabled(String accountNumber, Boolean enabled);

	/**
	 * Count by loan application status and id loan extern and enabled.
	 *
	 * @author mlamloum
	 * @param firstLoanApplicationStatus the first loan application status
	 * @param secondLoanApplicationStatus the second loan application status
	 * @param idAccountExtern the id account extern
	 * @param enabled the enabled
	 * @return the integer
	 */
	@Query("SELECT count(*) FROM Loan l WHERE (l.loanApplicationStatus = :firstLoanApplicationStatus or l.loanApplicationStatus = :secondLoanApplicationStatus) and l.idAccountExtern = :idAccountExtern and l.enabled = :enabled")
	Integer countByLoanApplicationStatusAndIdAccountExternAndEnabled(
			String firstLoanApplicationStatus, String secondLoanApplicationStatus,
			Long idAccountExtern, Boolean enabled);

	/**
	 * Gets the loans to turn on them job.
	 *
	 * @return the loans to turn on them job
	 */
	@Query(value = "SELECT ID_ACM_LOAN FROM ACM_LOAN lo JOIN ACM_WORKFLOW_STEP wf ON lo.ID_ACM_WORKFLOW_STEP = wf.ID_ACM_WORKFLOW_STEP WHERE DATEDIFF(HOUR, lo.DATE_LAST_UPDATE, GETDATE()) >= wf.MIN_SCORE_ACCEPTED AND wf.AUTOMATIC_STEP = 1 AND wf.ACCEPTATION_CONDITION = 'Last Update Date'",
			nativeQuery = true)
	List<BigInteger> getLoansToTurnOnThemJob();

	/**
	 * Find by loan instances ihm root.
	 *
	 * @param screen the screen
	 * @return the list
	 */
	@Query("select l from Loan l, WorkFlowStep wk where l.etapeWorkflow=wk.idWorkFlowStep and l.enabled = 1 and l.statutWorkflow not in (14,15,17) and wk.ibScreen = :screen  "
			+ "and l.signContarctValidation is not null ")
	List<Loan> findByLoanInstancesIbIhmRoot(String screen);

}
