/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.service;

import java.util.List;

import com.acm.utils.dtos.LoanApprovalProcessDTO;
import com.acm.utils.dtos.LoanDTO;
import com.acm.utils.dtos.ScheduleDTO;
import com.acm.utils.dtos.SettingMotifRejetsDTO;
import com.acm.utils.dtos.SettingTopupValidityDTO;

/**
 * {@link LoanAbacusService} interface.
 *
 * @author HaythemBenizid
 * @since 0.1.0
 */
public interface LoanAbacusService {

	/**
	 * Find list loan by statut=1 and from given limite value (USED ONLY BY BATCH).
	 * 
	 * @author HaythemBenizid
	 * @param limite the limite
	 * @return the loan DTO
	 */
	List<LoanDTO> find(Long limite);

	/**
	 * Find loan by account number.
	 * 
	 * @author idridi
	 * @param accountNumber the account number
	 * @return the list
	 */
	List<LoanDTO> findLoanByAccountNumber(String accountNumber);

	/**
	 * Find issued loans by given ID from ABACUS DB (USED ONLY BY BATCH).
	 * 
	 * @author HaythemBenizid
	 * @param ids the ids
	 * @return the list
	 */
	List<LoanDTO> findIssuedLoans(List<Long> ids);

	/**
	 * Find all motif rejet.
	 *
	 * @author ManelLamloum
	 * @return the list
	 */
	List<SettingMotifRejetsDTO> findAllMotifRejet();

	/**
	 * Find details.
	 * 
	 * @author YesserSomai
	 * @param idLoan the id loan
	 * @return the loan DTO
	 */
	LoanDTO findDetails(Long idLoan);

	/**
	 * Find schedule.
	 * 
	 * @author YesserSomai
	 * @param idLoan the id loan
	 * @return the list
	 */
	List<ScheduleDTO> findSchedule(Long idLoan);

	/**
	 * Find approval process for given Loan.
	 * 
	 * @author HaythemBenizid
	 * @param idLoan the id loan
	 * @return the list
	 */
	List<LoanApprovalProcessDTO> findApprovalProcess(Long idLoan);

	/**
	 * Find canceled loan.
	 *
	 * @author YesserSomai
	 * @return the list
	 */
	List<LoanDTO> findCanceledLoan();

	/**
	 * Find active account for customer and loan.
	 *
	 * @author HaythemBenizid
	 * @param idAccount the id account
	 * @return the long
	 */
	Long findActiveAccountForCustomerAndLoan(List<Long> idAccount);

	/**
	 * Gets the closing balanceby id loan.
	 * 
	 * @author idridi
	 * @param idLoanExtern the id loan extern
	 * @return the closing balanceby id loan
	 */
	Long getClosingBalancebyIdLoan(Long idLoanExtern);

	/**
	 * Check setting topup validity.
	 *
	 * @author mlamloum
	 * @param loanDTO the loan DTO
	 * @return the setting topup validity DTO
	 */
	SettingTopupValidityDTO checkSettingTopupValidity(LoanDTO loanDTO);
}
