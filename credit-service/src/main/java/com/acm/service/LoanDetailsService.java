/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.service;

import java.util.List;

import com.acm.utils.dtos.AcmCollateralDTO;
import com.acm.utils.dtos.CollaterolDTO;
import com.acm.utils.dtos.CustomerDTO;
import com.acm.utils.dtos.FinancialAnalysisDTO;
import com.acm.utils.dtos.GuarantorDTO;
import com.acm.utils.dtos.LoanDTO;
import com.acm.utils.dtos.LoanDetailsDTO;
import com.acm.utils.dtos.ScheduleDTO;
import com.acm.utils.dtos.SettingDocumentTypeDTO;

/**
 * {@link LoanDetailsService} interface.
 *
 * @author HaythemBenizid
 * @since 0.5.0
 */
public interface LoanDetailsService {

	/**
	 * Find loan details by ID loan extern from ABACUS DB.
	 *
	 * @author HaythemBenizid
	 * @param idLoanExtern the id loan extern
	 * @return the loan DTO
	 */
	LoanDetailsDTO findDetailsLoan(Long idLoanExtern);

	/**
	 * Find details customer by ID loan extern from ABACUS DB.
	 *
	 * @author HaythemBenizid
	 * @param idLoanExtern the id loan extern
	 * @return the loan details DTO
	 */
	CustomerDTO findDetailsCustomer(Long idLoanExtern);

	/**
	 * Find details guarantors by ID loan extern from ABACUS DB.
	 *
	 * @author YesserSomai
	 * @param idLoanExtern the id loan extern
	 * @return the loan details DTO
	 */
	List<GuarantorDTO> findGuarantors(Long idLoanExtern);

	/**
	 * Find details collaterols by ID loan extern from ABACUS DB.
	 *
	 * @author YesserSomai
	 * @param idLoanExtern the id loan extern
	 * @return the loan details DTO
	 */
	List<CollaterolDTO> findCollaterols(Long idLoanExtern);

	/**
	 * Find active and inactive collaterols.
	 *
	 * @author mlamloum
	 * @param idLoanExterns the id loan externs
	 * @return the list
	 */
	List<AcmCollateralDTO> findActiveAndInactiveCollaterols(List<Long> idLoanExterns);

	/**
	 * Find financialAnalysis by idLoan from ABACUS DB.
	 * 
	 * @author YesserSomai
	 * @param idLoan the id Loan
	 * @return the financialAnalysis DTO
	 */
	List<FinancialAnalysisDTO> findFinancialAnalysis(Long idLoan);

	/**
	 * Find customer account schedule by loan from ABACUS DB.
	 * 
	 * @author HaythemBenizid
	 * @param idLoanExtern the id loan extern
	 * @return the list {@link ScheduleDTO}
	 */
	List<ScheduleDTO> findCustomerAccountScheduleByLoan(Long idLoanExtern);

	/**
	 * Find required document by given params (idproduct).
	 * 
	 * @author HaythemBenizid
	 * @param loanDTO the loan DTO
	 * @return the list
	 */
	List<SettingDocumentTypeDTO> findRequiredDocument(LoanDTO loanDTO);
}
