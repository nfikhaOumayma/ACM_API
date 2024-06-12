/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.service;

import java.io.IOException;

import com.acm.exceptions.type.ApiAbacusException;
import com.acm.exceptions.type.CheckApprovelLevelException;
import com.acm.exceptions.type.CheckFeesException;
import com.acm.exceptions.type.CheckMezaCardException;
import com.acm.exceptions.type.CheckMezaCardUntrustException;
import com.acm.exceptions.type.CreditException;
import com.acm.exceptions.type.DisbursementException;
import com.acm.exceptions.type.EnableCriticalDataException;
import com.acm.exceptions.type.ResourcesNotFoundException;
import com.acm.exceptions.type.UploadDocumentNotFoundException;
import com.acm.exceptions.type.WorkFlowSettingException;
import com.acm.utils.dtos.LoanDTO;

/**
 * {@link LoanWorkflowSystemService} class.
 *
 * @author HaythemBenizid
 * @since 0.1.0
 */
public interface LoanWorkflowSystemService {

	/**
	 * Detect new loan.
	 *
	 * @author HaythemBenizid
	 * @param loanDTO the loan DTO
	 * @return the loan DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	LoanDTO processDetectNewLoan(LoanDTO loanDTO) throws ResourcesNotFoundException;

	/**
	 * Check etat required docs.
	 *
	 * @author HaythemBenizid
	 * @param loanDTO the loan DTO
	 * @return the loan DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 * @throws UploadDocumentNotFoundException the upload document not found exception
	 * @throws CheckApprovelLevelException the check approvel level exception
	 * @throws ApiAbacusException the api abacus exception
	 * @throws IOException Signals that an I/O exception has occurred.
	 * @throws CreditException the credit exception
	 * @throws DisbursementException the disbursement exception
	 * @throws WorkFlowSettingException the work flow setting exception
	 * @throws CheckMezaCardException the check meza card exception
	 * @throws CheckMezaCardUntrustException the check meza card untrust exception
	 * @throws CheckFeesException the check fees exception
	 */
	LoanDTO processCheckEtatRequiredDocs(LoanDTO loanDTO) throws ResourcesNotFoundException,
			UploadDocumentNotFoundException, CheckApprovelLevelException, ApiAbacusException,
			IOException, CreditException, DisbursementException, WorkFlowSettingException,
			CheckMezaCardException, CheckMezaCardUntrustException, CheckFeesException;

	/**
	 * Submit loan approval.
	 *
	 * @author HaythemBenizid
	 * @param loanDTO the loan DTO
	 * @return the loan DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 * @throws EnableCriticalDataException the enable critical data exception
	 * @throws CreditException the credit exception
	 */
	LoanDTO processSubmitLoanApproval(LoanDTO loanDTO)
			throws ResourcesNotFoundException, EnableCriticalDataException, CreditException;

	/**
	 * Process etat valide.
	 *
	 * @author HaythemBenizid
	 * @param loanDTO the loan DTO
	 * @return the loan DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	LoanDTO processEtatValide(LoanDTO loanDTO) throws ResourcesNotFoundException;

	/**
	 * Process etat rejete.
	 *
	 * @author HaythemBenizid
	 * @param loanDTO the loan DTO
	 * @return the loan DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 * @throws IOException Signals that an I/O exception has occurred.
	 * @throws ApiAbacusException the api abacus exception
	 */
	LoanDTO processEtatRejete(LoanDTO loanDTO)
			throws ResourcesNotFoundException, IOException, ApiAbacusException;

	/**
	 * Process etat canceled.
	 *
	 * @author HaythemBenizid
	 * @param loanDTO the loan DTO
	 * @return the loan DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 * @throws ApiAbacusException the api abacus exception
	 * @throws IOException Signals that an I/O exception has occurred.
	 */
	LoanDTO processEtatCanceled(LoanDTO loanDTO)
			throws ResourcesNotFoundException, ApiAbacusException, IOException;

	/**
	 * Process check approbation.
	 *
	 * @author HaythemBenizid
	 * @param loanDTO the loan DTO
	 * @return the loan DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 * @throws CheckApprovelLevelException the check approvel level exception
	 */
	LoanDTO processCheckApprobationLevel(LoanDTO loanDTO)
			throws ResourcesNotFoundException, CheckApprovelLevelException;

	/**
	 * Process check guarantor collateral.
	 *
	 * @author HaythemBenizid
	 * @param loanDTO the loan DTO
	 * @return the loan DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	LoanDTO processCheckGuarantorCollateral(LoanDTO loanDTO) throws ResourcesNotFoundException;

	/**
	 * Data control CBS.
	 *
	 * @author HaythemBenizid
	 * @param loanDTO the loan DTO
	 * @return the loan DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	LoanDTO processDataControlCBS(LoanDTO loanDTO) throws ResourcesNotFoundException;

	/**
	 * Process assign to.
	 *
	 * @author HaythemBenizid
	 * @param loanDTO the loan DTO
	 * @return the loan DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	LoanDTO processAssignTo(LoanDTO loanDTO) throws ResourcesNotFoundException;

	/**
	 * Check Screening.
	 *
	 * @author YesserSomai
	 * @param loanDTO the loan DTO
	 * @return the loan DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	LoanDTO processCheckScreening(LoanDTO loanDTO) throws ResourcesNotFoundException;

	/**
	 * Check FieldVisit.
	 *
	 * @author YesserSomai
	 * @param loanDTO the loan DTO
	 * @return the loan DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	LoanDTO processCheckFieldVisit(LoanDTO loanDTO) throws ResourcesNotFoundException;

	/**
	 * processAuditRisk.
	 *
	 * @author YesserSomai
	 * @param loanDTO the loan DTO
	 * @return the loan DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	LoanDTO processAuditRisk(LoanDTO loanDTO) throws ResourcesNotFoundException;

}
