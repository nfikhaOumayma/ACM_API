/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.service;

import java.io.IOException;

import com.acm.exceptions.type.ApiAbacusException;
import com.acm.exceptions.type.CheckAppL1NotFoundException;
import com.acm.exceptions.type.CheckAppL2NotFoundException;
import com.acm.exceptions.type.CheckAppL3NotFoundException;
import com.acm.exceptions.type.CheckAppL4NotFoundException;
import com.acm.exceptions.type.CheckApprovelLevelException;
import com.acm.exceptions.type.CheckFeesException;
import com.acm.exceptions.type.CheckMezaCardException;
import com.acm.exceptions.type.CheckMezaCardUntrustException;
import com.acm.exceptions.type.CollateralNotFoundException;
import com.acm.exceptions.type.CreditException;
import com.acm.exceptions.type.DisbursementException;
import com.acm.exceptions.type.EnableCriticalDataException;
import com.acm.exceptions.type.FieldVisitNotFoundException;
import com.acm.exceptions.type.GuarantorsNotFoundException;
import com.acm.exceptions.type.InformCustomerNotFoundException;
import com.acm.exceptions.type.InitialCheckException;
import com.acm.exceptions.type.ResourcesNotFoundException;
import com.acm.exceptions.type.UploadDocumentNotFoundException;
import com.acm.exceptions.type.UploadSignedDocNotFoundExepction;
import com.acm.exceptions.type.WorkFlowSettingException;
import com.acm.utils.dtos.LoanDTO;

// TODO: Auto-generated Javadoc
/**
 * {@link LoanWorkflowUserActionService} class.
 *
 * @author HaythemBenizid
 * @since 0.6.0
 */
public interface LoanWorkflowUserActionService {

	/**
	 * Action initial check.
	 *
	 * @author ymezrani
	 * @param loanDTO the loan DTO
	 * @return the loan DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	LoanDTO actionDynamicWorkflow(LoanDTO loanDTO) throws ResourcesNotFoundException;

	/**
	 * Action initial check.
	 *
	 * @author HaythemBenizid
	 * @param loanDTO the loan DTO
	 * @return the loan DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 * @throws InitialCheckException the initialCheck not found exception
	 * @throws ApiAbacusException the api abacus exception
	 * @throws IOException Signals that an I/O exception has occurred.
	 */
	LoanDTO actionInitialCheck(LoanDTO loanDTO) throws ResourcesNotFoundException,
			InitialCheckException, ApiAbacusException, IOException;

	/**
	 * Action field visit.
	 *
	 * @author HaythemBenizid
	 * @param loanDTO the loan DTO
	 * @return the loan DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 * @throws FieldVisitNotFoundException the fieldVisit not found exception
	 */
	LoanDTO actionFieldVisit(LoanDTO loanDTO)
			throws ResourcesNotFoundException, FieldVisitNotFoundException;

	/**
	 * Action inform customer.
	 *
	 * @author HaythemBenizid
	 * @param loanDTO the loan DTO
	 * @return the loan DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 * @throws InformCustomerNotFoundException the informCustomer not found exception
	 * @throws CheckApprovelLevelException the check approvel level exception
	 * @throws IOException Signals that an I/O exception has occurred.
	 * @throws ApiAbacusException the api abacus exception
	 * @throws CheckAppL1NotFoundException the check app L 1 not found exception
	 * @throws CheckAppL2NotFoundException the check app L 2 not found exception
	 * @throws CheckAppL3NotFoundException the check app L 3 not found exception
	 * @throws CheckAppL4NotFoundException the check app L 4 not found exception
	 */
	LoanDTO actionInformCustomer(LoanDTO loanDTO) throws ResourcesNotFoundException,
			InformCustomerNotFoundException, CheckApprovelLevelException, IOException,
			ApiAbacusException, CheckAppL1NotFoundException, CheckAppL2NotFoundException,
			CheckAppL3NotFoundException, CheckAppL4NotFoundException;

	/**
	 * Action inform customer after docs signe.
	 *
	 * @author HaythemBenizid
	 * @param loanDTO the loan DTO
	 * @return the loan DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 * @throws InformCustomerNotFoundException the informCustomer not found exception
	 */
	LoanDTO actionInformCustomerAfterDocsSigne(LoanDTO loanDTO)
			throws ResourcesNotFoundException, InformCustomerNotFoundException;

	/**
	 * Action check guarantor.
	 *
	 * @author HaythemBenizid
	 * @param loanDTO the loan DTO
	 * @return the loan DTO
	 * @throws GuarantorsNotFoundException the guaranteers not found exception
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	LoanDTO actionCheckGuarantor(LoanDTO loanDTO)
			throws GuarantorsNotFoundException, ResourcesNotFoundException;

	/**
	 * Action check collateral.
	 *
	 * @author HaythemBenizid
	 * @param loanDTO the loan DTO
	 * @return the loan DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 * @throws CollateralNotFoundException the collateral not found exception
	 */
	LoanDTO actionCheckCollateral(LoanDTO loanDTO)
			throws ResourcesNotFoundException, CollateralNotFoundException;

	/**
	 * Action upload documents.
	 *
	 * @author HaythemBenizid
	 * @param loanDTO the loan DTO
	 * @return the loan DTO
	 * @throws CheckApprovelLevelException the check approvel level exception
	 * @throws ApiAbacusException the api abacus exception
	 * @throws CreditException the credit exception
	 * @throws ResourcesNotFoundException the resources not found exception
	 * @throws UploadDocumentNotFoundException the upload document not found exception
	 * @throws IOException Signals that an I/O exception has occurred.
	 * @throws DisbursementException the disbursement exception
	 * @throws WorkFlowSettingException the work flow setting exception
	 * @throws CheckMezaCardException the check meza card exception
	 * @throws CheckMezaCardUntrustException the check meza card untrust exception
	 * @throws CheckFeesException the check fees exception
	 */
	LoanDTO actionUploadDocuments(LoanDTO loanDTO)
			throws CheckApprovelLevelException, ApiAbacusException, CreditException,
			ResourcesNotFoundException, UploadDocumentNotFoundException, IOException,
			DisbursementException, WorkFlowSettingException, CheckMezaCardException,
			CheckMezaCardUntrustException, CheckFeesException;

	/**
	 * Action add financial analysis.
	 *
	 * @author HaythemBenizid
	 * @param loanDTO the loan DTO
	 * @return the loan DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	LoanDTO actionAddFinancialAnalysis(LoanDTO loanDTO) throws ResourcesNotFoundException;

	/**
	 * Action check L 1.
	 *
	 * @author HaythemBenizid
	 * @param loanDTO the loan DTO
	 * @return the loan DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 * @throws CheckAppL1NotFoundException the checkAppL1 not found exception
	 * @throws CheckAppL2NotFoundException the checkAppL2 not found exception
	 * @throws CheckAppL3NotFoundException the checkAppL3 not found exception
	 * @throws CheckAppL4NotFoundException the checkAppL4 not found exception
	 * @throws CheckApprovelLevelException the check approvel level exception
	 * @throws ApiAbacusException the api abacus exception
	 * @throws IOException Signals that an I/O exception has occurred.
	 */
	LoanDTO actionCheckL1(LoanDTO loanDTO)
			throws ResourcesNotFoundException, CheckAppL1NotFoundException,
			CheckAppL2NotFoundException, CheckAppL3NotFoundException, CheckAppL4NotFoundException,
			CheckApprovelLevelException, ApiAbacusException, IOException;

	/**
	 * Action check L 2.
	 *
	 * @author HaythemBenizid
	 * @param loanDTO the loan DTO
	 * @return the loan DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 * @throws CheckAppL2NotFoundException the checkAppL2 not found exception
	 * @throws CheckAppL1NotFoundException the checkAppL1 not found exception
	 * @throws CheckAppL3NotFoundException the checkAppL4 not found exception
	 * @throws CheckAppL4NotFoundException the checkAppL3 not found exception
	 * @throws CheckApprovelLevelException the check approvel level exception
	 * @throws ApiAbacusException the api abacus exception
	 * @throws IOException Signals that an I/O exception has occurred.
	 */
	LoanDTO actionCheckL2(LoanDTO loanDTO)
			throws ResourcesNotFoundException, CheckAppL2NotFoundException,
			CheckAppL1NotFoundException, CheckAppL3NotFoundException, CheckAppL4NotFoundException,
			CheckApprovelLevelException, ApiAbacusException, IOException;

	/**
	 * Action check L 3.
	 *
	 * @author HaythemBenizid
	 * @param loanDTO the loan DTO
	 * @return the loan DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 * @throws CheckAppL3NotFoundException the checkAppL3 not found exception
	 * @throws CheckAppL1NotFoundException the checkAppL1 not found exception
	 * @throws CheckAppL2NotFoundException the checkAppL2 not found exception
	 * @throws CheckAppL4NotFoundException the checkAppL4 not found exception <<<<<<< HEAD
	 * @throws CheckApprovelLevelException the check approvel level exception
	 * @throws ApiAbacusException the api abacus exception
	 * @throws IOException Signals that an I/O exception has occurred.
	 */
	LoanDTO actionCheckL3(LoanDTO loanDTO)
			throws ResourcesNotFoundException, CheckAppL3NotFoundException,
			CheckAppL1NotFoundException, CheckAppL2NotFoundException, CheckAppL4NotFoundException,
			CheckApprovelLevelException, ApiAbacusException, IOException;

	/**
	 * Action check L 4.
	 *
	 * @author HaythemBenizid
	 * @param loanDTO the loan DTO
	 * @return the loan DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 * @throws CheckAppL4NotFoundException the checkAppL4 not found exception
	 * @throws CheckAppL1NotFoundException the checkAppL1 not found exception
	 * @throws CheckAppL2NotFoundException the checkAppL2 not found exception
	 * @throws CheckAppL3NotFoundException the checkAppL3 not found exception
	 * @throws CheckApprovelLevelException the check approvel level exception
	 * @throws ApiAbacusException the api abacus exception
	 * @throws IOException Signals that an I/O exception has occurred.
	 */
	LoanDTO actionCheckL4(LoanDTO loanDTO)
			throws ResourcesNotFoundException, CheckAppL4NotFoundException,
			CheckAppL1NotFoundException, CheckAppL2NotFoundException, CheckAppL3NotFoundException,
			CheckApprovelLevelException, ApiAbacusException, IOException;

	/**
	 * Action upload signed agreements.
	 *
	 * @author HaythemBenizid
	 * @param loanDTO the loan DTO
	 * @return the loan DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 * @throws UploadSignedDocNotFoundExepction the uploadSignedDoc not found exception
	 * @throws CheckApprovelLevelException the check approvel level exception
	 * @throws ApiAbacusException the api abacus exception
	 * @throws IOException Signals that an I/O exception has occurred.
	 * @throws CreditException the credit exception
	 * @throws UploadDocumentNotFoundException the upload document not found exception
	 * @throws DisbursementException the disbursement exception
	 * @throws WorkFlowSettingException the work flow setting exception
	 * @throws CheckMezaCardException the check meza card exception
	 * @throws CheckMezaCardUntrustException the check meza card untrust exception
	 * @throws CheckFeesException the check fees exception
	 */
	LoanDTO actionUploadSignedAgreements(LoanDTO loanDTO)
			throws ResourcesNotFoundException, UploadSignedDocNotFoundExepction,
			CheckApprovelLevelException, ApiAbacusException, IOException, CreditException,
			UploadDocumentNotFoundException, DisbursementException, WorkFlowSettingException,
			CheckMezaCardException, CheckMezaCardUntrustException, CheckFeesException;

	/**
	 * Action correctifs.
	 *
	 * @author HaythemBenizid
	 * @param loanDTO the loan DTO
	 * @return the loan DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	LoanDTO actionCorrectifs(LoanDTO loanDTO) throws ResourcesNotFoundException;

	/**
	 * Action Screening.
	 *
	 * @author YesserSomai
	 * @param loanDTO the loan DTO
	 * @return the loan DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 * @throws CheckApprovelLevelException the check approvel level exception
	 * @throws ApiAbacusException the api abacus exception
	 * @throws IOException Signals that an I/O exception has occurred.
	 * @throws CreditException the credit exception
	 * @throws UploadDocumentNotFoundException the upload document not found exception
	 * @throws DisbursementException the disbursement exception
	 * @throws WorkFlowSettingException the work flow setting exception
	 * @throws CheckMezaCardException the check meza card exception
	 * @throws CheckMezaCardUntrustException the check meza card untrust exception
	 * @throws CheckFeesException the check fees exception
	 */
	LoanDTO actionScreening(LoanDTO loanDTO) throws ResourcesNotFoundException,
			CheckApprovelLevelException, ApiAbacusException, IOException, CreditException,
			UploadDocumentNotFoundException, DisbursementException, WorkFlowSettingException,
			CheckMezaCardException, CheckMezaCardUntrustException, CheckFeesException;

	/**
	 * Action Audit.
	 *
	 * @author YesserSomai
	 * @param loanDTO the loan DTO
	 * @return the loan DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 * @throws EnableCriticalDataException the enable critical data exception
	 * @throws CreditException the credit exception
	 */
	LoanDTO actionAudit(LoanDTO loanDTO)
			throws ResourcesNotFoundException, EnableCriticalDataException, CreditException;

	/**
	 * Action Risk.
	 *
	 * @author YesserSomai
	 * @param loanDTO the loan DTO
	 * @return the loan DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 * @throws EnableCriticalDataException the enable critical data exception
	 * @throws CreditException the credit exception
	 */
	LoanDTO actionRisk(LoanDTO loanDTO)
			throws ResourcesNotFoundException, EnableCriticalDataException, CreditException;

	/**
	 * Action Complete loan data.
	 *
	 * @author HaythemBenizid
	 * @param loanDTO the loan DTO
	 * @return the loan DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 * @throws CreditException the credit exception
	 */

	LoanDTO actionCompletData(LoanDTO loanDTO) throws ResourcesNotFoundException, CreditException;

	/**
	 * Action center revision.
	 *
	 * @param loanDTO the loan DTO
	 * @return the loan DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 * @throws UploadSignedDocNotFoundExepction the upload signed doc not found exepction
	 * @throws CheckApprovelLevelException the check approvel level exception
	 * @throws ApiAbacusException the api abacus exception
	 * @throws IOException Signals that an I/O exception has occurred.
	 * @throws CreditException the credit exception
	 * @throws UploadDocumentNotFoundException the upload document not found exception
	 * @throws DisbursementException the disbursement exception
	 * @throws WorkFlowSettingException the work flow setting exception
	 * @throws CheckMezaCardException the check meza card exception
	 * @throws CheckMezaCardUntrustException the check meza card untrust exception
	 * @throws CheckFeesException the check fees exception
	 */
	LoanDTO actionCentralRevision(LoanDTO loanDTO)
			throws ResourcesNotFoundException, UploadSignedDocNotFoundExepction,
			CheckApprovelLevelException, ApiAbacusException, IOException, CreditException,
			UploadDocumentNotFoundException, DisbursementException, WorkFlowSettingException,
			CheckMezaCardException, CheckMezaCardUntrustException, CheckFeesException;

	/**
	 * Action dynamic audit risk.
	 *
	 * @param loanDTO the loan DTO
	 * @return the loan DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 * @throws EnableCriticalDataException the enable critical data exception
	 * @throws CreditException the credit exception
	 */
	LoanDTO actionDynamicAuditRisk(LoanDTO loanDTO) throws ResourcesNotFoundException, EnableCriticalDataException, CreditException;

}
