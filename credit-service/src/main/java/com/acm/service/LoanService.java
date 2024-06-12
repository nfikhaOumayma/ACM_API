/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.service;

import java.io.IOException;
import java.time.LocalDate;
import java.util.List;

import com.acm.constants.common.ACMConstantWorkflowStatuts;
import com.acm.exceptions.type.ApiAbacusException;
import com.acm.exceptions.type.CalculateAgeException;
import com.acm.exceptions.type.CancelIssuedLoanException;
import com.acm.exceptions.type.CheckAppL1NotFoundException;
import com.acm.exceptions.type.CheckAppL2NotFoundException;
import com.acm.exceptions.type.CheckAppL3NotFoundException;
import com.acm.exceptions.type.CheckAppL4NotFoundException;
import com.acm.exceptions.type.CheckApprovelLevelException;
import com.acm.exceptions.type.CheckFeesException;
import com.acm.exceptions.type.CheckMezaCardException;
import com.acm.exceptions.type.CheckMezaCardUntrustException;
import com.acm.exceptions.type.CollateralNotFoundException;
import com.acm.exceptions.type.ConditionalApproveException;
import com.acm.exceptions.type.CreditException;
import com.acm.exceptions.type.CustomerContactException;
import com.acm.exceptions.type.CustomerMaxActiveAccountException;
import com.acm.exceptions.type.DisbursementException;
import com.acm.exceptions.type.EnableCriticalDataException;
import com.acm.exceptions.type.FieldVisitNotFoundException;
import com.acm.exceptions.type.GuarantorsNotFoundException;
import com.acm.exceptions.type.InformCustomerNotFoundException;
import com.acm.exceptions.type.InitialCheckException;
import com.acm.exceptions.type.LoanAlreadyExistException;
import com.acm.exceptions.type.ResourcesNotFoundException;
import com.acm.exceptions.type.UploadDocumentNotFoundException;
import com.acm.exceptions.type.UploadSignedDocNotFoundExepction;
import com.acm.exceptions.type.WorkFlowSettingException;
import com.acm.utils.dtos.CustomerDTO;
import com.acm.utils.dtos.LoanDTO;
import com.acm.utils.dtos.ReportingDTO;
import com.acm.utils.dtos.ReportingListDTO;
import com.acm.utils.dtos.pagination.LoanPaginationDTO;

/**
 * {@link LoanService} interface.
 *
 * @author HaythemBenizid
 * @since 0.1.0
 */
public interface LoanService {

	/**
	 * Find {@link LoanDTO} by given ID.
	 *
	 * @author HaythemBenizid
	 * @param id the id
	 * @return the loan DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	LoanDTO find(Long id) throws ResourcesNotFoundException;

	/**
	 * Find by id ib loan.
	 *
	 * @author mlamloum
	 * @param idIbLoan the id ib loan
	 * @return the list
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	List<LoanDTO> findByIdIbLoan(Long idIbLoan) throws ResourcesNotFoundException;

	/**
	 * Find by id extern.
	 *
	 * @author idridi
	 * @param idExtern the id extern
	 * @return the loan DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	LoanDTO findByIdExtern(Long idExtern) throws ResourcesNotFoundException;

	/**
	 * Count loans by status. ('1') New || ('2') Drafts || ('3') Awaiting Approval || ('4') Approved
	 * || ('5') Rejected ('6') Cancelled || ('7') Correctifs : {@link ACMConstantWorkflowStatuts}
	 *
	 * @author HaythemBenizid
	 * @param statusTab the statusTab
	 * @return the long
	 */
	Long count(String statusTab);

	/**
	 * Count unassigned loans.
	 * 
	 * @author idridi
	 * @return the long
	 */
	Long countUnassignedLoans();

	/**
	 * Find {@link LoanDTO} by given cuAccount and enabled row only.
	 *
	 * @author HaythemBenizid
	 * @param cuAccountId the cu account id
	 * @return the loan DTO
	 */
	LoanDTO findByIdAccountExtern(Long cuAccountId);

	/**
	 * Find {@link LoanDTO} by parent_id (ID loan Group) and enabled row only.
	 *
	 * @author HaythemBenizid
	 * @param parentId the parent id
	 * @return the list
	 */
	List<LoanDTO> findByParentId(Long parentId);

	/**
	 * Find {@link LoanDTO} by given customer Id.
	 *
	 * @author HaythemBenizid
	 * @param customerId the customer id
	 * @return the loan DTO
	 */
	List<LoanDTO> findByIdCustomer(Long customerId);

	/**
	 * Find {@link LoanDTO} by given customer Id.
	 *
	 * @author hbeji
	 * @param customerDTO the customer DTO
	 * @return the loan DTO
	 */
	List<LoanDTO> findByCustomer(CustomerDTO customerDTO);

	/**
	 * Find by statut workflow (used in Batch).
	 * 
	 * @author HaythemBenizid
	 * @param statutWorkflow the statut workflow
	 * @return the list
	 */
	List<LoanDTO> findByStatutWorkflow(Integer statutWorkflow);

	/**
	 * Find {@link List} of {@link LoanDTO} by given params.
	 * 
	 * @author HaythemBenizid
	 * @param loanDTO the loan DTO
	 * @return the list
	 */
	List<LoanDTO> find(LoanDTO loanDTO);

	/**
	 * Find {@link List} of {@link LoanDTO} by given params.
	 * 
	 * @author hbeji
	 * @param loanDTO the loan DTO
	 * @return the list
	 */
	List<LoanDTO> findByAccount(LoanDTO loanDTO);

	/**
	 * Find {@link LoanPaginationDTO} by page size & page number & given params
	 * ({@link LoanPaginationDTO}).
	 * 
	 * @author HaythemBenizid
	 * @param loanPaginationDTO the loan pagination DTO
	 * @return the list
	 */
	LoanPaginationDTO find(LoanPaginationDTO loanPaginationDTO);

	/**
	 * Find Un-Assignment {@link LoanPaginationDTO} by page size & page number & given params
	 * ({@link LoanPaginationDTO}).
	 * 
	 * @author HaythemBenizid
	 * @param loanPaginationDTO the loan pagination DTO
	 * @return the list
	 */
	LoanPaginationDTO findUnAssignment(LoanPaginationDTO loanPaginationDTO);

	/**
	 * The method used for saving the given {@link LoanDTO}.
	 *
	 * @author HaythemBenizid
	 * @param loanDTO the loan DTO
	 * @return the loan DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 * @throws CreditException the credit exception
	 * @throws CalculateAgeException the calculate age exception
	 * @throws ConditionalApproveException the conditional approve exception
	 * @throws ApiAbacusException the api abacus exception
	 * @throws DisbursementException the disbursement exception
	 * @throws CheckApprovelLevelException the check approvel level exception
	 * @throws EnableCriticalDataException the enable critical data exception
	 * @throws WorkFlowSettingException the work flow setting exception
	 * @throws IOException Signals that an I/O exception has occurred.
	 * @throws InformCustomerNotFoundException the inform customer not found exception
	 * @throws CheckAppL1NotFoundException the check app L 1 not found exception
	 * @throws CheckAppL2NotFoundException the check app L 2 not found exception
	 * @throws CheckAppL3NotFoundException the check app L 3 not found exception
	 * @throws CheckAppL4NotFoundException the check app L 4 not found exception
	 * @throws UploadSignedDocNotFoundExepction the upload signed doc not found exepction
	 * @throws UploadDocumentNotFoundException the upload document not found exception
	 */
	LoanDTO save(LoanDTO loanDTO)
			throws ResourcesNotFoundException, CreditException, CalculateAgeException,
			ConditionalApproveException, ApiAbacusException, DisbursementException,
			CheckApprovelLevelException, EnableCriticalDataException, WorkFlowSettingException,
			IOException, InformCustomerNotFoundException, CheckAppL1NotFoundException,
			CheckAppL2NotFoundException, CheckAppL3NotFoundException, CheckAppL4NotFoundException,
			UploadSignedDocNotFoundExepction, UploadDocumentNotFoundException;

	/**
	 * Validate issued by batch.
	 *
	 * @author HaythemBenizid
	 * @param loanDTOs the loan DT os
	 * @return the list
	 * @throws UploadDocumentNotFoundException the upload document not found exception
	 * @throws DisbursementException the disbursement exception
	 * @throws ConditionalApproveException the conditional approve exception
	 * @throws WorkFlowSettingException the work flow setting exception
	 */
	List<LoanDTO> validateIssuedByBatch(List<LoanDTO> loanDTOs)
			throws UploadDocumentNotFoundException, DisbursementException,
			ConditionalApproveException, WorkFlowSettingException;

	/**
	 * The method used for updating the given {@link LoanDTO} by ID.
	 *
	 * @author HaythemBenizid
	 * @param id the id
	 * @param loanDTO the loan DTO
	 * @return the loan DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	LoanDTO save(Long id, LoanDTO loanDTO) throws ResourcesNotFoundException;

	/**
	 * The method used for updating the category (ALERT / WARNING) the given {@link LoanDTO}.
	 *
	 * @author HaythemBenizid
	 * @param loanDTO the loan DTO
	 * @return the loan DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	LoanDTO saveForTimer(LoanDTO loanDTO) throws ResourcesNotFoundException;

	/**
	 * The method used for rejected the given {@link LoanDTO}.
	 *
	 * @author HaythemBenizid
	 * @param loanDTO the loan DTO
	 * @return the loan DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 * @throws ApiAbacusException the api abacus exception
	 * @throws IOException Signals that an I/O exception has occurred.
	 */
	LoanDTO rejected(LoanDTO loanDTO)
			throws ResourcesNotFoundException, ApiAbacusException, IOException;

	/**
	 * The method used for cancelled the given {@link LoanDTO}.
	 *
	 * @author HaythemBenizid
	 * @param loanDTO the loan DTO
	 * @return the loan DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 * @throws IOException Signals that an I/O exception has occurred.
	 * @throws ApiAbacusException the api abacus exception
	 * @throws CancelIssuedLoanException the cancel issued loan exception
	 */
	LoanDTO cancelled(LoanDTO loanDTO) throws ResourcesNotFoundException, IOException,
			ApiAbacusException, CancelIssuedLoanException;

	/**
	 * Delete {@link LoanDTO} by given params.
	 * 
	 * @author HaythemBenizid
	 * @param loanDTO the loan DTO
	 */
	void delete(LoanDTO loanDTO);

	/**
	 * Calculate first repayment date.
	 *
	 * @param maxissueDate the maxissue date
	 * @param daterepaymentdate the daterepaymentdate
	 * @return the local date
	 */
	LocalDate calculateFirstRepaymentDate(Integer maxissueDate, Integer daterepaymentdate);

	/**
	 * Validate loan flowing our workflow diagram.
	 *
	 * @author HaythemBenizid
	 * @param loanDTO the loan DTO
	 * @return the loan DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 * @throws InitialCheckException the initialCheck not found exception
	 * @throws FieldVisitNotFoundException the fieldVisit not found exception
	 * @throws GuarantorsNotFoundException the guaranteers not found exception
	 * @throws CollateralNotFoundException the collateral not found exception
	 * @throws CheckAppL1NotFoundException the checkAppL1 not found exception
	 * @throws InformCustomerNotFoundException the informCustomer not found exception
	 * @throws UploadSignedDocNotFoundExepction the uploadSignedDoc not found exception
	 * @throws CheckAppL2NotFoundException the checkAppL2 not found exception
	 * @throws CheckAppL3NotFoundException the checkAppL3 not found exception
	 * @throws CheckAppL4NotFoundException the checkAppL4 not found exception
	 * @throws CheckApprovelLevelException the check approvel level exception
	 * @throws ApiAbacusException the api abacus exception
	 * @throws IOException Signals that an I/O exception has occurred.
	 * @throws CreditException the credit exception
	 * @throws UploadDocumentNotFoundException the upload document not found exception
	 * @throws EnableCriticalDataException the enable critical data exception
	 * @throws DisbursementException the disbursement exception
	 * @throws ConditionalApproveException the conditional approve exception
	 * @throws WorkFlowSettingException the work flow setting exception
	 * @throws CheckMezaCardException the check meza card exception
	 * @throws CheckMezaCardUntrustException the check meza card untrust exception
	 * @throws CheckFeesException the check fees exception
	 */
	LoanDTO validate(LoanDTO loanDTO)
			throws ResourcesNotFoundException, InitialCheckException, FieldVisitNotFoundException,
			GuarantorsNotFoundException, CollateralNotFoundException, CheckAppL1NotFoundException,
			InformCustomerNotFoundException, UploadSignedDocNotFoundExepction,
			CheckAppL2NotFoundException, CheckAppL3NotFoundException, CheckAppL4NotFoundException,
			CheckApprovelLevelException, ApiAbacusException, IOException, CreditException,
			UploadDocumentNotFoundException, EnableCriticalDataException, DisbursementException,
			ConditionalApproveException, WorkFlowSettingException, CheckMezaCardException,
			CheckMezaCardUntrustException, CheckFeesException;

	/**
	 * Complete workflow for childs.
	 *
	 * @author HaythemBenizid
	 * @param loanDTO the loan DTO
	 * @param loansChild the loans child
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
	void completeWorkflowForChilds(LoanDTO loanDTO, List<LoanDTO> loansChild)
			throws ResourcesNotFoundException, CheckApprovelLevelException, ApiAbacusException,
			IOException, CreditException, UploadDocumentNotFoundException, DisbursementException,
			WorkFlowSettingException, CheckMezaCardException, CheckMezaCardUntrustException,
			CheckFeesException;

	/**
	 * The method used for declined the given {@link LoanDTO}.
	 *
	 * @author MoezMhiri
	 * @param loanDTO the loan DTO
	 * @return the loan DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	LoanDTO declined(LoanDTO loanDTO) throws ResourcesNotFoundException;

	/**
	 * Load filter status workflow.Used in dashboard Table.
	 * 
	 * @author HaythemBenizid
	 * @param loanDTO the loan DTO
	 * @return the list
	 */
	List<LoanDTO> loadFilterStatusWorkflow(LoanDTO loanDTO);

	/**
	 * Load filter product.Used in dashboard Table.
	 * 
	 * @author HaythemBenizid
	 * @param loanDTO the loan DTO
	 * @return the list
	 */
	List<LoanDTO> loadFilterProduct(LoanDTO loanDTO);

	/**
	 * The method used for reassign the given {@link LoanDTO}.
	 *
	 * @author MoezMhiri
	 * @param loanDTO the loan DTO
	 * @return the loan DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	LoanDTO reassigned(LoanDTO loanDTO) throws ResourcesNotFoundException;

	/**
	 * Validate loan groupes flowing our workflow diagram.
	 *
	 * @author MoezMhiri
	 * @param loanDTOs the list loan DTO
	 * @return the list loan DTO
	 * @throws ApiAbacusException the api abacus exception
	 * @throws IOException Signals that an I/O exception has occurred.
	 * @throws CreditException the credit exception
	 * @throws UploadDocumentNotFoundException the upload document not found exception
	 * @throws DisbursementException the disbursement exception
	 * @throws ConditionalApproveException the conditional approve exception
	 * @throws WorkFlowSettingException the work flow setting exception
	 * @throws CheckMezaCardException the check meza card exception
	 * @throws CheckMezaCardUntrustException the check meza card untrust exception
	 * @throws CheckFeesException the check fees exception
	 */
	List<LoanDTO> validateAll(List<LoanDTO> loanDTOs) throws ApiAbacusException, IOException,
			CreditException, UploadDocumentNotFoundException, DisbursementException,
			ConditionalApproveException, WorkFlowSettingException, CheckMezaCardException,
			CheckMezaCardUntrustException, CheckFeesException;

	/**
	 * save To Abacus via API.
	 *
	 * @author MoezMhiri
	 * @param loanDTO the loan DTO
	 * @return loan DTO
	 * @throws CustomerMaxActiveAccountException the customer max active account exception
	 * @throws ApiAbacusException the api abacus exception
	 * @throws IOException Signals that an I/O exception has occurred.
	 * @throws CalculateAgeException the calculate age exception
	 */
	LoanDTO saveToAbacus(LoanDTO loanDTO) throws CustomerMaxActiveAccountException,
			ApiAbacusException, IOException, CalculateAgeException;

	/**
	 * update To ACM and update To Abacus via API.
	 *
	 * @author MoezMhiri
	 * @param loanDTO the loan DTO
	 * @return loan DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 * @throws ApiAbacusException the api abacus exception
	 */
	LoanDTO updateForApplication(LoanDTO loanDTO)
			throws ResourcesNotFoundException, ApiAbacusException;

	/**
	 * Assign to customer.
	 *
	 * @author Salmen Fatnassi
	 * @param loanDTO the loan DTO
	 * @return the loan DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 * @throws CustomerContactException the customer contact exception
	 */
	LoanDTO assignToCustomer(LoanDTO loanDTO)
			throws ResourcesNotFoundException, CustomerContactException;

	/**
	 * Save groupe loan to abacus.
	 *
	 * @author MoezMhiri
	 * @param loanDTOs this list loan DTO
	 * @return the list loan DTO
	 * @throws ApiAbacusException the api abacus exception
	 */
	List<LoanDTO> saveLoanGroupToAbacus(List<LoanDTO> loanDTOs) throws ApiAbacusException;

	/**
	 * Find data for reporting Excel file by given params.
	 * 
	 * @author HaythemBenizid
	 * @param reportingDTO the reporting DTO
	 * @return the reporting list DTO
	 */
	ReportingListDTO find(ReportingDTO reportingDTO);

	/**
	 * Update for application for group.
	 *
	 * @author YesserSomai
	 * @param loanDTOs the loan DT os
	 * @return the loan DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 * @throws ApiAbacusException the api abacus exception
	 * @throws IOException Signals that an I/O exception has occurred.
	 */
	LoanDTO updateGroupApplication(List<LoanDTO> loanDTOs)
			throws ResourcesNotFoundException, ApiAbacusException, IOException;

	/**
	 * Update child and group (Use only for update amount child and grp).
	 *
	 * @author MoezMhiri
	 * @param id the id
	 * @param loanDTO the loan DTO
	 * @return the loan DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	LoanDTO updateChildAndGroup(Long id, LoanDTO loanDTO) throws ResourcesNotFoundException;

	/**
	 * Cancel loans.
	 *
	 * @author YesserSomai
	 * @param listIdLoanExtern the list id loan extern
	 * @return the list Validate ready for disbursement.
	 */
	List<LoanDTO> cancelLoans(List<Long> listIdLoanExtern);

	/**
	 * Validate ready for disbursement.
	 *
	 * @author Ines Dridi
	 * @param loanDTO the loan DTO
	 * @return the loan DTO
	 * @throws ApiAbacusException the api abacus exception
	 * @throws IOException Signals that an I/O exception has occurred.
	 * @throws ResourcesNotFoundException the resources not found exception
	 * @throws DisbursementException the disbursement exception
	 */
	LoanDTO validateReadyForDisbursement(LoanDTO loanDTO) throws ApiAbacusException, IOException,
			ResourcesNotFoundException, DisbursementException;

	/**
	 * Check loan status issued.
	 * 
	 * @author Ines Dridi
	 * @param idLoanExtern the id loan extern
	 * @return the boolean
	 */
	Boolean checkLoanStatusIssued(Long idLoanExtern);

	/**
	 * Update status.
	 * 
	 * @author moezMhiri
	 * @param loanDTO the loan DTO
	 * @return the loan DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	LoanDTO updateStatus(LoanDTO loanDTO) throws ResourcesNotFoundException;

	/**
	 * Check loan status.
	 * 
	 * @author moezMhiri
	 * @param loanDTO the loan DTO
	 * @return the boolean
	 */
	Boolean checkLoanStatus(LoanDTO loanDTO);

	/**
	 * Find Loan for connected User and his collaborators.
	 * 
	 * @author Ines Dridi
	 * @return the list
	 */
	List<LoanDTO> findByOwners();

	/**
	 * Load filter product for unassigned loans.
	 * 
	 * @author idridi
	 * @param loanDTO the loan DTO
	 * @return the list
	 */
	List<LoanDTO> loadFilterProductForUnassignedLoans(LoanDTO loanDTO);

	/**
	 * Load filter status for unassigned loans.
	 *
	 * @param loanDTO the loan DTO
	 * @return the list
	 */
	List<LoanDTO> loadFilterStatusForUnassignedLoans(LoanDTO loanDTO);

	/**
	 * Assign loan.
	 * 
	 * @author idridi
	 * @param loanDTO the loan DTO
	 * @return the loan DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 * @throws CreditException the credit exception
	 */
	LoanDTO assignLoan(LoanDTO loanDTO) throws ResourcesNotFoundException, CreditException;

	/**
	 * Load filter branch.Used in dashboard Table.
	 *
	 * @author ManelLamloum
	 * @param loanDTO the loan DTO
	 * @return the list
	 */
	List<LoanDTO> loadFilterBranch(LoanDTO loanDTO);

	/**
	 * Synchronize loans.
	 *
	 * @author idridi
	 * @return the integer
	 * @throws ResourcesNotFoundException the resources not found exception
	 * @throws CreditException the credit exception
	 * @throws CalculateAgeException the calculate age exception
	 * @throws ConditionalApproveException the conditional approve exception
	 * @throws ApiAbacusException the api abacus exception
	 * @throws DisbursementException the disbursement exception
	 * @throws CheckApprovelLevelException the check approvel level exception
	 * @throws EnableCriticalDataException the enable critical data exception
	 * @throws WorkFlowSettingException the work flow setting exception
	 * @throws IOException Signals that an I/O exception has occurred.
	 * @throws InformCustomerNotFoundException the inform customer not found exception
	 * @throws CheckAppL1NotFoundException the check app L 1 not found exception
	 * @throws CheckAppL2NotFoundException the check app L 2 not found exception
	 * @throws CheckAppL3NotFoundException the check app L 3 not found exception
	 * @throws CheckAppL4NotFoundException the check app L 4 not found exception
	 * @throws UploadSignedDocNotFoundExepction the upload signed doc not found exepction
	 * @throws UploadDocumentNotFoundException the upload document not found exception
	 */
	Integer synchronizeLoans()
			throws ResourcesNotFoundException, CreditException, CalculateAgeException,
			ConditionalApproveException, ApiAbacusException, DisbursementException,
			CheckApprovelLevelException, EnableCriticalDataException, WorkFlowSettingException,
			IOException, InformCustomerNotFoundException, CheckAppL1NotFoundException,
			CheckAppL2NotFoundException, CheckAppL3NotFoundException, CheckAppL4NotFoundException,
			UploadSignedDocNotFoundExepction, UploadDocumentNotFoundException;

	/**
	 * Synchronize loans.
	 *
	 * @author idridi
	 * @return the integer Synchronize issued loans. the integer
	 * @throws UploadDocumentNotFoundException the upload document not found exception
	 * @throws Exception the exception
	 */
	Integer synchronizeIssuedLoans() throws UploadDocumentNotFoundException, Exception;

	/**
	 * Synchronize cancelled loans.
	 * 
	 * @author idridi
	 * @return the integer
	 */
	Integer synchronizeCancelledLoans();

	/**
	 * Synchronize loan by account number.
	 *
	 * @author idridi
	 * @param accountNumber the account number
	 * @return the loan DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 * @throws CreditException the credit exception
	 * @throws CalculateAgeException the calculate age exception
	 * @throws LoanAlreadyExistException the loan already exist exception
	 * @throws ConditionalApproveException the conditional approve exception
	 * @throws ApiAbacusException the api abacus exception
	 * @throws DisbursementException the disbursement exception
	 * @throws CheckApprovelLevelException the check approvel level exception
	 * @throws EnableCriticalDataException the enable critical data exception
	 * @throws WorkFlowSettingException the work flow setting exception
	 * @throws IOException Signals that an I/O exception has occurred.
	 * @throws InformCustomerNotFoundException the inform customer not found exception
	 * @throws CheckAppL1NotFoundException the check app L 1 not found exception
	 * @throws CheckAppL2NotFoundException the check app L 2 not found exception
	 * @throws CheckAppL3NotFoundException the check app L 3 not found exception
	 * @throws CheckAppL4NotFoundException the check app L 4 not found exception
	 * @throws UploadSignedDocNotFoundExepction the upload signed doc not found exepction
	 * @throws UploadDocumentNotFoundException the upload document not found exception
	 */
	LoanDTO synchronizeLoanByAccountNumber(String accountNumber) throws ResourcesNotFoundException,
			CreditException, CalculateAgeException, LoanAlreadyExistException,
			ConditionalApproveException, ApiAbacusException, DisbursementException,
			CheckApprovelLevelException, EnableCriticalDataException, WorkFlowSettingException,
			IOException, InformCustomerNotFoundException, CheckAppL1NotFoundException,
			CheckAppL2NotFoundException, CheckAppL3NotFoundException, CheckAppL4NotFoundException,
			UploadSignedDocNotFoundExepction, UploadDocumentNotFoundException;

	/**
	 * Gets the closing balanceby id loan extern.
	 *
	 * @author idridi
	 * @param idLoanExtern the id loan extern
	 * @return the closing balanceby id loan extern
	 * @throws ApiAbacusException the api abacus exception
	 */
	Long getClosingBalancebyIdLoanExtern(Long idLoanExtern) throws ApiAbacusException;

	/**
	 * Refinance / Topup loan.
	 *
	 * @author mlamloum
	 * @param loanDTO the loan DTO
	 * @throws IOException Signals that an I/O exception has occurred.
	 * @throws ApiAbacusException the api abacus exception
	 */
	void refinanceLoan(LoanDTO loanDTO) throws IOException, ApiAbacusException;

	/**
	 * Update all loan.
	 * 
	 * @author mlamloum
	 * @param LoanDTOs the loan DT os
	 * @param action the action
	 */
	void updateAllLoan(List<LoanDTO> LoanDTOs, String action);

	/**
	 * Update loan branches.
	 * 
	 * @author mlamloum
	 * @param customerDTOs the customer DT os
	 */
	void updateLoanBranches(List<CustomerDTO> customerDTOs);

	/**
	 * Loan review.
	 *
	 * @param loanDTO the loan DTO
	 * @return the loan DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	LoanDTO LoanReview(LoanDTO loanDTO) throws ResourcesNotFoundException;

	/**
	 * Count topups by account.
	 * 
	 * @author mlamloum
	 * @param accountId the account id
	 * @return the integer
	 */
	Integer countTopupsByAccount(Long accountId);

	/**
	 * Find by supplier.
	 *
	 * @param supplierId the supplier id
	 * @return the list
	 */
	List<LoanDTO> findBySupplier(Long supplierId);

	/**
	 * Automatic step loan.
	 *
	 * @param loanDTO the loan DTO
	 * @return the loan DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 * @throws CustomerMaxActiveAccountException the customer max active account exception
	 * @throws ApiAbacusException the api abacus exception
	 * @throws IOException Signals that an I/O exception has occurred.
	 * @throws CalculateAgeException the calculate age exception
	 * @throws CreditException the credit exception
	 * @throws ConditionalApproveException the conditional approve exception
	 * @throws DisbursementException the disbursement exception
	 * @throws CheckApprovelLevelException the check approvel level exception
	 * @throws EnableCriticalDataException the enable critical data exception
	 * @throws WorkFlowSettingException the work flow setting exception
	 * @throws InformCustomerNotFoundException the inform customer not found exception
	 * @throws CheckAppL1NotFoundException the check app L 1 not found exception
	 * @throws CheckAppL2NotFoundException the check app L 2 not found exception
	 * @throws CheckAppL3NotFoundException the check app L 3 not found exception
	 * @throws CheckAppL4NotFoundException the check app L 4 not found exception
	 * @throws UploadSignedDocNotFoundExepction the upload signed doc not found exepction
	 * @throws UploadDocumentNotFoundException the upload document not found exception
	 */
	LoanDTO automaticStepLoan(LoanDTO loanDTO) throws ResourcesNotFoundException,
			CustomerMaxActiveAccountException, ApiAbacusException, IOException,
			CalculateAgeException, CreditException, ConditionalApproveException,
			DisbursementException, CheckApprovelLevelException, EnableCriticalDataException,
			WorkFlowSettingException, InformCustomerNotFoundException, CheckAppL1NotFoundException,
			CheckAppL2NotFoundException, CheckAppL3NotFoundException, CheckAppL4NotFoundException,
			UploadSignedDocNotFoundExepction, UploadDocumentNotFoundException;

	/**
	 * Job loans sanad.
	 */
	void jobLoansSanad();

	/**
	 * Update max installment.
	 *
	 * @param loanDTO the loan DTO
	 * @return the loan DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	LoanDTO updateMaxInstallment(LoanDTO loanDTO) throws ResourcesNotFoundException;

	/**
	 * Evaluer expression.
	 *
	 * @param expression the expression
	 * @param loanDTO the loan DTO
	 * @return the object
	 */
	Object evaluerExpression(String expression, LoanDTO loanDTO);

}
