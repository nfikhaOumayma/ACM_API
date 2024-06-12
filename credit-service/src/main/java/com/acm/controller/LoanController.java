/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.controller;

import java.io.IOException;
import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestHeader;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

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
import com.acm.service.LoanService;
import com.acm.utils.dtos.CustomerDTO;
import com.acm.utils.dtos.LoanDTO;
import com.acm.utils.dtos.pagination.LoanPaginationDTO;

/**
 * This class @{link LoanController} used to control all the loan requests.
 *
 * @author HaythemBenizid
 * @since 0.1.0
 */
@RestController
@RequestMapping("/loans")
public class LoanController {

	/** The loan service. */
	@Autowired
	private LoanService loanService;

	/**
	 * Find by id.
	 *
	 * @author HaythemBenizid
	 * @param id the id
	 * @return the loan DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	@GetMapping("/{id}")
	public LoanDTO findById(@PathVariable("id") Long id) throws ResourcesNotFoundException {

		return loanService.find(id);
	}

	/**
	 * Find by id extern.
	 *
	 * @author idridi
	 * @param idExtern the id extern
	 * @return the loan DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	@GetMapping("/find-by-idExtern/{idExtern}")
	public LoanDTO findByIdExtern(@PathVariable("idExtern") Long idExtern)
			throws ResourcesNotFoundException {

		return loanService.findByIdExtern(idExtern);
	}

	/**
	 * Find by id ib loan.
	 *
	 * @author mlamloum
	 * @param idIbLoan the id ib loan
	 * @return the list
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	@GetMapping("/find-by-idIbLoan/{idIbLoan}")
	public List<LoanDTO> findByIdIbLoan(@PathVariable("idIbLoan") Long idIbLoan)
			throws ResourcesNotFoundException {

		return loanService.findByIdIbLoan(idIbLoan);
	}

	/**
	 * Find by statut workflow.
	 * 
	 * @author HaythemBenizid
	 * @param statutWorkflow the statut workflow
	 * @return the list
	 */
	@GetMapping("/find-by-statut/{statutWorkflow}")
	public List<LoanDTO> findByStatutWorkflow(
			@PathVariable("statutWorkflow") Integer statutWorkflow) {

		return loanService.findByStatutWorkflow(statutWorkflow);
	}

	/**
	 * Find {@link List} of {@link LoanDTO} by Requested params.
	 * 
	 * @author HaythemBenizid
	 * @param loanDTO the loan DTO
	 * @return the list
	 */
	@PostMapping("/")
	public List<LoanDTO> find(@RequestBody LoanDTO loanDTO) {

		return loanService.find(loanDTO);
	}

	/**
	 * Find {@link List} of {@link LoanDTO} by Requested params.
	 * 
	 * @author hbeji
	 * @param loanDTO the loan DTO
	 * @return the list
	 */
	@PostMapping("/by-account")
	public List<LoanDTO> findByAccount(@RequestBody LoanDTO loanDTO) {

		return loanService.findByAccount(loanDTO);
	}

	/**
	 * Find pagination.
	 * 
	 * @author HaythemBenizid
	 * @param loanPaginationDTO the loan pagination DTO
	 * @return the loan pagination DTO
	 */
	@PostMapping("/find-pagination")
	public LoanPaginationDTO findPagination(@RequestBody LoanPaginationDTO loanPaginationDTO) {

		return loanService.find(loanPaginationDTO);
	}

	/**
	 * Find un assignment pagination.
	 *
	 * @author HaythemBenizid
	 * @param loanPaginationDTO the loan pagination DTO
	 * @return the loan pagination DTO
	 */
	@PostMapping("/find-unassigned-pagination")
	public LoanPaginationDTO findUnAssignmentPagination(
			@RequestBody LoanPaginationDTO loanPaginationDTO) {

		return loanService.findUnAssignment(loanPaginationDTO);
	}

	/**
	 * Creates the LoanDTO by new value.
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
	@PostMapping("/create")
	public LoanDTO create(@RequestBody LoanDTO loanDTO)
			throws ResourcesNotFoundException, CreditException, CalculateAgeException,
			ConditionalApproveException, ApiAbacusException, DisbursementException,
			CheckApprovelLevelException, EnableCriticalDataException, WorkFlowSettingException,
			IOException, InformCustomerNotFoundException, CheckAppL1NotFoundException,
			CheckAppL2NotFoundException, CheckAppL3NotFoundException, CheckAppL4NotFoundException,
			UploadSignedDocNotFoundExepction, UploadDocumentNotFoundException {

		return loanService.save(loanDTO);
	}

	/**
	 * Validate issued loans by batch (USED ONLY BY BATCH).
	 *
	 * @author HaythemBenizid
	 * @param loanDTOs the loan DT os
	 * @param token the token
	 * @return the list
	 * @throws UploadDocumentNotFoundException the upload document not found exception
	 * @throws DisbursementException the disbursement exception
	 * @throws ConditionalApproveException the conditional approve exception
	 * @throws WorkFlowSettingException the work flow setting exception
	 */
	@PostMapping("/validate-by-batch")
	public List<LoanDTO> validateIssuedByBatch(@RequestBody List<LoanDTO> loanDTOs,
			@RequestHeader("Authorization") String token) throws UploadDocumentNotFoundException,
			DisbursementException, ConditionalApproveException, WorkFlowSettingException {

		return loanService.validateIssuedByBatch(loanDTOs);
	}

	/**
	 * Update the parameter by id.
	 * 
	 * @author HaythemBenizid
	 * @param loanDTO the loan DTO
	 * @return the loan DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	@PutMapping("/update")
	public LoanDTO update(@RequestBody LoanDTO loanDTO) throws ResourcesNotFoundException {

		return loanService.save(loanDTO.getLoanId(), loanDTO);
	}

	/**
	 * Delete document using his id.
	 * 
	 * @author HaythemBenizid
	 * @param id the id
	 */
	@DeleteMapping("/{id}")
	public void delete(@PathVariable("id") Long id) {

		loanService.delete(new LoanDTO(id));
	}

	/**
	 * Rejected.
	 *
	 * @author HaythemBenizid
	 * @param loanDTO the loan DTO
	 * @return the loan DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 * @throws ApiAbacusException the api abacus exception
	 * @throws IOException Signals that an I/O exception has occurred.
	 */
	@PostMapping("/rejected")
	public LoanDTO rejected(@RequestBody LoanDTO loanDTO)
			throws ResourcesNotFoundException, ApiAbacusException, IOException {

		loanDTO.setIsNotFromWorkflow(Boolean.TRUE);
		return loanService.rejected(loanDTO);
	}

	/**
	 * Rejected.
	 *
	 * @param loanDTO the loan DTO
	 * @param token the token
	 * @return the loan DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 * @throws ApiAbacusException the api abacus exception
	 * @throws IOException Signals that an I/O exception has occurred.
	 */
	@PostMapping("/rejectedWithToken")
	public LoanDTO rejected(@RequestBody LoanDTO loanDTO,
			@RequestHeader("Authorization") String token)
			throws ResourcesNotFoundException, ApiAbacusException, IOException {

		loanDTO.setIsNotFromWorkflow(Boolean.TRUE);
		return loanService.rejected(loanDTO);
	}

	/**
	 * Cancelled.
	 *
	 * @author HaythemBenizid
	 * @param loanDTO the loan DTO
	 * @return the loan DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 * @throws ApiAbacusException the api abacus exception
	 * @throws IOException Signals that an I/O exception has occurred.
	 * @throws CancelIssuedLoanException the cancel issued loan exception
	 */
	@PostMapping("/cancelled")
	public LoanDTO cancelled(@RequestBody LoanDTO loanDTO) throws ResourcesNotFoundException,
			ApiAbacusException, IOException, CancelIssuedLoanException {

		loanDTO.setIsNotFromWorkflow(Boolean.TRUE);

		return loanService.cancelled(loanDTO);
	}

	/**
	 * Validate : method to validate given loan flowing the workflow.
	 *
	 * @author HaythemBenizid
	 * @param loanDTO the loan DTO
	 * @param token the token
	 * @return the loan DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 * @throws GuarantorsNotFoundException the guaranteers not found exception
	 * @throws InitialCheckException the initialCheck not found exception
	 * @throws FieldVisitNotFoundException the fieldVisit not found exception
	 * @throws CollateralNotFoundException the collateral not found exception
	 * @throws UploadDocumentNotFoundException the uploadDocument not found exception
	 * @throws CheckAppL1NotFoundException the checkAppL1 not found exception
	 * @throws InformCustomerNotFoundException the informCustomer not found exception
	 * @throws UploadSignedDocNotFoundExepction the uploadSignedDoc not found exception
	 * @throws CheckAppL2NotFoundException the checkAppL2 not found exception
	 * @throws CheckAppL3NotFoundException the CheckAppL3 not found exception
	 * @throws CheckAppL4NotFoundException the CheckAppL4 not found exception
	 * @throws CheckApprovelLevelException the check approvel level exception
	 * @throws ApiAbacusException the api abacus exception
	 * @throws IOException Signals that an I/O exception has occurred.
	 * @throws CreditException the credit exception
	 * @throws EnableCriticalDataException the enable critical data exception
	 * @throws DisbursementException the disbursement exception
	 * @throws ConditionalApproveException the conditional approve exception
	 * @throws WorkFlowSettingException the work flow setting exception
	 * @throws CheckMezaCardException the check meza card exception
	 * @throws CheckMezaCardUntrustException the check meza card untrust exception
	 * @throws CheckFeesException the check fees exception
	 */
	@PostMapping("/validate")
	public LoanDTO validate(@RequestBody LoanDTO loanDTO,
			@RequestHeader("Authorization") String token) throws ResourcesNotFoundException,
			GuarantorsNotFoundException, InitialCheckException, FieldVisitNotFoundException,
			CollateralNotFoundException, UploadDocumentNotFoundException,
			CheckAppL1NotFoundException, InformCustomerNotFoundException,
			UploadSignedDocNotFoundExepction, CheckAppL2NotFoundException,
			CheckAppL3NotFoundException, CheckAppL4NotFoundException, CheckApprovelLevelException,
			ApiAbacusException, IOException, CreditException, EnableCriticalDataException,
			DisbursementException, ConditionalApproveException, WorkFlowSettingException,
			CheckMezaCardException, CheckMezaCardUntrustException, CheckFeesException {

		return loanService.validate(loanDTO);

	}

	/**
	 * Validate.
	 *
	 * @param loanDTO the loan DTO
	 * @return the loan DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 * @throws GuarantorsNotFoundException the guarantors not found exception
	 * @throws InitialCheckException the initial check exception
	 * @throws FieldVisitNotFoundException the field visit not found exception
	 * @throws CollateralNotFoundException the collateral not found exception
	 * @throws UploadDocumentNotFoundException the upload document not found exception
	 * @throws CheckAppL1NotFoundException the check app L 1 not found exception
	 * @throws InformCustomerNotFoundException the inform customer not found exception
	 * @throws UploadSignedDocNotFoundExepction the upload signed doc not found exepction
	 * @throws CheckAppL2NotFoundException the check app L 2 not found exception
	 * @throws CheckAppL3NotFoundException the check app L 3 not found exception
	 * @throws CheckAppL4NotFoundException the check app L 4 not found exception
	 * @throws CheckApprovelLevelException the check approvel level exception
	 * @throws ApiAbacusException the api abacus exception
	 * @throws IOException Signals that an I/O exception has occurred.
	 * @throws CreditException the credit exception
	 * @throws EnableCriticalDataException the enable critical data exception
	 * @throws DisbursementException the disbursement exception
	 * @throws ConditionalApproveException the conditional approve exception
	 * @throws WorkFlowSettingException the work flow setting exception
	 * @throws CheckMezaCardException the check meza card exception
	 * @throws CheckMezaCardUntrustException the check meza card untrust exception
	 * @throws CheckFeesException the check fees exception
	 */
	@PostMapping("/validateWithToken")
	public LoanDTO validate(@RequestBody LoanDTO loanDTO) throws ResourcesNotFoundException,
			GuarantorsNotFoundException, InitialCheckException, FieldVisitNotFoundException,
			CollateralNotFoundException, UploadDocumentNotFoundException,
			CheckAppL1NotFoundException, InformCustomerNotFoundException,
			UploadSignedDocNotFoundExepction, CheckAppL2NotFoundException,
			CheckAppL3NotFoundException, CheckAppL4NotFoundException, CheckApprovelLevelException,
			ApiAbacusException, IOException, CreditException, EnableCriticalDataException,
			DisbursementException, ConditionalApproveException, WorkFlowSettingException,
			CheckMezaCardException, CheckMezaCardUntrustException, CheckFeesException {

		return loanService.validate(loanDTO);

	}

	/**
	 * Validate by id.
	 *
	 * @param loanDTO the loan DTO
	 * @return the loan DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 * @throws GuarantorsNotFoundException the guarantors not found exception
	 * @throws InitialCheckException the initial check exception
	 * @throws FieldVisitNotFoundException the field visit not found exception
	 * @throws CollateralNotFoundException the collateral not found exception
	 * @throws UploadDocumentNotFoundException the upload document not found exception
	 * @throws CheckAppL1NotFoundException the check app L 1 not found exception
	 * @throws InformCustomerNotFoundException the inform customer not found exception
	 * @throws UploadSignedDocNotFoundExepction the upload signed doc not found exepction
	 * @throws CheckAppL2NotFoundException the check app L 2 not found exception
	 * @throws CheckAppL3NotFoundException the check app L 3 not found exception
	 * @throws CheckAppL4NotFoundException the check app L 4 not found exception
	 * @throws CheckApprovelLevelException the check approvel level exception
	 * @throws ApiAbacusException the api abacus exception
	 * @throws IOException Signals that an I/O exception has occurred.
	 * @throws CreditException the credit exception
	 * @throws EnableCriticalDataException the enable critical data exception
	 * @throws DisbursementException the disbursement exception
	 * @throws ConditionalApproveException the conditional approve exception
	 * @throws WorkFlowSettingException the work flow setting exception
	 * @throws CheckMezaCardException the check meza card exception
	 * @throws CheckMezaCardUntrustException the check meza card untrust exception
	 * @throws CheckFeesException the check fees exception
	 */
	@PostMapping("/validateById")
	public LoanDTO validateById(@RequestBody LoanDTO loanDTO) throws ResourcesNotFoundException,
			GuarantorsNotFoundException, InitialCheckException, FieldVisitNotFoundException,
			CollateralNotFoundException, UploadDocumentNotFoundException,
			CheckAppL1NotFoundException, InformCustomerNotFoundException,
			UploadSignedDocNotFoundExepction, CheckAppL2NotFoundException,
			CheckAppL3NotFoundException, CheckAppL4NotFoundException, CheckApprovelLevelException,
			ApiAbacusException, IOException, CreditException, EnableCriticalDataException,
			DisbursementException, ConditionalApproveException, WorkFlowSettingException,
			CheckMezaCardException, CheckMezaCardUntrustException, CheckFeesException {

		LoanDTO loanToValidate = loanService.find(loanDTO.getLoanId());
		loanToValidate
				.setWorkFlowAction(ACMConstantWorkflowStatuts.WORKFLOW_REQUEST_ACTION_APPROVE);
		loanToValidate.setNote("Automatically Approved After Customer Signature");
		return loanService.validate(loanToValidate);

	}

	/**
	 * Validate ready for disbursement.
	 *
	 * @author Ines Dridi
	 * @param loanDTO the loan DTO
	 * @return the loan DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 * @throws ApiAbacusException the api abacus exception
	 * @throws IOException Signals that an I/O exception has occurred.
	 * @throws DisbursementException the disbursement exception
	 */
	@PostMapping("/validate-for-disbusrsement")
	public LoanDTO validateReadyForDisbursement(@RequestBody LoanDTO loanDTO)
			throws ResourcesNotFoundException, ApiAbacusException, IOException,
			DisbursementException {

		return loanService.validateReadyForDisbursement(loanDTO);
	}

	/**
	 * Load filter status workflow.Used in dashboard Table.
	 * 
	 * @author HaythemBenizid
	 * @param loanDTO the loan DTO
	 * @return the list
	 */
	@PostMapping("/load-filter-status-workflow")
	public List<LoanDTO> loadFilterStatusWorkflow(@RequestBody LoanDTO loanDTO) {

		return loanService.loadFilterStatusWorkflow(loanDTO);
	}

	/**
	 * Load filter product.Used in dashboard Table.
	 * 
	 * @author HaythemBenizid
	 * @param loanDTO the loan DTO
	 * @return the list
	 */
	@PostMapping("/load-filter-product")
	public List<LoanDTO> loadFilterProduct(@RequestBody LoanDTO loanDTO) {

		return loanService.loadFilterProduct(loanDTO);
	}

	/**
	 * Reassigned.
	 *
	 * @author MoezMhiri
	 * @param loanDTO the loan DTO
	 * @return the loan DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	@PostMapping("/reassigned")
	public LoanDTO reassigned(@RequestBody LoanDTO loanDTO) throws ResourcesNotFoundException {

		return loanService.reassigned(loanDTO);
	}

	/**
	 * Validate : method to validate given list loan flowing the workflow.
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
	@PostMapping("/validate-all")
	public List<LoanDTO> validateAll(@RequestBody List<LoanDTO> loanDTOs) throws ApiAbacusException,
			IOException, CreditException, UploadDocumentNotFoundException, DisbursementException,
			ConditionalApproveException, WorkFlowSettingException, CheckMezaCardException,
			CheckMezaCardUntrustException, CheckFeesException {

		return loanService.validateAll(loanDTOs);
	}

	/**
	 * Creates the LoanDTO by new value in abacus via API.
	 *
	 * @author MoezMhiri
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
	@PostMapping("/create-to-abacus")
	public LoanDTO createToAbacus(@RequestBody LoanDTO loanDTO) throws ResourcesNotFoundException,
			CustomerMaxActiveAccountException, ApiAbacusException, IOException,
			CalculateAgeException, CreditException, ConditionalApproveException,
			DisbursementException, CheckApprovelLevelException, EnableCriticalDataException,
			WorkFlowSettingException, InformCustomerNotFoundException, CheckAppL1NotFoundException,
			CheckAppL2NotFoundException, CheckAppL3NotFoundException, CheckAppL4NotFoundException,
			UploadSignedDocNotFoundExepction, UploadDocumentNotFoundException {

		LoanDTO newLoanDTO = loanService.saveToAbacus(loanDTO);
		return loanService.automaticStepLoan(newLoanDTO);
	}

	/**
	 * Update loan.
	 *
	 * @author MoezMhiri
	 * @param loanDTO the loan DTO
	 * @return the loan DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 * @throws ApiAbacusException the api abacus exception
	 */
	@PutMapping("/update-loan")
	public LoanDTO updateLoan(@RequestBody LoanDTO loanDTO)
			throws ResourcesNotFoundException, ApiAbacusException {

		return loanService.updateForApplication(loanDTO);
	}

	/**
	 * Update assign customer.
	 *
	 * @author Salmen Fatnassi
	 * @param loanDTO the loan DTO
	 * @return the loan DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 * @throws CustomerContactException the customer contact exception
	 */
	@PutMapping("/update-assign-customer")
	public LoanDTO updateAssignCustomer(@RequestBody LoanDTO loanDTO)
			throws ResourcesNotFoundException, CustomerContactException {

		return loanService.assignToCustomer(loanDTO);
	}

	/**
	 * Creates the LoanDTOs of groupe by new value in abacus via API.
	 *
	 * @author MoezMhiri
	 * @param loanDTOs the loan DTOs
	 * @return the list loan DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 * @throws ApiAbacusException the api abacus exception
	 */
	@PostMapping("/create-loan-grp-to-abacus")
	public List<LoanDTO> createLoanGroupToAbacus(@RequestBody List<LoanDTO> loanDTOs)
			throws ResourcesNotFoundException, ApiAbacusException {

		return loanService.saveLoanGroupToAbacus(loanDTOs);
	}

	/**
	 * update Group Loan Application.
	 *
	 * @author YesserSomai
	 * @param loanDTOs the loan DT os
	 * @return the loan DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 * @throws ApiAbacusException the api abacus exception
	 * @throws IOException Signals that an I/O exception has occurred.
	 */
	@PutMapping("/update-loan-group")
	public LoanDTO updateGroupApplication(@RequestBody List<LoanDTO> loanDTOs)
			throws ResourcesNotFoundException, ApiAbacusException, IOException {

		return loanService.updateGroupApplication(loanDTOs);
	}

	/**
	 * Update child loan and his grp loan.
	 * 
	 * @author MoezMhiri
	 * @param loanDTO the loan DTO
	 * @return the loan DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	@PutMapping("/update-child-and-grp")
	public LoanDTO updateChildAndGroup(@RequestBody LoanDTO loanDTO)
			throws ResourcesNotFoundException {

		return loanService.updateChildAndGroup(loanDTO.getLoanId(), loanDTO);
	}

	/**
	 * Check loan status issued.
	 * 
	 * @author Ines Dridi
	 * @param idLoanExtern the id loan extern
	 * @return the boolean
	 */
	@GetMapping("/check-issued-status/{idLoanExtern}")
	public Boolean checkLoanStatusIssued(@PathVariable("idLoanExtern") Long idLoanExtern) {

		return loanService.checkLoanStatusIssued(idLoanExtern);
	}

	/**
	 * Update status.
	 * 
	 * @author moezMhiri
	 * @param loanDTO the loan DTO
	 * @return the loan DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	@PutMapping("/update-status")
	public LoanDTO updateStatus(@RequestBody LoanDTO loanDTO) throws ResourcesNotFoundException {

		return loanService.updateStatus(loanDTO);
	}

	/**
	 * Count my task.
	 *
	 * @author HaythemBenizid
	 * @return the long
	 */
	@GetMapping("/count-my-task")
	public Long countMyTask() {

		return loanService.count("myTask");
	}

	/**
	 * Count tab new.
	 *
	 * @author HaythemBenizid
	 * @return the long
	 */
	@GetMapping("/count-tab-new")
	public Long countTabNew() {

		// statut 1 : New
		return loanService.count(ACMConstantWorkflowStatuts.STATUS_TAB_NEW);
	}

	/**
	 * Count tab drafts.
	 *
	 * @author HaythemBenizid
	 * @return the long
	 */
	@GetMapping("/count-tab-drafts")
	public Long countTabDrafts() {

		// statut 2 : Drafts
		return loanService.count(ACMConstantWorkflowStatuts.STATUS_TAB_DRAFTS);
	}

	/**
	 * Count tab pending approval.
	 *
	 * @author HaythemBenizid
	 * @return the long
	 */
	@GetMapping("/count-tab-pendingApproval")
	public Long countTabPendingApproval() {

		// statut 3 : PendingApproval
		return loanService.count(ACMConstantWorkflowStatuts.STATUS_TAB_PENDING_APPROVAL);
	}

	/**
	 * Count tab approved.
	 *
	 * @author HaythemBenizid
	 * @return the long
	 */
	@GetMapping("/count-tab-approved")
	public Long countTabApproved() {

		// statut 4 : Approved
		return loanService.count(ACMConstantWorkflowStatuts.STATUS_TAB_APPROVED);

	}

	/**
	 * Count tab rejected.
	 *
	 * @author HaythemBenizid
	 * @return the long
	 */
	@GetMapping("/count-tab-rejected")
	public Long countTabRejected() {

		// statut 5 : Rejected
		return loanService.count(ACMConstantWorkflowStatuts.STATUS_TAB_REJECTED);

	}

	/**
	 * Count tab cancelled.
	 *
	 * @author HaythemBenizid
	 * @return the long
	 */
	@GetMapping("/count-tab-cancelled")
	public Long countTabCancelled() {

		// statut 6 : Cancelled
		return loanService.count(ACMConstantWorkflowStatuts.STATUS_TAB_CANCELLED);
	}

	/**
	 * Count tab review.
	 *
	 * @author HaythemBenizid
	 * @return the long
	 */
	@GetMapping("/count-tab-review")
	public Long countTabReview() {

		// statut 7 : Review
		return loanService.count(ACMConstantWorkflowStatuts.STATUS_TAB_REVIEW);
	}

	/**
	 * Count tab unassigned.
	 *
	 * @author idridi
	 * @return the long
	 */
	@GetMapping("/count-tab-unassigned")
	public Long countTabUnassigned() {

		// statut 8 : unassigned
		return loanService.countUnassignedLoans();
	}

	/**
	 * Load filter product for unassigned loans.
	 * 
	 * @author idridi
	 * @param loanDTO the loan DTO
	 * @return the list
	 */
	@PostMapping("/load-filter-product-loans_unassigned")
	public List<LoanDTO> loadFilterProductForUnassignedLoans(@RequestBody LoanDTO loanDTO) {

		return loanService.loadFilterProductForUnassignedLoans(loanDTO);
	}

	/**
	 * Load filter product for unassigned loans.
	 * 
	 * @author idridi
	 * @param loanDTO the loan DTO
	 * @return the list
	 */
	@PostMapping("/load-filter-status-loans_unassigned")
	public List<LoanDTO> loadFilterStatusForUnassignedLoans(@RequestBody LoanDTO loanDTO) {

		return loanService.loadFilterStatusForUnassignedLoans(loanDTO);
	}

	/**
	 * Assign loan.
	 * 
	 * @author idridi
	 * @param loanDTO the loan DTO
	 * @return the loan DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 * @throws CreditException the credit exception
	 */
	@PutMapping("/assign-loan")
	public LoanDTO assignLoan(@RequestBody LoanDTO loanDTO)
			throws ResourcesNotFoundException, CreditException {

		return loanService.assignLoan(loanDTO);
	}

	/**
	 * Count tab review.
	 *
	 * @author HaythemBenizid
	 * @return the long
	 */
	@GetMapping("/count-tab-issued")
	public Long countTabIssued() {

		// statut 8 : Issued
		return loanService.count(ACMConstantWorkflowStatuts.STATUS_TAB_ISSUED);
	}

	/**
	 * Load filter branch.
	 *
	 * @author ManelLamloum
	 * @param loanDTO the loan DTO
	 * @return the list
	 */
	@PostMapping("/load-filter-branch")
	public List<LoanDTO> loadFilterBranch(@RequestBody LoanDTO loanDTO) {

		return loanService.loadFilterBranch(loanDTO);
	}

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

	@GetMapping("/get-new-loans-from-abacus")
	public Integer synchronizeLoans()
			throws ResourcesNotFoundException, CreditException, CalculateAgeException,
			ConditionalApproveException, ApiAbacusException, DisbursementException,
			CheckApprovelLevelException, EnableCriticalDataException, WorkFlowSettingException,
			IOException, InformCustomerNotFoundException, CheckAppL1NotFoundException,
			CheckAppL2NotFoundException, CheckAppL3NotFoundException, CheckAppL4NotFoundException,
			UploadSignedDocNotFoundExepction, UploadDocumentNotFoundException {

		return loanService.synchronizeLoans();
	}

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

	@GetMapping("/get-loan-from-abacus-by-accountNumber/{accountNumber}")
	public LoanDTO synchronizeLoanByAccountNumber(
			@PathVariable("accountNumber") String accountNumber) throws ResourcesNotFoundException,
			CreditException, CalculateAgeException, LoanAlreadyExistException,
			ConditionalApproveException, ApiAbacusException, DisbursementException,
			CheckApprovelLevelException, EnableCriticalDataException, WorkFlowSettingException,
			IOException, InformCustomerNotFoundException, CheckAppL1NotFoundException,
			CheckAppL2NotFoundException, CheckAppL3NotFoundException, CheckAppL4NotFoundException,
			UploadSignedDocNotFoundExepction, UploadDocumentNotFoundException {

		return loanService.synchronizeLoanByAccountNumber(accountNumber);
	}

	/**
	 * Synchronize issued loans.
	 *
	 * @author idridi
	 * @return the integer
	 * @throws Exception the exception
	 */
	@GetMapping("/synchronize-issued-loans-abacus-acm")
	public Integer synchronizeIssuedLoans() throws Exception {

		return loanService.synchronizeIssuedLoans();
	}

	/**
	 * Synchronize cancelled loans.
	 * 
	 * @author idridi
	 * @return the integer
	 */
	@GetMapping("/synchronize-cancelled-loans-abacus-acm")
	public Integer synchronizeCancelledLoans() {

		return loanService.synchronizeCancelledLoans();
	}

	/**
	 * Find loans by customer id.
	 * 
	 * @author hbeji
	 * @param customerID the customer id
	 * @return the list
	 */
	@GetMapping("/find-by-customerID/{customerID}")
	public List<LoanDTO> findLoansByCustomerId(@PathVariable("customerID") long customerID) {

		return loanService.findByIdCustomer(customerID);
	}

	/**
	 * Find loans by customer id.
	 *
	 * @author hbeji
	 * @param customerDTO the customer DTO
	 * @return the list
	 */
	@PostMapping("/find-by-customer")
	public List<LoanDTO> findLoansByCustomer(@RequestBody CustomerDTO customerDTO) {

		return loanService.findByCustomer(customerDTO);
	}

	/**
	 * Gets the closing balanceby id loan extern.
	 *
	 * @author idridi
	 * @param idLoanExtern the id loan extern
	 * @return the closing balanceby id loan extern
	 * @throws ApiAbacusException the api abacus exception
	 */
	@GetMapping("/get-closing-balance-by-idExtern/{idLoanExtern}")
	public Long getClosingBalancebyIdLoanExtern(@PathVariable("idLoanExtern") long idLoanExtern)
			throws ApiAbacusException {

		return loanService.getClosingBalancebyIdLoanExtern(idLoanExtern);
	}

	/**
	 * Refinance loan.
	 *
	 * @author mlamloum
	 * @param loanDTO the loan DTO
	 * @throws ApiAbacusException the api abacus exception
	 * @throws IOException Signals that an I/O exception has occurred.
	 */
	@PutMapping("/refinance-loan")
	public void refinanceLoan(@RequestBody LoanDTO loanDTO) throws ApiAbacusException, IOException {

		loanService.refinanceLoan(loanDTO);
	}

	/**
	 * Update all loan.
	 *
	 * @param loanDTOs the loan DT os
	 * @param action the action
	 * @throws ResourcesNotFoundException the resources not found exception
	 * @throws CreditException the credit exception
	 */
	@PutMapping("/update-all-loan/{action}")
	public void updateAllLoan(@RequestBody List<LoanDTO> loanDTOs,
			@PathVariable("action") String action)
			throws ResourcesNotFoundException, CreditException {

		loanService.updateAllLoan(loanDTOs, action);
	}

	/**
	 * Find loans by id account extern.
	 *
	 * @param idAccountExtern the id account extern
	 * @return the loan DTO
	 */
	@GetMapping("/find-by-idAccountExtern/{idAccountExtern}")
	public LoanDTO findLoansByIdAccountExtern(
			@PathVariable("idAccountExtern") long idAccountExtern) {

		return loanService.findByIdAccountExtern(idAccountExtern);

	}

	/**
	 * Update loan branches.
	 *
	 * @param customerDTOs the customer DT os
	 */
	@PutMapping("/update-loan-branches")
	public void updateLoanBranches(@RequestBody List<CustomerDTO> customerDTOs) {

		loanService.updateLoanBranches(customerDTOs);
	}

	/**
	 * Loan review.
	 *
	 * @param loanDTO the loan DTO
	 * @return the loan DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	@PostMapping("/loan-review")
	public LoanDTO loanReview(@RequestBody LoanDTO loanDTO) throws ResourcesNotFoundException {

		return loanService.LoanReview(loanDTO);

	}

	/**
	 * Count topups by account.
	 *
	 * @param accountId the account id
	 * @return the integer
	 */
	@GetMapping("/count-topup-by-acccount/{accountId}")
	public Integer countTopupsByAccount(@PathVariable("accountId") long accountId) {

		return loanService.countTopupsByAccount(accountId);
	}

	/**
	 * Find loans by supplier.
	 *
	 * @param supplierId the supplier id
	 * @return the list
	 */
	@GetMapping("/find-by-supplier/{supplierId}")
	public List<LoanDTO> findLoansBySupplier(@PathVariable Long supplierId) {

		return loanService.findBySupplier(supplierId);
	}

	/**
	 * Automatic step.
	 *
	 * @param loanDTO the loan DTO
	 * @return the loan DTO
	 * @throws CustomerMaxActiveAccountException the customer max active account exception
	 * @throws ApiAbacusException the api abacus exception
	 * @throws CalculateAgeException the calculate age exception
	 * @throws ResourcesNotFoundException the resources not found exception
	 * @throws IOException Signals that an I/O exception has occurred.
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
	@PostMapping("/automatic-step")
	public LoanDTO automaticStep(@RequestBody LoanDTO loanDTO)
			throws CustomerMaxActiveAccountException, ApiAbacusException, CalculateAgeException,
			ResourcesNotFoundException, IOException, CreditException, ConditionalApproveException,
			DisbursementException, CheckApprovelLevelException, EnableCriticalDataException,
			WorkFlowSettingException, InformCustomerNotFoundException, CheckAppL1NotFoundException,
			CheckAppL2NotFoundException, CheckAppL3NotFoundException, CheckAppL4NotFoundException,
			UploadSignedDocNotFoundExepction, UploadDocumentNotFoundException {

		return loanService.automaticStepLoan(loanDTO);
	}

	/**
	 * Job loans sanad.
	 *
	 * @param token the token
	 */
	@GetMapping("/get-sanad-loan-automatic")
	public void jobLoansSanad(@RequestHeader("Authorization") String token) {

		loanService.jobLoansSanad();
	}

}
