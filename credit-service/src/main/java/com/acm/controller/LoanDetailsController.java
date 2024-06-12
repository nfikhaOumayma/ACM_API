/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.controller;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import com.acm.client.ParametrageClient;
import com.acm.client.TransversClient;
import com.acm.constants.common.ACMConstantWorkflowStatuts;
import com.acm.constants.common.CommonErrorCode;
import com.acm.constants.common.CommonExceptionsMessage;
import com.acm.constants.common.CommonFunctions;
import com.acm.exceptions.model.ExceptionResponseMessage;
import com.acm.exceptions.type.ApiAbacusException;
import com.acm.exceptions.type.CalculateAgeEndLoanException;
import com.acm.service.LoanDetailsService;
import com.acm.utils.date.DateUtil;
import com.acm.utils.dtos.AcmCollateralDTO;
import com.acm.utils.dtos.AcmStatutsDTO;
import com.acm.utils.dtos.CollaterolDTO;
import com.acm.utils.dtos.CustomerDTO;
import com.acm.utils.dtos.FinancialAnalysisDTO;
import com.acm.utils.dtos.GuarantorDTO;
import com.acm.utils.dtos.LoanDTO;
import com.acm.utils.dtos.LoanDetailsDTO;
import com.acm.utils.dtos.ProductDTO;
import com.acm.utils.dtos.ScheduleDTO;
import com.acm.utils.dtos.SettingDocumentTypeDTO;
import com.acm.utils.models.transvers.LoanScheduleAPI;

/**
 * This class @{link LoanDetailsController} used to control all the loan requests.
 *
 * @author HaythemBenizid
 * @since 0.5.0
 */
@RestController
@RequestMapping("/loans")
public class LoanDetailsController {

	/** The loan details service. */
	@Autowired
	private LoanDetailsService loanDetailsService;

	/** The parametrage client. */
	@Autowired
	private ParametrageClient parametrageClient;

	/** The transvers client service. */
	@Autowired
	private TransversClient transversClient;

	/**
	 * Find loan details by ID loan extern from ABACUS DB.
	 *
	 * @author HaythemBenizid
	 * @param idLoanExtern the id loan extern
	 * @return the loan DTO
	 */
	@GetMapping("/data-abacus/loan-details/{idLoanExtern}")
	public LoanDetailsDTO findDetailsLoan(@PathVariable("idLoanExtern") Long idLoanExtern) {

		return loanDetailsService.findDetailsLoan(idLoanExtern);
	}

	/**
	 * Find details customer by ID loan extern from ABACUS DB.
	 *
	 * @author HaythemBenizid
	 * @param idLoanExtern the id loan extern
	 * @return the loan details DTO
	 */
	@GetMapping("/data-abacus/customer-loan/{idLoanExtern}")
	public CustomerDTO findDetailsCustomer(@PathVariable("idLoanExtern") Long idLoanExtern) {

		return loanDetailsService.findDetailsCustomer(idLoanExtern);
	}

	/**
	 * Find customer account schedule by loan.
	 * 
	 * @author HaythemBenizid
	 * @param idLoanExtern the id loan extern
	 * @return the list
	 */
	@GetMapping("/data-abacus/customer-account-schedule/{idLoanExtern}")
	public List<ScheduleDTO> findCustomerAccountScheduleByLoan(
			@PathVariable("idLoanExtern") Long idLoanExtern) {

		return loanDetailsService.findCustomerAccountScheduleByLoan(idLoanExtern);
	}

	/**
	 * Find list of guarantors by given loan ID.
	 * 
	 * @author HaythemBenizid
	 * @param idLoan the id loan
	 * @return the list
	 */
	@GetMapping("/data-abacus/guarantors/{idLoan}")
	public List<GuarantorDTO> findGuarantors(@PathVariable("idLoan") Long idLoan) {

		return loanDetailsService.findGuarantors(idLoan);
	}

	/**
	 * Find list of collaterol by given loan ID.
	 * 
	 * @author YesserSomai
	 * @param idLoan the id loan
	 * @return the list of Collaterols
	 */
	@GetMapping("/data-abacus/collaterols/{idLoan}")
	public List<CollaterolDTO> findCollaterols(@PathVariable("idLoan") Long idLoan) {

		return loanDetailsService.findCollaterols(idLoan);
	}

	/**
	 * Find list of Financial Analysis by given loan ID.
	 * 
	 * @author YesserSomai
	 * @param idLoanExtern the id idLoanExtern
	 * @return the list of Financial Analysis
	 */
	@GetMapping("/data-abacus/financialanalysis/{idLoan}")
	public List<FinancialAnalysisDTO> findFinancialAnalysis(
			@PathVariable("idLoan") Long idLoanExtern) {

		return loanDetailsService.findFinancialAnalysis(idLoanExtern);
	}

	/**
	 * Find required document by given params.
	 * 
	 * @author HaythemBenizid
	 * @param loanDTO the loan DTO
	 * @return the list
	 */
	@PostMapping("/findRequiredDocument")
	public List<SettingDocumentTypeDTO> findRequiredDocument(@RequestBody LoanDTO loanDTO) {

		return loanDetailsService.findRequiredDocument(loanDTO);
	}

	/**
	 * Calculate loan schedules.
	 * 
	 * @param loanDTO the loan DTO
	 * @return the loan schedule API
	 * @throws ApiAbacusException the api abacus exception
	 * @throws IOException Signals that an I/O exception has occurred.
	 * @throws CalculateAgeEndLoanException the calculate age end loan exception
	 */
	@PostMapping("/load-data-api-abacus/calculate-loan-schedules")
	public LoanScheduleAPI calculateLoanSchedules(@RequestBody LoanDTO loanDTO)
			throws ApiAbacusException, IOException, CalculateAgeEndLoanException {

		LoanScheduleAPI loanScheduleAPI;
		try {
			// find product object by ID
			loanDTO.setProductDTO(
					parametrageClient.findProductById(loanDTO.getProductId().longValue()));
			// calling calculate API
			loanScheduleAPI = transversClient.calculateLoanSchedules(loanDTO);
		}
		catch (Exception e) {
			// Fire Exception
			throw new ApiAbacusException(CommonErrorCode.API_ABACUS, e.getMessage());
		}

		List<ScheduleDTO> scheduleDTOs = loanScheduleAPI.getLoanSchedule();
		// check age customer end of loan
		long age = DateUtil.calculateAgeEndLoan(loanDTO.getCustomerDTO().getDateOfBirth(),
				scheduleDTOs.get(scheduleDTOs.size() - 2).getRepaymentDate());
		if (age >= loanDTO.getProductDTO().getMaximumAge()) {
			throw new CalculateAgeEndLoanException(
					new ExceptionResponseMessage(CommonErrorCode.CUSTOMER_INVALID_AGE,
							CommonExceptionsMessage.CUSTOMER_INVALID_AGE),
					CommonExceptionsMessage.CUSTOMER_INVALID_AGE);
		}
		else {
			age = DateUtil.calculateAgeEndLoan(loanDTO.getCustomerDTO().getDateOfBirth(),
					scheduleDTOs.get(0).getRepaymentDate());
			if (age < loanDTO.getProductDTO().getMinimumAge()) {
				throw new CalculateAgeEndLoanException(
						new ExceptionResponseMessage(CommonErrorCode.CUSTOMER_INVALID_AGE,
								CommonExceptionsMessage.CUSTOMER_INVALID_AGE),
						CommonExceptionsMessage.CUSTOMER_INVALID_AGE);
			}
		}
		return loanScheduleAPI;
	}

	/**
	 * Find Product.
	 *
	 * @author YesserSomai
	 * @param productId the product id
	 * @return find the product by productId
	 */
	@GetMapping("/data-abacus/product/{productid}")
	public ProductDTO findProduct(@PathVariable("productid") Long productId) {

		return transversClient.findProduct(productId);
	}

	/**
	 * Find fee repayment.
	 *
	 * @author Salmen Fatnassi
	 * @param idAccount the id account
	 * @return the long
	 */
	@GetMapping("/load-data-abacus/fee-repayment/{idAccount}")
	public Long findFeeRepayment(@PathVariable("idAccount") Long idAccount) {

		return transversClient.findFeeRepayment(idAccount);
	}

	/**
	 * Find application fee.
	 *
	 * @author MoezMhiri
	 * @param idAccount the id account
	 * @return the long
	 */
	@GetMapping("/load-data-abacus/application-fee/{idAccount}")
	public Long findApplicationFee(@PathVariable("idAccount") Long idAccount) {

		return transversClient.findApplicationFee(idAccount);
	}

	/**
	 * Load Loan status.
	 * 
	 * @author HaythemBenizid
	 * @return the list
	 */
	@GetMapping("/status")
	public List<AcmStatutsDTO> loadCategoryType() {

		List<AcmStatutsDTO> acmStatutsDTOs = new ArrayList<>();
		acmStatutsDTOs.add(new AcmStatutsDTO(
				CommonFunctions.mappingStatus(ACMConstantWorkflowStatuts.UPDATE_LOAN_DATA).getKey(),
				CommonFunctions.mappingStatus(ACMConstantWorkflowStatuts.UPDATE_LOAN_DATA)
						.getValue()));
		acmStatutsDTOs.add(new AcmStatutsDTO(
				CommonFunctions.mappingStatus(ACMConstantWorkflowStatuts.GUARANTOR).getKey(),
				CommonFunctions.mappingStatus(ACMConstantWorkflowStatuts.GUARANTOR).getValue()));

		acmStatutsDTOs.add(new AcmStatutsDTO(
				CommonFunctions.mappingStatus(ACMConstantWorkflowStatuts.FIELD_VISIT).getKey(),
				CommonFunctions.mappingStatus(ACMConstantWorkflowStatuts.FIELD_VISIT).getValue()));

		acmStatutsDTOs.add(new AcmStatutsDTO(
				CommonFunctions.mappingStatus(ACMConstantWorkflowStatuts.ADD_DOCUMENTS).getKey(),
				CommonFunctions.mappingStatus(ACMConstantWorkflowStatuts.ADD_DOCUMENTS)
						.getValue()));

		acmStatutsDTOs.add(new AcmStatutsDTO(
				CommonFunctions.mappingStatus(ACMConstantWorkflowStatuts.APPROVAL_L1).getKey(),
				CommonFunctions.mappingStatus(ACMConstantWorkflowStatuts.APPROVAL_L1).getValue()));

		acmStatutsDTOs.add(new AcmStatutsDTO(
				CommonFunctions.mappingStatus(ACMConstantWorkflowStatuts.APPROVAL_L2).getKey(),
				CommonFunctions.mappingStatus(ACMConstantWorkflowStatuts.APPROVAL_L2).getValue()));

		acmStatutsDTOs.add(new AcmStatutsDTO(
				CommonFunctions.mappingStatus(ACMConstantWorkflowStatuts.APPROVAL_L3).getKey(),
				CommonFunctions.mappingStatus(ACMConstantWorkflowStatuts.APPROVAL_L3).getValue()));

		acmStatutsDTOs.add(new AcmStatutsDTO(
				CommonFunctions.mappingStatus(ACMConstantWorkflowStatuts.APPROVAL_L4).getKey(),
				CommonFunctions.mappingStatus(ACMConstantWorkflowStatuts.APPROVAL_L4).getValue()));

		acmStatutsDTOs.add(new AcmStatutsDTO(
				CommonFunctions.mappingStatus(ACMConstantWorkflowStatuts.CUSTOMER_DECISION)
						.getKey(),
				CommonFunctions.mappingStatus(ACMConstantWorkflowStatuts.CUSTOMER_DECISION)
						.getValue()));

		acmStatutsDTOs.add(new AcmStatutsDTO(
				CommonFunctions.mappingStatus(ACMConstantWorkflowStatuts.UPLOAD_SIGNED_AGREEMENT)
						.getKey(),
				CommonFunctions.mappingStatus(ACMConstantWorkflowStatuts.UPLOAD_SIGNED_AGREEMENT)
						.getValue()));

		acmStatutsDTOs.add(new AcmStatutsDTO(
				CommonFunctions.mappingStatus(ACMConstantWorkflowStatuts.DISBURSEMENT_CASE_CLOSURE)
						.getKey(),
				CommonFunctions.mappingStatus(ACMConstantWorkflowStatuts.DISBURSEMENT_CASE_CLOSURE)
						.getValue()));

		acmStatutsDTOs.add(new AcmStatutsDTO(
				CommonFunctions.mappingStatus(ACMConstantWorkflowStatuts.REJECTED).getKey(),
				CommonFunctions.mappingStatus(ACMConstantWorkflowStatuts.REJECTED).getValue()));

		acmStatutsDTOs.add(new AcmStatutsDTO(
				CommonFunctions.mappingStatus(ACMConstantWorkflowStatuts.CANCELLED).getKey(),
				CommonFunctions.mappingStatus(ACMConstantWorkflowStatuts.CANCELLED).getValue()));

		acmStatutsDTOs.add(new AcmStatutsDTO(
				CommonFunctions.mappingStatus(ACMConstantWorkflowStatuts.REVIEW).getKey(),
				CommonFunctions.mappingStatus(ACMConstantWorkflowStatuts.REVIEW).getValue()));

		acmStatutsDTOs.add(new AcmStatutsDTO(
				CommonFunctions.mappingStatus(ACMConstantWorkflowStatuts.DECLINE).getKey(),
				CommonFunctions.mappingStatus(ACMConstantWorkflowStatuts.DECLINE).getValue()));

		acmStatutsDTOs.add(new AcmStatutsDTO(
				CommonFunctions.mappingStatus(ACMConstantWorkflowStatuts.SCREENING).getKey(),
				CommonFunctions.mappingStatus(ACMConstantWorkflowStatuts.SCREENING).getValue()));

		acmStatutsDTOs.add(new AcmStatutsDTO(
				CommonFunctions.mappingStatus(ACMConstantWorkflowStatuts.AUDIT).getKey(),
				CommonFunctions.mappingStatus(ACMConstantWorkflowStatuts.AUDIT).getValue()));

		acmStatutsDTOs.add(new AcmStatutsDTO(
				CommonFunctions.mappingStatus(ACMConstantWorkflowStatuts.RISK).getKey(),
				CommonFunctions.mappingStatus(ACMConstantWorkflowStatuts.RISK).getValue()));

		acmStatutsDTOs.add(new AcmStatutsDTO(
				CommonFunctions.mappingStatus(ACMConstantWorkflowStatuts.ISSUED).getKey(),
				CommonFunctions.mappingStatus(ACMConstantWorkflowStatuts.ISSUED).getValue()));

		return acmStatutsDTOs;
	}

	/**
	 * Load status LOAN ABACUS (Issued=4, Charged off=8, Bad debt = 16, Transferred=32,
	 * Cancelled=64).
	 *
	 * @author HaythemBenizid
	 * @return the list
	 */
	@GetMapping("/status-loan-abacus")
	public List<AcmStatutsDTO> loadStatusABACUS() {

		List<AcmStatutsDTO> acmStatutsDTOs = new ArrayList<>();
		acmStatutsDTOs.add(new AcmStatutsDTO(4, "Issued"));
		acmStatutsDTOs.add(new AcmStatutsDTO(8, "Charged off"));
		acmStatutsDTOs.add(new AcmStatutsDTO(16, "Bad debt"));
		acmStatutsDTOs.add(new AcmStatutsDTO(32, "Transferred"));
		acmStatutsDTOs.add(new AcmStatutsDTO(64, "Cancelled"));
		return acmStatutsDTOs;
	}

	/**
	 * Find active and inactive collaterols.
	 *
	 * @author mlamloum
	 * @param idLoans the id loans
	 * @return the list
	 */
	@PostMapping("/data-abacus/all-collaterols")
	public List<AcmCollateralDTO> findActiveAndInactiveCollaterols(
			@RequestBody List<Long> idLoans) {

		return loanDetailsService.findActiveAndInactiveCollaterols(idLoans);
	}
}
