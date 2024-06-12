/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.api_abacus.controller;

import java.io.IOException;
import java.net.URISyntaxException;
import java.security.KeyManagementException;
import java.security.KeyStoreException;
import java.security.NoSuchAlgorithmException;
import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import com.acm.api_abacus.model.LoanAbacusAPIModel;
import com.acm.api_abacus.service.DisburseLoanService;
import com.acm.api_abacus.service.LoanApprovalApiService;
import com.acm.api_abacus.service.LoanCalculateApiService;
import com.acm.api_abacus.service.LoanCancelApiService;
import com.acm.api_abacus.service.LoanCreateUpdateApiService;
import com.acm.api_charge_off.dtos.ReadFileCsvDTO;
import com.acm.api_charge_off.dtos.ResponseInfoChargeOffDTO;
import com.acm.exceptions.type.ApiAbacusException;
import com.acm.exceptions.type.CheckApprovelLevelException;
import com.acm.utils.dtos.DisburseDTO;
import com.acm.utils.dtos.DisburseResponse;
import com.acm.utils.dtos.JournalEnteriesToAbacusDTO;
import com.acm.utils.dtos.LoanDTO;
import com.acm.utils.dtos.PaymentApiAbacusDTO;
import com.acm.utils.dtos.RequestPaymentApiAbacusDTO;
import com.acm.utils.dtos.ResponseGetInfoPaymentAbacusDTO;
import com.acm.utils.enums.CustomerType;
import com.acm.utils.models.transvers.LoanScheduleAPI;

/**
 * This class @{link LoanAPIController}.
 *
 * @author HaythemBenizid
 * @since 0.9.0
 */
@RestController
@RequestMapping("/load-data-api-abacus")
public class LoanAPIController {

	/** The loan cancel api service. */
	@Autowired
	private LoanCancelApiService loanCancelApiService;

	/** The loan calculate api service. */
	@Autowired
	private LoanCalculateApiService loanCalculateApiService;

	/** The loan approval api service. */
	@Autowired
	private LoanApprovalApiService loanApprovalApiService;

	/** The loan create update api service. */
	@Autowired
	private LoanCreateUpdateApiService loanCreateUpdateApiService;

	/** The disburse loan service. */
	@Autowired
	private DisburseLoanService disburseLoanService;

	/**
	 * Calculate loan schedules.
	 *
	 * @author HaythemBenizid
	 * @param loanDTO the loan DTO
	 * @return the list
	 * @throws IOException Signals that an I/O exception has occurred.
	 * @throws ApiAbacusException the api abacus exception
	 */
	@PostMapping("/calculate-loan-schedules")
	public LoanScheduleAPI calculateLoanSchedules(@RequestBody LoanDTO loanDTO)
			throws IOException, ApiAbacusException {

		return loanCalculateApiService.calculate(loanDTO);
	}

	/**
	 * Cancel loan.
	 *
	 * @author HaythemBenizid
	 * @param loanDTO the loan DTO
	 * @throws IOException Signals that an I/O exception has occurred.
	 * @throws ApiAbacusException the api abacus exception
	 */
	@PostMapping("/cancel-loan")
	public void cancelLoan(@RequestBody LoanDTO loanDTO) throws IOException, ApiAbacusException {

		loanCancelApiService.cancel(loanDTO);
	}

	/**
	 * Approve loan INDIV / ORG.
	 *
	 * @author HaythemBenizid
	 * @param loanDTO the loan DTO
	 * @throws IOException Signals that an I/O exception has occurred.
	 * @throws CheckApprovelLevelException the check approvel level exception
	 * @throws ApiAbacusException the api abacus exception
	 */
	@PostMapping("/approve-loan")
	public void approveLoan(@RequestBody LoanDTO loanDTO)
			throws IOException, CheckApprovelLevelException, ApiAbacusException {

		loanApprovalApiService.approvel(loanDTO);
	}

	/**
	 * Approve loan GRP.
	 *
	 * @author HaythemBenizid
	 * @param loanDTOs the loan DT os
	 * @throws IOException Signals that an I/O exception has occurred.
	 * @throws CheckApprovelLevelException the check approvel level exception
	 * @throws ApiAbacusException the api abacus exception
	 */
	@PostMapping("/approve-loan-group")
	public void approvelGroup(@RequestBody List<LoanDTO> loanDTOs)
			throws IOException, CheckApprovelLevelException, ApiAbacusException {

		loanApprovalApiService.approvelGroup(loanDTOs);
	}

	/**
	 * Creates the loan INDIV.
	 *
	 * @author MoezMhiri
	 * @param loanDTO the loan DTO
	 * @return the loan DTO
	 * @throws IOException Signals that an I/O exception has occurred.
	 * @throws ApiAbacusException the api abacus exception
	 */
	@PostMapping("/create-loan-indiv")
	public LoanDTO createLoanINDIV(@RequestBody LoanDTO loanDTO)
			throws IOException, ApiAbacusException {

		return loanCreateUpdateApiService.save(loanDTO, CustomerType.INDIV);
	}

	/**
	 * Creates the loan GRP.
	 *
	 * @author HaythemBenizid
	 * @param loanDTOs the loan DT os
	 * @return the list
	 * @throws IOException Signals that an I/O exception has occurred.
	 * @throws ApiAbacusException the api abacus exception
	 */
	@PostMapping("/create-loan-grp")
	public List<LoanDTO> createLoanGRP(@RequestBody List<LoanDTO> loanDTOs)
			throws IOException, ApiAbacusException {

		return loanCreateUpdateApiService.save(loanDTOs);
	}

	/**
	 * Creates the loan ORG.
	 *
	 * @author HaythemBenizid
	 * @param loanDTO the loan DTO
	 * @return the loan DTO
	 * @throws IOException Signals that an I/O exception has occurred.
	 * @throws ApiAbacusException the api abacus exception
	 */
	@PostMapping("/create-loan-org")
	public LoanDTO createLoanORG(@RequestBody LoanDTO loanDTO)
			throws IOException, ApiAbacusException {

		return loanCreateUpdateApiService.save(loanDTO, CustomerType.ORG);
	}

	/**
	 * Update loan INDIV - ORG.
	 *
	 * @author HaythemBenizid
	 * @param loanDTO the loan DTO
	 * @throws IOException Signals that an I/O exception has occurred.
	 * @throws ApiAbacusException the api abacus exception
	 * @throws CheckApprovelLevelException the check approvel level exception
	 */
	@PostMapping("/update-loan-indiv-org")
	public void updateLoanINDIVORG(@RequestBody LoanDTO loanDTO)
			throws IOException, ApiAbacusException, CheckApprovelLevelException {

		loanCreateUpdateApiService.update(loanDTO);
	}

	/**
	 * update the loan GRP.
	 *
	 * @author HaythemBenizid
	 * @param loanDTOs the loan DT os
	 * @throws IOException Signals that an I/O exception has occurred.
	 * @throws ApiAbacusException the api abacus exception
	 */
	@PostMapping("/update-loan-grp")
	public void updateLoanGRP(@RequestBody List<LoanDTO> loanDTOs)
			throws IOException, ApiAbacusException {

		loanCreateUpdateApiService.update(loanDTOs);
	}

	/**
	 * Find Loan by accountId.
	 *
	 * @author HaythemBenizid
	 * @param accountId the account id
	 * @return the loan abacus API model
	 * @throws ApiAbacusException the api abacus exception
	 * @throws IOException Signals that an I/O exception has occurred.
	 * @throws URISyntaxException the URI syntax exception
	 */
	@GetMapping("/loan-data/{accountId}")
	public LoanAbacusAPIModel findByAccountId(@PathVariable("accountId") Long accountId)
			throws ApiAbacusException, IOException, URISyntaxException {

		return loanCreateUpdateApiService.getData(accountId);
	}

	/**
	 * Creates the journal entry.
	 *
	 * @param journalEnteriesToAbacus the journal enteries to abacus
	 * @return the string
	 * @throws IOException Signals that an I/O exception has occurred.
	 * @throws ApiAbacusException the api abacus exception
	 * @throws KeyManagementException the key management exception
	 * @throws KeyStoreException the key store exception
	 * @throws NoSuchAlgorithmException the no such algorithm exception
	 * @throws URISyntaxException the URI syntax exception
	 * @throws IllegalArgumentException the illegal argument exception
	 * @throws IllegalAccessException the illegal access exception
	 */
	@PostMapping("/create-journal-entry")
	public String createJournalEntry(
			@RequestBody JournalEnteriesToAbacusDTO journalEnteriesToAbacus) throws IOException,
			ApiAbacusException, KeyManagementException, KeyStoreException, NoSuchAlgorithmException,
			URISyntaxException, IllegalArgumentException, IllegalAccessException {

		return loanCreateUpdateApiService.createJournalEntry(journalEnteriesToAbacus);
	}

	/**
	 * Disburse loan.
	 *
	 * @param disburseDto the disburse dto
	 * @return the disburse response
	 * @throws URISyntaxException the URI syntax exception
	 * @throws ApiAbacusException the api abacus exception
	 * @throws KeyManagementException the key management exception
	 * @throws KeyStoreException the key store exception
	 * @throws NoSuchAlgorithmException the no such algorithm exception
	 */
	@PostMapping("/disburse-loan")
	public DisburseResponse disburseLoan(@RequestBody DisburseDTO disburseDto)
			throws URISyntaxException, ApiAbacusException, KeyManagementException,
			KeyStoreException, NoSuchAlgorithmException {

		return disburseLoanService.disburseLoan(disburseDto);
	}

	/**
	 * Payment loan.
	 *
	 * @param paymentApiAbacusDTO the payment api abacus DTO
	 * @param usernameAbacus the username abacus
	 * @param paymentFrom the payment from
	 * @return the boolean
	 * @throws IOException Signals that an I/O exception has occurred.
	 * @throws ApiAbacusException the api abacus exception
	 */
	@PostMapping("/payment-loan")
	public Boolean paymentLoan(@RequestBody PaymentApiAbacusDTO paymentApiAbacusDTO,
			@RequestParam(value = "username") String usernameAbacus,
			@RequestParam(value = "paymentFrom") String paymentFrom)
			throws IOException, ApiAbacusException {

		return loanCreateUpdateApiService.paymentLoan(paymentApiAbacusDTO, usernameAbacus,
				paymentFrom);
	}

	/**
	 * Charge off abacus.
	 *
	 * @param chargeOffDTO the charge off DTO
	 * @return the response entity
	 * @throws URISyntaxException the URI syntax exception
	 * @throws ApiAbacusException the api abacus exception
	 * @throws KeyManagementException the key management exception
	 * @throws KeyStoreException the key store exception
	 * @throws NoSuchAlgorithmException the no such algorithm exception
	 * @throws IOException Signals that an I/O exception has occurred.
	 */
	@PostMapping("/charge-off")
	public ResponseEntity<String> chargeOffAbacus(
			@RequestBody ResponseInfoChargeOffDTO chargeOffDTO)
			throws URISyntaxException, ApiAbacusException, KeyManagementException,
			KeyStoreException, NoSuchAlgorithmException, IOException {

		return loanCreateUpdateApiService.chargeOffAbacus(chargeOffDTO);
	}

	/**
	 * Read list data csv file charge off abacus.
	 *
	 * @param listDataCsvDTO the list data csv DTO
	 * @return the response entity
	 * @throws KeyManagementException the key management exception
	 * @throws ApiAbacusException the api abacus exception
	 * @throws KeyStoreException the key store exception
	 * @throws NoSuchAlgorithmException the no such algorithm exception
	 * @throws IOException Signals that an I/O exception has occurred.
	 * @throws URISyntaxException the URI syntax exception
	 */
	@PostMapping("/listDataCsvFile-charge-off")
	public ResponseEntity<String> readListDataCsvFileChargeOffAbacus(
			@RequestBody List<ReadFileCsvDTO> listDataCsvDTO)
			throws KeyManagementException, ApiAbacusException, KeyStoreException,
			NoSuchAlgorithmException, IOException, URISyntaxException {

		return loanCreateUpdateApiService.readListDataCsvFileChargeOffAbacus(listDataCsvDTO);
	}

	/**
	 * Find informations charge off.
	 *
	 * @param accountId the account id
	 * @return the response info charge off DTO
	 * @throws ApiAbacusException the api abacus exception
	 * @throws IOException Signals that an I/O exception has occurred.
	 * @throws URISyntaxException the URI syntax exception
	 */
	@GetMapping("/get-informations-chargeOff/{cuAccountId}")
	public ResponseInfoChargeOffDTO findInformationsChargeOff(
			@PathVariable("cuAccountId") Long accountId)
			throws ApiAbacusException, IOException, URISyntaxException {

		return loanCreateUpdateApiService.findInformationsChargeOff(accountId);
	}

	/**
	 * Find informations payment.
	 *
	 * @param accountId the account id
	 * @return the response get info payment abacus DTO
	 * @throws ApiAbacusException the api abacus exception
	 * @throws IOException Signals that an I/O exception has occurred.
	 * @throws URISyntaxException the URI syntax exception
	 */
	@GetMapping("/get-informations-payment/{cuAccountId}")
	public ResponseGetInfoPaymentAbacusDTO findInformationsPayment(
			@PathVariable("cuAccountId") Long accountId)
			throws ApiAbacusException, IOException, URISyntaxException {

		return loanCreateUpdateApiService.findInformationsPayment(accountId);
	}

	/**
	 * Receipt initialize by cu account id.
	 *
	 * @param request the request
	 * @return true, if successful
	 * @throws ApiAbacusException the api abacus exception
	 * @throws IOException Signals that an I/O exception has occurred.
	 * @throws URISyntaxException the URI syntax exception
	 */
	@PostMapping("/receipt-informations")
	public boolean receiptInitializeByCuAccountId(@RequestBody RequestPaymentApiAbacusDTO request)
			throws ApiAbacusException, IOException, URISyntaxException {

		return loanCreateUpdateApiService.receiptInitializeByCuAccountId(request);
	}
}
