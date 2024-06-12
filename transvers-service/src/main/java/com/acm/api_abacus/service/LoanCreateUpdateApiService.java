/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.api_abacus.service;

import java.io.IOException;
import java.net.URISyntaxException;
import java.security.KeyManagementException;
import java.security.KeyStoreException;
import java.security.NoSuchAlgorithmException;
import java.util.List;

import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.RequestBody;

import com.acm.api_abacus.model.LoanAbacusAPIModel;
import com.acm.api_charge_off.dtos.ReadFileCsvDTO;
import com.acm.api_charge_off.dtos.ResponseInfoChargeOffDTO;
import com.acm.exceptions.type.ApiAbacusException;
import com.acm.exceptions.type.CheckApprovelLevelException;
import com.acm.utils.dtos.JournalEnteriesToAbacusDTO;
import com.acm.utils.dtos.LoanDTO;
import com.acm.utils.dtos.PaymentApiAbacusDTO;
import com.acm.utils.dtos.RequestPaymentApiAbacusDTO;
import com.acm.utils.dtos.ResponseGetInfoPaymentAbacusDTO;
import com.acm.utils.enums.CustomerType;

/**
 * {@link LoanCreateUpdateApiService} interface.
 *
 * @author HaythemBenizid
 * @since 1.0.10
 */
public interface LoanCreateUpdateApiService {

	/**
	 * create Loan (INDIV / ORG) by given params in ABACUS-DB using API.
	 *
	 * @author HaythemBenizid
	 * @param loanDTO the loan DTO
	 * @param customerType the customer type
	 * @return the loan DTO
	 * @throws IOException Signals that an I/O exception has occurred.
	 * @throws ApiAbacusException the api abacus exception
	 */
	LoanDTO save(LoanDTO loanDTO, CustomerType customerType) throws IOException, ApiAbacusException;

	/**
	 * create Loan (GRP) by given params in ABACUS-DB using API.
	 *
	 * @author HaythemBenizid
	 * @param loanDTOs the loan DT os
	 * @return the loan DTO
	 * @throws IOException Signals that an I/O exception has occurred.
	 * @throws ApiAbacusException the api abacus exception
	 */
	List<LoanDTO> save(List<LoanDTO> loanDTOs) throws IOException, ApiAbacusException;

	/**
	 * update Loan (INDIV / ORG) by given params in ABACUS-DB using API.
	 *
	 * @author HaythemBenizid
	 * @param loanDTO the loan DTO
	 * @throws IOException Signals that an I/O exception has occurred.
	 * @throws ApiAbacusException the api abacus exception
	 * @throws CheckApprovelLevelException the check approvel level exception
	 */
	void update(LoanDTO loanDTO)
			throws IOException, ApiAbacusException, CheckApprovelLevelException;

	/**
	 * update Loan (GRP) by given params in ABACUS-DB using API.
	 *
	 * @author HaythemBenizid
	 * @param loanDTOs the loan DT os
	 * @throws IOException Signals that an I/O exception has occurred.
	 * @throws ApiAbacusException the api abacus exception
	 */
	void update(List<LoanDTO> loanDTOs) throws IOException, ApiAbacusException;

	/**
	 * Gets the data by given loan CU Account ID.
	 *
	 * @author HaythemBenizid
	 * @param accountId the account id
	 * @return the data
	 * @throws IOException Signals that an I/O exception has occurred.
	 * @throws ApiAbacusException the api abacus exception
	 * @throws URISyntaxException the URI syntax exception
	 */
	LoanAbacusAPIModel getData(Long accountId)
			throws IOException, ApiAbacusException, URISyntaxException;

	/**
	 * Payment loan.
	 *
	 * @param paymentApiAbacusDTO the payment api abacus DTO
	 * @param username the username
	 * @param paymentFrom the payment from
	 * @return the boolean
	 * @throws IOException Signals that an I/O exception has occurred.
	 * @throws ApiAbacusException the api abacus exception
	 */
	Boolean paymentLoan(PaymentApiAbacusDTO paymentApiAbacusDTO, String username,
			String paymentFrom) throws IOException, ApiAbacusException;

	/**
	 * Creates the journal entry.
	 *
	 * @param journalEnteriesToAbacus the journal enteries to abacus
	 * @return the string
	 * @throws KeyManagementException the key management exception
	 * @throws KeyStoreException the key store exception
	 * @throws NoSuchAlgorithmException the no such algorithm exception
	 * @throws URISyntaxException the URI syntax exception
	 * @throws IllegalArgumentException the illegal argument exception
	 * @throws IllegalAccessException the illegal access exception
	 */
	String createJournalEntry(JournalEnteriesToAbacusDTO journalEnteriesToAbacus)
			throws KeyManagementException, KeyStoreException, NoSuchAlgorithmException,
			URISyntaxException, IllegalArgumentException, IllegalAccessException;

	/**
	 * Find informations charge off.
	 *
	 * @param accountId the account id
	 * @return the response info charge off DTO
	 * @throws IOException Signals that an I/O exception has occurred.
	 * @throws ApiAbacusException the api abacus exception
	 */
	ResponseInfoChargeOffDTO findInformationsChargeOff(Long accountId)
			throws IOException, ApiAbacusException;

	/**
	 * Charge off abacus.
	 *
	 * @param chargeOffDTO the charge off DTO
	 * @return the response entity
	 * @throws IOException Signals that an I/O exception has occurred.
	 * @throws ApiAbacusException the api abacus exception
	 * @throws URISyntaxException the URI syntax exception
	 * @throws KeyManagementException the key management exception
	 * @throws KeyStoreException the key store exception
	 * @throws NoSuchAlgorithmException the no such algorithm exception
	 */
	ResponseEntity<String> chargeOffAbacus(ResponseInfoChargeOffDTO chargeOffDTO)
			throws IOException, ApiAbacusException, URISyntaxException, KeyManagementException,
			KeyStoreException, NoSuchAlgorithmException;

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
	ResponseEntity<String> readListDataCsvFileChargeOffAbacus(
			@RequestBody List<ReadFileCsvDTO> listDataCsvDTO)
			throws KeyManagementException, ApiAbacusException, KeyStoreException,
			NoSuchAlgorithmException, IOException, URISyntaxException;

	/**
	 * Find informations payment.
	 *
	 * @param accountId the account id
	 * @return the response get info payment abacus DTO
	 * @throws IOException Signals that an I/O exception has occurred.
	 * @throws ApiAbacusException the api abacus exception
	 */
	ResponseGetInfoPaymentAbacusDTO findInformationsPayment(Long accountId)
			throws IOException, ApiAbacusException;

	/**
	 * Receipt initialize by cu account id.
	 *
	 * @param requestPayment the request payment
	 * @return true, if successful
	 * @throws IOException Signals that an I/O exception has occurred.
	 * @throws ApiAbacusException the api abacus exception
	 */
	boolean receiptInitializeByCuAccountId(RequestPaymentApiAbacusDTO requestPayment)
			throws IOException, ApiAbacusException;

}
