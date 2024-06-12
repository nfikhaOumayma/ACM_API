/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.service.batch;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.batch.item.ItemWriter;
import org.springframework.beans.factory.annotation.Autowired;

import com.acm.client.CreditClient;
import com.acm.client.TransversClient;
import com.acm.constants.common.CommonConstants;
import com.acm.constants.common.CommonErrorCode;
import com.acm.constants.common.CommonExceptionsMessage;
import com.acm.exceptions.model.ExceptionResponseMessage;
import com.acm.exceptions.model.TechnicalException;
import com.acm.exceptions.type.CreditException;
import com.acm.service.AcmEnvironnementService;
import com.acm.utils.dtos.CUAccountPortfolioTransferredDTO;
import com.acm.utils.dtos.CustomerDTO;
import com.acm.utils.dtos.LoanDTO;
import com.acm.utils.validation.ACMValidationUtils;

/**
 * The Class TransferPortfolioWriter.
 */
public class TransferPortfolioWriter implements ItemWriter<CUAccountPortfolioTransferredDTO> {

	/** The Constant logger. */
	private static final Logger logger = LoggerFactory.getLogger(TransferPortfolioWriter.class);

	/** The credit client. */
	@Autowired
	CreditClient creditClient;

	/** The transvers client. */
	@Autowired
	TransversClient transversClient;

	/** The acm environnement service. */
	@Autowired
	private AcmEnvironnementService acmEnvironnementService;

	/*
	 * (non-Javadoc)
	 * @see org.springframework.batch.item.ItemWriter#write(java.util.List)
	 */
	@Override
	public void write(List<? extends CUAccountPortfolioTransferredDTO> items) throws Exception {

		// init loans list to be portfolio updated
		List<LoanDTO> loanDTOs = new ArrayList<>();
		// init customer list to be portfolio updated
		List<CustomerDTO> customerDTOs = new ArrayList<>();
		// init customers ids list ( to use in case of exception produced)
		List<String> customerIds = new ArrayList<>();
		// init loans ids list ( to use in case of exception produced)
		List<String> loanIds = new ArrayList<>();

		// loop on CUAccountPortfolioTransferredDTO list
		for (CUAccountPortfolioTransferredDTO item : items) {
			// init loanDTO
			LoanDTO loanDTO = new LoanDTO();
			// set loan portfolio data
			loanDTO.setIdAccountExtern(item.getCuAccountId());
			loanDTO.setPortfolioId(item.getToPorfolio());
			loanDTO.setPortfolioCode(item.getToPorfolioCode());
			loanDTO.setPortfolioDescription(item.getToPorfolioName());
			// add loanDTO to loanDTOs list
			loanDTOs.add(loanDTO);
			// check if customerDTOs list does not contain the customerId of item (because in
			// customerDTOs, customers to update should not be duplicated )
			if (ACMValidationUtils.isNullOrEmpty(customerDTOs.stream()
					.filter(customer -> customer.getCustomerIdExtern().equals(item.getCustomerId()))
					.collect(Collectors.toList()))) {
				// init customerDTO
				CustomerDTO customerDTO = new CustomerDTO();
				// set customerDTO portfolio data
				customerDTO.setCustomerIdExtern(item.getCustomerId());
				customerDTO.setAccountPortfolioID(item.getToPorfolio());
				customerDTO.setAccountPortfolioCode(item.getToPorfolioCode());
				customerDTO.setAccountPortfolioDescription(item.getToPorfolioName());
				// add customerDTO to customerDTOs list
				customerDTOs.add(customerDTO);
			}
		}
		try {
			// update customers portfolios
			creditClient.updateAllCustomer(customerDTOs);
			// update loans portfolios
			creditClient.updateAllLoan(loanDTOs, CommonConstants.SYNCHRONIZE_PORTFOLIOS);

		}
		catch (Exception e) {
			logger.error(
					"Synchronize Transferred Portfolios: error while updating portfolio data in acm_loan with cuAccountIds  : {}  and acm_customer with customer id extern: {}",
					loanIds, customerIds);
			throw new CreditException(
					new ExceptionResponseMessage(
							CommonErrorCode.ERROR_WHILE_UPDATE_CUSTOMERS_PORTFOLIOS,
							CommonExceptionsMessage.ERROR_WHILE_UPDATE_CUSTOMERS_PORTFOLIOS,
							new TechnicalException()),
					CommonExceptionsMessage.ERROR_WHILE_UPDATE_CUSTOMERS_PORTFOLIOS);
		}
		try {
			// update last CUAccount portfolio transfered Id synchronized
			acmEnvironnementService.updateLimite(
					CommonConstants.LAST_CUACCOUNT_PORTFOLIO_TRANSFERID,
					items.get(items.size() - 1).getId().toString());
		}
		catch (Exception e) {
			logger.error(
					"Error wile updating acm_environment value with key = LAST_CUACCOUNT_PORTFOLIO_TRANSFERID .error = {} ",
					e.getMessage());
		}
	}

}
