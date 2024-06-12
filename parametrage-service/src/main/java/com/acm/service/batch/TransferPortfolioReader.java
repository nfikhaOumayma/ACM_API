/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.service.batch;

import java.util.List;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.batch.item.ItemReader;
import org.springframework.batch.item.NonTransientResourceException;
import org.springframework.batch.item.ParseException;
import org.springframework.batch.item.UnexpectedInputException;
import org.springframework.beans.factory.annotation.Autowired;

import com.acm.client.TransversClient;
import com.acm.constants.common.CommonLoggerMessage;
import com.acm.exceptions.type.ResourcesNotFoundException;
import com.acm.service.AcmEnvironnementService;
import com.acm.utils.dtos.AcmEnvironnementDTO;
import com.acm.utils.dtos.CUAccountPortfolioTransferredDTO;
import com.acm.utils.models.AcmEnvironnement;

/**
 * The Class TransferPortfolioReader.
 */
public class TransferPortfolioReader implements ItemReader<CUAccountPortfolioTransferredDTO> {

	/** The Constant logger. */
	private static final Logger logger = LoggerFactory.getLogger(TransferPortfolioReader.class);

	/** The transvers client. */
	@Autowired
	private TransversClient transversClient;

	/** The count. */
	private int count = 0;

	/** The account portfolio transferred DT os. */
	private List<CUAccountPortfolioTransferredDTO> accountPortfolioTransferredDTOs;

	/** The acm environnement service. */
	@Autowired
	private AcmEnvironnementService acmEnvironnementService;

	/*
	 * (non-Javadoc)
	 * @see org.springframework.batch.item.ItemReader#read()
	 */
	@Override
	public CUAccountPortfolioTransferredDTO read() throws Exception, UnexpectedInputException,
			ParseException, NonTransientResourceException {

		if (accountPortfolioTransferredDTOs == null) {
			accountPortfolioTransferredDTOs = loanDataFromAbacus();
		}
		if (count < accountPortfolioTransferredDTOs.size()) {
			CUAccountPortfolioTransferredDTO accountPortfolioTransferredDTO =
					accountPortfolioTransferredDTOs.get(count);
			count++;
			return accountPortfolioTransferredDTO;
		}
		else {
			count = 0;
			accountPortfolioTransferredDTOs = null;
		}
		return null;
	}

	/**
	 * Loan data from abacus.
	 *
	 * @return the list
	 */
	private List<CUAccountPortfolioTransferredDTO> loanDataFromAbacus() {

		try {
			// loading the index where the last job has stopped
			AcmEnvironnementDTO environnementDTO =
					acmEnvironnementService.find("LAST_CUACCOUNT_PORTFOLIO_TRANSFERID");
			// loading list of customers from ABACUS DB
			List<CUAccountPortfolioTransferredDTO> abacusCustomers =
					transversClient.findCuAccountPortfolioTransferred(Long
							.valueOf(environnementDTO != null ? environnementDTO.getValue() : "0"));
			return abacusCustomers;

		}
		catch (ResourcesNotFoundException e) {
			logger.error(CommonLoggerMessage.NOT_FOUND_OBJECT,
					AcmEnvironnement.class.getSimpleName());
			logger.error(e.getMessage());
			return null;
		}
	}
}
