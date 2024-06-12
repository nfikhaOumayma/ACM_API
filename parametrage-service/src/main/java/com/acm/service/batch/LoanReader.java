/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.service.batch;

import java.util.ArrayList;
import java.util.List;

import org.hibernate.jdbc.BatchFailedException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.batch.item.ItemReader;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;

import com.acm.client.TransversClient;
import com.acm.constants.common.CommonFunctions;
import com.acm.constants.common.CommonLoggerMessage;
import com.acm.exceptions.type.ResourcesNotFoundException;
import com.acm.service.AcmEnvironnementService;
import com.acm.utils.dtos.AcmEnvironnementDTO;
import com.acm.utils.dtos.LoanDTO;
import com.acm.utils.models.AcmEnvironnement;
import com.acm.utils.models.Loan;

/**
 * {@link LoanReader} class.
 *
 * @author HaythemBenizid
 * @since 0.1.0
 */
public class LoanReader implements ItemReader<Loan> {

	/** logger. */
	private static final Logger logger = LoggerFactory.getLogger(LoanReader.class);

	/** The transvers client. */
	@Autowired
	private TransversClient transversClient;

	/** The acm environnement service. */
	@Autowired
	private AcmEnvironnementService acmEnvironnementService;

	/** The count. */
	private int count = 0;

	/** The token. */
	private String token = "NOT";

	/** The url serveur authentification. */
	@Value("${url.serveur.authentification}")
	private String urlServeurAuthentification;

	/*
	 * (non-Javadoc)
	 * @see org.springframework.batch.item.ItemReader#read()
	 */
	@Override
	public Loan read() throws BatchFailedException {

		logger.debug("### init reader process");
		if ("NOT".equals(token)) {
			token = "Bearer " + CommonFunctions.generateToken(urlServeurAuthentification);
		}
		List<LoanDTO> loansAbacus = new ArrayList<>();
		try {
			// loading the index where the last job has stopped
			AcmEnvironnementDTO environnementDTO =
					acmEnvironnementService.find("LIMITE_ID_LOAN_EXTERNE");
			// loading list loan from ABACUS DB
			loansAbacus = transversClient.find(token,
					Long.valueOf(environnementDTO != null ? environnementDTO.getValue() : "0"));
		}
		catch (ResourcesNotFoundException e) {
			logger.error(CommonLoggerMessage.NOT_FOUND_OBJECT,
					AcmEnvironnement.class.getSimpleName());
			logger.error(e.getMessage());
			throw new BatchFailedException("Error Read Lean ");
		}
		catch (Exception e) {
			logger.error("Failed to get list");
			logger.error(CommonLoggerMessage.ERROR_WHILE_CONNECTING_TO_REMOTE_SERVICE,
					e.getMessage());
			// re-generate token if expired
			token = "Bearer " + CommonFunctions.generateToken(urlServeurAuthentification);
			throw new BatchFailedException("Error Read Lean ");

		}
		// reading founded list if not empty
		if (count < loansAbacus.size()) {
			Loan loan = new Loan(
					loansAbacus.get(count).getPortfolioId() != null
							? loansAbacus.get(count).getPortfolioId()
							: 0,
					loansAbacus.get(count).getIdLoanExtern(),
					loansAbacus.get(count).getIdAccountExtern(),
					loansAbacus.get(count).getAccountNumber(),
					loansAbacus.get(count).getApplyDate(), loansAbacus.get(count).getProductCode(),
					loansAbacus.get(count).getProductDescription(),
					loansAbacus.get(count).getCustomerName(),
					loansAbacus.get(count).getPortfolioCode(),
					loansAbacus.get(count).getPortfolioDescription(),
					loansAbacus.get(count).getCurrencySymbol(),
					loansAbacus.get(count).getCurrencyDecimalPlaces(),
					loansAbacus.get(count).getProductId(), loansAbacus.get(count).getCustomerId(),
					loansAbacus.get(count).getApplyAmountTotal(),
					loansAbacus.get(count).getGracePeriod(),
					loansAbacus.get(count).getIndustryCode(),
					loansAbacus.get(count).getIndustryCodeDescription(),
					loansAbacus.get(count).getIssueDate(), loansAbacus.get(count).getCreationDate(),
					loansAbacus.get(count).getTermPeriodNum(),
					loansAbacus.get(count).getPaymentFreq(),
					loansAbacus.get(count).getIssueFeeAmount(),
					loansAbacus.get(count).getProductRate(),
					loansAbacus.get(count).getLoanReasonCode(),
					loansAbacus.get(count).getLoanReasonDescription(),
					loansAbacus.get(count).getInitialPaymentDate(),
					loansAbacus.get(count).getNormalPayment(),
					loansAbacus.get(count).getIgnoreOddDays(),
					loansAbacus.get(count).getPeriodsDeferred(),
					loansAbacus.get(count).getCalculateInitialPaymentDate(),
					loansAbacus.get(count).getTermPeriodID(), loansAbacus.get(count).getBranchID(),
					loansAbacus.get(count).getBranchName(),
					loansAbacus.get(count).getBranchDescription(),
					loansAbacus.get(count).getCustomerType(),
					loansAbacus.get(count).getCommunityCULoanID(),
					loansAbacus.get(count).getGuarantorSourceId(),
					loansAbacus.get(count).getSourceOfFundsID(),
					loansAbacus.get(count).getRefinanceReasonId(),
					loansAbacus.get(count).getDistrictCodeId(),
					loansAbacus.get(count).getIntPayPeriodNum(),
					loansAbacus.get(count).getLoanCalculationMode(),
					loansAbacus.get(count).getApr(), loansAbacus.get(count).getEffectiveIntRate(),
					token);
			count++;
			return loan;
		}
		else {
			count = 0;
		}
		return null;
	}
}
