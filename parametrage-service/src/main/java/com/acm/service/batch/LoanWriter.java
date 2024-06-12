/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.service.batch;

import java.util.ArrayList;
import java.util.List;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.batch.item.ItemWriter;
import org.springframework.beans.factory.annotation.Autowired;

import com.acm.client.CreditClient;
import com.acm.client.ReportingClient;
import com.acm.constants.common.CommonConstants;
import com.acm.constants.common.CommonLoggerMessage;
import com.acm.utils.dtos.LoanDTO;
import com.acm.utils.dtos.MailDTO;
import com.acm.utils.models.Loan;
import com.acm.utils.validation.ACMValidationUtils;

import feign.FeignException;

/**
 * {@link LoanWriter} class.
 *
 * @author HaythemBenizid
 * @since 0.1.0
 */
public class LoanWriter implements ItemWriter<Loan> {

	/** logger. */
	private static final Logger logger = LoggerFactory.getLogger(LoanWriter.class);

	/** The mail sender client. */
	@Autowired
	private ReportingClient mailSenderClient;

	/** The default ACM receiver mail. */
	@Autowired
	private String defaultACMReceiverMail;

	/** The batch credit client. */
	@Autowired
	private CreditClient batchCreditClient;

	/*
	 * (non-Javadoc)
	 * @see org.springframework.batch.item.ItemWriter#write(java.util.List)
	 */
	@Override
	public void write(List<? extends Loan> loans) throws Exception {

		logger.debug("### LoanWriter : list size = {}", loans.size());
		if (!ACMValidationUtils.isNullOrEmpty(loans)) {
			// saving data in DB
			saveLoans(loans);
		}
		logger.debug("### LoanWriter :: DONE");
	}

	/**
	 * Save loans.
	 * 
	 * @author HaythemBenizid
	 * @param loans the loans
	 */
	private void saveLoans(List<? extends Loan> loans) {

		List<LoanDTO> addedLoans = new ArrayList<>();
		for (Loan loan : loans) {
			if (loan != null && loan.getIdLoanExtern() != null) {
				try {
					// insert imported loan into ACM DB
					LoanDTO loanDTO = batchCreditClient.createByBatch(new LoanDTO(
							loan.getPortfolioId() != null ? loan.getPortfolioId() : 0,
							loan.getIdLoanExtern(), loan.getIdAccountExtern(),
							loan.getAccountNumberExtern(), loan.getApplyDate(),
							loan.getProductCode(), loan.getProductDescription(),
							loan.getCustomerName(), loan.getPortfolioCode(),
							loan.getPortfolioDescription(), loan.getCurrencySymbol(),
							loan.getCurrencyDecimalPlaces(), loan.getProductId(),
							loan.getCustomerId(), loan.getApplyAmountTotal(), loan.getGracePeriod(),
							loan.getIndustryCode(), loan.getIndustryCodeDescription(),
							loan.getIssueDate(), loan.getCreationDate(), loan.getTermPeriodNum(),
							loan.getPaymentFreq(), loan.getIssueFeeAmount(), loan.getProductRate(),
							loan.getLoanReasonCode(), loan.getLoanReasonDescription(),
							loan.getInitialPaymentDate(), loan.getNormalPayment(),
							loan.getIgnoreOddDays(), loan.getPeriodsDeferred(),
							loan.getCalculateInitialPaymentDate(), loan.getTermPeriodID(),
							loan.getBranchID(), loan.getBranchName(), loan.getBranchDescription(),
							loan.getCustomerType(), loan.getCommunityCULoanID(),
							loan.getGuarantorSourceId(), loan.getSourceOfFundsID(),
							loan.getRefinanceReasonId(), loan.getDistrictCodeId(),
							loan.getIntPayPeriodNum(), loan.getLoanCalculationMode(), loan.getApr(),
							loan.getEffectiveIntRate(), CommonConstants.NEW_APPLICATION),
							loan.getToken());
					// prepare to send mail notification if not null
					if (loanDTO != null) {
						logger.debug(
								"Loan with id_loan = [{}] / id_loan_Extern = [{}] was successfully added in ACM DB",
								loanDTO.getLoanId(), loanDTO.getIdLoanExtern());
						addedLoans.add(loanDTO);
					}
				}
				catch (FeignException e) {
					logger.error("Failed to save the loan in DB");
					logger.error(CommonLoggerMessage.ERROR_WHILE_CONNECTING_TO_REMOTE_SERVICE,
							e.getMessage());
				}
			}
		}
		// notification par mail
		sendMail(addedLoans, loans.get(0).getToken());
		logger.debug("saving new loans in ACM-DB :: DONE");
	}

	/**
	 * Send mail.
	 *
	 * @author HaythemBenizid
	 * @param loansDtos the loans dtos
	 * @param token the token
	 */
	private void sendMail(List<LoanDTO> loansDtos, String token) {

		if (!ACMValidationUtils.isNullOrEmpty(loansDtos)) {
			try {
				String accountsNumbers = loansDtos.get(0).getAccountNumber();
				for (int i = 1; i < loansDtos.size(); i++) {
					accountsNumbers = accountsNumbers + " / " + loansDtos.get(i).getAccountNumber();
				}
				mailSenderClient.sendMail(
						new MailDTO(CommonConstants.NO_REPLAY_EMAIL, defaultACMReceiverMail,
								"Processing [ " + loansDtos.size()
										+ " ] LOAN from ABACUS-DB successfully",
								"Account Number : [ " + accountsNumbers + " ]."),
						token);
			}
			catch (FeignException e) {
				logger.error("Failed to send Mail");
				logger.error(CommonLoggerMessage.ERROR_WHILE_CONNECTING_TO_REMOTE_SERVICE,
						e.getMessage());
			}
			logger.debug("Sending Email Notification :: DONE");
		}
	}
}
