/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.utils.dtos;

import java.math.BigDecimal;
import java.util.Date;
import java.util.List;

import com.acm.constants.common.ACMConstantWorkflowStatuts;
import com.acm.constants.common.CommonFunctions;

/**
 * {@link CustomerAccountDTO} class.
 *
 * @author HaythemBenizid
 * @since 0.4.0
 */
public class CustomerAccountDTO extends GenericDTO implements java.io.Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = -7637384454449523010L;

	/** The customer id. */
	private Long customerId;

	/** The loan id. */
	private Long loanId;

	/** The cu account id. */
	private Long cuAccountId;
	/** The account. */
	private String account;

	/** The issue date. */
	private Date issueDate;

	/** The issue amount. */
	private BigDecimal issueAmount;

	/** The portfolio id. */
	private String portfolioId;

	/** The statut id. */
	private Integer statutId;

	/** The product id abacus. */
	private Long productIdAbacus;
	/**
	 * The statut : Applied=1, Approved=2, Issued=4, Charged off=8, Bad debt = 16, Transferred=32,
	 * Cancelled=64.
	 */
	private String statut;

	/** The account rating. */
	private String accountRating;

	/** The balance. */
	private BigDecimal balance;

	/** The schedule DT os. */
	private List<ScheduleDTO> scheduleDTOs;

	/** The can topup. */
	private Boolean canTopup;

	/** The topup produit. */
	private Boolean topupProduct;

	/** The refinance produit. */
	private Boolean refinanceProduct;

	/** The currency code. */
	private String currencyCode;

	/**
	 * Instantiates a new customer DTO.
	 */
	public CustomerAccountDTO() {

		/*
		 * EMPTY
		 */
	}

	/**
	 * Gets the customer id.
	 *
	 * @return the customerId
	 */
	public Long getCustomerId() {

		return customerId;
	}

	/**
	 * Sets the customer id.
	 *
	 * @param customerId the customerId to set
	 */
	public void setCustomerId(Long customerId) {

		this.customerId = customerId;
	}

	/**
	 * Gets the loan id.
	 *
	 * @return the loanId
	 */
	public Long getLoanId() {

		return loanId;
	}

	/**
	 * Sets the loan id.
	 *
	 * @param loanId the loanId to set
	 */
	public void setLoanId(Long loanId) {

		this.loanId = loanId;
	}

	/**
	 * Gets the account.
	 *
	 * @return the account
	 */
	public String getAccount() {

		return account;
	}

	/**
	 * Sets the account.
	 *
	 * @param account the account to set
	 */
	public void setAccount(String account) {

		this.account = account;
	}

	/**
	 * Gets the issue date.
	 *
	 * @return the issueDate
	 */
	public Date getIssueDate() {

		return issueDate;
	}

	/**
	 * Sets the issue date.
	 *
	 * @param issueDate the issueDate to set
	 */
	public void setIssueDate(Date issueDate) {

		this.issueDate = issueDate;
	}

	/**
	 * Gets the issue amount.
	 *
	 * @return the issueAmount
	 */
	public BigDecimal getIssueAmount() {

		return issueAmount;
	}

	/**
	 * Sets the issue amount.
	 *
	 * @param issueAmount the issueAmount to set
	 */
	public void setIssueAmount(BigDecimal issueAmount) {

		this.issueAmount = issueAmount;
	}

	/**
	 * Gets the portfolio id.
	 *
	 * @return the portfolioId
	 */
	public String getPortfolioId() {

		return portfolioId;
	}

	/**
	 * Sets the portfolio id.
	 *
	 * @param portfolioId the portfolioId to set
	 */
	public void setPortfolioId(String portfolioId) {

		this.portfolioId = portfolioId;
	}

	/**
	 * Gets the account rating.
	 *
	 * @return the accountRating
	 */
	public String getAccountRating() {

		return accountRating;
	}

	/**
	 * Sets the account rating.
	 *
	 * @param accountRating the accountRating to set
	 */
	public void setAccountRating(String accountRating) {

		this.accountRating = accountRating;
	}

	/**
	 * Gets the balance.
	 *
	 * @return the balance
	 */
	public BigDecimal getBalance() {

		return balance;
	}

	/**
	 * Sets the balance.
	 *
	 * @param balance the balance to set
	 */
	public void setBalance(BigDecimal balance) {

		this.balance = balance;
	}

	/**
	 * Gets the statut id.
	 *
	 * @return the statutId
	 */
	public Integer getStatutId() {

		return statutId;
	}

	/**
	 * Sets the statut id.
	 *
	 * @param statutId the statutId to set
	 */
	public void setStatutId(Integer statutId) {

		this.statutId = statutId;
	}

	/**
	 * Gets the statut. Applied=1, Approved=2, Issued=4, Charged off=8, Bad debt = 16,
	 * Transferred=32, Cancelled=64
	 *
	 * @return the statut
	 */
	public String getStatut() {

		if (statutId != null) {
			switch (this.statutId) {
				case 1:
					this.statut = CommonFunctions
							.mappingStatus(ACMConstantWorkflowStatuts.LOAN_ABACUS_STATUS_APPLIED)
							.getValue();
					break;
				case 2:
					this.statut = CommonFunctions
							.mappingStatus(ACMConstantWorkflowStatuts.LOAN_ABACUS_STATUS_APPROVED)
							.getValue();
					break;
				case 4:
					this.statut = CommonFunctions
							.mappingStatus(ACMConstantWorkflowStatuts.LOAN_ABACUS_STATUS_ISSUED)
							.getValue();
					break;
				case 8:
					this.statut = CommonFunctions
							.mappingStatus(
									ACMConstantWorkflowStatuts.LOAN_ABACUS_STATUS_CHARGED_OFF)
							.getValue();
					break;
				case 16:
					this.statut = CommonFunctions
							.mappingStatus(ACMConstantWorkflowStatuts.LOAN_ABACUS_STATUS_BAD_DEBT)
							.getValue();
					break;
				case 32:
					this.statut = CommonFunctions
							.mappingStatus(
									ACMConstantWorkflowStatuts.LOAN_ABACUS_STATUS_TRANSFERRED)
							.getValue();
					break;
				case 64:
					this.statut = CommonFunctions
							.mappingStatus(ACMConstantWorkflowStatuts.LOAN_ABACUS_STATUS_CANCELLED)
							.getValue();
					break;
				default:
					break;
			}
		}
		return statut;
	}

	/**
	 * Sets the statut.
	 *
	 * @param statut the statut to set
	 */
	public void setStatut(String statut) {

		this.statut = statut;
	}

	/**
	 * Gets the schedule DT os.
	 *
	 * @return the scheduleDTOs
	 */
	public List<ScheduleDTO> getScheduleDTOs() {

		return scheduleDTOs;
	}

	/**
	 * Sets the schedule DT os.
	 *
	 * @param scheduleDTOs the scheduleDTOs to set
	 */
	public void setScheduleDTOs(List<ScheduleDTO> scheduleDTOs) {

		this.scheduleDTOs = scheduleDTOs;
	}

	/**
	 * Gets the can topup.
	 *
	 * @return the can topup
	 */
	public Boolean getCanTopup() {

		return canTopup;
	}

	/**
	 * Sets the can topup.
	 *
	 * @param canTopup the new can topup
	 */
	public void setCanTopup(Boolean canTopup) {

		this.canTopup = canTopup;
	}

	/**
	 * Gets the product id abacus.
	 *
	 * @return the product id abacus
	 */
	public Long getProductIdAbacus() {

		return productIdAbacus;
	}

	/**
	 * Sets the product id abacus.
	 *
	 * @param productIdAbacus the new product id abacus
	 */
	public void setProductIdAbacus(Long productIdAbacus) {

		this.productIdAbacus = productIdAbacus;
	}

	/**
	 * Gets the topup product.
	 *
	 * @return the topup product
	 */
	public Boolean getTopupProduct() {

		return topupProduct;
	}

	/**
	 * Sets the topup product.
	 *
	 * @param topupProduct the new topup product
	 */
	public void setTopupProduct(Boolean topupProduct) {

		this.topupProduct = topupProduct;
	}

	/**
	 * Gets the refinance product.
	 *
	 * @return the refinance product
	 */
	public Boolean getRefinanceProduct() {

		return refinanceProduct;
	}

	/**
	 * Sets the refinance product.
	 *
	 * @param refinanceProduct the new refinance product
	 */
	public void setRefinanceProduct(Boolean refinanceProduct) {

		this.refinanceProduct = refinanceProduct;
	}

	/**
	 * Gets the cu account id.
	 *
	 * @return the cu account id
	 */
	public Long getCuAccountId() {

		return cuAccountId;
	}

	/**
	 * Sets the cu account id.
	 *
	 * @param cuAccountId the new cu account id
	 */
	public void setCuAccountId(Long cuAccountId) {

		this.cuAccountId = cuAccountId;
	}

	/**
	 * Gets the currency code.
	 *
	 * @return the currency code
	 */
	public String getCurrencyCode() {

		return currencyCode;
	}

	/**
	 * Sets the currency code.
	 *
	 * @param currencyCode the new currency code
	 */
	public void setCurrencyCode(String currencyCode) {

		this.currencyCode = currencyCode;
	}

}
