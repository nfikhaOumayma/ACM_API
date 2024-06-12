/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.utils.dtos;

import java.io.Serializable;

/**
 * The Class LoanDetailsInfoResponseDTO.
 */
public class LoanDetailsInfoResponseDTO extends GenericDTO implements Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = 7853970386493001173L;

	/** The loan id. */
	private Long loanId;

	/** The remaining amount. */
	private Double remainingAmount;

	/** The first repayment date. */
	private String firstRepaymentDate;

	/** The irr. */
	private Double irr;

	/** The apr. */
	private Double apr;

	/** The total fees. */
	private Double totalFees;

	/** The balance. */
	private Double balance;

	/** The status. */
	private String status;

	/** The principal. */
	private Double principal;

	/** The principal paid. */
	private Double principalPaid;

	/** The profit. */
	private Double profit;

	/** The profit paid. */
	private Double profitPaid;

	/** The total paid. */
	private Double totalPaid;

	/** The loan reason. */
	private String loanReason;

	/**
	 * Gets the loan id.
	 *
	 * @return the loan id
	 */
	public Long getLoanId() {

		return loanId;
	}

	/**
	 * Sets the loan id.
	 *
	 * @param loanId the new loan id
	 */
	public void setLoanId(Long loanId) {

		this.loanId = loanId;
	}

	/**
	 * Gets the remaining amount.
	 *
	 * @return the remaining amount
	 */
	public Double getRemainingAmount() {

		return remainingAmount;
	}

	/**
	 * Sets the remaining amount.
	 *
	 * @param remainingAmount the new remaining amount
	 */
	public void setRemainingAmount(Double remainingAmount) {

		this.remainingAmount = remainingAmount;
	}

	/**
	 * Gets the first repayment date.
	 *
	 * @return the first repayment date
	 */
	public String getFirstRepaymentDate() {

		return firstRepaymentDate;
	}

	/**
	 * Sets the first repayment date.
	 *
	 * @param firstRepaymentDate the new first repayment date
	 */
	public void setFirstRepaymentDate(String firstRepaymentDate) {

		this.firstRepaymentDate = firstRepaymentDate;
	}

	/**
	 * Gets the irr.
	 *
	 * @return the irr
	 */
	public Double getIrr() {

		return irr;
	}

	/**
	 * Sets the irr.
	 *
	 * @param irr the new irr
	 */
	public void setIrr(Double irr) {

		this.irr = irr;
	}

	/**
	 * Gets the apr.
	 *
	 * @return the apr
	 */
	public Double getApr() {

		return apr;
	}

	/**
	 * Sets the apr.
	 *
	 * @param apr the new apr
	 */
	public void setApr(Double apr) {

		this.apr = apr;
	}

	/**
	 * Gets the total fees.
	 *
	 * @return the total fees
	 */
	public Double getTotalFees() {

		return totalFees;
	}

	/**
	 * Sets the total fees.
	 *
	 * @param totalFees the new total fees
	 */
	public void setTotalFees(Double totalFees) {

		this.totalFees = totalFees;
	}

	/**
	 * Gets the balance.
	 *
	 * @return the balance
	 */
	public Double getBalance() {

		return balance;
	}

	/**
	 * Sets the balance.
	 *
	 * @param balance the new balance
	 */
	public void setBalance(Double balance) {

		this.balance = balance;
	}

	/**
	 * Gets the status.
	 *
	 * @return the status
	 */
	public String getStatus() {

		return status;
	}

	/**
	 * Sets the status.
	 *
	 * @param status the new status
	 */
	public void setStatus(String status) {

		this.status = status;
	}

	/**
	 * Gets the principal.
	 *
	 * @return the principal
	 */
	public Double getPrincipal() {

		return principal;
	}

	/**
	 * Sets the principal.
	 *
	 * @param principal the new principal
	 */
	public void setPrincipal(Double principal) {

		this.principal = principal;
	}

	/**
	 * Gets the principal paid.
	 *
	 * @return the principal paid
	 */
	public Double getPrincipalPaid() {

		return principalPaid;
	}

	/**
	 * Sets the principal paid.
	 *
	 * @param principalPaid the new principal paid
	 */
	public void setPrincipalPaid(Double principalPaid) {

		this.principalPaid = principalPaid;
	}

	/**
	 * Gets the profit.
	 *
	 * @return the profit
	 */
	public Double getProfit() {

		return profit;
	}

	/**
	 * Sets the profit.
	 *
	 * @param profit the new profit
	 */
	public void setProfit(Double profit) {

		this.profit = profit;
	}

	/**
	 * Gets the profit paid.
	 *
	 * @return the profit paid
	 */
	public Double getProfitPaid() {

		return profitPaid;
	}

	/**
	 * Sets the profit paid.
	 *
	 * @param profitPaid the new profit paid
	 */
	public void setProfitPaid(Double profitPaid) {

		this.profitPaid = profitPaid;
	}

	/**
	 * Gets the total paid.
	 *
	 * @return the total paid
	 */
	public Double getTotalPaid() {

		return totalPaid;
	}

	/**
	 * Sets the total paid.
	 *
	 * @param totalPaid the new total paid
	 */
	public void setTotalPaid(Double totalPaid) {

		this.totalPaid = totalPaid;
	}

	/**
	 * Gets the loan reason.
	 *
	 * @return the loan reason
	 */
	public String getLoanReason() {
		return loanReason;
	}

	/**
	 * Sets the loan reason.
	 *
	 * @param loanReason the new loan reason
	 */
	public void setLoanReason(String loanReason) {
		this.loanReason = loanReason;
	}

}
