/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.api_abacus.model;

import java.io.Serializable;
import java.util.Date;

/**
 * {@link LoanAbacusAPIModelCuLoanSchedule} class.
 *
 * @author HaythemBenizid
 * @since 1.0.5
 */
public class LoanAbacusAPIModelCuLoanSchedule implements Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = 4515358274048167678L;

	/** The schedule ID. */
	private int scheduleID;

	/** The cu loan part ID. */
	private int cuLoanPartID;

	/** The period. */
	private int period;

	/** The repayment date. */
	private Date repaymentDate;

	/** The date changed. */
	private boolean dateChanged;

	/** The total repayment. */
	private double totalRepayment;

	/** The fee repayment. */
	private double feeRepayment;

	/** The fee amt paid. */
	private double feeAmtPaid;

	/** The fee paid. */
	private boolean feePaid;

	/** The uds fee repayment. */
	private double udsFeeRepayment;

	/** The uds fee paid. */
	private boolean udsFeePaid;

	/** The interest repayment. */
	private double interestRepayment;

	/** The interest amt paid. */
	private double interestAmtPaid;

	/** The comp int repayment. */
	private double compIntRepayment;

	/** The comp int amt paid. */
	private double compIntAmtPaid;

	/** The interest charged. */
	private boolean interestCharged;

	/** The loan repayment. */
	private double loanRepayment;

	/** The loan repayment paid. */
	private double loanRepaymentPaid;

	/** The balance. */
	private double balance;

	/** The user defined. */
	private boolean userDefined;

	/** The int only. */
	private boolean intOnly;

	/** The int pay. */
	private boolean intPay;

	/** The customer ID. */
	private int customerID;

	/** The savings payment. */
	private double savingsPayment;

	/** The savings payment paid. */
	private double savingsPaymentPaid;

	/** The all paid. */
	private boolean allPaid;

	/** The active. */
	private boolean active;

	/** The fee 2 repayment. */
	private double fee2Repayment;

	/** The fee 2 amt paid. */
	private double fee2AmtPaid;

	/** The fee 2 paid. */
	private boolean fee2Paid;

	/** The deferred interest. */
	private double deferredInterest;

	/** The recognition processed. */
	private boolean recognitionProcessed;

	/** The suspeneded interest. */
	private boolean suspenededInterest;

	/**
	 * Instantiates a new loan abacus API model cu loan schedule.
	 */
	public LoanAbacusAPIModelCuLoanSchedule() {

		// Empty
	}

	/**
	 * Gets the schedule ID.
	 *
	 * @return the scheduleID
	 */
	public int getScheduleID() {

		return scheduleID;
	}

	/**
	 * Sets the schedule ID.
	 *
	 * @param scheduleID the scheduleID to set
	 */
	public void setScheduleID(int scheduleID) {

		this.scheduleID = scheduleID;
	}

	/**
	 * Gets the cu loan part ID.
	 *
	 * @return the cuLoanPartID
	 */
	public int getCuLoanPartID() {

		return cuLoanPartID;
	}

	/**
	 * Sets the cu loan part ID.
	 *
	 * @param cuLoanPartID the cuLoanPartID to set
	 */
	public void setCuLoanPartID(int cuLoanPartID) {

		this.cuLoanPartID = cuLoanPartID;
	}

	/**
	 * Gets the period.
	 *
	 * @return the period
	 */
	public int getPeriod() {

		return period;
	}

	/**
	 * Sets the period.
	 *
	 * @param period the period to set
	 */
	public void setPeriod(int period) {

		this.period = period;
	}

	/**
	 * Gets the repayment date.
	 *
	 * @return the repaymentDate
	 */
	public Date getRepaymentDate() {

		return repaymentDate;
	}

	/**
	 * Sets the repayment date.
	 *
	 * @param repaymentDate the repaymentDate to set
	 */
	public void setRepaymentDate(Date repaymentDate) {

		this.repaymentDate = repaymentDate;
	}

	/**
	 * Checks if is date changed.
	 *
	 * @return the dateChanged
	 */
	public boolean isDateChanged() {

		return dateChanged;
	}

	/**
	 * Sets the date changed.
	 *
	 * @param dateChanged the dateChanged to set
	 */
	public void setDateChanged(boolean dateChanged) {

		this.dateChanged = dateChanged;
	}

	/**
	 * Gets the total repayment.
	 *
	 * @return the totalRepayment
	 */
	public double getTotalRepayment() {

		return totalRepayment;
	}

	/**
	 * Sets the total repayment.
	 *
	 * @param totalRepayment the totalRepayment to set
	 */
	public void setTotalRepayment(double totalRepayment) {

		this.totalRepayment = totalRepayment;
	}

	/**
	 * Gets the fee repayment.
	 *
	 * @return the feeRepayment
	 */
	public double getFeeRepayment() {

		return feeRepayment;
	}

	/**
	 * Sets the fee repayment.
	 *
	 * @param feeRepayment the feeRepayment to set
	 */
	public void setFeeRepayment(double feeRepayment) {

		this.feeRepayment = feeRepayment;
	}

	/**
	 * Gets the fee amt paid.
	 *
	 * @return the feeAmtPaid
	 */
	public double getFeeAmtPaid() {

		return feeAmtPaid;
	}

	/**
	 * Sets the fee amt paid.
	 *
	 * @param feeAmtPaid the feeAmtPaid to set
	 */
	public void setFeeAmtPaid(double feeAmtPaid) {

		this.feeAmtPaid = feeAmtPaid;
	}

	/**
	 * Checks if is fee paid.
	 *
	 * @return the feePaid
	 */
	public boolean isFeePaid() {

		return feePaid;
	}

	/**
	 * Sets the fee paid.
	 *
	 * @param feePaid the feePaid to set
	 */
	public void setFeePaid(boolean feePaid) {

		this.feePaid = feePaid;
	}

	/**
	 * Gets the uds fee repayment.
	 *
	 * @return the udsFeeRepayment
	 */
	public double getUdsFeeRepayment() {

		return udsFeeRepayment;
	}

	/**
	 * Sets the uds fee repayment.
	 *
	 * @param udsFeeRepayment the udsFeeRepayment to set
	 */
	public void setUdsFeeRepayment(double udsFeeRepayment) {

		this.udsFeeRepayment = udsFeeRepayment;
	}

	/**
	 * Checks if is uds fee paid.
	 *
	 * @return the udsFeePaid
	 */
	public boolean isUdsFeePaid() {

		return udsFeePaid;
	}

	/**
	 * Sets the uds fee paid.
	 *
	 * @param udsFeePaid the udsFeePaid to set
	 */
	public void setUdsFeePaid(boolean udsFeePaid) {

		this.udsFeePaid = udsFeePaid;
	}

	/**
	 * Gets the interest repayment.
	 *
	 * @return the interestRepayment
	 */
	public double getInterestRepayment() {

		return interestRepayment;
	}

	/**
	 * Sets the interest repayment.
	 *
	 * @param interestRepayment the interestRepayment to set
	 */
	public void setInterestRepayment(double interestRepayment) {

		this.interestRepayment = interestRepayment;
	}

	/**
	 * Gets the interest amt paid.
	 *
	 * @return the interestAmtPaid
	 */
	public double getInterestAmtPaid() {

		return interestAmtPaid;
	}

	/**
	 * Sets the interest amt paid.
	 *
	 * @param interestAmtPaid the interestAmtPaid to set
	 */
	public void setInterestAmtPaid(double interestAmtPaid) {

		this.interestAmtPaid = interestAmtPaid;
	}

	/**
	 * Gets the comp int repayment.
	 *
	 * @return the compIntRepayment
	 */
	public double getCompIntRepayment() {

		return compIntRepayment;
	}

	/**
	 * Sets the comp int repayment.
	 *
	 * @param compIntRepayment the compIntRepayment to set
	 */
	public void setCompIntRepayment(double compIntRepayment) {

		this.compIntRepayment = compIntRepayment;
	}

	/**
	 * Gets the comp int amt paid.
	 *
	 * @return the compIntAmtPaid
	 */
	public double getCompIntAmtPaid() {

		return compIntAmtPaid;
	}

	/**
	 * Sets the comp int amt paid.
	 *
	 * @param compIntAmtPaid the compIntAmtPaid to set
	 */
	public void setCompIntAmtPaid(double compIntAmtPaid) {

		this.compIntAmtPaid = compIntAmtPaid;
	}

	/**
	 * Checks if is interest charged.
	 *
	 * @return the interestCharged
	 */
	public boolean isInterestCharged() {

		return interestCharged;
	}

	/**
	 * Sets the interest charged.
	 *
	 * @param interestCharged the interestCharged to set
	 */
	public void setInterestCharged(boolean interestCharged) {

		this.interestCharged = interestCharged;
	}

	/**
	 * Gets the loan repayment.
	 *
	 * @return the loanRepayment
	 */
	public double getLoanRepayment() {

		return loanRepayment;
	}

	/**
	 * Sets the loan repayment.
	 *
	 * @param loanRepayment the loanRepayment to set
	 */
	public void setLoanRepayment(double loanRepayment) {

		this.loanRepayment = loanRepayment;
	}

	/**
	 * Gets the loan repayment paid.
	 *
	 * @return the loanRepaymentPaid
	 */
	public double getLoanRepaymentPaid() {

		return loanRepaymentPaid;
	}

	/**
	 * Sets the loan repayment paid.
	 *
	 * @param loanRepaymentPaid the loanRepaymentPaid to set
	 */
	public void setLoanRepaymentPaid(double loanRepaymentPaid) {

		this.loanRepaymentPaid = loanRepaymentPaid;
	}

	/**
	 * Gets the balance.
	 *
	 * @return the balance
	 */
	public double getBalance() {

		return balance;
	}

	/**
	 * Sets the balance.
	 *
	 * @param balance the balance to set
	 */
	public void setBalance(double balance) {

		this.balance = balance;
	}

	/**
	 * Checks if is user defined.
	 *
	 * @return the userDefined
	 */
	public boolean isUserDefined() {

		return userDefined;
	}

	/**
	 * Sets the user defined.
	 *
	 * @param userDefined the userDefined to set
	 */
	public void setUserDefined(boolean userDefined) {

		this.userDefined = userDefined;
	}

	/**
	 * Checks if is int only.
	 *
	 * @return the intOnly
	 */
	public boolean isIntOnly() {

		return intOnly;
	}

	/**
	 * Sets the int only.
	 *
	 * @param intOnly the intOnly to set
	 */
	public void setIntOnly(boolean intOnly) {

		this.intOnly = intOnly;
	}

	/**
	 * Checks if is int pay.
	 *
	 * @return the intPay
	 */
	public boolean isIntPay() {

		return intPay;
	}

	/**
	 * Sets the int pay.
	 *
	 * @param intPay the intPay to set
	 */
	public void setIntPay(boolean intPay) {

		this.intPay = intPay;
	}

	/**
	 * Gets the customer ID.
	 *
	 * @return the customerID
	 */
	public int getCustomerID() {

		return customerID;
	}

	/**
	 * Sets the customer ID.
	 *
	 * @param customerID the customerID to set
	 */
	public void setCustomerID(int customerID) {

		this.customerID = customerID;
	}

	/**
	 * Gets the savings payment.
	 *
	 * @return the savingsPayment
	 */
	public double getSavingsPayment() {

		return savingsPayment;
	}

	/**
	 * Sets the savings payment.
	 *
	 * @param savingsPayment the savingsPayment to set
	 */
	public void setSavingsPayment(double savingsPayment) {

		this.savingsPayment = savingsPayment;
	}

	/**
	 * Gets the savings payment paid.
	 *
	 * @return the savingsPaymentPaid
	 */
	public double getSavingsPaymentPaid() {

		return savingsPaymentPaid;
	}

	/**
	 * Sets the savings payment paid.
	 *
	 * @param savingsPaymentPaid the savingsPaymentPaid to set
	 */
	public void setSavingsPaymentPaid(double savingsPaymentPaid) {

		this.savingsPaymentPaid = savingsPaymentPaid;
	}

	/**
	 * Checks if is all paid.
	 *
	 * @return the allPaid
	 */
	public boolean isAllPaid() {

		return allPaid;
	}

	/**
	 * Sets the all paid.
	 *
	 * @param allPaid the allPaid to set
	 */
	public void setAllPaid(boolean allPaid) {

		this.allPaid = allPaid;
	}

	/**
	 * Checks if is active.
	 *
	 * @return the active
	 */
	public boolean isActive() {

		return active;
	}

	/**
	 * Sets the active.
	 *
	 * @param active the active to set
	 */
	public void setActive(boolean active) {

		this.active = active;
	}

	/**
	 * Gets the fee 2 repayment.
	 *
	 * @return the fee2Repayment
	 */
	public double getFee2Repayment() {

		return fee2Repayment;
	}

	/**
	 * Sets the fee 2 repayment.
	 *
	 * @param fee2Repayment the fee2Repayment to set
	 */
	public void setFee2Repayment(double fee2Repayment) {

		this.fee2Repayment = fee2Repayment;
	}

	/**
	 * Gets the fee 2 amt paid.
	 *
	 * @return the fee2AmtPaid
	 */
	public double getFee2AmtPaid() {

		return fee2AmtPaid;
	}

	/**
	 * Sets the fee 2 amt paid.
	 *
	 * @param fee2AmtPaid the fee2AmtPaid to set
	 */
	public void setFee2AmtPaid(double fee2AmtPaid) {

		this.fee2AmtPaid = fee2AmtPaid;
	}

	/**
	 * Checks if is fee 2 paid.
	 *
	 * @return the fee2Paid
	 */
	public boolean isFee2Paid() {

		return fee2Paid;
	}

	/**
	 * Sets the fee 2 paid.
	 *
	 * @param fee2Paid the fee2Paid to set
	 */
	public void setFee2Paid(boolean fee2Paid) {

		this.fee2Paid = fee2Paid;
	}

	/**
	 * Gets the deferred interest.
	 *
	 * @return the deferredInterest
	 */
	public double getDeferredInterest() {

		return deferredInterest;
	}

	/**
	 * Sets the deferred interest.
	 *
	 * @param deferredInterest the deferredInterest to set
	 */
	public void setDeferredInterest(double deferredInterest) {

		this.deferredInterest = deferredInterest;
	}

	/**
	 * Checks if is recognition processed.
	 *
	 * @return the recognitionProcessed
	 */
	public boolean isRecognitionProcessed() {

		return recognitionProcessed;
	}

	/**
	 * Sets the recognition processed.
	 *
	 * @param recognitionProcessed the recognitionProcessed to set
	 */
	public void setRecognitionProcessed(boolean recognitionProcessed) {

		this.recognitionProcessed = recognitionProcessed;
	}

	/**
	 * Checks if is suspeneded interest.
	 *
	 * @return the suspenededInterest
	 */
	public boolean isSuspenededInterest() {

		return suspenededInterest;
	}

	/**
	 * Sets the suspeneded interest.
	 *
	 * @param suspenededInterest the suspenededInterest to set
	 */
	public void setSuspenededInterest(boolean suspenededInterest) {

		this.suspenededInterest = suspenededInterest;
	}

	/**
	 * To string.
	 *
	 * @return the string
	 */
	/*
	 * (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {

		return "LoanAbacusAPIModelCuLoanSchedule [scheduleID=" + scheduleID + ", cuLoanPartID="
				+ cuLoanPartID + ", period=" + period + ", repaymentDate=" + repaymentDate
				+ ", dateChanged=" + dateChanged + ", totalRepayment=" + totalRepayment
				+ ", feeRepayment=" + feeRepayment + ", feeAmtPaid=" + feeAmtPaid + ", feePaid="
				+ feePaid + ", udsFeeRepayment=" + udsFeeRepayment + ", udsFeePaid=" + udsFeePaid
				+ ", interestRepayment=" + interestRepayment + ", interestAmtPaid="
				+ interestAmtPaid + ", compIntRepayment=" + compIntRepayment + ", compIntAmtPaid="
				+ compIntAmtPaid + ", interestCharged=" + interestCharged + ", loanRepayment="
				+ loanRepayment + ", loanRepaymentPaid=" + loanRepaymentPaid + ", balance="
				+ balance + ", userDefined=" + userDefined + ", intOnly=" + intOnly + ", intPay="
				+ intPay + ", customerID=" + customerID + ", savingsPayment=" + savingsPayment
				+ ", savingsPaymentPaid=" + savingsPaymentPaid + ", allPaid=" + allPaid
				+ ", active=" + active + ", fee2Repayment=" + fee2Repayment + ", fee2AmtPaid="
				+ fee2AmtPaid + ", fee2Paid=" + fee2Paid + ", deferredInterest=" + deferredInterest
				+ ", recognitionProcessed=" + recognitionProcessed + ", suspenededInterest="
				+ suspenededInterest + "]";
	}

}
