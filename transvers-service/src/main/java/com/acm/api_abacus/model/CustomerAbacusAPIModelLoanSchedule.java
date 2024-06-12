/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.api_abacus.model;

import java.io.Serializable;
import java.math.BigDecimal;

/**
 * {@link CustomerAbacusAPIModelLoanSchedule} class.
 *
 * @author MoezMhiri
 * @since 1.11.5
 */
public class CustomerAbacusAPIModelLoanSchedule implements Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = -5699025294830175957L;

	/** The cu account ID. */
	public int cuAccountID;

	/** The schedule ID. */
	public Object scheduleID;

	/** The cu loan part ID. */
	public Object cuLoanPartID;

	/** The period. */
	public int period;

	/** The repayment date. */
	public String repaymentDate;

	/** The total repayment. */
	public BigDecimal totalRepayment;

	/** The total repayment paid. */
	public String totalRepaymentPaid;

	/** The fee repayment. */
	public double feeRepayment;

	/** The fee amt paid. */
	public Object feeAmtPaid;

	/** The uds fee repayment. */
	public Object udsFeeRepayment;

	/** The uds fee paid. */
	public Object udsFeePaid;

	/** The interest repayment. */
	public BigDecimal interestRepayment;

	/** The interest amt paid. */
	public String interestAmtPaid;

	/** The comp int repayment. */
	public Object compIntRepayment;

	/** The comp int amt paid. */
	public Object compIntAmtPaid;

	/** The loan repayment. */
	public BigDecimal loanRepayment;

	/** The loan repayment paid. */
	public String loanRepaymentPaid;

	/** The penalty due. */
	public String penaltyDue;

	/** The penalty due paid. */
	public String penaltyDuePaid;

	/** The outstanding fee due. */
	public String outstandingFeeDue;

	/** The outstanding fee due paid. */
	public String outstandingFeeDuePaid;

	/** The written off amount. */
	public String writtenOffAmount;

	/** The balance. */
	public BigDecimal balance;

	/** The savings payment. */
	public Object savingsPayment;

	/** The savings payment paid. */
	public Object savingsPaymentPaid;

	/** The all paid. */
	public Object allPaid;

	/** The status. */
	public String status;

	/** The repaid date. */
	public String repaidDate;

	/** The repayment amount. */
	public Object repaymentAmount;

	/** The late days. */
	public int lateDays;

	/** The total repayment string. */
	public String totalRepaymentString;

	/** The fee repayment string. */
	public String feeRepaymentString;

	/** The interest repayment string. */
	public String interestRepaymentString;

	/** The loan repayment string. */
	public String loanRepaymentString;

	/** The balance string. */
	public String balanceString;

	/** The date changed. */
	public boolean dateChanged;

	/**
	 * Gets the cu account ID.
	 *
	 * @return the cu account ID
	 */
	public int getCuAccountID() {

		return cuAccountID;
	}

	/**
	 * Sets the cu account ID.
	 *
	 * @param cuAccountID the new cu account ID
	 */
	public void setCuAccountID(int cuAccountID) {

		this.cuAccountID = cuAccountID;
	}

	/**
	 * Gets the schedule ID.
	 *
	 * @return the schedule ID
	 */
	public Object getScheduleID() {

		return scheduleID;
	}

	/**
	 * Sets the schedule ID.
	 *
	 * @param scheduleID the new schedule ID
	 */
	public void setScheduleID(Object scheduleID) {

		this.scheduleID = scheduleID;
	}

	/**
	 * Gets the cu loan part ID.
	 *
	 * @return the cu loan part ID
	 */
	public Object getCuLoanPartID() {

		return cuLoanPartID;
	}

	/**
	 * Sets the cu loan part ID.
	 *
	 * @param cuLoanPartID the new cu loan part ID
	 */
	public void setCuLoanPartID(Object cuLoanPartID) {

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
	 * @param period the new period
	 */
	public void setPeriod(int period) {

		this.period = period;
	}

	/**
	 * Gets the repayment date.
	 *
	 * @return the repayment date
	 */
	public String getRepaymentDate() {

		return repaymentDate;
	}

	/**
	 * Sets the repayment date.
	 *
	 * @param repaymentDate the new repayment date
	 */
	public void setRepaymentDate(String repaymentDate) {

		this.repaymentDate = repaymentDate;
	}

	/**
	 * Gets the total repayment.
	 *
	 * @return the total repayment
	 */
	public BigDecimal getTotalRepayment() {

		return totalRepayment;
	}

	/**
	 * Sets the total repayment.
	 *
	 * @param totalRepayment the new total repayment
	 */
	public void setTotalRepayment(BigDecimal totalRepayment) {

		this.totalRepayment = totalRepayment;
	}

	/**
	 * Gets the total repayment paid.
	 *
	 * @return the total repayment paid
	 */
	public String getTotalRepaymentPaid() {

		return totalRepaymentPaid;
	}

	/**
	 * Sets the total repayment paid.
	 *
	 * @param totalRepaymentPaid the new total repayment paid
	 */
	public void setTotalRepaymentPaid(String totalRepaymentPaid) {

		this.totalRepaymentPaid = totalRepaymentPaid;
	}

	/**
	 * Gets the fee repayment.
	 *
	 * @return the fee repayment
	 */
	public double getFeeRepayment() {

		return feeRepayment;
	}

	/**
	 * Sets the fee repayment.
	 *
	 * @param feeRepayment the new fee repayment
	 */
	public void setFeeRepayment(double feeRepayment) {

		this.feeRepayment = feeRepayment;
	}

	/**
	 * Gets the fee amt paid.
	 *
	 * @return the fee amt paid
	 */
	public Object getFeeAmtPaid() {

		return feeAmtPaid;
	}

	/**
	 * Sets the fee amt paid.
	 *
	 * @param feeAmtPaid the new fee amt paid
	 */
	public void setFeeAmtPaid(Object feeAmtPaid) {

		this.feeAmtPaid = feeAmtPaid;
	}

	/**
	 * Gets the uds fee repayment.
	 *
	 * @return the uds fee repayment
	 */
	public Object getUdsFeeRepayment() {

		return udsFeeRepayment;
	}

	/**
	 * Sets the uds fee repayment.
	 *
	 * @param udsFeeRepayment the new uds fee repayment
	 */
	public void setUdsFeeRepayment(Object udsFeeRepayment) {

		this.udsFeeRepayment = udsFeeRepayment;
	}

	/**
	 * Gets the uds fee paid.
	 *
	 * @return the uds fee paid
	 */
	public Object getUdsFeePaid() {

		return udsFeePaid;
	}

	/**
	 * Sets the uds fee paid.
	 *
	 * @param udsFeePaid the new uds fee paid
	 */
	public void setUdsFeePaid(Object udsFeePaid) {

		this.udsFeePaid = udsFeePaid;
	}

	/**
	 * Gets the interest repayment.
	 *
	 * @return the interest repayment
	 */
	public BigDecimal getInterestRepayment() {

		return interestRepayment;
	}

	/**
	 * Sets the interest repayment.
	 *
	 * @param interestRepayment the new interest repayment
	 */
	public void setInterestRepayment(BigDecimal interestRepayment) {

		this.interestRepayment = interestRepayment;
	}

	/**
	 * Gets the interest amt paid.
	 *
	 * @return the interest amt paid
	 */
	public String getInterestAmtPaid() {

		return interestAmtPaid;
	}

	/**
	 * Sets the interest amt paid.
	 *
	 * @param interestAmtPaid the new interest amt paid
	 */
	public void setInterestAmtPaid(String interestAmtPaid) {

		this.interestAmtPaid = interestAmtPaid;
	}

	/**
	 * Gets the comp int repayment.
	 *
	 * @return the comp int repayment
	 */
	public Object getCompIntRepayment() {

		return compIntRepayment;
	}

	/**
	 * Sets the comp int repayment.
	 *
	 * @param compIntRepayment the new comp int repayment
	 */
	public void setCompIntRepayment(Object compIntRepayment) {

		this.compIntRepayment = compIntRepayment;
	}

	/**
	 * Gets the comp int amt paid.
	 *
	 * @return the comp int amt paid
	 */
	public Object getCompIntAmtPaid() {

		return compIntAmtPaid;
	}

	/**
	 * Sets the comp int amt paid.
	 *
	 * @param compIntAmtPaid the new comp int amt paid
	 */
	public void setCompIntAmtPaid(Object compIntAmtPaid) {

		this.compIntAmtPaid = compIntAmtPaid;
	}

	/**
	 * Gets the loan repayment.
	 *
	 * @return the loan repayment
	 */
	public BigDecimal getLoanRepayment() {

		return loanRepayment;
	}

	/**
	 * Sets the loan repayment.
	 *
	 * @param loanRepayment the new loan repayment
	 */
	public void setLoanRepayment(BigDecimal loanRepayment) {

		this.loanRepayment = loanRepayment;
	}

	/**
	 * Gets the loan repayment paid.
	 *
	 * @return the loan repayment paid
	 */
	public String getLoanRepaymentPaid() {

		return loanRepaymentPaid;
	}

	/**
	 * Sets the loan repayment paid.
	 *
	 * @param loanRepaymentPaid the new loan repayment paid
	 */
	public void setLoanRepaymentPaid(String loanRepaymentPaid) {

		this.loanRepaymentPaid = loanRepaymentPaid;
	}

	/**
	 * Gets the penalty due.
	 *
	 * @return the penalty due
	 */
	public String getPenaltyDue() {

		return penaltyDue;
	}

	/**
	 * Sets the penalty due.
	 *
	 * @param penaltyDue the new penalty due
	 */
	public void setPenaltyDue(String penaltyDue) {

		this.penaltyDue = penaltyDue;
	}

	/**
	 * Gets the penalty due paid.
	 *
	 * @return the penalty due paid
	 */
	public String getPenaltyDuePaid() {

		return penaltyDuePaid;
	}

	/**
	 * Sets the penalty due paid.
	 *
	 * @param penaltyDuePaid the new penalty due paid
	 */
	public void setPenaltyDuePaid(String penaltyDuePaid) {

		this.penaltyDuePaid = penaltyDuePaid;
	}

	/**
	 * Gets the outstanding fee due.
	 *
	 * @return the outstanding fee due
	 */
	public String getOutstandingFeeDue() {

		return outstandingFeeDue;
	}

	/**
	 * Sets the outstanding fee due.
	 *
	 * @param outstandingFeeDue the new outstanding fee due
	 */
	public void setOutstandingFeeDue(String outstandingFeeDue) {

		this.outstandingFeeDue = outstandingFeeDue;
	}

	/**
	 * Gets the outstanding fee due paid.
	 *
	 * @return the outstanding fee due paid
	 */
	public String getOutstandingFeeDuePaid() {

		return outstandingFeeDuePaid;
	}

	/**
	 * Sets the outstanding fee due paid.
	 *
	 * @param outstandingFeeDuePaid the new outstanding fee due paid
	 */
	public void setOutstandingFeeDuePaid(String outstandingFeeDuePaid) {

		this.outstandingFeeDuePaid = outstandingFeeDuePaid;
	}

	/**
	 * Gets the written off amount.
	 *
	 * @return the written off amount
	 */
	public String getWrittenOffAmount() {

		return writtenOffAmount;
	}

	/**
	 * Sets the written off amount.
	 *
	 * @param writtenOffAmount the new written off amount
	 */
	public void setWrittenOffAmount(String writtenOffAmount) {

		this.writtenOffAmount = writtenOffAmount;
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
	 * @param balance the new balance
	 */
	public void setBalance(BigDecimal balance) {

		this.balance = balance;
	}

	/**
	 * Gets the savings payment.
	 *
	 * @return the savings payment
	 */
	public Object getSavingsPayment() {

		return savingsPayment;
	}

	/**
	 * Sets the savings payment.
	 *
	 * @param savingsPayment the new savings payment
	 */
	public void setSavingsPayment(Object savingsPayment) {

		this.savingsPayment = savingsPayment;
	}

	/**
	 * Gets the savings payment paid.
	 *
	 * @return the savings payment paid
	 */
	public Object getSavingsPaymentPaid() {

		return savingsPaymentPaid;
	}

	/**
	 * Sets the savings payment paid.
	 *
	 * @param savingsPaymentPaid the new savings payment paid
	 */
	public void setSavingsPaymentPaid(Object savingsPaymentPaid) {

		this.savingsPaymentPaid = savingsPaymentPaid;
	}

	/**
	 * Gets the all paid.
	 *
	 * @return the all paid
	 */
	public Object getAllPaid() {

		return allPaid;
	}

	/**
	 * Sets the all paid.
	 *
	 * @param allPaid the new all paid
	 */
	public void setAllPaid(Object allPaid) {

		this.allPaid = allPaid;
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
	 * Gets the repaid date.
	 *
	 * @return the repaid date
	 */
	public String getRepaidDate() {

		return repaidDate;
	}

	/**
	 * Sets the repaid date.
	 *
	 * @param repaidDate the new repaid date
	 */
	public void setRepaidDate(String repaidDate) {

		this.repaidDate = repaidDate;
	}

	/**
	 * Gets the repayment amount.
	 *
	 * @return the repayment amount
	 */
	public Object getRepaymentAmount() {

		return repaymentAmount;
	}

	/**
	 * Sets the repayment amount.
	 *
	 * @param repaymentAmount the new repayment amount
	 */
	public void setRepaymentAmount(Object repaymentAmount) {

		this.repaymentAmount = repaymentAmount;
	}

	/**
	 * Gets the late days.
	 *
	 * @return the late days
	 */
	public int getLateDays() {

		return lateDays;
	}

	/**
	 * Sets the late days.
	 *
	 * @param lateDays the new late days
	 */
	public void setLateDays(int lateDays) {

		this.lateDays = lateDays;
	}

	/**
	 * Gets the total repayment string.
	 *
	 * @return the total repayment string
	 */
	public String getTotalRepaymentString() {

		return totalRepaymentString;
	}

	/**
	 * Sets the total repayment string.
	 *
	 * @param totalRepaymentString the new total repayment string
	 */
	public void setTotalRepaymentString(String totalRepaymentString) {

		this.totalRepaymentString = totalRepaymentString;
	}

	/**
	 * Gets the fee repayment string.
	 *
	 * @return the fee repayment string
	 */
	public String getFeeRepaymentString() {

		return feeRepaymentString;
	}

	/**
	 * Sets the fee repayment string.
	 *
	 * @param feeRepaymentString the new fee repayment string
	 */
	public void setFeeRepaymentString(String feeRepaymentString) {

		this.feeRepaymentString = feeRepaymentString;
	}

	/**
	 * Gets the interest repayment string.
	 *
	 * @return the interest repayment string
	 */
	public String getInterestRepaymentString() {

		return interestRepaymentString;
	}

	/**
	 * Sets the interest repayment string.
	 *
	 * @param interestRepaymentString the new interest repayment string
	 */
	public void setInterestRepaymentString(String interestRepaymentString) {

		this.interestRepaymentString = interestRepaymentString;
	}

	/**
	 * Gets the loan repayment string.
	 *
	 * @return the loan repayment string
	 */
	public String getLoanRepaymentString() {

		return loanRepaymentString;
	}

	/**
	 * Sets the loan repayment string.
	 *
	 * @param loanRepaymentString the new loan repayment string
	 */
	public void setLoanRepaymentString(String loanRepaymentString) {

		this.loanRepaymentString = loanRepaymentString;
	}

	/**
	 * Gets the balance string.
	 *
	 * @return the balance string
	 */
	public String getBalanceString() {

		return balanceString;
	}

	/**
	 * Sets the balance string.
	 *
	 * @param balanceString the new balance string
	 */
	public void setBalanceString(String balanceString) {

		this.balanceString = balanceString;
	}

	/**
	 * Checks if is date changed.
	 *
	 * @return true, if is date changed
	 */
	public boolean isDateChanged() {

		return dateChanged;
	}

	/**
	 * Sets the date changed.
	 *
	 * @param dateChanged the new date changed
	 */
	public void setDateChanged(boolean dateChanged) {

		this.dateChanged = dateChanged;
	}

	/**
	 * To string.
	 *
	 * @return the string
	 */
	@Override
	public String toString() {

		return "CustomerAbacusAPIModelLoanSchedule [cuAccountID=" + cuAccountID + ", scheduleID="
				+ scheduleID + ", cuLoanPartID=" + cuLoanPartID + ", period=" + period
				+ ", repaymentDate=" + repaymentDate + ", totalRepayment=" + totalRepayment
				+ ", totalRepaymentPaid=" + totalRepaymentPaid + ", feeRepayment=" + feeRepayment
				+ ", feeAmtPaid=" + feeAmtPaid + ", udsFeeRepayment=" + udsFeeRepayment
				+ ", udsFeePaid=" + udsFeePaid + ", interestRepayment=" + interestRepayment
				+ ", interestAmtPaid=" + interestAmtPaid + ", compIntRepayment=" + compIntRepayment
				+ ", compIntAmtPaid=" + compIntAmtPaid + ", loanRepayment=" + loanRepayment
				+ ", loanRepaymentPaid=" + loanRepaymentPaid + ", penaltyDue=" + penaltyDue
				+ ", penaltyDuePaid=" + penaltyDuePaid + ", outstandingFeeDue=" + outstandingFeeDue
				+ ", outstandingFeeDuePaid=" + outstandingFeeDuePaid + ", writtenOffAmount="
				+ writtenOffAmount + ", balance=" + balance + ", savingsPayment=" + savingsPayment
				+ ", savingsPaymentPaid=" + savingsPaymentPaid + ", allPaid=" + allPaid
				+ ", status=" + status + ", repaidDate=" + repaidDate + ", repaymentAmount="
				+ repaymentAmount + ", lateDays=" + lateDays + ", totalRepaymentString="
				+ totalRepaymentString + ", feeRepaymentString=" + feeRepaymentString
				+ ", interestRepaymentString=" + interestRepaymentString + ", loanRepaymentString="
				+ loanRepaymentString + ", balanceString=" + balanceString + ", dateChanged="
				+ dateChanged + "]";
	}

}
