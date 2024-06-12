/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.utils.dtos;

import java.math.BigDecimal;
import java.util.Date;

/**
 * The Class ScheduleApiDTO.
 */
public class ScheduleApiDTO {

	/** The cu account ID. */
	private Integer cuAccountID;

	/** The schedule ID. */
	private Integer scheduleID;

	/** The cu loan part ID. */
	private Integer cuLoanPartID;

	/** The period. */
	private Integer period;

	/** The repayment date. */
	private String repaymentDate;

	/** The total repayment. */
	private BigDecimal totalRepayment;

	/** The total repayment paid. */
	private String totalRepaymentPaid;

	/** The fee repayment. */
	private BigDecimal feeRepayment;

	/** The fee amt paid. */
	private BigDecimal feeAmtPaid;

	/** The uds fee repayment. */
	private BigDecimal udsFeeRepayment;

	/** The uds fee paid. */
	private BigDecimal udsFeePaid;

	/** The interest repayment. */
	private BigDecimal interestRepayment;

	/** The interest amt paid. */
	private String interestAmtPaid;

	/** The comp int repayment. */
	private BigDecimal compIntRepayment;

	/** The comp int amt paid. */
	private BigDecimal compIntAmtPaid;

	/** The loan repayment. */
	private BigDecimal loanRepayment;

	/** The loan repayment paid. */
	private String loanRepaymentPaid;

	/** The penalty due. */
	private String penaltyDue;

	/** The penalty due paid. */
	private String penaltyDuePaid;

	/** The outstanding fee due. */
	private String outstandingFeeDue;

	/** The outstanding fee due paid. */
	private String outstandingFeeDuePaid;

	/** The written off amount. */
	private String writtenOffAmount;

	/** The balance. */
	private BigDecimal balance;

	/** The savings payment. */
	private BigDecimal savingsPayment;

	/** The savings payment paid. */
	private BigDecimal savingsPaymentPaid;

	/** The all paid. */
	private Boolean allPaid;

	/** The status. */
	private String status;

	/** The repaid date. */
	private String repaidDate;

	/** The repayment amount. */
	private BigDecimal repaymentAmount;

	/** The late days. */
	private Integer lateDays;

	/** The total repayment string. */
	private String totalRepaymentString;

	/** The fee repayment string. */
	private String feeRepaymentString;

	/** The interest repayment string. */
	private String interestRepaymentString;

	/** The loan repayment string. */
	private String loanRepaymentString;

	/** The balance string. */
	private String balanceString;

	/** The date changed. */
	private Boolean dateChanged;

	/** The repaidOn. */
	private Date repaidOn;

	/**
	 * Instantiates a new schedule api DTO.
	 */
	public ScheduleApiDTO() {

	}

	/**
	 * Gets the cu account ID.
	 *
	 * @return the cu account ID
	 */
	public Integer getCuAccountID() {

		return cuAccountID;
	}

	/**
	 * Sets the cu account ID.
	 *
	 * @param cuAccountID the new cu account ID
	 */
	public void setCuAccountID(Integer cuAccountID) {

		this.cuAccountID = cuAccountID;
	}

	/**
	 * Gets the schedule ID.
	 *
	 * @return the schedule ID
	 */
	public Integer getScheduleID() {

		return scheduleID;
	}

	/**
	 * Sets the schedule ID.
	 *
	 * @param scheduleID the new schedule ID
	 */
	public void setScheduleID(Integer scheduleID) {

		this.scheduleID = scheduleID;
	}

	/**
	 * Gets the cu loan part ID.
	 *
	 * @return the cu loan part ID
	 */
	public Integer getCuLoanPartID() {

		return cuLoanPartID;
	}

	/**
	 * Sets the cu loan part ID.
	 *
	 * @param cuLoanPartID the new cu loan part ID
	 */
	public void setCuLoanPartID(Integer cuLoanPartID) {

		this.cuLoanPartID = cuLoanPartID;
	}

	/**
	 * Gets the period.
	 *
	 * @return the period
	 */
	public Integer getPeriod() {

		return period;
	}

	/**
	 * Sets the period.
	 *
	 * @param period the new period
	 */
	public void setPeriod(Integer period) {

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
	public BigDecimal getFeeRepayment() {

		return feeRepayment;
	}

	/**
	 * Sets the fee repayment.
	 *
	 * @param feeRepayment the new fee repayment
	 */
	public void setFeeRepayment(BigDecimal feeRepayment) {

		this.feeRepayment = feeRepayment;
	}

	/**
	 * Gets the fee amt paid.
	 *
	 * @return the fee amt paid
	 */
	public BigDecimal getFeeAmtPaid() {

		return feeAmtPaid;
	}

	/**
	 * Sets the fee amt paid.
	 *
	 * @param feeAmtPaid the new fee amt paid
	 */
	public void setFeeAmtPaid(BigDecimal feeAmtPaid) {

		this.feeAmtPaid = feeAmtPaid;
	}

	/**
	 * Gets the uds fee repayment.
	 *
	 * @return the uds fee repayment
	 */
	public BigDecimal getUdsFeeRepayment() {

		return udsFeeRepayment;
	}

	/**
	 * Sets the uds fee repayment.
	 *
	 * @param udsFeeRepayment the new uds fee repayment
	 */
	public void setUdsFeeRepayment(BigDecimal udsFeeRepayment) {

		this.udsFeeRepayment = udsFeeRepayment;
	}

	/**
	 * Gets the uds fee paid.
	 *
	 * @return the uds fee paid
	 */
	public BigDecimal getUdsFeePaid() {

		return udsFeePaid;
	}

	/**
	 * Sets the uds fee paid.
	 *
	 * @param udsFeePaid the new uds fee paid
	 */
	public void setUdsFeePaid(BigDecimal udsFeePaid) {

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
	public BigDecimal getCompIntRepayment() {

		return compIntRepayment;
	}

	/**
	 * Sets the comp int repayment.
	 *
	 * @param compIntRepayment the new comp int repayment
	 */
	public void setCompIntRepayment(BigDecimal compIntRepayment) {

		this.compIntRepayment = compIntRepayment;
	}

	/**
	 * Gets the comp int amt paid.
	 *
	 * @return the comp int amt paid
	 */
	public BigDecimal getCompIntAmtPaid() {

		return compIntAmtPaid;
	}

	/**
	 * Sets the comp int amt paid.
	 *
	 * @param compIntAmtPaid the new comp int amt paid
	 */
	public void setCompIntAmtPaid(BigDecimal compIntAmtPaid) {

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
	public BigDecimal getSavingsPayment() {

		return savingsPayment;
	}

	/**
	 * Sets the savings payment.
	 *
	 * @param savingsPayment the new savings payment
	 */
	public void setSavingsPayment(BigDecimal savingsPayment) {

		this.savingsPayment = savingsPayment;
	}

	/**
	 * Gets the savings payment paid.
	 *
	 * @return the savings payment paid
	 */
	public BigDecimal getSavingsPaymentPaid() {

		return savingsPaymentPaid;
	}

	/**
	 * Sets the savings payment paid.
	 *
	 * @param savingsPaymentPaid the new savings payment paid
	 */
	public void setSavingsPaymentPaid(BigDecimal savingsPaymentPaid) {

		this.savingsPaymentPaid = savingsPaymentPaid;
	}

	/**
	 * Gets the all paid.
	 *
	 * @return the all paid
	 */
	public Boolean getAllPaid() {

		return allPaid;
	}

	/**
	 * Sets the all paid.
	 *
	 * @param allPaid the new all paid
	 */
	public void setAllPaid(Boolean allPaid) {

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
	public BigDecimal getRepaymentAmount() {

		return repaymentAmount;
	}

	/**
	 * Sets the repayment amount.
	 *
	 * @param repaymentAmount the new repayment amount
	 */
	public void setRepaymentAmount(BigDecimal repaymentAmount) {

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
	 * @return the boolean
	 */
	public Boolean isDateChanged() {

		return dateChanged;
	}

	/**
	 * Sets the date changed.
	 *
	 * @param dateChanged the new date changed
	 */
	public void setDateChanged(Boolean dateChanged) {

		this.dateChanged = dateChanged;
	}

	/**
	 * Gets the repaid on.
	 *
	 * @return the repaid on
	 */
	public Date getRepaidOn() {

		return repaidOn;
	}

	/**
	 * Sets the repaid on.
	 *
	 * @param repaidOn the new repaid on
	 */
	public void setRepaidOn(Date repaidOn) {

		this.repaidOn = repaidOn;
	}

	/**
	 * Gets the date changed.
	 *
	 * @return the date changed
	 */
	public Boolean getDateChanged() {

		return dateChanged;
	}

	/**
	 * Sets the late days.
	 *
	 * @param lateDays the new late days
	 */
	public void setLateDays(Integer lateDays) {

		this.lateDays = lateDays;
	}

}
