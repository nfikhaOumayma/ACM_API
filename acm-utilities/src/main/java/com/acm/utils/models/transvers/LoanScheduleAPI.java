/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.utils.models.transvers;

import java.io.Serializable;
import java.math.BigDecimal;
import java.util.Date;
import java.util.List;

import com.acm.utils.dtos.ScheduleDTO;

/**
 * {@link LoanScheduleAPI} class.
 *
 * @author HaythemBenizid
 * @since 0.9.0
 */
public class LoanScheduleAPI implements Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = -6353293949841421427L;

	/** The loan schedule. */
	private List<ScheduleDTO> loanSchedule;

	/** The apr. */
	private BigDecimal apr;

	/** The maturity date. */
	private Date maturityDate;

	/** The initial payment date. */
	private Date initialPaymentDate;

	/** The normal payment. */
	private BigDecimal normalPayment;

	/** The message. */
	private String message;

	/** The status code. */
	private String statusCode;

	/** The effective interest amount. */
	private BigDecimal effectiveInterestRate;

	/** The insurance premium. */
	private BigDecimal insurancePremium;

	/** The issue amount. */
	private BigDecimal issueAmount;

	/** The effective interest rate standard. */
	private BigDecimal effectiveInterestRateStandard;

	/** The interest rate. */
	private BigDecimal interestRate;

	/** The issue fee. */
	private BigDecimal issueFee;

	/** The fee amt 1. */
	private BigDecimal feeAmt1;

	/**
	 * Instantiates a new loan schedule API.
	 */
	public LoanScheduleAPI() {

		/*
		 * Empty
		 */
	}

	/**
	 * Instantiates a new loan schedule API.
	 *
	 * @param loanSchedule the loan schedule
	 * @param apr the apr
	 * @param maturityDate the maturity date
	 * @param initialPaymentDate the initial payment date
	 * @param normalPayment the normal payment
	 * @param effectiveInterestRate the effective interest rate
	 * @param insurancePremium the insurance premium
	 * @param issueAmount the issue amount
	 * @param effectiveInterestRateStandard the effective interest rate standard
	 * @param interestRate the interest rate
	 * @param issueFee the issue fee
	 * @param feeAmt1 the fee amt 1
	 */
	public LoanScheduleAPI(List<ScheduleDTO> loanSchedule, BigDecimal apr, Date maturityDate,
			Date initialPaymentDate, BigDecimal normalPayment, BigDecimal effectiveInterestRate,
			BigDecimal insurancePremium, BigDecimal issueAmount,
			BigDecimal effectiveInterestRateStandard, BigDecimal interestRate, BigDecimal issueFee,
			BigDecimal feeAmt1) {

		this.loanSchedule = loanSchedule;
		this.apr = apr;
		this.maturityDate = maturityDate;
		this.initialPaymentDate = initialPaymentDate;
		this.normalPayment = normalPayment;
		this.effectiveInterestRate = effectiveInterestRate;
		this.insurancePremium = insurancePremium;
		this.issueAmount = issueAmount;
		this.effectiveInterestRateStandard = effectiveInterestRateStandard;
		this.interestRate = interestRate;
		this.issueFee = issueFee;
		this.feeAmt1 = feeAmt1;
	}

	/**
	 * Instantiates a new login API.
	 *
	 * @param message the message
	 * @param statusCode the status code
	 */
	public LoanScheduleAPI(String message, String statusCode) {

		this.setMessage(message);
		this.setStatusCode(statusCode);
	}

	/**
	 * Gets the loan schedule.
	 *
	 * @return the loanSchedule
	 */
	public List<ScheduleDTO> getLoanSchedule() {

		return loanSchedule;
	}

	/**
	 * Sets the loan schedule.
	 *
	 * @param loanSchedule the loanSchedule to set
	 */
	public void setLoanSchedule(List<ScheduleDTO> loanSchedule) {

		this.loanSchedule = loanSchedule;
	}

	/**
	 * Gets the message.
	 *
	 * @return the message
	 */
	public String getMessage() {

		return message;
	}

	/**
	 * Sets the message.
	 *
	 * @param message the message to set
	 */
	public void setMessage(String message) {

		this.message = message;
	}

	/**
	 * Gets the status code.
	 *
	 * @return the statusCode
	 */
	public String getStatusCode() {

		return statusCode;
	}

	/**
	 * Sets the status code.
	 *
	 * @param statusCode the statusCode to set
	 */
	public void setStatusCode(String statusCode) {

		this.statusCode = statusCode;
	}

	/**
	 * Gets the apr.
	 *
	 * @return the apr
	 */
	public BigDecimal getApr() {

		return apr;
	}

	/**
	 * Sets the apr.
	 *
	 * @param apr the apr to set
	 */
	public void setApr(BigDecimal apr) {

		this.apr = apr;
	}

	/**
	 * Gets the maturity date.
	 *
	 * @return the maturityDate
	 */
	public Date getMaturityDate() {

		return maturityDate;
	}

	/**
	 * Sets the maturity date.
	 *
	 * @param maturityDate the maturityDate to set
	 */
	public void setMaturityDate(Date maturityDate) {

		this.maturityDate = maturityDate;
	}

	/**
	 * Gets the initial payment date.
	 *
	 * @return the initialPaymentDate
	 */
	public Date getInitialPaymentDate() {

		return initialPaymentDate;
	}

	/**
	 * Sets the initial payment date.
	 *
	 * @param initialPaymentDate the initialPaymentDate to set
	 */
	public void setInitialPaymentDate(Date initialPaymentDate) {

		this.initialPaymentDate = initialPaymentDate;
	}

	/**
	 * Gets the normal payment.
	 *
	 * @return the normalPayment
	 */
	public BigDecimal getNormalPayment() {

		return normalPayment;
	}

	/**
	 * Sets the normal payment.
	 *
	 * @param normalPayment the normalPayment to set
	 */
	public void setNormalPayment(BigDecimal normalPayment) {

		this.normalPayment = normalPayment;
	}

	/**
	 * Gets the effective interest rate.
	 *
	 * @return the effectiveInterestRate
	 */
	public BigDecimal getEffectiveInterestRate() {

		return effectiveInterestRate;
	}

	/**
	 * Sets the effective interest rate.
	 *
	 * @param effectiveInterestRate the effectiveInterestRate to set
	 */
	public void setEffectiveInterestRate(BigDecimal effectiveInterestRate) {

		this.effectiveInterestRate = effectiveInterestRate;
	}

	/**
	 * Gets the insurance premium.
	 *
	 * @return the insurancePremium
	 */
	public BigDecimal getInsurancePremium() {

		return insurancePremium;
	}

	/**
	 * Sets the insurance premium.
	 *
	 * @param insurancePremium the insurancePremium to set
	 */
	public void setInsurancePremium(BigDecimal insurancePremium) {

		this.insurancePremium = insurancePremium;
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
	 * Gets the effective interest rate standard.
	 *
	 * @return the effective interest rate standard
	 */
	public BigDecimal getEffectiveInterestRateStandard() {

		return effectiveInterestRateStandard;
	}

	/**
	 * Sets the effective interest rate standard.
	 *
	 * @param effectiveInterestRateStandard the new effective interest rate standard
	 */
	public void setEffectiveInterestRateStandard(BigDecimal effectiveInterestRateStandard) {

		this.effectiveInterestRateStandard = effectiveInterestRateStandard;
	}

	/*
	 * (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {

		return "LoanScheduleAPI [loanSchedule=" + loanSchedule + ", apr=" + apr + ", maturityDate="
				+ maturityDate + ", initialPaymentDate=" + initialPaymentDate + ", normalPayment="
				+ normalPayment + ", message=" + message + ", statusCode=" + statusCode + "]";
	}

	/**
	 * Gets the interest rate.
	 *
	 * @return the interest rate
	 */
	public BigDecimal getInterestRate() {

		return interestRate;
	}

	/**
	 * Sets the interest rate.
	 *
	 * @param interestRate the new interest rate
	 */
	public void setInterestRate(BigDecimal interestRate) {

		this.interestRate = interestRate;
	}

	/**
	 * Gets the issue fee.
	 *
	 * @return the issueFee
	 */
	public BigDecimal getIssueFee() {

		return issueFee;
	}

	/**
	 * Sets the issue fee.
	 *
	 * @param issueFee the issueFee to set
	 */
	public void setIssueFee(BigDecimal issueFee) {

		this.issueFee = issueFee;
	}

	/**
	 * Gets the fee amt 1.
	 *
	 * @return the feeAmt1
	 */
	public BigDecimal getFeeAmt1() {

		return feeAmt1;
	}

	/**
	 * Sets the fee amt 1.
	 *
	 * @param feeAmt1 the feeAmt1 to set
	 */
	public void setFeeAmt1(BigDecimal feeAmt1) {

		this.feeAmt1 = feeAmt1;
	}

}
