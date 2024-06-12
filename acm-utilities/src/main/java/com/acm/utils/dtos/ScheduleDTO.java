/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.utils.dtos;

import java.io.Serializable;
import java.math.BigDecimal;
import java.util.Date;

import com.acm.constants.common.ACMConstantWorkflowStatuts;
import com.acm.constants.common.CommonFunctions;
import com.fasterxml.jackson.annotation.JsonFormat;

/**
 * {@link ScheduleDTO} class.
 *
 * @author YesserSomai
 * @since 0.2.0
 */
public class ScheduleDTO extends GenericDTO implements Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = 3069469559006549684L;

	/** The id. */
	private Long id;

	/** The ib loan id. */
	private Long ibLoanId;

	/** The period. */
	private Integer period;

	/** The repayment date. */
	@JsonFormat(shape = JsonFormat.Shape.STRING, pattern = "dd/MM/yyyy")
	private Date repaymentDate;

	/** The total repayment. */
	private BigDecimal totalRepayment;

	/** The loan repayment. */
	private BigDecimal loanRepayment;

	/** The interest repayment. */
	private BigDecimal interestRepayment;

	/** The balance. */
	private BigDecimal balance;

	/** The interest amt paid. */
	private String interestAmtPaid;

	/** The loan repayment paid. */
	private String loanRepaymentPaid;

	/** The status. */
	private BigDecimal status;

	/** The status label. */
	private String statusLabel;

	/** The repaid on. */
	private Date repaidOn;

	/** The nb arrears days. */
	private BigDecimal nbArrearsDays;

	/** The customer name. */
	private String customerName;

	/** The id loan extern. */
	private Long idLoanExtern;

	/** The customer number. */
	private String customerNumber;

	/** The paid on => RETURN : "Not_yet_Paid" or dd/MM/yyyy. */
	private String paidOn;

	/** The term period num. */
	private Integer termPeriodNum;

	/** The penality due. */
	private String penalityDue;

	/** The penality paid. */
	private String penalityPaid;

	/** The Amount written off. */
	private String amountWrittenOff;
	/** The Late days. */
	private int LateDays;

	/**
	 * Instantiates a new schedule DTO.
	 */
	public ScheduleDTO() {

	}

	/**
	 * Instantiates a new schedule DTO.
	 *
	 * @param totalRepayment the total repayment
	 * @param loanRepayment the loan repayment
	 * @param interestRepayment the interest repayment
	 */
	public ScheduleDTO(BigDecimal totalRepayment, BigDecimal loanRepayment,
			BigDecimal interestRepayment) {

		this.totalRepayment = totalRepayment;
		this.loanRepayment = loanRepayment;
		this.interestRepayment = interestRepayment;
	}

	/**
	 * Instantiates a new schedule DTO : USED in method
	 * :CustomerAbacusService#loadCheckPaiment(String).
	 *
	 * @param period the period
	 * @param repaymentDate the repayment date
	 * @param customerName the customer name
	 * @param idLoanExtern the id loan extern
	 * @param customerNumber the customer number
	 * @param paidOn the paid on
	 * @param balance the balance
	 * @param termPeriodNum the term period num
	 */
	public ScheduleDTO(Integer period, Date repaymentDate, String customerName, Long idLoanExtern,
			String customerNumber, String paidOn, BigDecimal balance, Integer termPeriodNum) {

		this.period = period;
		this.repaymentDate = repaymentDate;
		this.customerName = customerName;
		this.idLoanExtern = idLoanExtern;
		this.customerNumber = customerNumber;
		this.paidOn = paidOn;
		this.balance = balance;
		this.termPeriodNum = termPeriodNum;
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
	 * @param period the period to set
	 */
	public void setPeriod(Integer period) {

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
	 * Gets the total repayment.
	 *
	 * @return the totalRepayment
	 */
	public BigDecimal getTotalRepayment() {

		return totalRepayment;
	}

	/**
	 * Sets the total repayment.
	 *
	 * @param totalRepayment the totalRepayment to set
	 */
	public void setTotalRepayment(BigDecimal totalRepayment) {

		this.totalRepayment = totalRepayment;
	}

	/**
	 * Gets the loan repayment.
	 *
	 * @return the loanRepayment
	 */
	public BigDecimal getLoanRepayment() {

		return loanRepayment;
	}

	/**
	 * Sets the loan repayment.
	 *
	 * @param loanRepayment the loanRepayment to set
	 */
	public void setLoanRepayment(BigDecimal loanRepayment) {

		this.loanRepayment = loanRepayment;
	}

	/**
	 * Gets the interest repayment.
	 *
	 * @return the interestRepayment
	 */
	public BigDecimal getInterestRepayment() {

		return interestRepayment;
	}

	/**
	 * Sets the interest repayment.
	 *
	 * @param interestRepayment the interestRepayment to set
	 */
	public void setInterestRepayment(BigDecimal interestRepayment) {

		this.interestRepayment = interestRepayment;
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
	 * Gets the interest amt paid.
	 *
	 * @return the interestAmtPaid
	 */
	public String getInterestAmtPaid() {

		return interestAmtPaid;
	}

	/**
	 * Sets the interest amt paid.
	 *
	 * @param interestAmtPaid the interestAmtPaid to set
	 */
	public void setInterestAmtPaid(String interestAmtPaid) {

		this.interestAmtPaid = interestAmtPaid;
	}

	/**
	 * Gets the loan repayment paid.
	 *
	 * @return the loanRepaymentPaid
	 */
	public String getLoanRepaymentPaid() {

		return loanRepaymentPaid;
	}

	/**
	 * Sets the loan repayment paid.
	 *
	 * @param loanRepaymentPaid the loanRepaymentPaid to set
	 */
	public void setLoanRepaymentPaid(String loanRepaymentPaid) {

		this.loanRepaymentPaid = loanRepaymentPaid;
	}

	/**
	 * Gets the status.
	 *
	 * @return the status
	 */
	public BigDecimal getStatus() {

		return status;
	}

	/**
	 * Sets the status.
	 *
	 * @param status the status to set
	 */
	public void setStatus(BigDecimal status) {

		this.status = status;
	}

	/**
	 * Gets the repaid on.
	 *
	 * @return the repaidOn
	 */
	public Date getRepaidOn() {

		return repaidOn;
	}

	/**
	 * Sets the repaid on.
	 *
	 * @param repaidOn the repaidOn to set
	 */
	public void setRepaidOn(Date repaidOn) {

		this.repaidOn = repaidOn;
	}

	/**
	 * Gets the nb arrears days.
	 *
	 * @return the nbArrearsDays
	 */
	public BigDecimal getNbArrearsDays() {

		return nbArrearsDays;
	}

	/**
	 * Sets the nb arrears days.
	 *
	 * @param nbArrearsDays the nbArrearsDays to set
	 */
	public void setNbArrearsDays(BigDecimal nbArrearsDays) {

		this.nbArrearsDays = nbArrearsDays;
	}

	/**
	 * Gets the status label.
	 *
	 * @return the statusLabel
	 */
	public String getStatusLabel() {

		if (this.status != null) {
			switch (this.status.intValue()) {
				case 1:
					this.statusLabel = CommonFunctions.mappingStatus(
							ACMConstantWorkflowStatuts.CUSTOMER_ACCOUNT_SCHEDULE_STATUS_PAID)
							.getValue();
					break;
				case 2:
					this.statusLabel = CommonFunctions.mappingStatus(
							ACMConstantWorkflowStatuts.CUSTOMER_ACCOUNT_SCHEDULE_STATUS_PARTIALLY_PAID)
							.getValue();
					break;
				case 3:
					this.statusLabel = CommonFunctions.mappingStatus(
							ACMConstantWorkflowStatuts.CUSTOMER_ACCOUNT_SCHEDULE_STATUS_NOT_PAID)
							.getValue();
					break;

				default:
					break;
			}
		}
		return statusLabel;
	}

	/**
	 * Sets the status label.
	 *
	 * @param statusLabel the statusLabel to set
	 */
	public void setStatusLabel(String statusLabel) {

		this.statusLabel = statusLabel;
	}

	/**
	 * Gets the customer name.
	 *
	 * @return the customerName
	 */
	public String getCustomerName() {

		return customerName;
	}

	/**
	 * Sets the customer name.
	 *
	 * @param customerName the customerName to set
	 */
	public void setCustomerName(String customerName) {

		this.customerName = customerName;
	}

	/**
	 * Gets the customer number.
	 *
	 * @return the customerNumber
	 */
	public String getCustomerNumber() {

		return customerNumber;
	}

	/**
	 * Sets the customer number.
	 *
	 * @param customerNumber the customerNumber to set
	 */
	public void setCustomerNumber(String customerNumber) {

		this.customerNumber = customerNumber;
	}

	/**
	 * Gets the paid on.
	 *
	 * @return the paidOn
	 */
	public String getPaidOn() {

		return paidOn;
	}

	/**
	 * Sets the paid on.
	 *
	 * @param paidOn the paidOn to set
	 */
	public void setPaidOn(String paidOn) {

		this.paidOn = paidOn;
	}

	/**
	 * Gets the id loan extern.
	 *
	 * @return the idLoanExtern
	 */
	public Long getIdLoanExtern() {

		return idLoanExtern;
	}

	/**
	 * Sets the id loan extern.
	 *
	 * @param idLoanExtern the idLoanExtern to set
	 */
	public void setIdLoanExtern(Long idLoanExtern) {

		this.idLoanExtern = idLoanExtern;
	}

	/**
	 * Gets the term period num.
	 *
	 * @return the termPeriodNum
	 */
	public Integer getTermPeriodNum() {

		return termPeriodNum;
	}

	/**
	 * Sets the term period num.
	 *
	 * @param termPeriodNum the termPeriodNum to set
	 */
	public void setTermPeriodNum(Integer termPeriodNum) {

		this.termPeriodNum = termPeriodNum;
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

		return "ScheduleDTO [period=" + period + ", repaymentDate=" + repaymentDate
				+ ", totalRepayment=" + totalRepayment + ", loanRepayment=" + loanRepayment
				+ ", interestRepayment=" + interestRepayment + ", balance=" + balance
				+ ", interestAmtPaid=" + interestAmtPaid + ", loanRepaymentPaid="
				+ loanRepaymentPaid + ", status=" + status + ", statusLabel=" + statusLabel
				+ ", repaidOn=" + repaidOn + ", nbArrearsDays=" + nbArrearsDays + "]";
	}

	/**
	 * <<<<<<< HEAD Gets the ib loan id.
	 *
	 * @return the ib loan id
	 */
	public Long getIbLoanId() {

		return ibLoanId;
	}

	/**
	 * Sets the ib loan id.
	 *
	 * @param ibLoanId the new ib loan id
	 */
	public void setIbLoanId(Long ibLoanId) {

		this.ibLoanId = ibLoanId;
	}

	/**
	 * Gets the id.
	 *
	 * @return the id
	 */
	public Long getId() {

		return id;
	}

	/**
	 * Sets the id.
	 *
	 * @param id the new id
	 */
	public void setId(Long id) {

		this.id = id;
	}

	/**
	 * Gets the amount written off.
	 *
	 * @return the amount written off
	 */
	public String getAmountWrittenOff() {

		return amountWrittenOff;
	}

	/**
	 * Sets the amount written off.
	 *
	 * @param amountWrittenOff the new amount written off
	 */
	public void setAmountWrittenOff(String amountWrittenOff) {

		this.amountWrittenOff = amountWrittenOff;
	}

	/**
	 * Gets the penality due.
	 *
	 * @return the penality due
	 */
	public String getPenalityDue() {

		return penalityDue;
	}

	/**
	 * Sets the penality due.
	 *
	 * @param penalityDue the new penality due
	 */
	public void setPenalityDue(String penalityDue) {

		this.penalityDue = penalityDue;
	}

	/**
	 * Gets the penality paid.
	 *
	 * @return the penality paid
	 */
	public String getPenalityPaid() {

		return penalityPaid;
	}

	/**
	 * Sets the penality paid.
	 *
	 * @param penalityPaid the new penality paid
	 */
	public void setPenalityPaid(String penalityPaid) {

		this.penalityPaid = penalityPaid;
	}

	/**
	 * Gets the late days.
	 *
	 * @return the late days
	 */
	public int getLateDays() {

		return LateDays;
	}

	/**
	 * Sets the late days.
	 *
	 * @param lateDays the new late days
	 */
	public void setLateDays(int lateDays) {

		LateDays = lateDays;
	}

}
