/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.utils.dtos;

import java.io.Serializable;
import java.util.List;

/**
 * {@link ReportingListGroupByDTO} class.
 *
 * @author HaythemBenizid
 * @since 1.1.1
 */
public class ReportingListGroupByDTO implements Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = 1507150868151482211L;

	/** FOR LOAN APPLICATION : The loan DT os. */
	private List<LoanDTO> loanDTOs;

	/** FOR LOAN APPLICATION : The total amount. */
	private Long totalAmount;

	/** FOR LOAN APPLICATION : The total issue amount. */
	private Long totalIssueAmount;

	/** FOR COLLECTION FOLLOW UP : The reporting schedules status DT os. */
	private List<ReportingSchedulesStatusDTO> reportingSchedulesStatusDTOs;

	/** FOR COLLECTION FOLLOW UP : The total instalment amount. */
	private Long totalInstalmentAmount;

	/** FOR COLLECTION FOLLOW UP : The total instalment principal. */
	private Long totalInstalmentPrincipal;

	/** FOR COLLECTION FOLLOW UP : The total instalment interest. */
	private Long totalInstalmentInterest;

	/** FOR COLLECTION FOLLOW UP : The total instal principal paid. */
	private Long totalInstalPrincipalPaid;

	/** FOR COLLECTION FOLLOW UP : The total instal interest paid. */
	private Long totalInstalInterestPaid;

	/** FOR COLLECTION FOLLOW UP : The total instal total paid. */
	private Long totalInstalTotalPaid;

	/** FOR COLLECTION FOLLOW UP : The total total principal paid. */
	private Long totalTotalPrincipalPaid;

	/** FOR COLLECTION FOLLOW UP : The total total interest paid. */
	private Long totalTotalInterestPaid;

	/** FOR COLLECTION FOLLOW UP : The total total paid amount. */
	private Long totalTotalPaidAmount;

	/** FOR COLLECTION FOLLOW UP : The total unpaid principal. */
	private Long totalUnpaidPrincipal;

	/** FOR COLLECTION FOLLOW UP : The total unpaid interest. */
	private Long totalUnpaidInterest;

	/** FOR COLLECTION FOLLOW UP : The total unpaid amount. */
	private Long totalUnpaidAmount;

	/** FOR COLLECTION FOLLOW UP : The total remaining principal. */
	private Long totalRemainingPrincipal;

	/** FOR COLLECTION FOLLOW UP : The total remaininginterest. */
	private Long totalRemaininginterest;

	/** FOR COLLECTION FOLLOW UP : The total remaining amount. */
	private Long totalRemainingAmount;

	/** The total records. */
	private Integer totalRecords;

	/**
	 * Instantiates a new reporting list group by DTO.
	 */
	public ReportingListGroupByDTO() {

		/*
		 * EMPTY
		 */
	}

	/**
	 * Gets the loan DT os.
	 *
	 * @return the loanDTOs
	 */
	public List<LoanDTO> getLoanDTOs() {

		return loanDTOs;
	}

	/**
	 * Sets the loan DT os.
	 *
	 * @param loanDTOs the loanDTOs to set
	 */
	public void setLoanDTOs(List<LoanDTO> loanDTOs) {

		this.loanDTOs = loanDTOs;
	}

	/**
	 * Gets the total amount.
	 *
	 * @return the totalAmount
	 */
	public Long getTotalAmount() {

		return totalAmount;
	}

	/**
	 * Sets the total amount.
	 *
	 * @param totalAmount the totalAmount to set
	 */
	public void setTotalAmount(Long totalAmount) {

		this.totalAmount = totalAmount;
	}

	/**
	 * Gets the total records.
	 *
	 * @return the totalRecords
	 */
	public Integer getTotalRecords() {

		return totalRecords;
	}

	/**
	 * Sets the total records.
	 *
	 * @param totalRecords the totalRecords to set
	 */
	public void setTotalRecords(Integer totalRecords) {

		this.totalRecords = totalRecords;
	}

	/**
	 * Gets the total issue amount.
	 *
	 * @return the totalIssueAmount
	 */
	public Long getTotalIssueAmount() {

		return totalIssueAmount;
	}

	/**
	 * Sets the total issue amount.
	 *
	 * @param totalIssueAmount the totalIssueAmount to set
	 */
	public void setTotalIssueAmount(Long totalIssueAmount) {

		this.totalIssueAmount = totalIssueAmount;
	}

	/**
	 * Gets the reporting schedules status DT os.
	 *
	 * @return the reportingSchedulesStatusDTOs
	 */
	public List<ReportingSchedulesStatusDTO> getReportingSchedulesStatusDTOs() {

		return reportingSchedulesStatusDTOs;
	}

	/**
	 * Sets the reporting schedules status DT os.
	 *
	 * @param reportingSchedulesStatusDTOs the reportingSchedulesStatusDTOs to set
	 */
	public void setReportingSchedulesStatusDTOs(
			List<ReportingSchedulesStatusDTO> reportingSchedulesStatusDTOs) {

		this.reportingSchedulesStatusDTOs = reportingSchedulesStatusDTOs;
	}

	/**
	 * Gets the total instalment amount.
	 *
	 * @return the totalInstalmentAmount
	 */
	public Long getTotalInstalmentAmount() {

		return totalInstalmentAmount;
	}

	/**
	 * Sets the total instalment amount.
	 *
	 * @param totalInstalmentAmount the totalInstalmentAmount to set
	 */
	public void setTotalInstalmentAmount(Long totalInstalmentAmount) {

		this.totalInstalmentAmount = totalInstalmentAmount;
	}

	/**
	 * Gets the total instalment principal.
	 *
	 * @return the totalInstalmentPrincipal
	 */
	public Long getTotalInstalmentPrincipal() {

		return totalInstalmentPrincipal;
	}

	/**
	 * Sets the total instalment principal.
	 *
	 * @param totalInstalmentPrincipal the totalInstalmentPrincipal to set
	 */
	public void setTotalInstalmentPrincipal(Long totalInstalmentPrincipal) {

		this.totalInstalmentPrincipal = totalInstalmentPrincipal;
	}

	/**
	 * Gets the total instalment interest.
	 *
	 * @return the totalInstalmentInterest
	 */
	public Long getTotalInstalmentInterest() {

		return totalInstalmentInterest;
	}

	/**
	 * Sets the total instalment interest.
	 *
	 * @param totalInstalmentInterest the totalInstalmentInterest to set
	 */
	public void setTotalInstalmentInterest(Long totalInstalmentInterest) {

		this.totalInstalmentInterest = totalInstalmentInterest;
	}

	/**
	 * Gets the total instal principal paid.
	 *
	 * @return the totalInstalPrincipalPaid
	 */
	public Long getTotalInstalPrincipalPaid() {

		return totalInstalPrincipalPaid;
	}

	/**
	 * Sets the total instal principal paid.
	 *
	 * @param totalInstalPrincipalPaid the totalInstalPrincipalPaid to set
	 */
	public void setTotalInstalPrincipalPaid(Long totalInstalPrincipalPaid) {

		this.totalInstalPrincipalPaid = totalInstalPrincipalPaid;
	}

	/**
	 * Gets the total instal interest paid.
	 *
	 * @return the totalInstalInterestPaid
	 */
	public Long getTotalInstalInterestPaid() {

		return totalInstalInterestPaid;
	}

	/**
	 * Sets the total instal interest paid.
	 *
	 * @param totalInstalInterestPaid the totalInstalInterestPaid to set
	 */
	public void setTotalInstalInterestPaid(Long totalInstalInterestPaid) {

		this.totalInstalInterestPaid = totalInstalInterestPaid;
	}

	/**
	 * Gets the total instal total paid.
	 *
	 * @return the totalInstalTotalPaid
	 */
	public Long getTotalInstalTotalPaid() {

		return totalInstalTotalPaid;
	}

	/**
	 * Sets the total instal total paid.
	 *
	 * @param totalInstalTotalPaid the totalInstalTotalPaid to set
	 */
	public void setTotalInstalTotalPaid(Long totalInstalTotalPaid) {

		this.totalInstalTotalPaid = totalInstalTotalPaid;
	}

	/**
	 * Gets the total total principal paid.
	 *
	 * @return the totalTotalPrincipalPaid
	 */
	public Long getTotalTotalPrincipalPaid() {

		return totalTotalPrincipalPaid;
	}

	/**
	 * Sets the total total principal paid.
	 *
	 * @param totalTotalPrincipalPaid the totalTotalPrincipalPaid to set
	 */
	public void setTotalTotalPrincipalPaid(Long totalTotalPrincipalPaid) {

		this.totalTotalPrincipalPaid = totalTotalPrincipalPaid;
	}

	/**
	 * Gets the total total interest paid.
	 *
	 * @return the totalTotalInterestPaid
	 */
	public Long getTotalTotalInterestPaid() {

		return totalTotalInterestPaid;
	}

	/**
	 * Sets the total total interest paid.
	 *
	 * @param totalTotalInterestPaid the totalTotalInterestPaid to set
	 */
	public void setTotalTotalInterestPaid(Long totalTotalInterestPaid) {

		this.totalTotalInterestPaid = totalTotalInterestPaid;
	}

	/**
	 * Gets the total total paid amount.
	 *
	 * @return the totalTotalPaidAmount
	 */
	public Long getTotalTotalPaidAmount() {

		return totalTotalPaidAmount;
	}

	/**
	 * Sets the total total paid amount.
	 *
	 * @param totalTotalPaidAmount the totalTotalPaidAmount to set
	 */
	public void setTotalTotalPaidAmount(Long totalTotalPaidAmount) {

		this.totalTotalPaidAmount = totalTotalPaidAmount;
	}

	/**
	 * Gets the total unpaid principal.
	 *
	 * @return the totalUnpaidPrincipal
	 */
	public Long getTotalUnpaidPrincipal() {

		return totalUnpaidPrincipal;
	}

	/**
	 * Sets the total unpaid principal.
	 *
	 * @param totalUnpaidPrincipal the totalUnpaidPrincipal to set
	 */
	public void setTotalUnpaidPrincipal(Long totalUnpaidPrincipal) {

		this.totalUnpaidPrincipal = totalUnpaidPrincipal;
	}

	/**
	 * Gets the total unpaid interest.
	 *
	 * @return the totalUnpaidInterest
	 */
	public Long getTotalUnpaidInterest() {

		return totalUnpaidInterest;
	}

	/**
	 * Sets the total unpaid interest.
	 *
	 * @param totalUnpaidInterest the totalUnpaidInterest to set
	 */
	public void setTotalUnpaidInterest(Long totalUnpaidInterest) {

		this.totalUnpaidInterest = totalUnpaidInterest;
	}

	/**
	 * Gets the total unpaid amount.
	 *
	 * @return the totalUnpaidAmount
	 */
	public Long getTotalUnpaidAmount() {

		return totalUnpaidAmount;
	}

	/**
	 * Sets the total unpaid amount.
	 *
	 * @param totalUnpaidAmount the totalUnpaidAmount to set
	 */
	public void setTotalUnpaidAmount(Long totalUnpaidAmount) {

		this.totalUnpaidAmount = totalUnpaidAmount;
	}

	/**
	 * Gets the total remaining principal.
	 *
	 * @return the totalRemainingPrincipal
	 */
	public Long getTotalRemainingPrincipal() {

		return totalRemainingPrincipal;
	}

	/**
	 * Sets the total remaining principal.
	 *
	 * @param totalRemainingPrincipal the totalRemainingPrincipal to set
	 */
	public void setTotalRemainingPrincipal(Long totalRemainingPrincipal) {

		this.totalRemainingPrincipal = totalRemainingPrincipal;
	}

	/**
	 * Gets the total remaininginterest.
	 *
	 * @return the totalRemaininginterest
	 */
	public Long getTotalRemaininginterest() {

		return totalRemaininginterest;
	}

	/**
	 * Sets the total remaininginterest.
	 *
	 * @param totalRemaininginterest the totalRemaininginterest to set
	 */
	public void setTotalRemaininginterest(Long totalRemaininginterest) {

		this.totalRemaininginterest = totalRemaininginterest;
	}

	/**
	 * Gets the total remaining amount.
	 *
	 * @return the totalRemainingAmount
	 */
	public Long getTotalRemainingAmount() {

		return totalRemainingAmount;
	}

	/**
	 * Sets the total remaining amount.
	 *
	 * @param totalRemainingAmount the totalRemainingAmount to set
	 */
	public void setTotalRemainingAmount(Long totalRemainingAmount) {

		this.totalRemainingAmount = totalRemainingAmount;
	}

}
