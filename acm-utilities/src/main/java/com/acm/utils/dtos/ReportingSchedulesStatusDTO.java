/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.utils.dtos;

import java.io.Serializable;
import java.util.Date;

/**
 * {@link ReportingSchedulesStatusDTO} class.
 *
 * @author HaythemBenizid
 * @since 1.1.2
 */
public class ReportingSchedulesStatusDTO implements Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = -655319003554798102L;

	/** The account number. */
	private String accountNumber;

	/** The group number. */
	private String groupNumber;

	/** The group name. */
	private String groupName;

	/** The customer number. */
	private String customerNumber;

	/** The customer name. */
	private String customerName;

	/** The customer type. */
	private Integer customerType;

	/** The customer type label. */
	private String customerTypeLabel;

	/** The customer gender. */
	private Integer customerGender;

	/** The customer gender label. */
	private String customerGenderLabel;

	/** The repayment date. */
	private Date repaymentDate;

	/** The instalment amount. */
	private Long instalmentAmount;

	/** The instalment principal. */
	private Long instalmentPrincipal;

	/** The instalment interest. */
	private Long instalmentInterest;

	/** The instalment principal paid. */
	private Long instalmentPrincipalPaid;

	/** The instalmentinterest paid. */
	private Long instalmentinterestPaid;

	/** The instalment total paid. */
	private Long instalmentTotalPaid;

	/** The total principal paid. */
	private Long totalPrincipalPaid;

	/** The total interest paid. */
	private Long totalInterestPaid;

	/** The total paid amount. */
	private Long totalPaidAmount;

	/** The unpaid principal. */
	private Long unpaidPrincipal;

	/** The upaid interest. */
	private Long upaidInterest;

	/** The upaid amount. */
	private Long upaidAmount;

	/** The late days. */
	private Integer lateDays;

	/** The nb unpaidinstalment. */
	private Integer nbUnpaidinstalment;

	/** The remaining principal. */
	private Long remainingPrincipal;

	/** The remaining interest. */
	private Long remainingInterest;

	/** The remaining amount. */
	private Long remainingAmount;

	/** The source offunds. */
	private Integer sourceOffunds;

	/** The source offunds label. */
	private String sourceOffundsLabel;

	/** The branche. */
	private Integer branche;

	/** The branche label. */
	private String brancheLabel;

	/** The loan officer. */
	private Integer loanOfficer;

	/** The loan officer label. */
	private String loanOfficerLabel;

	/** The loan reason ID. */
	private Integer loanReasonID;

	/** The loan reason ID label. */
	private String loanReasonIDLabel;

	/** The product ID. */
	private Integer productID;

	/** The product ID label. */
	private String productIDLabel;

	/** The loan status. */
	private Integer loanStatus;

	/** The loan status label. */
	private String loanStatusLabel;

	/** The issue date. */
	private Date issueDate;

	/**
	 * Instantiates a new reporting schedules status DTO.
	 */
	public ReportingSchedulesStatusDTO() {

		/*
		 * EMPTY
		 */
	}

	/**
	 * Instantiates a new reporting schedules status DTO.
	 *
	 * @param accountNumber the account number
	 * @param groupNumber the group number
	 * @param groupName the group name
	 * @param customerNumber the customer number
	 * @param customerName the customer name
	 * @param customerType the customer type
	 * @param customerTypeLabel the customer type label
	 * @param customerGender the customer gender
	 * @param customerGenderLabel the customer gender label
	 * @param repaymentDate the repayment date
	 * @param instalmentAmount the instalment amount
	 * @param instalmentPrincipal the instalment principal
	 * @param instalmentInterest the instalment interest
	 * @param instalmentPrincipalPaid the instalment principal paid
	 * @param instalmentinterestPaid the instalmentinterest paid
	 * @param instalmentTotalPaid the instalment total paid
	 * @param totalPrincipalPaid the total principal paid
	 * @param totalInterestPaid the total interest paid
	 * @param totalPaidAmount the total paid amount
	 * @param unpaidPrincipal the unpaid principal
	 * @param upaidInterest the upaid interest
	 * @param upaidAmount the upaid amount
	 * @param lateDays the late days
	 * @param nbUnpaidinstalment the nb unpaidinstalment
	 * @param remainingPrincipal the remaining principal
	 * @param remainingInterest the remaining interest
	 * @param remainingAmount the remaining amount
	 * @param sourceOffunds the source offunds
	 * @param sourceOffundsLabel the source offunds label
	 * @param branche the branche
	 * @param brancheLabel the branche label
	 * @param loanOfficer the loan officer
	 * @param loanOfficerLabel the loan officer label
	 * @param loanReasonID the loan reason ID
	 * @param loanReasonIDLabel the loan reason ID label
	 * @param productID the product ID
	 * @param productIDLabel the product ID label
	 * @param loanStatus the loan status
	 * @param loanStatusLabel the loan status label
	 * @param issueDate the issue date
	 */
	public ReportingSchedulesStatusDTO(String accountNumber, String groupNumber, String groupName,
			String customerNumber, String customerName, Integer customerType,
			String customerTypeLabel, Integer customerGender, String customerGenderLabel,
			Date repaymentDate, Long instalmentAmount, Long instalmentPrincipal,
			Long instalmentInterest, Long instalmentPrincipalPaid, Long instalmentinterestPaid,
			Long instalmentTotalPaid, Long totalPrincipalPaid, Long totalInterestPaid,
			Long totalPaidAmount, Long unpaidPrincipal, Long upaidInterest, Long upaidAmount,
			Integer lateDays, Integer nbUnpaidinstalment, Long remainingPrincipal,
			Long remainingInterest, Long remainingAmount, Integer sourceOffunds,
			String sourceOffundsLabel, Integer branche, String brancheLabel, Integer loanOfficer,
			String loanOfficerLabel, Integer loanReasonID, String loanReasonIDLabel,
			Integer productID, String productIDLabel, Integer loanStatus, String loanStatusLabel,
			Date issueDate) {

		this.accountNumber = accountNumber;
		this.groupNumber = groupNumber;
		this.groupName = groupName;
		this.customerNumber = customerNumber;
		this.customerName = customerName;
		this.customerType = customerType;
		this.customerTypeLabel = customerTypeLabel;
		this.customerGender = customerGender;
		this.customerGenderLabel = customerGenderLabel;
		this.repaymentDate = repaymentDate;
		this.instalmentAmount = instalmentAmount;
		this.instalmentPrincipal = instalmentPrincipal;
		this.instalmentInterest = instalmentInterest;
		this.instalmentPrincipalPaid = instalmentPrincipalPaid;
		this.instalmentinterestPaid = instalmentinterestPaid;
		this.instalmentTotalPaid = instalmentTotalPaid;
		this.totalPrincipalPaid = totalPrincipalPaid;
		this.totalInterestPaid = totalInterestPaid;
		this.totalPaidAmount = totalPaidAmount;
		this.unpaidPrincipal = unpaidPrincipal;
		this.upaidInterest = upaidInterest;
		this.upaidAmount = upaidAmount;
		this.lateDays = lateDays;
		this.nbUnpaidinstalment = nbUnpaidinstalment;
		this.remainingPrincipal = remainingPrincipal;
		this.remainingInterest = remainingInterest;
		this.remainingAmount = remainingAmount;
		this.sourceOffunds = sourceOffunds;
		this.sourceOffundsLabel = sourceOffundsLabel;
		this.branche = branche;
		this.brancheLabel = brancheLabel;
		this.loanOfficer = loanOfficer;
		this.loanOfficerLabel = loanOfficerLabel;
		this.loanReasonID = loanReasonID;
		this.loanReasonIDLabel = loanReasonIDLabel;
		this.productID = productID;
		this.productIDLabel = productIDLabel;
		this.loanStatus = loanStatus;
		this.loanStatusLabel = loanStatusLabel;
		this.issueDate = issueDate;
	}

	/**
	 * Gets the account number.
	 *
	 * @return the accountNumber
	 */
	public String getAccountNumber() {

		return accountNumber;
	}

	/**
	 * Sets the account number.
	 *
	 * @param accountNumber the accountNumber to set
	 */
	public void setAccountNumber(String accountNumber) {

		this.accountNumber = accountNumber;
	}

	/**
	 * Gets the group number.
	 *
	 * @return the groupNumber
	 */
	public String getGroupNumber() {

		return groupNumber;
	}

	/**
	 * Sets the group number.
	 *
	 * @param groupNumber the groupNumber to set
	 */
	public void setGroupNumber(String groupNumber) {

		this.groupNumber = groupNumber;
	}

	/**
	 * Gets the group name.
	 *
	 * @return the groupName
	 */
	public String getGroupName() {

		return groupName;
	}

	/**
	 * Sets the group name.
	 *
	 * @param groupName the groupName to set
	 */
	public void setGroupName(String groupName) {

		this.groupName = groupName;
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
	 * Gets the customer type.
	 *
	 * @return the customerType
	 */
	public Integer getCustomerType() {

		return customerType;
	}

	/**
	 * Sets the customer type.
	 *
	 * @param customerType the customerType to set
	 */
	public void setCustomerType(Integer customerType) {

		this.customerType = customerType;
	}

	/**
	 * Gets the customer type label.
	 *
	 * @return the customerTypeLabel
	 */
	public String getCustomerTypeLabel() {

		return customerTypeLabel;
	}

	/**
	 * Sets the customer type label.
	 *
	 * @param customerTypeLabel the customerTypeLabel to set
	 */
	public void setCustomerTypeLabel(String customerTypeLabel) {

		this.customerTypeLabel = customerTypeLabel;
	}

	/**
	 * Gets the customer gender.
	 *
	 * @return the customerGender
	 */
	public Integer getCustomerGender() {

		return customerGender;
	}

	/**
	 * Sets the customer gender.
	 *
	 * @param customerGender the customerGender to set
	 */
	public void setCustomerGender(Integer customerGender) {

		this.customerGender = customerGender;
	}

	/**
	 * Gets the customer gender label.
	 *
	 * @return the customerGenderLabel
	 */
	public String getCustomerGenderLabel() {

		return customerGenderLabel;
	}

	/**
	 * Sets the customer gender label.
	 *
	 * @param customerGenderLabel the customerGenderLabel to set
	 */
	public void setCustomerGenderLabel(String customerGenderLabel) {

		this.customerGenderLabel = customerGenderLabel;
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
	 * Gets the instalment amount.
	 *
	 * @return the instalmentAmount
	 */
	public Long getInstalmentAmount() {

		return instalmentAmount;
	}

	/**
	 * Sets the instalment amount.
	 *
	 * @param instalmentAmount the instalmentAmount to set
	 */
	public void setInstalmentAmount(Long instalmentAmount) {

		this.instalmentAmount = instalmentAmount;
	}

	/**
	 * Gets the instalment principal.
	 *
	 * @return the instalmentPrincipal
	 */
	public Long getInstalmentPrincipal() {

		return instalmentPrincipal;
	}

	/**
	 * Sets the instalment principal.
	 *
	 * @param instalmentPrincipal the instalmentPrincipal to set
	 */
	public void setInstalmentPrincipal(Long instalmentPrincipal) {

		this.instalmentPrincipal = instalmentPrincipal;
	}

	/**
	 * Gets the instalment interest.
	 *
	 * @return the instalmentInterest
	 */
	public Long getInstalmentInterest() {

		return instalmentInterest;
	}

	/**
	 * Sets the instalment interest.
	 *
	 * @param instalmentInterest the instalmentInterest to set
	 */
	public void setInstalmentInterest(Long instalmentInterest) {

		this.instalmentInterest = instalmentInterest;
	}

	/**
	 * Gets the instalment principal paid.
	 *
	 * @return the instalmentPrincipalPaid
	 */
	public Long getInstalmentPrincipalPaid() {

		return instalmentPrincipalPaid;
	}

	/**
	 * Sets the instalment principal paid.
	 *
	 * @param instalmentPrincipalPaid the instalmentPrincipalPaid to set
	 */
	public void setInstalmentPrincipalPaid(Long instalmentPrincipalPaid) {

		this.instalmentPrincipalPaid = instalmentPrincipalPaid;
	}

	/**
	 * Gets the instalmentinterest paid.
	 *
	 * @return the instalmentinterestPaid
	 */
	public Long getInstalmentinterestPaid() {

		return instalmentinterestPaid;
	}

	/**
	 * Sets the instalmentinterest paid.
	 *
	 * @param instalmentinterestPaid the instalmentinterestPaid to set
	 */
	public void setInstalmentinterestPaid(Long instalmentinterestPaid) {

		this.instalmentinterestPaid = instalmentinterestPaid;
	}

	/**
	 * Gets the instalment total paid.
	 *
	 * @return the instalmentTotalPaid
	 */
	public Long getInstalmentTotalPaid() {

		return instalmentTotalPaid;
	}

	/**
	 * Sets the instalment total paid.
	 *
	 * @param instalmentTotalPaid the instalmentTotalPaid to set
	 */
	public void setInstalmentTotalPaid(Long instalmentTotalPaid) {

		this.instalmentTotalPaid = instalmentTotalPaid;
	}

	/**
	 * Gets the total principal paid.
	 *
	 * @return the totalPrincipalPaid
	 */
	public Long getTotalPrincipalPaid() {

		return totalPrincipalPaid;
	}

	/**
	 * Sets the total principal paid.
	 *
	 * @param totalPrincipalPaid the totalPrincipalPaid to set
	 */
	public void setTotalPrincipalPaid(Long totalPrincipalPaid) {

		this.totalPrincipalPaid = totalPrincipalPaid;
	}

	/**
	 * Gets the total interest paid.
	 *
	 * @return the totalInterestPaid
	 */
	public Long getTotalInterestPaid() {

		return totalInterestPaid;
	}

	/**
	 * Sets the total interest paid.
	 *
	 * @param totalInterestPaid the totalInterestPaid to set
	 */
	public void setTotalInterestPaid(Long totalInterestPaid) {

		this.totalInterestPaid = totalInterestPaid;
	}

	/**
	 * Gets the total paid amount.
	 *
	 * @return the totalPaidAmount
	 */
	public Long getTotalPaidAmount() {

		return totalPaidAmount;
	}

	/**
	 * Sets the total paid amount.
	 *
	 * @param totalPaidAmount the totalPaidAmount to set
	 */
	public void setTotalPaidAmount(Long totalPaidAmount) {

		this.totalPaidAmount = totalPaidAmount;
	}

	/**
	 * Gets the unpaid principal.
	 *
	 * @return the unpaidPrincipal
	 */
	public Long getUnpaidPrincipal() {

		return unpaidPrincipal;
	}

	/**
	 * Sets the unpaid principal.
	 *
	 * @param unpaidPrincipal the unpaidPrincipal to set
	 */
	public void setUnpaidPrincipal(Long unpaidPrincipal) {

		this.unpaidPrincipal = unpaidPrincipal;
	}

	/**
	 * Gets the upaid interest.
	 *
	 * @return the upaidInterest
	 */
	public Long getUpaidInterest() {

		return upaidInterest;
	}

	/**
	 * Sets the upaid interest.
	 *
	 * @param upaidInterest the upaidInterest to set
	 */
	public void setUpaidInterest(Long upaidInterest) {

		this.upaidInterest = upaidInterest;
	}

	/**
	 * Gets the upaid amount.
	 *
	 * @return the upaidAmount
	 */
	public Long getUpaidAmount() {

		return upaidAmount;
	}

	/**
	 * Sets the upaid amount.
	 *
	 * @param upaidAmount the upaidAmount to set
	 */
	public void setUpaidAmount(Long upaidAmount) {

		this.upaidAmount = upaidAmount;
	}

	/**
	 * Gets the late days.
	 *
	 * @return the lateDays
	 */
	public Integer getLateDays() {

		return lateDays;
	}

	/**
	 * Sets the late days.
	 *
	 * @param lateDays the lateDays to set
	 */
	public void setLateDays(Integer lateDays) {

		this.lateDays = lateDays;
	}

	/**
	 * Gets the nb unpaidinstalment.
	 *
	 * @return the nbUnpaidinstalment
	 */
	public Integer getNbUnpaidinstalment() {

		return nbUnpaidinstalment;
	}

	/**
	 * Sets the nb unpaidinstalment.
	 *
	 * @param nbUnpaidinstalment the nbUnpaidinstalment to set
	 */
	public void setNbUnpaidinstalment(Integer nbUnpaidinstalment) {

		this.nbUnpaidinstalment = nbUnpaidinstalment;
	}

	/**
	 * Gets the remaining principal.
	 *
	 * @return the remainingPrincipal
	 */
	public Long getRemainingPrincipal() {

		return remainingPrincipal;
	}

	/**
	 * Sets the remaining principal.
	 *
	 * @param remainingPrincipal the remainingPrincipal to set
	 */
	public void setRemainingPrincipal(Long remainingPrincipal) {

		this.remainingPrincipal = remainingPrincipal;
	}

	/**
	 * Gets the remaining interest.
	 *
	 * @return the remainingInterest
	 */
	public Long getRemainingInterest() {

		return remainingInterest;
	}

	/**
	 * Sets the remaining interest.
	 *
	 * @param remainingInterest the remainingInterest to set
	 */
	public void setRemainingInterest(Long remainingInterest) {

		this.remainingInterest = remainingInterest;
	}

	/**
	 * Gets the remaining amount.
	 *
	 * @return the remainingAmount
	 */
	public Long getRemainingAmount() {

		return remainingAmount;
	}

	/**
	 * Sets the remaining amount.
	 *
	 * @param remainingAmount the remainingAmount to set
	 */
	public void setRemainingAmount(Long remainingAmount) {

		this.remainingAmount = remainingAmount;
	}

	/**
	 * Gets the source offunds.
	 *
	 * @return the sourceOffunds
	 */
	public Integer getSourceOffunds() {

		return sourceOffunds;
	}

	/**
	 * Sets the source offunds.
	 *
	 * @param sourceOffunds the sourceOffunds to set
	 */
	public void setSourceOffunds(Integer sourceOffunds) {

		this.sourceOffunds = sourceOffunds;
	}

	/**
	 * Gets the source offunds label.
	 *
	 * @return the sourceOffundsLabel
	 */
	public String getSourceOffundsLabel() {

		return sourceOffundsLabel;
	}

	/**
	 * Sets the source offunds label.
	 *
	 * @param sourceOffundsLabel the sourceOffundsLabel to set
	 */
	public void setSourceOffundsLabel(String sourceOffundsLabel) {

		this.sourceOffundsLabel = sourceOffundsLabel;
	}

	/**
	 * Gets the branche.
	 *
	 * @return the branche
	 */
	public Integer getBranche() {

		return branche;
	}

	/**
	 * Sets the branche.
	 *
	 * @param branche the branche to set
	 */
	public void setBranche(Integer branche) {

		this.branche = branche;
	}

	/**
	 * Gets the branche label.
	 *
	 * @return the brancheLabel
	 */
	public String getBrancheLabel() {

		return brancheLabel;
	}

	/**
	 * Sets the branche label.
	 *
	 * @param brancheLabel the brancheLabel to set
	 */
	public void setBrancheLabel(String brancheLabel) {

		this.brancheLabel = brancheLabel;
	}

	/**
	 * Gets the loan officer.
	 *
	 * @return the loanOfficer
	 */
	public Integer getLoanOfficer() {

		return loanOfficer;
	}

	/**
	 * Sets the loan officer.
	 *
	 * @param loanOfficer the loanOfficer to set
	 */
	public void setLoanOfficer(Integer loanOfficer) {

		this.loanOfficer = loanOfficer;
	}

	/**
	 * Gets the loan officer label.
	 *
	 * @return the loanOfficerLabel
	 */
	public String getLoanOfficerLabel() {

		return loanOfficerLabel;
	}

	/**
	 * Sets the loan officer label.
	 *
	 * @param loanOfficerLabel the loanOfficerLabel to set
	 */
	public void setLoanOfficerLabel(String loanOfficerLabel) {

		this.loanOfficerLabel = loanOfficerLabel;
	}

	/**
	 * Gets the loan reason ID.
	 *
	 * @return the loanReasonID
	 */
	public Integer getLoanReasonID() {

		return loanReasonID;
	}

	/**
	 * Sets the loan reason ID.
	 *
	 * @param loanReasonID the loanReasonID to set
	 */
	public void setLoanReasonID(Integer loanReasonID) {

		this.loanReasonID = loanReasonID;
	}

	/**
	 * Gets the loan reason ID label.
	 *
	 * @return the loanReasonIDLabel
	 */
	public String getLoanReasonIDLabel() {

		return loanReasonIDLabel;
	}

	/**
	 * Sets the loan reason ID label.
	 *
	 * @param loanReasonIDLabel the loanReasonIDLabel to set
	 */
	public void setLoanReasonIDLabel(String loanReasonIDLabel) {

		this.loanReasonIDLabel = loanReasonIDLabel;
	}

	/**
	 * Gets the product ID.
	 *
	 * @return the productID
	 */
	public Integer getProductID() {

		return productID;
	}

	/**
	 * Sets the product ID.
	 *
	 * @param productID the productID to set
	 */
	public void setProductID(Integer productID) {

		this.productID = productID;
	}

	/**
	 * Gets the product ID label.
	 *
	 * @return the productIDLabel
	 */
	public String getProductIDLabel() {

		return productIDLabel;
	}

	/**
	 * Sets the product ID label.
	 *
	 * @param productIDLabel the productIDLabel to set
	 */
	public void setProductIDLabel(String productIDLabel) {

		this.productIDLabel = productIDLabel;
	}

	/**
	 * Gets the loan status.
	 *
	 * @return the loanStatus
	 */
	public Integer getLoanStatus() {

		return loanStatus;
	}

	/**
	 * Sets the loan status.
	 *
	 * @param loanStatus the loanStatus to set
	 */
	public void setLoanStatus(Integer loanStatus) {

		this.loanStatus = loanStatus;
	}

	/**
	 * Gets the loan status label.
	 *
	 * @return the loanStatusLabel
	 */
	public String getLoanStatusLabel() {

		return loanStatusLabel;
	}

	/**
	 * Sets the loan status label.
	 *
	 * @param loanStatusLabel the loanStatusLabel to set
	 */
	public void setLoanStatusLabel(String loanStatusLabel) {

		this.loanStatusLabel = loanStatusLabel;
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

}
