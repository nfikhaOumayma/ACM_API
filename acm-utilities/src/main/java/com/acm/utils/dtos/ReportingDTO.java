/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.utils.dtos;

import java.io.Serializable;
import java.math.BigDecimal;
import java.util.Date;
import java.util.List;

/**
 * {@link ReportingDTO} class.
 *
 * @author HaythemBenizid
 * @since 1.1.1
 */
public class ReportingDTO implements Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = 1089394100664452820L;

	/** The product DT os. */
	private List<ProductDTO> productDTOs;

	/** The branche DT os. */
	private List<BrancheDTO> brancheDTOs;

	/** The user DT os. */
	private List<UserDTO> userDTOs;

	/** The loan amount min. */
	private BigDecimal loanAmountMin;

	/** The loan amount max. */
	private BigDecimal loanAmountMax;

	/** The loan create date min. */
	private Date loanCreateDateMin;

	/** The loan create date max. */
	private Date loanCreateDateMax;

	/** The loan issue date min. */
	private Date loanIssueDateMin;

	/** The loan issue date max. */
	private Date loanIssueDateMax;

	/** The loan status. */
	private List<AcmStatutsDTO> loanStatus;

	/** The customer number. */
	private String customerNumber;

	/** The loan source of funds DT os. */
	private List<LoanSourceOfFundsDTO> loanSourceOfFundsDTOs;

	/** The instalment date min. */
	private Date instalmentDateMin;

	/** The instalment date max. */
	private Date instalmentDateMax;

	/** The group number. */
	private String groupNumber;

	/** The product. */
	private Boolean product;

	/** The branch. */
	private Boolean branch;

	/** The loan officer. */
	private Boolean loanOfficer;

	/** The summary. */
	// Only show total : applied amount / issue amount
	private Boolean summary;

	/** The details. */
	// show all table
	private Boolean details;

	/**
	 * Instantiates a new reporting DTO.
	 */
	public ReportingDTO() {

		/*
		 * EMPTY
		 */
	}

	/**
	 * Gets the product DT os.
	 *
	 * @return the productDTOs
	 */
	public List<ProductDTO> getProductDTOs() {

		return productDTOs;
	}

	/**
	 * Sets the product DT os.
	 *
	 * @param productDTOs the productDTOs to set
	 */
	public void setProductDTOs(List<ProductDTO> productDTOs) {

		this.productDTOs = productDTOs;
	}

	/**
	 * Gets the branche DT os.
	 *
	 * @return the brancheDTOs
	 */
	public List<BrancheDTO> getBrancheDTOs() {

		return brancheDTOs;
	}

	/**
	 * Sets the branche DT os.
	 *
	 * @param brancheDTOs the brancheDTOs to set
	 */
	public void setBrancheDTOs(List<BrancheDTO> brancheDTOs) {

		this.brancheDTOs = brancheDTOs;
	}

	/**
	 * Gets the user DT os.
	 *
	 * @return the userDTOs
	 */
	public List<UserDTO> getUserDTOs() {

		return userDTOs;
	}

	/**
	 * Sets the user DT os.
	 *
	 * @param userDTOs the userDTOs to set
	 */
	public void setUserDTOs(List<UserDTO> userDTOs) {

		this.userDTOs = userDTOs;
	}

	/**
	 * Gets the loan amount min.
	 *
	 * @return the loanAmountMin
	 */
	public BigDecimal getLoanAmountMin() {

		return loanAmountMin;
	}

	/**
	 * Sets the loan amount min.
	 *
	 * @param loanAmountMin the loanAmountMin to set
	 */
	public void setLoanAmountMin(BigDecimal loanAmountMin) {

		this.loanAmountMin = loanAmountMin;
	}

	/**
	 * Gets the loan amount max.
	 *
	 * @return the loanAmountMax
	 */
	public BigDecimal getLoanAmountMax() {

		return loanAmountMax;
	}

	/**
	 * Sets the loan amount max.
	 *
	 * @param loanAmountMax the loanAmountMax to set
	 */
	public void setLoanAmountMax(BigDecimal loanAmountMax) {

		this.loanAmountMax = loanAmountMax;
	}

	/**
	 * Gets the loan create date min.
	 *
	 * @return the loanCreateDateMin
	 */
	public Date getLoanCreateDateMin() {

		return loanCreateDateMin;
	}

	/**
	 * Sets the loan create date min.
	 *
	 * @param loanCreateDateMin the loanCreateDateMin to set
	 */
	public void setLoanCreateDateMin(Date loanCreateDateMin) {

		this.loanCreateDateMin = loanCreateDateMin;
	}

	/**
	 * Gets the loan create date max.
	 *
	 * @return the loanCreateDateMax
	 */
	public Date getLoanCreateDateMax() {

		return loanCreateDateMax;
	}

	/**
	 * Sets the loan create date max.
	 *
	 * @param loanCreateDateMax the loanCreateDateMax to set
	 */
	public void setLoanCreateDateMax(Date loanCreateDateMax) {

		this.loanCreateDateMax = loanCreateDateMax;
	}

	/**
	 * Gets the loan issue date min.
	 *
	 * @return the loanIssueDateMin
	 */
	public Date getLoanIssueDateMin() {

		return loanIssueDateMin;
	}

	/**
	 * Sets the loan issue date min.
	 *
	 * @param loanIssueDateMin the loanIssueDateMin to set
	 */
	public void setLoanIssueDateMin(Date loanIssueDateMin) {

		this.loanIssueDateMin = loanIssueDateMin;
	}

	/**
	 * Gets the loan issue date max.
	 *
	 * @return the loanIssueDateMax
	 */
	public Date getLoanIssueDateMax() {

		return loanIssueDateMax;
	}

	/**
	 * Sets the loan issue date max.
	 *
	 * @param loanIssueDateMax the loanIssueDateMax to set
	 */
	public void setLoanIssueDateMax(Date loanIssueDateMax) {

		this.loanIssueDateMax = loanIssueDateMax;
	}

	/**
	 * Gets the loan status.
	 *
	 * @return the loanStatus
	 */
	public List<AcmStatutsDTO> getLoanStatus() {

		return loanStatus;
	}

	/**
	 * Sets the loan status.
	 *
	 * @param loanStatus the loanStatus to set
	 */
	public void setLoanStatus(List<AcmStatutsDTO> loanStatus) {

		this.loanStatus = loanStatus;
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
	 * Gets the product.
	 *
	 * @return the product
	 */
	public Boolean getProduct() {

		return product;
	}

	/**
	 * Sets the product.
	 *
	 * @param product the product to set
	 */
	public void setProduct(Boolean product) {

		this.product = product;
	}

	/**
	 * Gets the branch.
	 *
	 * @return the branch
	 */
	public Boolean getBranch() {

		return branch;
	}

	/**
	 * Sets the branch.
	 *
	 * @param branch the branch to set
	 */
	public void setBranch(Boolean branch) {

		this.branch = branch;
	}

	/**
	 * Gets the loan officer.
	 *
	 * @return the loanOfficer
	 */
	public Boolean getLoanOfficer() {

		return loanOfficer;
	}

	/**
	 * Sets the loan officer.
	 *
	 * @param loanOfficer the loanOfficer to set
	 */
	public void setLoanOfficer(Boolean loanOfficer) {

		this.loanOfficer = loanOfficer;
	}

	/**
	 * Gets the summary.
	 *
	 * @return the summary
	 */
	public Boolean getSummary() {

		return summary;
	}

	/**
	 * Sets the summary.
	 *
	 * @param summary the summary to set
	 */
	public void setSummary(Boolean summary) {

		this.summary = summary;
	}

	/**
	 * Gets the details.
	 *
	 * @return the details
	 */
	public Boolean getDetails() {

		return details;
	}

	/**
	 * Sets the details.
	 *
	 * @param details the details to set
	 */
	public void setDetails(Boolean details) {

		this.details = details;
	}

	/**
	 * Gets the loan source of funds DT os.
	 *
	 * @return the loanSourceOfFundsDTOs
	 */
	public List<LoanSourceOfFundsDTO> getLoanSourceOfFundsDTOs() {

		return loanSourceOfFundsDTOs;
	}

	/**
	 * Sets the loan source of funds DT os.
	 *
	 * @param loanSourceOfFundsDTOs the loanSourceOfFundsDTOs to set
	 */
	public void setLoanSourceOfFundsDTOs(List<LoanSourceOfFundsDTO> loanSourceOfFundsDTOs) {

		this.loanSourceOfFundsDTOs = loanSourceOfFundsDTOs;
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
	 * Gets the instalment date min.
	 *
	 * @return the instalmentDateMin
	 */
	public Date getInstalmentDateMin() {

		return instalmentDateMin;
	}

	/**
	 * Sets the instalment date min.
	 *
	 * @param instalmentDateMin the instalmentDateMin to set
	 */
	public void setInstalmentDateMin(Date instalmentDateMin) {

		this.instalmentDateMin = instalmentDateMin;
	}

	/**
	 * Gets the instalment date max.
	 *
	 * @return the instalmentDateMax
	 */
	public Date getInstalmentDateMax() {

		return instalmentDateMax;
	}

	/**
	 * Sets the instalment date max.
	 *
	 * @param instalmentDateMax the instalmentDateMax to set
	 */
	public void setInstalmentDateMax(Date instalmentDateMax) {

		this.instalmentDateMax = instalmentDateMax;
	}

}
