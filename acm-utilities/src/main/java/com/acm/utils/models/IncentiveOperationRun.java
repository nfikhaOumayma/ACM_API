/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.utils.models;

import java.io.Serializable;
import java.util.Date;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.NamedStoredProcedureQuery;
import javax.persistence.ParameterMode;
import javax.persistence.StoredProcedureParameter;
import javax.persistence.Table;

/**
 * {@link IncentiveOperationRun} class.
 *
 * @author HaythemBenizid
 * @since 1.0.8
 */
@Entity
@Table(name = "ACM_INCENTIVE_RUN_OPERATION")
@NamedStoredProcedureQuery(name = "ACM_PROC_INCENTIVE_OPERATION",
		procedureName = "ACM_PROC_INCENTIVE_OPERATION",
		parameters = {@StoredProcedureParameter(mode = ParameterMode.OUT, name = "toltal",
				type = Integer.class)})
public class IncentiveOperationRun implements Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = 4417268719848853870L;

	/** The id. */
	@Id
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	@Column(name = "ID_ACM_INCENTIVE_RUN_OPERATION", unique = true, nullable = false)
	private Long id;

	/** The report name. */
	@Column(name = "REPORT_NAME")
	private String reportName;

	/** The role. */
	@Column(name = "ROLE")
	private String role;

	/** The loan officer name. */
	@Column(name = "LOAN_OFFICER_NAME")
	private String loanOfficerName;

	/** The user name. */
	@Column(name = "USERNAME")
	private String userName;

	/** The branch. */
	@Column(name = "BRANCH")
	private String branch;

	/** The total loan amount. */
	@Column(name = "TOTALE_LOAN_AMOUNT_MEL")
	private Long totalLoanAmountMEL;

	/** The issue loan month. */
	@Column(name = "ISSUE_LOAN_MONTH_MEL")
	private Long issueLoanMonthMEL;

	/** The incentive type. */
	@Column(name = "INCENTIVE_TYPE_MEL")
	private String incentiveTypeMEL;

	/** The total loan amount. */
	@Column(name = "TOTALE_LOAN_AMOUNT_VSE")
	private Long totalLoanAmountVSE;

	/** The issue loan month. */
	@Column(name = "ISSUE_LOAN_MONTH_VSE")
	private Long issueLoanMonthVSE;

	/** The incentive type. */
	@Column(name = "INCENTIVE_TYPE_VSE")
	private String incentiveTypeVSE;

	/** The incentive value. */
	@Column(name = "INCENTIVE_VALUE")
	private Long incentiveValue;

	/** The month. */
	@Column(name = "MONTH")
	private String month;

	/** The run date. */
	@Column(name = "RUN_DATE")
	private Date runDate;

	/**
	 * Instantiates a new incentive operation run.
	 */
	public IncentiveOperationRun() {

		/*
		 * EMPTY
		 */
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
	 * @param id the id to set
	 */
	public void setId(Long id) {

		this.id = id;
	}

	/**
	 * Gets the report name.
	 *
	 * @return the reportName
	 */
	public String getReportName() {

		return reportName;
	}

	/**
	 * Sets the report name.
	 *
	 * @param reportName the reportName to set
	 */
	public void setReportName(String reportName) {

		this.reportName = reportName;
	}

	/**
	 * Gets the role.
	 *
	 * @return the role
	 */
	public String getRole() {

		return role;
	}

	/**
	 * Sets the role.
	 *
	 * @param role the role to set
	 */
	public void setRole(String role) {

		this.role = role;
	}

	/**
	 * Gets the loan officer name.
	 *
	 * @return the loanOfficerName
	 */
	public String getLoanOfficerName() {

		return loanOfficerName;
	}

	/**
	 * Sets the loan officer name.
	 *
	 * @param loanOfficerName the loanOfficerName to set
	 */
	public void setLoanOfficerName(String loanOfficerName) {

		this.loanOfficerName = loanOfficerName;
	}

	/**
	 * Gets the user name.
	 *
	 * @return the userName
	 */
	public String getUserName() {

		return userName;
	}

	/**
	 * Sets the user name.
	 *
	 * @param userName the userName to set
	 */
	public void setUserName(String userName) {

		this.userName = userName;
	}

	/**
	 * Gets the branch.
	 *
	 * @return the branch
	 */
	public String getBranch() {

		return branch;
	}

	/**
	 * Sets the branch.
	 *
	 * @param branch the branch to set
	 */
	public void setBranch(String branch) {

		this.branch = branch;
	}

	/**
	 * Gets the total loan amount MEL.
	 *
	 * @return the totalLoanAmountMEL
	 */
	public Long getTotalLoanAmountMEL() {

		return totalLoanAmountMEL;
	}

	/**
	 * Sets the total loan amount MEL.
	 *
	 * @param totalLoanAmountMEL the totalLoanAmountMEL to set
	 */
	public void setTotalLoanAmountMEL(Long totalLoanAmountMEL) {

		this.totalLoanAmountMEL = totalLoanAmountMEL;
	}

	/**
	 * Gets the issue loan month MEL.
	 *
	 * @return the issueLoanMonthMEL
	 */
	public Long getIssueLoanMonthMEL() {

		return issueLoanMonthMEL;
	}

	/**
	 * Sets the issue loan month MEL.
	 *
	 * @param issueLoanMonthMEL the issueLoanMonthMEL to set
	 */
	public void setIssueLoanMonthMEL(Long issueLoanMonthMEL) {

		this.issueLoanMonthMEL = issueLoanMonthMEL;
	}

	/**
	 * Gets the incentive type MEL.
	 *
	 * @return the incentiveTypeMEL
	 */
	public String getIncentiveTypeMEL() {

		return incentiveTypeMEL;
	}

	/**
	 * Sets the incentive type MEL.
	 *
	 * @param incentiveTypeMEL the incentiveTypeMEL to set
	 */
	public void setIncentiveTypeMEL(String incentiveTypeMEL) {

		this.incentiveTypeMEL = incentiveTypeMEL;
	}

	/**
	 * Gets the total loan amount VSE.
	 *
	 * @return the totalLoanAmountVSE
	 */
	public Long getTotalLoanAmountVSE() {

		return totalLoanAmountVSE;
	}

	/**
	 * Sets the total loan amount VSE.
	 *
	 * @param totalLoanAmountVSE the totalLoanAmountVSE to set
	 */
	public void setTotalLoanAmountVSE(Long totalLoanAmountVSE) {

		this.totalLoanAmountVSE = totalLoanAmountVSE;
	}

	/**
	 * Gets the issue loan month VSE.
	 *
	 * @return the issueLoanMonthVSE
	 */
	public Long getIssueLoanMonthVSE() {

		return issueLoanMonthVSE;
	}

	/**
	 * Sets the issue loan month VSE.
	 *
	 * @param issueLoanMonthVSE the issueLoanMonthVSE to set
	 */
	public void setIssueLoanMonthVSE(Long issueLoanMonthVSE) {

		this.issueLoanMonthVSE = issueLoanMonthVSE;
	}

	/**
	 * Gets the incentive type VSE.
	 *
	 * @return the incentiveTypeVSE
	 */
	public String getIncentiveTypeVSE() {

		return incentiveTypeVSE;
	}

	/**
	 * Sets the incentive type VSE.
	 *
	 * @param incentiveTypeVSE the incentiveTypeVSE to set
	 */
	public void setIncentiveTypeVSE(String incentiveTypeVSE) {

		this.incentiveTypeVSE = incentiveTypeVSE;
	}

	/**
	 * Gets the incentive value.
	 *
	 * @return the incentiveValue
	 */
	public Long getIncentiveValue() {

		return incentiveValue;
	}

	/**
	 * Sets the incentive value.
	 *
	 * @param incentiveValue the incentiveValue to set
	 */
	public void setIncentiveValue(Long incentiveValue) {

		this.incentiveValue = incentiveValue;
	}

	/**
	 * Gets the month.
	 *
	 * @return the month
	 */
	public String getMonth() {

		return month;
	}

	/**
	 * Sets the month.
	 *
	 * @param month the month to set
	 */
	public void setMonth(String month) {

		this.month = month;
	}

	/**
	 * Gets the run date.
	 *
	 * @return the runDate
	 */
	public Date getRunDate() {

		return runDate;
	}

	/**
	 * Sets the run date.
	 *
	 * @param runDate the runDate to set
	 */
	public void setRunDate(Date runDate) {

		this.runDate = runDate;
	}

	/*
	 * (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {

		return "IncentiveOperationRun [id=" + id + ", reportName=" + reportName + ", role=" + role
				+ ", loanOfficerName=" + loanOfficerName + ", userName=" + userName + ", branch="
				+ branch + ", totalLoanAmountMEL=" + totalLoanAmountMEL + ", issueLoanMonthMEL="
				+ issueLoanMonthMEL + ", incentiveTypeMEL=" + incentiveTypeMEL
				+ ", totalLoanAmountVSE=" + totalLoanAmountVSE + ", issueLoanMonthVSE="
				+ issueLoanMonthVSE + ", incentiveTypeVSE=" + incentiveTypeVSE + ", incentiveValue="
				+ incentiveValue + ", month=" + month + ", runDate=" + runDate + "]";
	}
}
