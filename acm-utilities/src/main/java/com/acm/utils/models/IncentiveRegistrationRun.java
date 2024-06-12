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
 * {@link IncentiveRegistrationRun} class.
 *
 * @author HaythemBenizid
 * @since 1.0.8
 */
@Entity
@Table(name = "ACM_INCENTIVE_RUN_REGISTRATION")
@NamedStoredProcedureQuery(name = "ACM_PROC_INCENTIVE_REGISTRATION",
		procedureName = "ACM_PROC_INCENTIVE_REGISTRATION",
		parameters = {@StoredProcedureParameter(mode = ParameterMode.OUT, name = "toltal",
				type = Integer.class)})
public class IncentiveRegistrationRun implements Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = 3068891812908901009L;

	/** The id. */
	@Id
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	@Column(name = "ID_ACM_INCENTIVE_RUN_REGISTRATION", unique = true, nullable = false)
	private Long id;

	/** The report name. */
	@Column(name = "REPORT_NAME")
	private String reportName;

	/** The loan officer name. */
	@Column(name = "LOAN_OFFICER_NAME")
	private String loanOfficerName;

	/** The user name. */
	@Column(name = "USERNAME")
	private String userName;

	/** The branch. */
	@Column(name = "BRANCH")
	private String branch;

	/** The total customer MEL. */
	@Column(name = "TOTALE_CUSTOMER_MEL")
	private Long totalCustomerMEL;

	/** The new customer MEL. */
	@Column(name = "NEW_CUSTOMER_MEL")
	private Long newCustomerMEL;

	/** The re newal customer MEL. */
	@Column(name = "RENEWAL_CUSTOMER_MEL")
	private Long reNewalCustomerMEL;

	/** The setting incentive new customer MEL. */
	@Column(name = "SETTING_INCENTIVE_NEW_CUSTOMER_MEL")
	private Long settingIncentiveNewCustomerMEL;

	/** The setting incentive re newal customer MEL. */
	@Column(name = "SETTING_INCENTIVE_RENEWAL_CUSTOMER_MEL")
	private Long settingIncentiveReNewalCustomerMEL;

	/** The incentive new customer MEL. */
	@Column(name = "INCENTIVE_NEW_CUSTOMER_MEL")
	private Long incentiveNewCustomerMEL;

	/** The incentive re newal customer MEL. */
	@Column(name = "INCENTIVE_RENEWAL_CUSTOMER_MEL")
	private Long incentiveReNewalCustomerMEL;

	/** The total customer VSE. */
	@Column(name = "TOTALE_CUSTOMER_VSE")
	private Long totalCustomerVSE;

	/** The new customer VSE. */
	@Column(name = "NEW_CUSTOMER_VSE")
	private Long newCustomerVSE;

	/** The re newal customer VSE. */
	@Column(name = "RENEWAL_CUSTOMER_VSE")
	private Long reNewalCustomerVSE;

	/** The setting incentive new customer. */
	@Column(name = "SETTING_INCENTIVE_NEW_CUSTOMER_VSE")
	private Long settingIncentiveNewCustomerVSE;

	/** The setting incentive re newal customer. */
	@Column(name = "SETTING_INCENTIVE_RENEWAL_CUSTOMER_VSE")
	private Long settingIncentiveReNewalCustomerVSE;

	/** The incentive new customer. */
	@Column(name = "INCENTIVE_NEW_CUSTOMER_VSE")
	private Long incentiveNewCustomerVSE;

	/** The incentive re newal customer. */
	@Column(name = "INCENTIVE_RENEWAL_CUSTOMER_VSE")
	private Long incentiveReNewalCustomerVSE;

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
	 * Instantiates a new incentive registration run.
	 */
	public IncentiveRegistrationRun() {

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
	 * Gets the total customer MEL.
	 *
	 * @return the totalCustomerMEL
	 */
	public Long getTotalCustomerMEL() {

		return totalCustomerMEL;
	}

	/**
	 * Sets the total customer MEL.
	 *
	 * @param totalCustomerMEL the totalCustomerMEL to set
	 */
	public void setTotalCustomerMEL(Long totalCustomerMEL) {

		this.totalCustomerMEL = totalCustomerMEL;
	}

	/**
	 * Gets the new customer MEL.
	 *
	 * @return the newCustomerMEL
	 */
	public Long getNewCustomerMEL() {

		return newCustomerMEL;
	}

	/**
	 * Sets the new customer MEL.
	 *
	 * @param newCustomerMEL the newCustomerMEL to set
	 */
	public void setNewCustomerMEL(Long newCustomerMEL) {

		this.newCustomerMEL = newCustomerMEL;
	}

	/**
	 * Gets the re newal customer MEL.
	 *
	 * @return the reNewalCustomerMEL
	 */
	public Long getReNewalCustomerMEL() {

		return reNewalCustomerMEL;
	}

	/**
	 * Sets the re newal customer MEL.
	 *
	 * @param reNewalCustomerMEL the reNewalCustomerMEL to set
	 */
	public void setReNewalCustomerMEL(Long reNewalCustomerMEL) {

		this.reNewalCustomerMEL = reNewalCustomerMEL;
	}

	/**
	 * Gets the setting incentive new customer MEL.
	 *
	 * @return the settingIncentiveNewCustomerMEL
	 */
	public Long getSettingIncentiveNewCustomerMEL() {

		return settingIncentiveNewCustomerMEL;
	}

	/**
	 * Sets the setting incentive new customer MEL.
	 *
	 * @param settingIncentiveNewCustomerMEL the settingIncentiveNewCustomerMEL to set
	 */
	public void setSettingIncentiveNewCustomerMEL(Long settingIncentiveNewCustomerMEL) {

		this.settingIncentiveNewCustomerMEL = settingIncentiveNewCustomerMEL;
	}

	/**
	 * Gets the setting incentive re newal customer MEL.
	 *
	 * @return the settingIncentiveReNewalCustomerMEL
	 */
	public Long getSettingIncentiveReNewalCustomerMEL() {

		return settingIncentiveReNewalCustomerMEL;
	}

	/**
	 * Sets the setting incentive re newal customer MEL.
	 *
	 * @param settingIncentiveReNewalCustomerMEL the settingIncentiveReNewalCustomerMEL to set
	 */
	public void setSettingIncentiveReNewalCustomerMEL(Long settingIncentiveReNewalCustomerMEL) {

		this.settingIncentiveReNewalCustomerMEL = settingIncentiveReNewalCustomerMEL;
	}

	/**
	 * Gets the incentive new customer MEL.
	 *
	 * @return the incentiveNewCustomerMEL
	 */
	public Long getIncentiveNewCustomerMEL() {

		return incentiveNewCustomerMEL;
	}

	/**
	 * Sets the incentive new customer MEL.
	 *
	 * @param incentiveNewCustomerMEL the incentiveNewCustomerMEL to set
	 */
	public void setIncentiveNewCustomerMEL(Long incentiveNewCustomerMEL) {

		this.incentiveNewCustomerMEL = incentiveNewCustomerMEL;
	}

	/**
	 * Gets the incentive re newal customer MEL.
	 *
	 * @return the incentiveReNewalCustomerMEL
	 */
	public Long getIncentiveReNewalCustomerMEL() {

		return incentiveReNewalCustomerMEL;
	}

	/**
	 * Sets the incentive re newal customer MEL.
	 *
	 * @param incentiveReNewalCustomerMEL the incentiveReNewalCustomerMEL to set
	 */
	public void setIncentiveReNewalCustomerMEL(Long incentiveReNewalCustomerMEL) {

		this.incentiveReNewalCustomerMEL = incentiveReNewalCustomerMEL;
	}

	/**
	 * Gets the total customer VSE.
	 *
	 * @return the totalCustomerVSE
	 */
	public Long getTotalCustomerVSE() {

		return totalCustomerVSE;
	}

	/**
	 * Sets the total customer VSE.
	 *
	 * @param totalCustomerVSE the totalCustomerVSE to set
	 */
	public void setTotalCustomerVSE(Long totalCustomerVSE) {

		this.totalCustomerVSE = totalCustomerVSE;
	}

	/**
	 * Gets the new customer VSE.
	 *
	 * @return the newCustomerVSE
	 */
	public Long getNewCustomerVSE() {

		return newCustomerVSE;
	}

	/**
	 * Sets the new customer VSE.
	 *
	 * @param newCustomerVSE the newCustomerVSE to set
	 */
	public void setNewCustomerVSE(Long newCustomerVSE) {

		this.newCustomerVSE = newCustomerVSE;
	}

	/**
	 * Gets the re newal customer VSE.
	 *
	 * @return the reNewalCustomerVSE
	 */
	public Long getReNewalCustomerVSE() {

		return reNewalCustomerVSE;
	}

	/**
	 * Sets the re newal customer VSE.
	 *
	 * @param reNewalCustomerVSE the reNewalCustomerVSE to set
	 */
	public void setReNewalCustomerVSE(Long reNewalCustomerVSE) {

		this.reNewalCustomerVSE = reNewalCustomerVSE;
	}

	/**
	 * Gets the setting incentive new customer VSE.
	 *
	 * @return the settingIncentiveNewCustomerVSE
	 */
	public Long getSettingIncentiveNewCustomerVSE() {

		return settingIncentiveNewCustomerVSE;
	}

	/**
	 * Sets the setting incentive new customer VSE.
	 *
	 * @param settingIncentiveNewCustomerVSE the settingIncentiveNewCustomerVSE to set
	 */
	public void setSettingIncentiveNewCustomerVSE(Long settingIncentiveNewCustomerVSE) {

		this.settingIncentiveNewCustomerVSE = settingIncentiveNewCustomerVSE;
	}

	/**
	 * Gets the setting incentive re newal customer VSE.
	 *
	 * @return the settingIncentiveReNewalCustomerVSE
	 */
	public Long getSettingIncentiveReNewalCustomerVSE() {

		return settingIncentiveReNewalCustomerVSE;
	}

	/**
	 * Sets the setting incentive re newal customer VSE.
	 *
	 * @param settingIncentiveReNewalCustomerVSE the settingIncentiveReNewalCustomerVSE to set
	 */
	public void setSettingIncentiveReNewalCustomerVSE(Long settingIncentiveReNewalCustomerVSE) {

		this.settingIncentiveReNewalCustomerVSE = settingIncentiveReNewalCustomerVSE;
	}

	/**
	 * Gets the incentive new customer VSE.
	 *
	 * @return the incentiveNewCustomerVSE
	 */
	public Long getIncentiveNewCustomerVSE() {

		return incentiveNewCustomerVSE;
	}

	/**
	 * Sets the incentive new customer VSE.
	 *
	 * @param incentiveNewCustomerVSE the incentiveNewCustomerVSE to set
	 */
	public void setIncentiveNewCustomerVSE(Long incentiveNewCustomerVSE) {

		this.incentiveNewCustomerVSE = incentiveNewCustomerVSE;
	}

	/**
	 * Gets the incentive re newal customer VSE.
	 *
	 * @return the incentiveReNewalCustomerVSE
	 */
	public Long getIncentiveReNewalCustomerVSE() {

		return incentiveReNewalCustomerVSE;
	}

	/**
	 * Sets the incentive re newal customer VSE.
	 *
	 * @param incentiveReNewalCustomerVSE the incentiveReNewalCustomerVSE to set
	 */
	public void setIncentiveReNewalCustomerVSE(Long incentiveReNewalCustomerVSE) {

		this.incentiveReNewalCustomerVSE = incentiveReNewalCustomerVSE;
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

		return "IncentiveRegistrationRun [id=" + id + ", reportName=" + reportName
				+ ", loanOfficerName=" + loanOfficerName + ", userName=" + userName + ", branch="
				+ branch + ", totalCustomerMEL=" + totalCustomerMEL + ", newCustomerMEL="
				+ newCustomerMEL + ", reNewalCustomerMEL=" + reNewalCustomerMEL
				+ ", settingIncentiveNewCustomerMEL=" + settingIncentiveNewCustomerMEL
				+ ", settingIncentiveReNewalCustomerMEL=" + settingIncentiveReNewalCustomerMEL
				+ ", incentiveNewCustomerMEL=" + incentiveNewCustomerMEL
				+ ", incentiveReNewalCustomerMEL=" + incentiveReNewalCustomerMEL
				+ ", totalCustomerVSE=" + totalCustomerVSE + ", newCustomerVSE=" + newCustomerVSE
				+ ", reNewalCustomerVSE=" + reNewalCustomerVSE + ", settingIncentiveNewCustomerVSE="
				+ settingIncentiveNewCustomerVSE + ", settingIncentiveReNewalCustomerVSE="
				+ settingIncentiveReNewalCustomerVSE + ", incentiveNewCustomerVSE="
				+ incentiveNewCustomerVSE + ", incentiveReNewalCustomerVSE="
				+ incentiveReNewalCustomerVSE + ", incentiveValue=" + incentiveValue + ", month="
				+ month + ", runDate=" + runDate + "]";
	}

}
