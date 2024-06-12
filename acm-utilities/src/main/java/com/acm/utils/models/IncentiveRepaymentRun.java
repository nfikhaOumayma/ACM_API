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
 * {@link IncentiveRepaymentRun} class.
 *
 * @author HaythemBenizid
 * @since 1.0.8
 */
@Entity
@Table(name = "ACM_INCENTIVE_RUN_REPAYMENT")
@NamedStoredProcedureQuery(name = "ACM_PROC_INCENTIVE_REPAYMENT_INSUANCE",
		procedureName = "ACM_PROC_INCENTIVE_REPAYMENT_INSUANCE",
		parameters = {@StoredProcedureParameter(mode = ParameterMode.OUT,
				name = "toltalLoan_issued", type = Integer.class)})
public class IncentiveRepaymentRun implements Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = -3942672395495239777L;

	/** The id. */
	@Id
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	@Column(name = "ID_ACM_INCENTIVE_RUN_REPAYMENT", unique = true, nullable = false)
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

	/** The loan supervisor. */
	@Column(name = "LOAN_SUPERVISOR")
	private String loanSupervisor;

	/** The loan branch manger. */
	@Column(name = "LOAN_BRANCH_MANAGER")
	private String loanBranchManger;

	/** The user name. */
	@Column(name = "USERNAME")
	private String userName;

	/** The branch. */
	@Column(name = "BRANCH")
	private String branch;

	/** The product category id. */
	@Column(name = "PRODUCT_CATEGORY_ID")
	private Long productCategoryId;

	/** The product list ids. */
	@Column(name = "PRODUCT_LIST_ID")
	private String productListIds;

	/** The product category description. */
	@Column(name = "PRODUCT_CATEGORY_DESCRIPTION")
	private String productCategoryDescription;

	/** The active customer. */
	@Column(name = "ACTIVE_CUSTOMER")
	private Long activeCustomer;

	/** The total loan amount. */
	@Column(name = "TOTALE_LOAN_AMOUNT")
	private Long totalLoanAmount;

	/** The issue loan month. */
	@Column(name = "ISSUE_LOAN_MONTH")
	private Long issueLoanMonth;

	/** The month. */
	@Column(name = "MONTH")
	private String month;

	/** The balance paid. */
	@Column(name = "BALANCE_PAID")
	private Long balancePaid;

	/** The balance not paid. */
	@Column(name = "BALANCE_NOT_PAID")
	private Long balanceNotPaid;

	/** The risk. */
	@Column(name = "RISK")
	private Float risk;

	/** The repayment paid. */
	@Column(name = "REPAYMENT_PAID")
	private Long repaymentPaid;

	/** The repayment not paid. */
	@Column(name = "REPAYMENT_NOT_PAID")
	private Long repaymentNotPaid;

	/** The repayment rate. */
	@Column(name = "REPAYMENT_RATE")
	private Long repaymentRate;

	/** The incentive value setting. */
	@Column(name = "INCENTIVE_VALUE_SETTING")
	private Long incentiveValueSetting;

	/** The based on. */
	@Column(name = "BASED_ON")
	private String basedOn;

	/** The productivity. */
	@Column(name = "PRODUCTIVITY")
	private Boolean productivity;

	/** The discount. */
	@Column(name = "DISCOUNT")
	private Boolean discount;

	/** The no discount value. */
	@Column(name = "INCENTIVE_VALUE_NO_DISCOUNT")
	private Long noDiscountValue;

	/** The incentive value. */
	@Column(name = "INCENTIVE_VALUE")
	private Long incentiveValue;

	/** The run date. */
	@Column(name = "RUN_DATE")
	private Date runDate;

	/**
	 * Instantiates a new incentive repayment run.
	 */
	public IncentiveRepaymentRun() {

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
	 * Gets the loan supervisor.
	 *
	 * @return the loanSupervisor
	 */
	public String getLoanSupervisor() {

		return loanSupervisor;
	}

	/**
	 * Sets the loan supervisor.
	 *
	 * @param loanSupervisor the loanSupervisor to set
	 */
	public void setLoanSupervisor(String loanSupervisor) {

		this.loanSupervisor = loanSupervisor;
	}

	/**
	 * Gets the loan branch manger.
	 *
	 * @return the loanBranchManger
	 */
	public String getLoanBranchManger() {

		return loanBranchManger;
	}

	/**
	 * Sets the loan branch manger.
	 *
	 * @param loanBranchManger the loanBranchManger to set
	 */
	public void setLoanBranchManger(String loanBranchManger) {

		this.loanBranchManger = loanBranchManger;
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
	 * Gets the product category id.
	 *
	 * @return the productCategoryId
	 */
	public Long getProductCategoryId() {

		return productCategoryId;
	}

	/**
	 * Sets the product category id.
	 *
	 * @param productCategoryId the productCategoryId to set
	 */
	public void setProductCategoryId(Long productCategoryId) {

		this.productCategoryId = productCategoryId;
	}

	/**
	 * Gets the product list ids.
	 *
	 * @return the productListIds
	 */
	public String getProductListIds() {

		return productListIds;
	}

	/**
	 * Sets the product list ids.
	 *
	 * @param productListIds the productListIds to set
	 */
	public void setProductListIds(String productListIds) {

		this.productListIds = productListIds;
	}

	/**
	 * Gets the product category description.
	 *
	 * @return the productCategoryDescription
	 */
	public String getProductCategoryDescription() {

		return productCategoryDescription;
	}

	/**
	 * Sets the product category description.
	 *
	 * @param productCategoryDescription the productCategoryDescription to set
	 */
	public void setProductCategoryDescription(String productCategoryDescription) {

		this.productCategoryDescription = productCategoryDescription;
	}

	/**
	 * Gets the active customer.
	 *
	 * @return the activeCustomer
	 */
	public Long getActiveCustomer() {

		return activeCustomer;
	}

	/**
	 * Sets the active customer.
	 *
	 * @param activeCustomer the activeCustomer to set
	 */
	public void setActiveCustomer(Long activeCustomer) {

		this.activeCustomer = activeCustomer;
	}

	/**
	 * Gets the total loan amount.
	 *
	 * @return the totalLoanAmount
	 */
	public Long getTotalLoanAmount() {

		return totalLoanAmount;
	}

	/**
	 * Sets the total loan amount.
	 *
	 * @param totalLoanAmount the totalLoanAmount to set
	 */
	public void setTotalLoanAmount(Long totalLoanAmount) {

		this.totalLoanAmount = totalLoanAmount;
	}

	/**
	 * Gets the issue loan month.
	 *
	 * @return the issueLoanMonth
	 */
	public Long getIssueLoanMonth() {

		return issueLoanMonth;
	}

	/**
	 * Sets the issue loan month.
	 *
	 * @param issueLoanMonth the issueLoanMonth to set
	 */
	public void setIssueLoanMonth(Long issueLoanMonth) {

		this.issueLoanMonth = issueLoanMonth;
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
	 * Gets the balance paid.
	 *
	 * @return the balancePaid
	 */
	public Long getBalancePaid() {

		return balancePaid;
	}

	/**
	 * Sets the balance paid.
	 *
	 * @param balancePaid the balancePaid to set
	 */
	public void setBalancePaid(Long balancePaid) {

		this.balancePaid = balancePaid;
	}

	/**
	 * Gets the balance not paid.
	 *
	 * @return the balanceNotPaid
	 */
	public Long getBalanceNotPaid() {

		return balanceNotPaid;
	}

	/**
	 * Sets the balance not paid.
	 *
	 * @param balanceNotPaid the balanceNotPaid to set
	 */
	public void setBalanceNotPaid(Long balanceNotPaid) {

		this.balanceNotPaid = balanceNotPaid;
	}

	/**
	 * Gets the risk.
	 *
	 * @return the risk
	 */
	public Float getRisk() {

		return risk;
	}

	/**
	 * Sets the risk.
	 *
	 * @param risk the risk to set
	 */
	public void setRisk(Float risk) {

		this.risk = risk;
	}

	/**
	 * Gets the repayment paid.
	 *
	 * @return the repaymentPaid
	 */
	public Long getRepaymentPaid() {

		return repaymentPaid;
	}

	/**
	 * Sets the repayment paid.
	 *
	 * @param repaymentPaid the repaymentPaid to set
	 */
	public void setRepaymentPaid(Long repaymentPaid) {

		this.repaymentPaid = repaymentPaid;
	}

	/**
	 * Gets the repayment not paid.
	 *
	 * @return the repaymentNotPaid
	 */
	public Long getRepaymentNotPaid() {

		return repaymentNotPaid;
	}

	/**
	 * Sets the repayment not paid.
	 *
	 * @param repaymentNotPaid the repaymentNotPaid to set
	 */
	public void setRepaymentNotPaid(Long repaymentNotPaid) {

		this.repaymentNotPaid = repaymentNotPaid;
	}

	/**
	 * Gets the repayment rate.
	 *
	 * @return the repaymentRate
	 */
	public Long getRepaymentRate() {

		return repaymentRate;
	}

	/**
	 * Sets the repayment rate.
	 *
	 * @param repaymentRate the repaymentRate to set
	 */
	public void setRepaymentRate(Long repaymentRate) {

		this.repaymentRate = repaymentRate;
	}

	/**
	 * Gets the incentive value setting.
	 *
	 * @return the incentiveValueSetting
	 */
	public Long getIncentiveValueSetting() {

		return incentiveValueSetting;
	}

	/**
	 * Sets the incentive value setting.
	 *
	 * @param incentiveValueSetting the incentiveValueSetting to set
	 */
	public void setIncentiveValueSetting(Long incentiveValueSetting) {

		this.incentiveValueSetting = incentiveValueSetting;
	}

	/**
	 * Gets the based on.
	 *
	 * @return the basedOn
	 */
	public String getBasedOn() {

		return basedOn;
	}

	/**
	 * Sets the based on.
	 *
	 * @param basedOn the basedOn to set
	 */
	public void setBasedOn(String basedOn) {

		this.basedOn = basedOn;
	}

	/**
	 * Gets the productivity.
	 *
	 * @return the productivity
	 */
	public Boolean getProductivity() {

		return productivity;
	}

	/**
	 * Sets the productivity.
	 *
	 * @param productivity the productivity to set
	 */
	public void setProductivity(Boolean productivity) {

		this.productivity = productivity;
	}

	/**
	 * Gets the discount.
	 *
	 * @return the discount
	 */
	public Boolean getDiscount() {

		return discount;
	}

	/**
	 * Sets the discount.
	 *
	 * @param discount the discount to set
	 */
	public void setDiscount(Boolean discount) {

		this.discount = discount;
	}

	/**
	 * Gets the no discount value.
	 *
	 * @return the noDiscountValue
	 */
	public Long getNoDiscountValue() {

		return noDiscountValue;
	}

	/**
	 * Sets the no discount value.
	 *
	 * @param noDiscountValue the noDiscountValue to set
	 */
	public void setNoDiscountValue(Long noDiscountValue) {

		this.noDiscountValue = noDiscountValue;
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

		return "IncentiveRepaymentRun [" + (id != null ? "id=" + id + ", " : "")
				+ (reportName != null ? "reportName=" + reportName + ", " : "")
				+ (role != null ? "role=" + role + ", " : "")
				+ (loanOfficerName != null ? "loanOfficerName=" + loanOfficerName + ", " : "")
				+ (loanSupervisor != null ? "loanSupervisor=" + loanSupervisor + ", " : "")
				+ (loanBranchManger != null ? "loanBranchManger=" + loanBranchManger + ", " : "")
				+ (userName != null ? "userName=" + userName + ", " : "")
				+ (branch != null ? "branch=" + branch + ", " : "")
				+ (productCategoryId != null ? "productCategoryId=" + productCategoryId + ", " : "")
				+ (productListIds != null ? "productListIds=" + productListIds + ", " : "")
				+ (productCategoryDescription != null
						? "productCategoryDescription=" + productCategoryDescription + ", "
						: "")
				+ (activeCustomer != null ? "activeCustomer=" + activeCustomer + ", " : "")
				+ (totalLoanAmount != null ? "totalLoanAmount=" + totalLoanAmount + ", " : "")
				+ (issueLoanMonth != null ? "issueLoanMonth=" + issueLoanMonth + ", " : "")
				+ (month != null ? "month=" + month + ", " : "")
				+ (balancePaid != null ? "balancePaid=" + balancePaid + ", " : "")
				+ (balanceNotPaid != null ? "balanceNotPaid=" + balanceNotPaid + ", " : "")
				+ (risk != null ? "risk=" + risk + ", " : "")
				+ (repaymentPaid != null ? "repaymentPaid=" + repaymentPaid + ", " : "")
				+ (repaymentNotPaid != null ? "repaymentNotPaid=" + repaymentNotPaid + ", " : "")
				+ (repaymentRate != null ? "repaymentRate=" + repaymentRate + ", " : "")
				+ (incentiveValueSetting != null
						? "incentiveValueSetting=" + incentiveValueSetting + ", "
						: "")
				+ (basedOn != null ? "basedOn=" + basedOn + ", " : "")
				+ (productivity != null ? "productivity=" + productivity + ", " : "")
				+ (discount != null ? "discount=" + discount + ", " : "")
				+ (noDiscountValue != null ? "noDiscountValue=" + noDiscountValue + ", " : "")
				+ (incentiveValue != null ? "incentiveValue=" + incentiveValue + ", " : "")
				+ (runDate != null ? "runDate=" + runDate : "") + "]";
	}

}
