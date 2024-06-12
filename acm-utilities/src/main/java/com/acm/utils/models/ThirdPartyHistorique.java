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
import javax.persistence.Table;

import com.acm.utils.enums.ThirdPartyCategory;

/**
 * {@link ThirdPartyHistorique} class.
 *
 * @author HaythemBenizid
 * @since 1.0.11
 */
@Entity
@Table(name = "ACM_3RD_PARTY_HISTORIQUE")
public class ThirdPartyHistorique extends GenericModel implements Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = 8513814933200981432L;

	/** The id. */
	@Id
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	@Column(name = "ID_ACM_3RD_PARTY_HISTORIQUE", unique = true, nullable = false)
	private Long id;

	/** The id loan. */
	@Column(name = "ID_LOAN")
	private Long idLoan;

	/** The id customer. */
	@Column(name = "ID_CUSTOMER")
	private Long idCustomer;

	/** The id customer guarantor. */
	@Column(name = "ID_CUSTOMER_GUARANTOR")
	private Long idCustomerGuarantor;

	/** The identity customer. */
	@Column(name = "IDENTITY_CUSTOMER")
	private String identityCustomer;

	/** The identity customer guarantor. */
	@Column(name = "IDENTITY_CUSTOMER_GUARANTOR")
	private String identityCustomerGuarantor;

	/** The category => {@link ThirdPartyCategory} AML - I-SCORE - KYC. */
	@Column(name = "CATEGORY")
	private String category;

	/** The status. */
	@Column(name = "REPONSE_STATUS")
	private String status;

	/** The request value. */
	@Column(name = "REQUEST_VALUE")
	private String requestValue;

	/** The response value. */
	@Column(name = "REPONSE_VALUE")
	private String responseValue;

	// DATA RELATED TO I-SCORE RESPONSE
	/** The score. */
	@Column(name = "SCORE")
	private Integer score;

	/** The active loans. */
	@Column(name = "ACTIVE_LOANS")
	private Integer activeLoans;

	/** The max num days due. */
	@Column(name = "MAX_NUM_DAYS_DUE")
	private Integer maxNumDaysDue;

	/** The query date. */
	@Column(name = "QUERY_DATE")
	private Date queryDate;

	/** The report iscore. */
	@Column(name = "REPORT_ISCORE")
	private byte[] reportIscore;

	/** The repport tag. */
	@Column(name = "REPORT_TAG")
	private String reportTag;

	/** The aml pourcentage. */
	@Column(name = "AML_CHECK_POURCENTAGE")
	private Integer amlPourcentage;

	/** The total approval amt. */
	@Column(name = "TOTAL_APPROVAL_AMT")
	private String totalApprovalAmt;

	/** The total monthly installment amt. */
	@Column(name = "TOTAL_MONTHLY_INSTALLMENT_AMT")
	private String totalMonthlyInstallmentAmt;

	/** The total balance amount. */
	@Column(name = "TOTAL_BALANCE_AMOUNT")
	private String totalBalanceAmount;

	/** The code color. */
	@Column(name = "RISK_LEVEL")
	private String riskLevel;

	/** The search query id. */
	@Column(name = "SEARCH_QUERY_ID")
	private Long searchQueryId;

	/** The customer id reis. */
	@Column(name = "CUSTOMER_ID_REIS")
	private Long customerReisId;

	/**
	 * Instantiates a new third party historique.
	 */
	public ThirdPartyHistorique() {

		/*
		 * EMPTY
		 */
	}

	/**
	 * Instantiates a new third party historique.
	 *
	 * @param idLoan the id loan
	 * @param idCustomer the id customer
	 * @param idCustomerGuarantor the id customer guarantor
	 * @param identityCustomer the identity customer
	 * @param identityCustomerGuarantor the identity customer guarantor
	 * @param category the category
	 * @param status the status
	 * @param requestValue the request value
	 * @param responseValue the response value
	 */
	public ThirdPartyHistorique(Long idLoan, Long idCustomer, Long idCustomerGuarantor,
			String identityCustomer, String identityCustomerGuarantor, String category,
			String status, String requestValue, String responseValue) {

		this.idLoan = idLoan;
		this.idCustomer = idCustomer;
		this.idCustomerGuarantor = idCustomerGuarantor;
		this.identityCustomer = identityCustomer;
		this.identityCustomerGuarantor = identityCustomerGuarantor;
		this.category = category;
		this.status = status;
		this.requestValue = requestValue;
		this.responseValue = responseValue;
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
	 * Gets the id loan.
	 *
	 * @return the idLoan
	 */
	public Long getIdLoan() {

		return idLoan;
	}

	/**
	 * Sets the id loan.
	 *
	 * @param idLoan the idLoan to set
	 */
	public void setIdLoan(Long idLoan) {

		this.idLoan = idLoan;
	}

	/**
	 * Gets the id customer.
	 *
	 * @return the idCustomer
	 */
	public Long getIdCustomer() {

		return idCustomer;
	}

	/**
	 * Sets the id customer.
	 *
	 * @param idCustomer the idCustomer to set
	 */
	public void setIdCustomer(Long idCustomer) {

		this.idCustomer = idCustomer;
	}

	/**
	 * Gets the id customer guarantor.
	 *
	 * @return the idCustomerGuarantor
	 */
	public Long getIdCustomerGuarantor() {

		return idCustomerGuarantor;
	}

	/**
	 * Sets the id customer guarantor.
	 *
	 * @param idCustomerGuarantor the idCustomerGuarantor to set
	 */
	public void setIdCustomerGuarantor(Long idCustomerGuarantor) {

		this.idCustomerGuarantor = idCustomerGuarantor;
	}

	/**
	 * Gets the category.
	 *
	 * @return the category
	 */
	public String getCategory() {

		return category;
	}

	/**
	 * Sets the category.
	 *
	 * @param category the category to set
	 */
	public void setCategory(String category) {

		this.category = category;
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
	 * @param status the status to set
	 */
	public void setStatus(String status) {

		this.status = status;
	}

	/**
	 * Gets the request value.
	 *
	 * @return the requestValue
	 */
	public String getRequestValue() {

		return requestValue;
	}

	/**
	 * Sets the request value.
	 *
	 * @param requestValue the requestValue to set
	 */
	public void setRequestValue(String requestValue) {

		this.requestValue = requestValue;
	}

	/**
	 * Gets the response value.
	 *
	 * @return the responseValue
	 */
	public String getResponseValue() {

		return responseValue;
	}

	/**
	 * Sets the response value.
	 *
	 * @param responseValue the responseValue to set
	 */
	public void setResponseValue(String responseValue) {

		this.responseValue = responseValue;
	}

	/**
	 * Gets the identity customer.
	 *
	 * @return the identityCustomer
	 */
	public String getIdentityCustomer() {

		return identityCustomer;
	}

	/**
	 * Sets the identity customer.
	 *
	 * @param identityCustomer the identityCustomer to set
	 */
	public void setIdentityCustomer(String identityCustomer) {

		this.identityCustomer = identityCustomer;
	}

	/**
	 * Gets the identity customer guarantor.
	 *
	 * @return the identityCustomerGuarantor
	 */
	public String getIdentityCustomerGuarantor() {

		return identityCustomerGuarantor;
	}

	/**
	 * Sets the identity customer guarantor.
	 *
	 * @param identityCustomerGuarantor the identityCustomerGuarantor to set
	 */
	public void setIdentityCustomerGuarantor(String identityCustomerGuarantor) {

		this.identityCustomerGuarantor = identityCustomerGuarantor;
	}

	/**
	 * Gets the score.
	 *
	 * @return the score
	 */
	public Integer getScore() {

		return score;
	}

	/**
	 * Sets the score.
	 *
	 * @param score the score to set
	 */
	public void setScore(Integer score) {

		this.score = score;
	}

	/**
	 * Gets the active loans.
	 *
	 * @return the activeLoans
	 */
	public Integer getActiveLoans() {

		return activeLoans;
	}

	/**
	 * Sets the active loans.
	 *
	 * @param activeLoans the activeLoans to set
	 */
	public void setActiveLoans(Integer activeLoans) {

		this.activeLoans = activeLoans;
	}

	/**
	 * Gets the max num days due.
	 *
	 * @return the maxNumDaysDue
	 */
	public Integer getMaxNumDaysDue() {

		return maxNumDaysDue;
	}

	/**
	 * Sets the max num days due.
	 *
	 * @param maxNumDaysDue the maxNumDaysDue to set
	 */
	public void setMaxNumDaysDue(Integer maxNumDaysDue) {

		this.maxNumDaysDue = maxNumDaysDue;
	}

	/**
	 * Gets the query date.
	 *
	 * @return the queryDate
	 */
	public Date getQueryDate() {

		return queryDate;
	}

	/**
	 * Sets the query date.
	 *
	 * @param queryDate the queryDate to set
	 */
	public void setQueryDate(Date queryDate) {

		this.queryDate = queryDate;
	}

	/**
	 * Gets the report iscore.
	 *
	 * @return the reportIscore
	 */
	public byte[] getReportIscore() {

		return reportIscore;
	}

	/**
	 * Sets the report iscore.
	 *
	 * @param reportIscore the reportIscore to set
	 */
	public void setReportIscore(byte[] reportIscore) {

		this.reportIscore = reportIscore;
	}

	/**
	 * Gets the report tag.
	 *
	 * @return the reportTag
	 */
	public String getReportTag() {

		return reportTag;
	}

	/**
	 * Sets the report tag.
	 *
	 * @param reportTag the reportTag to set
	 */
	public void setReportTag(String reportTag) {

		this.reportTag = reportTag;
	}

	/**
	 * Gets the aml pourcentage.
	 *
	 * @return the amlPourcentage
	 */
	public Integer getAmlPourcentage() {

		return amlPourcentage;
	}

	/**
	 * Sets the aml pourcentage.
	 *
	 * @param amlPourcentage the amlPourcentage to set
	 */
	public void setAmlPourcentage(Integer amlPourcentage) {

		this.amlPourcentage = amlPourcentage;
	}

	/**
	 * Gets the total approval amt.
	 *
	 * @return the totalApprovalAmt
	 */
	public String getTotalApprovalAmt() {

		return totalApprovalAmt;
	}

	/**
	 * Sets the total approval amt.
	 *
	 * @param totalApprovalAmt the totalApprovalAmt to set
	 */
	public void setTotalApprovalAmt(String totalApprovalAmt) {

		this.totalApprovalAmt = totalApprovalAmt;
	}

	/**
	 * Gets the total monthly installment amt.
	 *
	 * @return the totalMonthlyInstallmentAmt
	 */
	public String getTotalMonthlyInstallmentAmt() {

		return totalMonthlyInstallmentAmt;
	}

	/**
	 * Sets the total monthly installment amt.
	 *
	 * @param totalMonthlyInstallmentAmt the totalMonthlyInstallmentAmt to set
	 */
	public void setTotalMonthlyInstallmentAmt(String totalMonthlyInstallmentAmt) {

		this.totalMonthlyInstallmentAmt = totalMonthlyInstallmentAmt;
	}

	/**
	 * Gets the total balance amount.
	 *
	 * @return the totalBalanceAmount
	 */
	public String getTotalBalanceAmount() {

		return totalBalanceAmount;
	}

	/**
	 * Sets the total balance amount.
	 *
	 * @param totalBalanceAmount the totalBalanceAmount to set
	 */
	public void setTotalBalanceAmount(String totalBalanceAmount) {

		this.totalBalanceAmount = totalBalanceAmount;
	}

	/**
	 * Gets the risk level.
	 *
	 * @return the risk level
	 */
	public String getRiskLevel() {

		return riskLevel;
	}

	/**
	 * Sets the risk level.
	 *
	 * @param riskLevel the new risk level
	 */
	public void setRiskLevel(String riskLevel) {

		this.riskLevel = riskLevel;
	}

	/**
	 * Gets the search query id.
	 *
	 * @return the search query id
	 */
	public Long getSearchQueryId() {

		return searchQueryId;
	}

	/**
	 * Sets the search query id.
	 *
	 * @param searchQueryId the new search query id
	 */
	public void setSearchQueryId(Long searchQueryId) {

		this.searchQueryId = searchQueryId;
	}

	/**
	 * Gets the customer reis id.
	 *
	 * @return the customer reis id
	 */
	public Long getCustomerReisId() {

		return customerReisId;
	}

	/**
	 * Sets the customer reis id.
	 *
	 * @param customerReisId the new customer reis id
	 */
	public void setCustomerReisId(Long customerReisId) {

		this.customerReisId = customerReisId;
	}

}
