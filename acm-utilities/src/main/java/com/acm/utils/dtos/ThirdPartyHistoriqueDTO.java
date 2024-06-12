/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.utils.dtos;

import java.io.Serializable;
import java.util.Date;
import java.util.List;

import com.acm.utils.enums.ThirdPartyCategory;

// TODO: Auto-generated Javadoc
/**
 * {@link ThirdPartyHistoriqueDTO} class.
 *
 * @author HaythemBenizid
 * @since 1.0.11
 */
public class ThirdPartyHistoriqueDTO extends GenericDTO implements Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = -8056726580765934306L;

	/** The id. */
	private Long id;

	/** The id loan. */
	private Long idLoan;

	/** The id customer. */
	private Long idCustomer;

	/** The id customer guarantor. */
	private Long idCustomerGuarantor;

	/** The identity customer. */
	private String identityCustomer;

	/** The identity customer guarantor. */
	private String identityCustomerGuarantor;

	/** The category => {@link ThirdPartyCategory} AML - I-SCORE - KYC. */
	private String category;

	/** The status. */
	private String status;

	/** The request value. */
	private String requestValue;

	/** The response value. */
	private String responseValue;

	/** The date insertion. */
	private Date dateInsertion;

	/** The insert by. */
	private String insertBy;

	/** The score. */
	private Integer score;

	/** The active loans. */
	private Integer activeLoans;

	/** The max num days due. */
	private Integer maxNumDaysDue;

	/** The query date. */
	private Date queryDate;

	/** The repport tag. */
	private String reportTag;

	/** The aml pourcentage. */
	private Integer amlPourcentage;

	/** The total approval amt. */
	private String totalApprovalAmt;

	/** The total monthly installment amt. */
	private String totalMonthlyInstallmentAmt;

	/** The total balance amount. */
	private String totalBalanceAmount;

	/** The status list. */
	private List<String> statusList;

	/** The code color. */
	private String riskLevel;

	/** The search query id. */
	private Long searchQueryId;

	/** The custemer reis id. */
	private Long customerReisId;

	/**
	 * Instantiates a new third party historique DTO.
	 */
	public ThirdPartyHistoriqueDTO() {

		/*
		 * EMPTY
		 */
	}

	/**
	 * Instantiates a new third party historique DTO.
	 *
	 * @param identityCustomer the identity customer
	 * @param identityCustomerGuarantor the identity customer guarantor
	 * @param category the category
	 */
	public ThirdPartyHistoriqueDTO(String identityCustomer, String identityCustomerGuarantor,
			String category) {

		this.identityCustomer = identityCustomer;
		this.identityCustomerGuarantor = identityCustomerGuarantor;
		this.category = category;
	}

	/**
	 * Instantiates a new third party historique DTO.
	 *
	 * @param idLoan the id loan
	 * @param idCustomer the id customer
	 * @param idCustomerGuarantor the id customer guarantor
	 * @param category the category
	 * @param queryDate the query date
	 */
	public ThirdPartyHistoriqueDTO(Long idLoan, Long idCustomer, Long idCustomerGuarantor,
			String category, Date queryDate) {

		this.idLoan = idLoan;
		this.idCustomer = idCustomer;
		this.idCustomerGuarantor = idCustomerGuarantor;
		this.category = category;
		this.queryDate = queryDate;
	}

	/**
	 * Instantiates a new third party historique DTO.
	 *
	 * @param idLoan the id loan
	 * @param idCustomer the id customer
	 * @param idCustomerGuarantor the id customer guarantor
	 * @param category the category
	 * @param status the status
	 * @param requestValue the request value
	 * @param responseValue the response value
	 * @param identityCustomer the identity customer
	 * @param identityCustomerGuarantor the identity customer guarantor
	 */
	public ThirdPartyHistoriqueDTO(Long idLoan, Long idCustomer, Long idCustomerGuarantor,
			String category, String status, String requestValue, String responseValue,
			String identityCustomer, String identityCustomerGuarantor) {

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
	 * Gets the date insertion.
	 *
	 * @return the dateInsertion
	 */
	public Date getDateInsertion() {

		return dateInsertion;
	}

	/**
	 * Sets the date insertion.
	 *
	 * @param dateInsertion the dateInsertion to set
	 */
	public void setDateInsertion(Date dateInsertion) {

		this.dateInsertion = dateInsertion;
	}

	/**
	 * Gets the insert by. S
	 * 
	 * @return the insertBy
	 */
	public String getInsertBy() {

		return insertBy;
	}

	/**
	 * Sets the insert by.
	 *
	 * @param insertBy the insertBy to set
	 */
	public void setInsertBy(String insertBy) {

		this.insertBy = insertBy;
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
	 * Gets the status list.
	 *
	 * @return the statusList
	 */
	public List<String> getStatusList() {

		return statusList;
	}

	/**
	 * Sets the status list.
	 *
	 * @param statusList the statusList to set
	 */
	public void setStatusList(List<String> statusList) {

		this.statusList = statusList;
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
