/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.utils.models;

import java.io.Serializable;
import java.math.BigDecimal;
import java.util.Date;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.Table;

/**
 * The persistent class for the IB LOAN database table. {@link IbLoan} class.
 * 
 * @author MoezMhiri
 * @since 1.0.3
 */
@Entity
@Table(name = "IB_LOAN")
public class IBLoan extends GenericModel implements Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = -3178948530228815796L;

	/** The id loan. */
	@Id
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	@Column(name = "ID_IB_LOAN", unique = true, nullable = false)
	private Long id;

	/** The id loan extern. */
	@Column(name = "ID_LOAN_EXTERN")
	private Long idLoanExtern;

	/** The portfolio id. */
	@Column(name = "PORTFOLIO_ID")
	private Long portfolioId;

	/** The statut. */
	@Column(name = "STATUT", nullable = false)
	private Integer statut;

	/** The product description. */
	@Column(name = "LOANPRODUCT_DESCRIPTION")
	private String productDescription;

	/** The product code. */
	@Column(name = "LOANPRODUCT_CODE")
	private String productCode;

	/** The product id. */
	@Column(name = "PRODUCT_ID")
	private Integer productId;

	/** The customer name. */
	@Column(name = "CUSTOMER_NAME")
	private String customerName;

	/** The currency symbol. */
	@Column(name = "CURRENCY_SYMBOL")
	private String currencySymbol;

	/** The currency decimal places. */
	@Column(name = "CURRENCY_DECIMALPLACES")
	private Integer currencyDecimalPlaces;

	/** The apply date. */
	@Column(name = "APPLYDATE")
	private Date applyDate;

	/** The account portfolio code. */
	@Column(name = "ACCOUNTPORTFOLIO_CODE")
	private String portfolioCode;

	/** The account portfolio description. */
	@Column(name = "ACCOUNTPORTFOLIO_DESCRIPTION")
	private String portfolioDescription;

	/** The owner. */
	@Column(name = "LOAN_OWNER")
	private String owner;

	/** The owner name. */
	@Column(name = "LOAN_OWNER_NAME")
	private String ownerName;

	/** The note. */
	@Column(name = "LOAN_NOTE", length = 512)
	private String note;

	/** The applyamounttotal. */
	@Column(name = "APPLY_AMOUNT_TOTAL")
	private BigDecimal applyAmountTotal;

	/** The graceperiod. */
	@Column(name = "GRACE_PERIOD")
	private Integer gracePeriod;

	/** The issuedate. */
	@Column(name = "ISSUE_DATE")
	private Date issueDate;

	/** The term period num. */
	@Column(name = "TERM_PERIOD")
	private Integer termPeriodNum;

	/** The payment freq. */
	@Column(name = "PERIOD_FREQ")
	private Integer paymentFreq;

	/** The issue fee amount. */
	@Column(name = "ISSUE_FEE_AMOUNT")
	private BigDecimal issueFeeAmount;

	/** The initial payment date. */
	@Column(name = "INITIAL_PAYMENT_DATE")
	private Date initialPaymentDate;

	/** The normal payment. */
	@Column(name = "NORMAL_PAYMENT")
	private Long normalPayment;

	/** The periods deferred. */
	@Column(name = "PERIODS_DEFERRED")
	private Integer periodsDeferred;

	/** The term period ID. */
	@Column(name = "TERM_PERIOD_ID")
	private Long termPeriodID;

	/** The branchID. */
	@Column(name = "BRANCHID")
	private Integer branchID;

	/** The branch name. */
	@Column(name = "BRANCHE_NAME", length = 50)
	private String branchName;

	/** The branch description. */
	@Column(name = "BRANCHE_DESCRIPTION", length = 200)
	private String branchDescription;

	/** The customer. */
	@ManyToOne(fetch = FetchType.EAGER)
	@JoinColumn(name = "ID_ACM_CUSTOMER")
	private Customer customer;

	/** The loan type. */
	@Column(name = "LOAN_TYPE", length = 50)
	private String loanType;

	/** The Project description. */
	@Column(name = "PROJECT_DESCRIPTION", length = 512)
	private String projectDescription;

 

	/**
	 * Instantiates a new IB loan.
	 */
	public IBLoan() {

		/*
		 * 
		 */
	}

	/**
	 * Gets the portfolio id.
	 *
	 * @return the portfolioId
	 */
	public Long getPortfolioId() {

		return portfolioId;
	}

	/**
	 * Sets the portfolio id.
	 *
	 * @param portfolioId the portfolioId to set
	 */
	public void setPortfolioId(Long portfolioId) {

		this.portfolioId = portfolioId;
	}

	/**
	 * Gets the statut.
	 *
	 * @return the statut
	 */
	public Integer getStatut() {

		return statut;
	}

	/**
	 * Sets the statut.
	 *
	 * @param statut the new statut
	 */
	public void setStatut(Integer statut) {

		this.statut = statut;
	}

	/**
	 * Gets the product description.
	 *
	 * @return the productDescription
	 */
	public String getProductDescription() {

		return productDescription;
	}

	/**
	 * Sets the product description.
	 *
	 * @param productDescription the productDescription to set
	 */
	public void setProductDescription(String productDescription) {

		this.productDescription = productDescription;
	}

	/**
	 * Gets the product code.
	 *
	 * @return the productCode
	 */
	public String getProductCode() {

		return productCode;
	}

	/**
	 * Sets the product code.
	 *
	 * @param productCode the productCode to set
	 */
	public void setProductCode(String productCode) {

		this.productCode = productCode;
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
	 * Gets the currency symbol.
	 *
	 * @return the currencySymbol
	 */
	public String getCurrencySymbol() {

		return currencySymbol;
	}

	/**
	 * Sets the currency symbol.
	 *
	 * @param currencySymbol the currencySymbol to set
	 */
	public void setCurrencySymbol(String currencySymbol) {

		this.currencySymbol = currencySymbol;
	}

	/**
	 * Gets the currency decimal places.
	 *
	 * @return the currencyDecimalPlaces
	 */
	public Integer getCurrencyDecimalPlaces() {

		return currencyDecimalPlaces;
	}

	/**
	 * Sets the currency decimal places.
	 *
	 * @param currencyDecimalPlaces the currencyDecimalPlaces to set
	 */
	public void setCurrencyDecimalPlaces(Integer currencyDecimalPlaces) {

		this.currencyDecimalPlaces = currencyDecimalPlaces;
	}

	/**
	 * Gets the apply date.
	 *
	 * @return the applyDate
	 */
	public Date getApplyDate() {

		return applyDate;
	}

	/**
	 * Sets the apply date.
	 *
	 * @param applyDate the applyDate to set
	 */
	public void setApplyDate(Date applyDate) {

		this.applyDate = applyDate;
	}

	/**
	 * Gets the portfolio code.
	 *
	 * @return the portfolioCode
	 */
	public String getPortfolioCode() {

		return portfolioCode;
	}

	/**
	 * Sets the portfolio code.
	 *
	 * @param portfolioCode the portfolioCode to set
	 */
	public void setPortfolioCode(String portfolioCode) {

		this.portfolioCode = portfolioCode;
	}

	/**
	 * Gets the portfolio description.
	 *
	 * @return the portfolioDescription
	 */
	public String getPortfolioDescription() {

		return portfolioDescription;
	}

	/**
	 * Sets the portfolio description.
	 *
	 * @param portfolioDescription the portfolioDescription to set
	 */
	public void setPortfolioDescription(String portfolioDescription) {

		this.portfolioDescription = portfolioDescription;
	}

	/**
	 * Gets the owner.
	 *
	 * @return the owner
	 */
	public String getOwner() {

		return owner;
	}

	/**
	 * Sets the owner.
	 *
	 * @param owner the owner to set
	 */
	public void setOwner(String owner) {

		this.owner = owner;
	}

	/**
	 * Gets the product id.
	 *
	 * @return the productId
	 */
	public Integer getProductId() {

		return productId;
	}

	/**
	 * Sets the product id.
	 *
	 * @param productId the productId to set
	 */
	public void setProductId(Integer productId) {

		this.productId = productId;
	}

	/**
	 * Gets the note.
	 *
	 * @return the note
	 */
	public String getNote() {

		return note;
	}

	/**
	 * Sets the note.
	 *
	 * @param note the note to set
	 */
	public void setNote(String note) {

		this.note = note;
	}

	/**
	 * Gets the apply amount total.
	 *
	 * @return the applyAmountTotal
	 */
	public BigDecimal getApplyAmountTotal() {

		return applyAmountTotal;
	}

	/**
	 * Sets the apply amount total.
	 *
	 * @param applyAmountTotal the applyAmountTotal to set
	 */
	public void setApplyAmountTotal(BigDecimal applyAmountTotal) {

		this.applyAmountTotal = applyAmountTotal;
	}

	/**
	 * Gets the grace period.
	 *
	 * @return the gracePeriod
	 */
	public Integer getGracePeriod() {

		return gracePeriod;
	}

	/**
	 * Sets the grace period.
	 *
	 * @param gracePeriod the gracePeriod to set
	 */
	public void setGracePeriod(Integer gracePeriod) {

		this.gracePeriod = gracePeriod;
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
	 * Gets the payment freq.
	 *
	 * @return the paymentFreq
	 */
	public Integer getPaymentFreq() {

		return paymentFreq;
	}

	/**
	 * Sets the payment freq.
	 *
	 * @param paymentFreq the paymentFreq to set
	 */
	public void setPaymentFreq(Integer paymentFreq) {

		this.paymentFreq = paymentFreq;
	}

	/**
	 * Gets the issue fee amount.
	 *
	 * @return the issueFeeAmount
	 */
	public BigDecimal getIssueFeeAmount() {

		return issueFeeAmount;
	}

	/**
	 * Sets the issue fee amount.
	 *
	 * @param issueFeeAmount the issueFeeAmount to set
	 */
	public void setIssueFeeAmount(BigDecimal issueFeeAmount) {

		this.issueFeeAmount = issueFeeAmount;
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
	public Long getNormalPayment() {

		return normalPayment;
	}

	/**
	 * Sets the normal payment.
	 *
	 * @param normalPayment the normalPayment to set
	 */
	public void setNormalPayment(Long normalPayment) {

		this.normalPayment = normalPayment;
	}

	/**
	 * Gets the periods deferred.
	 *
	 * @return the periodsDeferred
	 */
	public Integer getPeriodsDeferred() {

		return periodsDeferred;
	}

	/**
	 * Sets the periods deferred.
	 *
	 * @param periodsDeferred the periodsDeferred to set
	 */
	public void setPeriodsDeferred(Integer periodsDeferred) {

		this.periodsDeferred = periodsDeferred;
	}

	/**
	 * Gets the term period ID.
	 *
	 * @return the termPeriodID
	 */
	public Long getTermPeriodID() {

		return termPeriodID;
	}

	/**
	 * Sets the term period ID.
	 *
	 * @param termPeriodID the termPeriodID to set
	 */
	public void setTermPeriodID(Long termPeriodID) {

		this.termPeriodID = termPeriodID;
	}

	/**
	 * Gets the branch ID.
	 * 
	 * @return the branchID
	 */
	public Integer getBranchID() {

		return branchID;
	}

	/**
	 * Sets the branch ID.
	 * 
	 * @param branchID the branchID to set
	 */
	public void setBranchID(Integer branchID) {

		this.branchID = branchID;
	}

	/**
	 * Gets the branch name.
	 * 
	 * @return the branchName
	 */
	public String getBranchName() {

		return branchName;
	}

	/**
	 * Sets the branch name.
	 * 
	 * @param branchName the branchName to set
	 */
	public void setBranchName(String branchName) {

		this.branchName = branchName;
	}

	/**
	 * Gets the branch description.
	 * 
	 * @return the branchDescription
	 */
	public String getBranchDescription() {

		return branchDescription;
	}

	/**
	 * Sets the branch description.
	 * 
	 * @param branchDescription the branchDescription to set
	 */
	public void setBranchDescription(String branchDescription) {

		this.branchDescription = branchDescription;
	}

	/**
	 * Gets the customer.
	 *
	 * @return the customer
	 */
	public Customer getCustomer() {

		return customer;
	}

	/**
	 * Sets the customer.
	 *
	 * @param customer the customer to set
	 */
	public void setCustomer(Customer customer) {

		this.customer = customer;
	}

	/**
	 * Gets the id ib loan.
	 * 
	 * @return the idIbLoan
	 */
	public Long getId() {

		return id;
	}

	/**
	 * Sets the id ib loan.
	 * 
	 * @param idIbLoan the idIbLoan to set
	 */
	public void setId(Long idIbLoan) {

		this.id = idIbLoan;
	}

	/**
	 * Gets the loan type.
	 *
	 * @return the loan type
	 */
	public String getLoanType() {

		return loanType;
	}

	/**
	 * Sets the loan type.
	 *
	 * @param loanType the new loan type
	 */
	public void setLoanType(String loanType) {

		this.loanType = loanType;
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
	 * Gets the owner name.
	 *
	 * @return the owner name
	 */
	public String getOwnerName() {

		return ownerName;
	}

	/**
	 * Sets the owner name.
	 *
	 * @param ownerName the new owner name
	 */
	public void setOwnerName(String ownerName) {

		this.ownerName = ownerName;
	}

	/**
	 * Gets the project description.
	 *
	 * @return the projectDescription
	 */
	public String getProjectDescription() {

		return projectDescription;
	}

	/**
	 * Sets the project description.
	 *
	 * @param projectDescription the projectDescription to set
	 */
	public void setProjectDescription(String projectDescription) {

		this.projectDescription = projectDescription;
	}

	/*
	 * (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {

		return "IBLoan [idIbLoan=" + id + ", idLoanExtern=" + idLoanExtern + ", portfolioId="
				+ portfolioId + ", statut=" + statut + ", productDescription=" + productDescription
				+ ", productCode=" + productCode + ", productId=" + productId + ", customerName="
				+ customerName + ", currencySymbol=" + currencySymbol + ", currencyDecimalPlaces="
				+ currencyDecimalPlaces + ", applyDate=" + applyDate + ", portfolioCode="
				+ portfolioCode + ", portfolioDescription=" + portfolioDescription + ", owner="
				+ owner + ", note=" + note + ", applyAmountTotal=" + applyAmountTotal
				+ ", gracePeriod=" + gracePeriod + ", issueDate=" + issueDate + ", termPeriodNum="
				+ termPeriodNum + ", paymentFreq=" + paymentFreq + ", issueFeeAmount="
				+ issueFeeAmount + ", initialPaymentDate=" + initialPaymentDate + ", normalPayment="
				+ normalPayment + ", periodsDeferred=" + periodsDeferred + ", termPeriodID="
				+ termPeriodID + ", branchID=" + branchID + ", branchName=" + branchName
				+ ", branchDescription=" + branchDescription + ", customer=" + customer
				+ ", loanType=" + loanType + "]";
	}
}
