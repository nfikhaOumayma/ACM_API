/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.utils.models;

import java.io.Serializable;
import java.math.BigDecimal;
import java.util.Date;
import java.util.HashSet;
import java.util.Set;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.EntityListeners;
import javax.persistence.FetchType;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.NamedQuery;
import javax.persistence.OneToMany;
import javax.persistence.Table;
import javax.persistence.Transient;

import com.acm.utils.audit.AuditTrailListener;

/**
 * The persistent class for the LOAN database table. {@link Loan} class.
 * 
 * @author HaythemBenizid
 * @since 0.1.0
 */
@Entity
@Table(name = "ACM_LOAN")
@NamedQuery(name = "Loan.findAll", query = "SELECT l FROM Loan l")
@EntityListeners(AuditTrailListener.class)
public class Loan extends GenericModel implements Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = 4508256347060684391L;

	/** The id loan. */
	@Id
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	@Column(name = "ID_ACM_LOAN", unique = true, nullable = false)
	private Long idLoan;

	/** The id loan extern. */
	@Column(name = "ID_LOAN_EXTERN", nullable = false)
	private Long idLoanExtern;

	/** The id account extern. */
	@Column(name = "ID_ACCOUNT_EXTERN")
	private Long idAccountExtern;

	/** The account number extern. */
	@Column(name = "ACCOUNT_NUMBER_EXTERN")
	private String accountNumberExtern;

	/** The process instance id. */
	@Column(name = "PROCESS_INSTANCE_ID", length = 256)
	private String processInstanceId;

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

	/** The id customer. */
	@Column(name = "CUSTOMER_ID")
	private Long customerId;

	/** The customer name. */
	@Column(name = "CUSTOMER_NAME")
	private String customerName;

	/** The applyamounttotal. */
	@Column(name = "APPLY_AMOUNT_TOTAL")
	private BigDecimal applyAmountTotal;

	/** The approvel amount. */
	@Column(name = "APPROVEL_AMOUNT")
	private BigDecimal approvelAmount;

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

	/** The statut workflow. */
	@Column(name = "STATUT_WORKFLOW")
	private Integer statutWorkflow;

	/** The statut libelle. */
	@Column(name = "STATUT_LIBELLE")
	private String statutLibelle;

	/** The etape workflow. */
	@Column(name = "ID_ACM_WORKFLOW_STEP")
	private Integer etapeWorkflow;

	/** The ihm root. */
	@Column(name = "IHM_WEB_ROOT", length = 512)
	private String ihmRoot;

	/** The note. */
	@Column(name = "LOAN_NOTE")
	private String note;

	/** The graceperiod. */
	@Column(name = "GRACE_PERIOD")
	private Integer gracePeriod;

	/** The industrycodeid. */
	@Column(name = "ACCOUNT_INDUSTRY_CODE_CODE", length = 256)
	private String industryCode;

	/** The industry code description. */
	@Column(name = "ACCOUNT_INDUSTRY_CODE_DESCRIPTION", length = 256)
	private String industryCodeDescription;

	/** The issuedate. */
	@Column(name = "ISSUE_DATE")
	private Date issueDate;

	/** The creation date. */
	@Column(name = "CREATION_DATE_ABACUS")
	private Date creationDate;

	/** The term period num. */
	@Column(name = "TERM_PERIOD")
	private Integer termPeriodNum;

	/** The payment freq. */
	@Column(name = "PERIOD_FREQ")
	private Integer paymentFreq;

	/** The issue fee amount. */
	@Column(name = "ISSUE_FEE_AMOUNT")
	private BigDecimal issueFeeAmount;

	/** The product rate. */
	@Column(name = "PRODUCT_RATE")
	private BigDecimal productRate;

	/** The loan reason code. */
	@Column(name = "PRODUCT_LOAN_REASONS_CODE", length = 256)
	private String loanReasonCode;

	/** The loan reason description. */
	@Column(name = "PRODUCT_LOAN_REASONS_DESCRIPTION", length = 256)
	private String loanReasonDescription;

	/** The initial payment date. */
	@Column(name = "INITIAL_PAYMENT_DATE")
	private Date initialPaymentDate;

	/** The normal payment. */
	@Column(name = "NORMAL_PAYMENT")
	private Long normalPayment;

	/** The ignore odd days. */
	@Column(name = "IGNORE_ODD_DAYS")
	private Boolean ignoreOddDays;

	/** The periods deferred. */
	@Column(name = "PERIODS_DEFERRED")
	private Integer periodsDeferred;

	/** The calculate initial payment date. */
	@Column(name = "CALCULATE_INITIAL_PAYMENT_DATE")
	private Boolean calculateInitialPaymentDate;

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

	/** The category. */
	@Column(name = "CATEGORY", length = 256)
	private String category;

	/** The change date status workflow. */
	@Column(name = "CHANGE_DATE_STATUS_WORKFLOW")
	private Date changeDateStatusWorkflow;

	/** The customer type. */
	@Column(name = "CUSTOMER_TYPE", length = 215)
	private String customerType;

	/** The parentid. */
	@Column(name = "PARENT_ID", nullable = false)
	private Long parentId;

	/** The customer. */
	@ManyToOne(fetch = FetchType.EAGER)
	@JoinColumn(name = "ID_ACM_CUSTOMER")
	private Customer customer;

	/** The process name. */
	@Column(name = "BPMN_PROCESS_NAME")
	private String processName;

	/** The community CU loan ID. */
	@Column(name = "COMMUNITY_CULOAN_ID")
	private Long communityCULoanID;

	/** The guarantor source id. */
	@Column(name = "GUARANTOR_SOURCE_ID")
	private Integer guarantorSourceId;

	/** The source of funds ID. */
	@Column(name = "SOURCE_OF_FUNDS_ID")
	private Integer sourceOfFundsID;

	/** The refinance reason id. */
	@Column(name = "REFINANCE_REASON_ID")
	private Integer refinanceReasonId;

	/** The district code id. */
	@Column(name = "DISTRICT_CODE_ID")
	private Integer districtCodeId;

	/** The interest freq. */
	@Column(name = "INTEREST_FREQ")
	private Integer interestFreq;

	/** The int pay period num. */
	@Column(name = "INT_PAY_PERIOD_NUM")
	private Integer intPayPeriodNum;

	/** The update loan. */
	@Column(name = "UPDATE_LOAN")
	private Boolean updateLoan;

	/** The loan assigned to customer. */
	@Column(name = "ASSIGN_CUSTOMER")
	private Boolean assignCustomer;

	/** The loan calculation mode. */
	@Column(name = "LOAN_CALCULATION_MODE")
	private Integer loanCalculationMode;

	/** The apr. */
	@Column(name = "APR")
	private BigDecimal apr;

	/** The effective int rate. */
	@Column(name = "EFFECTIVE_INT_RATE")
	private BigDecimal effectiveIntRate;

	/** The loan instances. */
	@OneToMany(mappedBy = "loan")
	private Set<LoanInstance> loanInstances = new HashSet<>();

	/** The group owner name. */
	@Column(name = "GROUP_OWNER_NAME")
	private String groupOwnerName;

	/** The group owner. */
	@Column(name = "GROUP_OWNER")
	private String groupOwner;

	/** The fee amt 1 Application Fees. */
	@Column(name = "FEE_AMOUNT1")
	private BigDecimal feeAmt1;

	/** The is topup refinance. */
	@Column(name = "LOAN_APPLICATION_STATUS")
	private String loanApplicationStatus;

	/** The is topup refinance. */
	@Column(name = "OPENING_BALANCE")
	private Integer openingBalance;

	/** The periods deferred type. */
	@Column(name = "PERIODS_DEFERRED_TYPE_ID")
	private Integer periodsDeferredType;

	/** The installment number. */
	@Column(name = "INSTALLMENT_NUMBER")
	private Integer installmentNumber;

	/** The ready for disb. */
	@Column(name = "READY_FOR_DISB")
	private Integer readyForDisb;

	/** The loan Assets. */
	@OneToMany(mappedBy = "loan")
	private Set<AssetLoan> loanAssets = new HashSet<>();

	/** The token. */
	@Transient
	private String token;

	/** The review from. */
	@Column(name = "REVIEW_FROM")
	private Long reviewFrom;

	/** The total interest. */
	@Column(name = "TOTAL_INTEREST")
	private BigDecimal totalInterest;

	/** The personal contribution. */
	@Column(name = "PERSONAL_CONTRIBUTION")
	private Integer personalContribution;

	/** The workflow completed. */
	@Column(name = "WORKFLOW_COMPLETED")
	private Boolean workflowCompleted;

	/** The id ib loan. */
	@Column(name = "ID_IB_loan")
	private Long idIbLoan;

	/** The other informations. */
	@Column(name = "OTHER_INFORMATIONS")
	private String otherInformations;

	/** The max installment. */
	@Column(name = "MAX_INSTALLMENT")
	private BigDecimal maxInstallment;

	/** The sign contarct validation. */
	@Column(name = "SIGN_CONTARCT_VALIDATION")
	private String signContarctValidation;

	/** The Bank information. */
	@Column(name = "BANK_INFORMATION")
	private String bankInformation;

	/**
	 * Instantiates a new loan.
	 */
	public Loan() {

		/*
		 * 
		 */
	}

	/**
	 * Instantiates a new loan.
	 *
	 * @param idLoanExtern the id loan extern
	 * @param idAccountExtern the id account extern
	 * @param accountNumberExtern the account number extern
	 * @param portfolioId the portfolio id
	 */
	public Loan(Long idLoanExtern, Long idAccountExtern, String accountNumberExtern,
			Long portfolioId) {

		this.idLoanExtern = idLoanExtern;
		this.idAccountExtern = idAccountExtern;
		this.accountNumberExtern = accountNumberExtern;
		this.portfolioId = portfolioId;
	}

	/**
	 * Instantiates a new loan : USED IN BATCH.
	 *
	 * @param portfolioId the portfolio id
	 * @param idLoanExtern the id loan extern
	 * @param idAccountExtern the id account extern
	 * @param accountNumberExtern the account number extern
	 * @param applyDate the apply date
	 * @param productCode the product code
	 * @param productDescription the product description
	 * @param customerName the customer name
	 * @param portfolioCode the portfolio code
	 * @param portfolioDescription the portfolio description
	 * @param currencySymbol the currency symbol
	 * @param currencyDecimalPlaces the currency decimal places
	 * @param productId the product id
	 * @param customerId the customer id
	 * @param applyAmountTotal the apply amount total
	 * @param gracePeriod the grace period
	 * @param industryCode the industry code
	 * @param industryCodeDescription the industry code description
	 * @param issueDate the issue date
	 * @param creationDate the creation date
	 * @param termPeriodNum the term period num
	 * @param paymentFreq the payment freq
	 * @param issueFeeAmount the issue fee amount
	 * @param productRate the product rate
	 * @param loanReasonCode the loan reason code
	 * @param loanReasonDescription the loan reason description
	 * @param initialPaymentDate the initial payment date
	 * @param normalPayment the normal payment
	 * @param ignoreOddDays the ignore odd days
	 * @param periodsDeferred the periods deferred
	 * @param calculateInitialPaymentDate the calculate initial payment date
	 * @param termPeriodID the term period ID
	 * @param branchID the branch ID
	 * @param branchName the branch name
	 * @param branchDescription the branch description
	 * @param customerType the customer type
	 * @param communityCULoanID the community CU loan ID
	 * @param guarantorSourceId the guarantor source id
	 * @param sourceOfFundsID the source of funds ID
	 * @param refinanceReasonId the refinance reason id
	 * @param districtCodeId the district code id
	 * @param intPayPeriodNum the int pay period num
	 * @param loanCalculationMode the loan calculation mode
	 * @param apr the apr
	 * @param effectiveIntRate the effective int rate
	 * @param token the token
	 */
	public Loan(Long portfolioId, Long idLoanExtern, Long idAccountExtern,
			String accountNumberExtern, Date applyDate, String productCode,
			String productDescription, String customerName, String portfolioCode,
			String portfolioDescription, String currencySymbol, Integer currencyDecimalPlaces,
			Integer productId, Long customerId, BigDecimal applyAmountTotal, Integer gracePeriod,
			String industryCode, String industryCodeDescription, Date issueDate, Date creationDate,
			Integer termPeriodNum, Integer paymentFreq, BigDecimal issueFeeAmount,
			BigDecimal productRate, String loanReasonCode, String loanReasonDescription,
			Date initialPaymentDate, Long normalPayment, Boolean ignoreOddDays,
			Integer periodsDeferred, Boolean calculateInitialPaymentDate, Long termPeriodID,
			Integer branchID, String branchName, String branchDescription, String customerType,
			Long communityCULoanID, Integer guarantorSourceId, Integer sourceOfFundsID,
			Integer refinanceReasonId, Integer districtCodeId, Integer intPayPeriodNum,
			Integer loanCalculationMode, BigDecimal apr, BigDecimal effectiveIntRate,
			String token) {

		this.portfolioId = portfolioId;
		this.idLoanExtern = idLoanExtern;
		this.idAccountExtern = idAccountExtern;
		this.accountNumberExtern = accountNumberExtern;
		this.applyDate = applyDate;
		this.productCode = productCode;
		this.productDescription = productDescription;
		this.customerName = customerName;
		this.portfolioCode = portfolioCode;
		this.portfolioDescription = portfolioDescription;
		this.currencySymbol = currencySymbol;
		this.currencyDecimalPlaces = currencyDecimalPlaces;
		this.productId = productId;
		this.customerId = customerId;

		this.applyAmountTotal = applyAmountTotal;
		this.gracePeriod = gracePeriod;
		this.industryCode = industryCode;
		this.industryCodeDescription = industryCodeDescription;
		this.issueDate = issueDate;
		this.creationDate = creationDate;
		this.termPeriodNum = termPeriodNum;
		this.paymentFreq = paymentFreq;
		this.issueFeeAmount = issueFeeAmount;
		this.productRate = productRate;
		this.loanReasonCode = loanReasonCode;
		this.loanReasonDescription = loanReasonDescription;

		this.initialPaymentDate = initialPaymentDate;
		this.normalPayment = normalPayment;
		this.ignoreOddDays = ignoreOddDays;
		this.periodsDeferred = periodsDeferred;
		this.calculateInitialPaymentDate = calculateInitialPaymentDate;
		this.termPeriodID = termPeriodID;

		this.branchID = branchID;
		this.branchName = branchName;
		this.branchDescription = branchDescription;

		this.customerType = customerType;
		this.communityCULoanID = communityCULoanID;

		this.guarantorSourceId = guarantorSourceId;
		this.sourceOfFundsID = sourceOfFundsID;
		this.refinanceReasonId = refinanceReasonId;
		this.districtCodeId = districtCodeId;
		this.intPayPeriodNum = intPayPeriodNum;

		this.loanCalculationMode = loanCalculationMode;
		this.apr = apr;
		this.effectiveIntRate = effectiveIntRate;

		this.token = token;
	}

	/**
	 * Instantiates a new loan.
	 *
	 * @param portfolioId the portfolio id
	 * @param idLoanExtern the id loan extern
	 * @param idAccountExtern the id account extern
	 * @param accountNumberExtern the account number extern
	 * @param applyDate the apply date
	 * @param productCode the product code
	 * @param productDescription the product description
	 * @param customerName the customer name
	 * @param portfolioCode the portfolio code
	 * @param portfolioDescription the portfolio description
	 * @param currencySymbol the currency symbol
	 * @param currencyDecimalPlaces the currency decimal places
	 * @param productId the product id
	 * @param customerId the customer id
	 * @param applyAmountTotal the apply amount total
	 * @param gracePeriod the grace period
	 * @param industryCode the industry code
	 * @param industryCodeDescription the industry code description
	 * @param issueDate the issue date
	 * @param creationDate the creation date
	 * @param termPeriodNum the term period num
	 * @param paymentFreq the payment freq
	 * @param issueFeeAmount the issue fee amount
	 * @param productRate the product rate
	 * @param loanReasonCode the loan reason code
	 * @param loanReasonDescription the loan reason description
	 * @param initialPaymentDate the initial payment date
	 * @param normalPayment the normal payment
	 * @param ignoreOddDays the ignore odd days
	 * @param periodsDeferred the periods deferred
	 * @param calculateInitialPaymentDate the calculate initial payment date
	 * @param termPeriodID the term period ID
	 * @param branchID the branch ID
	 * @param branchName the branch name
	 * @param branchDescription the branch description
	 * @param customerType the customer type
	 * @param communityCULoanID the community CU loan ID
	 * @param guarantorSourceId the guarantor source id
	 * @param sourceOfFundsID the source of funds ID
	 * @param refinanceReasonId the refinance reason id
	 * @param districtCodeId the district code id
	 * @param intPayPeriodNum the int pay period num
	 * @param loanCalculationMode the loan calculation mode
	 * @param apr the apr
	 * @param effectiveIntRate the effective int rate
	 */
	public Loan(Long portfolioId, Long idLoanExtern, Long idAccountExtern,
			String accountNumberExtern, Date applyDate, String productCode,
			String productDescription, String customerName, String portfolioCode,
			String portfolioDescription, String currencySymbol, Integer currencyDecimalPlaces,
			Integer productId, Long customerId, BigDecimal applyAmountTotal, Integer gracePeriod,
			String industryCode, String industryCodeDescription, Date issueDate, Date creationDate,
			Integer termPeriodNum, Integer paymentFreq, BigDecimal issueFeeAmount,
			BigDecimal productRate, String loanReasonCode, String loanReasonDescription,
			Date initialPaymentDate, Long normalPayment, Boolean ignoreOddDays,
			Integer periodsDeferred, Boolean calculateInitialPaymentDate, Long termPeriodID,
			Integer branchID, String branchName, String branchDescription, String customerType,
			Long communityCULoanID, Integer guarantorSourceId, Integer sourceOfFundsID,
			Integer refinanceReasonId, Integer districtCodeId, Integer intPayPeriodNum,
			Integer loanCalculationMode, BigDecimal apr, BigDecimal effectiveIntRate) {

		this.portfolioId = portfolioId;
		this.idLoanExtern = idLoanExtern;
		this.idAccountExtern = idAccountExtern;
		this.accountNumberExtern = accountNumberExtern;
		this.applyDate = applyDate;
		this.productCode = productCode;
		this.productDescription = productDescription;
		this.customerName = customerName;
		this.portfolioCode = portfolioCode;
		this.portfolioDescription = portfolioDescription;
		this.currencySymbol = currencySymbol;
		this.currencyDecimalPlaces = currencyDecimalPlaces;
		this.productId = productId;
		this.customerId = customerId;

		this.applyAmountTotal = applyAmountTotal;
		this.gracePeriod = gracePeriod;
		this.industryCode = industryCode;
		this.industryCodeDescription = industryCodeDescription;
		this.issueDate = issueDate;
		this.creationDate = creationDate;
		this.termPeriodNum = termPeriodNum;
		this.paymentFreq = paymentFreq;
		this.issueFeeAmount = issueFeeAmount;
		this.productRate = productRate;
		this.loanReasonCode = loanReasonCode;
		this.loanReasonDescription = loanReasonDescription;

		this.initialPaymentDate = initialPaymentDate;
		this.normalPayment = normalPayment;
		this.ignoreOddDays = ignoreOddDays;
		this.periodsDeferred = periodsDeferred;
		this.calculateInitialPaymentDate = calculateInitialPaymentDate;
		this.termPeriodID = termPeriodID;

		this.branchID = branchID;
		this.branchName = branchName;
		this.branchDescription = branchDescription;

		this.customerType = customerType;
		this.communityCULoanID = communityCULoanID;

		this.guarantorSourceId = guarantorSourceId;
		this.sourceOfFundsID = sourceOfFundsID;
		this.refinanceReasonId = refinanceReasonId;
		this.districtCodeId = districtCodeId;
		this.intPayPeriodNum = intPayPeriodNum;

		this.loanCalculationMode = loanCalculationMode;
		this.apr = apr;
		this.effectiveIntRate = effectiveIntRate;

	}

	/**
	 * Instantiates a new loan.
	 *
	 * @param idLoan the id loan
	 */
	public Loan(Long idLoan) {

		this.idLoan = idLoan;
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
	 * Gets the id loan.
	 *
	 * @return the idLoan
	 */
	public Long getIdLoan() {

		return idLoan;
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
	 * Gets the process instance id.
	 *
	 * @return the processInstanceId
	 */
	public String getProcessInstanceId() {

		return processInstanceId;
	}

	/**
	 * Sets the process instance id.
	 *
	 * @param processInstanceId the processInstanceId to set
	 */
	public void setProcessInstanceId(String processInstanceId) {

		this.processInstanceId = processInstanceId;
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
	 * Gets the id account extern.
	 *
	 * @return the idAccountExtern
	 */
	public Long getIdAccountExtern() {

		return idAccountExtern;
	}

	/**
	 * Sets the id account extern.
	 *
	 * @param idAccountExtern the idAccountExtern to set
	 */
	public void setIdAccountExtern(Long idAccountExtern) {

		this.idAccountExtern = idAccountExtern;
	}

	/**
	 * Gets the account number extern.
	 *
	 * @return the accountNumberExtern
	 */
	public String getAccountNumberExtern() {

		return accountNumberExtern;
	}

	/**
	 * Sets the account number extern.
	 *
	 * @param accountNumberExtern the accountNumberExtern to set
	 */
	public void setAccountNumberExtern(String accountNumberExtern) {

		this.accountNumberExtern = accountNumberExtern;
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
	 * Gets the owner name.
	 *
	 * @return the ownerName
	 */
	public String getOwnerName() {

		return ownerName;
	}

	/**
	 * Sets the owner name.
	 *
	 * @param ownerName the ownerName to set
	 */
	public void setOwnerName(String ownerName) {

		this.ownerName = ownerName;
	}

	/**
	 * Gets the statut workflow.
	 *
	 * @return the statutWorkflow
	 */
	public Integer getStatutWorkflow() {

		return statutWorkflow;
	}

	/**
	 * Sets the statut workflow.
	 *
	 * @param statutWorkflow the statutWorkflow to set
	 */
	public void setStatutWorkflow(Integer statutWorkflow) {

		this.statutWorkflow = statutWorkflow;
	}

	/**
	 * Gets the etape workflow.
	 *
	 * @return the etapeWorkflow
	 */
	public Integer getEtapeWorkflow() {

		return etapeWorkflow;
	}

	/**
	 * Gets the statut libelle.
	 *
	 * @return the statut libelle
	 */
	public String getStatutLibelle() {

		return statutLibelle;
	}

	/**
	 * Sets the statut libelle.
	 *
	 * @param statutLibelle the new statut libelle
	 */
	public void setStatutLibelle(String statutLibelle) {

		this.statutLibelle = statutLibelle;
	}

	/**
	 * Sets the etape workflow.
	 *
	 * @param etapeWorkflow the etapeWorkflow to set
	 */
	public void setEtapeWorkflow(Integer etapeWorkflow) {

		this.etapeWorkflow = etapeWorkflow;
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
	 * Gets the customer id.
	 *
	 * @return the customerId
	 */
	public Long getCustomerId() {

		return customerId;
	}

	/**
	 * Sets the customer id.
	 *
	 * @param customerId the customerId to set
	 */
	public void setCustomerId(Long customerId) {

		this.customerId = customerId;
	}

	/**
	 * Gets the ihm root.
	 *
	 * @return the ihmRoot
	 */
	public String getIhmRoot() {

		return ihmRoot;
	}

	/**
	 * Sets the ihm root.
	 *
	 * @param ihmRoot the ihmRoot to set
	 */
	public void setIhmRoot(String ihmRoot) {

		this.ihmRoot = ihmRoot;
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
	 * Gets the industry code.
	 *
	 * @return the industryCode
	 */
	public String getIndustryCode() {

		return industryCode;
	}

	/**
	 * Sets the industry code.
	 *
	 * @param industryCode the industryCode to set
	 */
	public void setIndustryCode(String industryCode) {

		this.industryCode = industryCode;
	}

	/**
	 * Gets the industry code description.
	 *
	 * @return the industryCodeDescription
	 */
	public String getIndustryCodeDescription() {

		return industryCodeDescription;
	}

	/**
	 * Sets the industry code description.
	 *
	 * @param industryCodeDescription the industryCodeDescription to set
	 */
	public void setIndustryCodeDescription(String industryCodeDescription) {

		this.industryCodeDescription = industryCodeDescription;
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
	 * Gets the creation date.
	 *
	 * @return the creationDate
	 */
	public Date getCreationDate() {

		return creationDate;
	}

	/**
	 * Sets the creation date.
	 *
	 * @param creationDate the creationDate to set
	 */
	public void setCreationDate(Date creationDate) {

		this.creationDate = creationDate;
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
	 * Gets the product rate.
	 *
	 * @return the productRate
	 */
	public BigDecimal getProductRate() {

		return productRate;
	}

	/**
	 * Sets the product rate.
	 *
	 * @param productRate the productRate to set
	 */
	public void setProductRate(BigDecimal productRate) {

		this.productRate = productRate;
	}

	/**
	 * Gets the loan reason code.
	 *
	 * @return the loanReasonCode
	 */
	public String getLoanReasonCode() {

		return loanReasonCode;
	}

	/**
	 * Sets the loan reason code.
	 *
	 * @param loanReasonCode the loanReasonCode to set
	 */
	public void setLoanReasonCode(String loanReasonCode) {

		this.loanReasonCode = loanReasonCode;
	}

	/**
	 * Gets the loan reason description.
	 *
	 * @return the loanReasonDescription
	 */
	public String getLoanReasonDescription() {

		return loanReasonDescription;
	}

	/**
	 * Sets the loan reason description.
	 *
	 * @param loanReasonDescription the loanReasonDescription to set
	 */
	public void setLoanReasonDescription(String loanReasonDescription) {

		this.loanReasonDescription = loanReasonDescription;
	}

	/**
	 * Gets the token.
	 *
	 * @return the token
	 */
	public String getToken() {

		return token;
	}

	/**
	 * Sets the token.
	 *
	 * @param token the token to set
	 */
	public void setToken(String token) {

		this.token = token;
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
	 * Gets the ignore odd days.
	 *
	 * @return the ignoreOddDays
	 */
	public Boolean getIgnoreOddDays() {

		return ignoreOddDays;
	}

	/**
	 * Sets the ignore odd days.
	 *
	 * @param ignoreOddDays the ignoreOddDays to set
	 */
	public void setIgnoreOddDays(Boolean ignoreOddDays) {

		this.ignoreOddDays = ignoreOddDays;
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
	 * Gets the calculate initial payment date.
	 *
	 * @return the calculateInitialPaymentDate
	 */
	public Boolean getCalculateInitialPaymentDate() {

		return calculateInitialPaymentDate;
	}

	/**
	 * Sets the calculate initial payment date.
	 *
	 * @param calculateInitialPaymentDate the calculateInitialPaymentDate to set
	 */
	public void setCalculateInitialPaymentDate(Boolean calculateInitialPaymentDate) {

		this.calculateInitialPaymentDate = calculateInitialPaymentDate;
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
	 * Gets the change date status workflow.
	 *
	 * @return the changeDateStatusWorkflow
	 */
	public Date getChangeDateStatusWorkflow() {

		return changeDateStatusWorkflow;
	}

	/**
	 * Sets the change date status workflow.
	 *
	 * @param changeDateStatusWorkflow the changeDateStatusWorkflow to set
	 */
	public void setChangeDateStatusWorkflow(Date changeDateStatusWorkflow) {

		this.changeDateStatusWorkflow = changeDateStatusWorkflow;
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
	 * Gets the approvel amount.
	 *
	 * @return the approvelAmount
	 */
	public BigDecimal getApprovelAmount() {

		return approvelAmount;
	}

	/**
	 * Sets the approvel amount.
	 *
	 * @param approvelAmount the approvelAmount to set
	 */
	public void setApprovelAmount(BigDecimal approvelAmount) {

		this.approvelAmount = approvelAmount;
	}

	/**
	 * Gets the process name.
	 *
	 * @return the processName
	 */
	public String getProcessName() {

		return processName;
	}

	/**
	 * Sets the process name.
	 *
	 * @param processName the processName to set
	 */
	public void setProcessName(String processName) {

		this.processName = processName;
	}

	/**
	 * Gets the customer type.
	 * 
	 * @return the customerType
	 */
	public String getCustomerType() {

		return customerType;
	}

	/**
	 * Sets the customer type.
	 * 
	 * @param customerType the customerType to set
	 */
	public void setCustomerType(String customerType) {

		this.customerType = customerType;
	}

	/**
	 * Gets the parent id.
	 * 
	 * @return the parentId
	 */
	public Long getParentId() {

		return parentId;
	}

	/**
	 * Sets the parent id.
	 * 
	 * @param parentId the parentId to set
	 */
	public void setParentId(Long parentId) {

		this.parentId = parentId;
	}

	/**
	 * Gets the community CU loan ID.
	 *
	 * @return the communityCULoanID
	 */
	public Long getCommunityCULoanID() {

		return communityCULoanID;
	}

	/**
	 * Sets the community CU loan ID.
	 *
	 * @param communityCULoanID the communityCULoanID to set
	 */
	public void setCommunityCULoanID(Long communityCULoanID) {

		this.communityCULoanID = communityCULoanID;
	}

	/**
	 * Gets the guarantor source id.
	 *
	 * @return the guarantorSourceId
	 */
	public Integer getGuarantorSourceId() {

		return guarantorSourceId;
	}

	/**
	 * Sets the guarantor source id.
	 *
	 * @param guarantorSourceId the guarantorSourceId to set
	 */
	public void setGuarantorSourceId(Integer guarantorSourceId) {

		this.guarantorSourceId = guarantorSourceId;
	}

	/**
	 * Gets the source of funds ID.
	 *
	 * @return the sourceOfFundsID
	 */
	public Integer getSourceOfFundsID() {

		return sourceOfFundsID;
	}

	/**
	 * Sets the source of funds ID.
	 *
	 * @param sourceOfFundsID the sourceOfFundsID to set
	 */
	public void setSourceOfFundsID(Integer sourceOfFundsID) {

		this.sourceOfFundsID = sourceOfFundsID;
	}

	/**
	 * Gets the refinance reason id.
	 *
	 * @return the refinanceReasonId
	 */
	public Integer getRefinanceReasonId() {

		return refinanceReasonId;
	}

	/**
	 * Sets the refinance reason id.
	 *
	 * @param refinanceReasonId the refinanceReasonId to set
	 */
	public void setRefinanceReasonId(Integer refinanceReasonId) {

		this.refinanceReasonId = refinanceReasonId;
	}

	/**
	 * Gets the district code id.
	 *
	 * @return the districtCodeId
	 */
	public Integer getDistrictCodeId() {

		return districtCodeId;
	}

	/**
	 * Sets the district code id.
	 *
	 * @param districtCodeId the districtCodeId to set
	 */
	public void setDistrictCodeId(Integer districtCodeId) {

		this.districtCodeId = districtCodeId;
	}

	/**
	 * Gets the interest freq.
	 *
	 * @return the interestFreq
	 */
	public Integer getInterestFreq() {

		return interestFreq;
	}

	/**
	 * Sets the interest freq.
	 *
	 * @param interestFreq the interestFreq to set
	 */
	public void setInterestFreq(Integer interestFreq) {

		this.interestFreq = interestFreq;
	}

	/**
	 * Gets the int pay period num.
	 *
	 * @return the intPayPeriodNum
	 */
	public Integer getIntPayPeriodNum() {

		return intPayPeriodNum;
	}

	/**
	 * Sets the int pay period num.
	 *
	 * @param intPayPeriodNum the intPayPeriodNum to set
	 */
	public void setIntPayPeriodNum(Integer intPayPeriodNum) {

		this.intPayPeriodNum = intPayPeriodNum;
	}

	/**
	 * Gets the loan instances.
	 *
	 * @return the loanInstances
	 */
	public Set<LoanInstance> getLoanInstances() {

		return loanInstances;
	}

	/**
	 * Sets the loan instances.
	 *
	 * @param loanInstances the loanInstances to set
	 */
	public void setLoanInstances(Set<LoanInstance> loanInstances) {

		this.loanInstances = loanInstances;
	}

	/**
	 * Gets the update loan.
	 *
	 * @return the updateLoan
	 */
	public Boolean getUpdateLoan() {

		return updateLoan;
	}

	/**
	 * Sets the update loan.
	 *
	 * @param updateLoan the updateLoan to set
	 */
	public void setUpdateLoan(Boolean updateLoan) {

		this.updateLoan = updateLoan;
	}

	/**
	 * Gets the assign customer.
	 *
	 * @return the assign customer
	 */
	public Boolean getAssignCustomer() {

		return assignCustomer;
	}

	/**
	 * Sets the assign customer.
	 *
	 * @param assignCustomer the new assign customer
	 */
	public void setAssignCustomer(Boolean assignCustomer) {

		this.assignCustomer = assignCustomer;
	}

	/**
	 * Gets the loan calculation mode.
	 *
	 * @return the loanCalculationMode
	 */
	public Integer getLoanCalculationMode() {

		return loanCalculationMode;
	}

	/**
	 * Sets the loan calculation mode.
	 *
	 * @param loanCalculationMode the loanCalculationMode to set
	 */
	public void setLoanCalculationMode(Integer loanCalculationMode) {

		this.loanCalculationMode = loanCalculationMode;
	}

	/**
	 * Gets the apr.
	 *
	 * @return the apr
	 */
	public BigDecimal getApr() {

		return apr;
	}

	/**
	 * Sets the apr.
	 *
	 * @param apr the apr to set
	 */
	public void setApr(BigDecimal apr) {

		this.apr = apr;
	}

	/**
	 * Gets the effective int rate.
	 *
	 * @return the effectiveIntRate
	 */
	public BigDecimal getEffectiveIntRate() {

		return effectiveIntRate;
	}

	/**
	 * Sets the effective int rate.
	 *
	 * @param effectiveIntRate the effectiveIntRate to set
	 */
	public void setEffectiveIntRate(BigDecimal effectiveIntRate) {

		this.effectiveIntRate = effectiveIntRate;
	}

	/**
	 * Gets the group owner name.
	 *
	 * @return the group owner name
	 */
	public String getGroupOwnerName() {

		return groupOwnerName;
	}

	/**
	 * Sets the group owner name.
	 *
	 * @param groupOwnerName the new group owner name
	 */
	public void setGroupOwnerName(String groupOwnerName) {

		this.groupOwnerName = groupOwnerName;
	}

	/**
	 * Gets the group owner.
	 *
	 * @return the group owner
	 */
	public String getGroupOwner() {

		return groupOwner;
	}

	/**
	 * Sets the group owner.
	 *
	 * @param groupOwner the new group owner
	 */
	public void setGroupOwner(String groupOwner) {

		this.groupOwner = groupOwner;
	}

	/**
	 * Gets the fee amt 1.
	 *
	 * @return the feeAmt1
	 */
	public BigDecimal getFeeAmt1() {

		return feeAmt1;
	}

	/**
	 * Sets the fee amt 1.
	 *
	 * @param feeAmt1 the feeAmt1 to set
	 */
	public void setFeeAmt1(BigDecimal feeAmt1) {

		this.feeAmt1 = feeAmt1;
	}

	/**
	 * Gets the loan application status.
	 *
	 * @return the loan application status
	 */
	public String getLoanApplicationStatus() {

		return loanApplicationStatus;
	}

	/**
	 * Sets the loan application status.
	 *
	 * @param loanApplicationStatus the new loan application status
	 */
	public void setLoanApplicationStatus(String loanApplicationStatus) {

		this.loanApplicationStatus = loanApplicationStatus;
	}

	/**
	 * Gets the opening balance.
	 *
	 * @return the opening balance
	 */
	public Integer getOpeningBalance() {

		return openingBalance;
	}

	/**
	 * Sets the opening balance.
	 *
	 * @param openingBalance the new opening balance
	 */
	public void setOpeningBalance(Integer openingBalance) {

		this.openingBalance = openingBalance;
	}

	/**
	 * Gets the periods deferred type.
	 *
	 * @return the periods deferred type
	 */
	public Integer getPeriodsDeferredType() {

		return periodsDeferredType;
	}

	/**
	 * Sets the periods deferred type.
	 *
	 * @param periodsDeferredType the new periods deferred type
	 */
	public void setPeriodsDeferredType(Integer periodsDeferredType) {

		this.periodsDeferredType = periodsDeferredType;
	}

	/**
	 * Gets the installment number.
	 *
	 * @return the installment number
	 */
	public Integer getInstallmentNumber() {

		return installmentNumber;
	}

	/**
	 * Sets the installment number.
	 *
	 * @param installmentNumber the new installment number
	 */
	public void setInstallmentNumber(Integer installmentNumber) {

		this.installmentNumber = installmentNumber;
	}

	/**
	 * Gets the ready for disb.
	 *
	 * @return the ready for disb
	 */
	public Integer getReadyForDisb() {

		return readyForDisb;
	}

	/**
	 * Sets the ready for disb.
	 *
	 * @param readyForDisb the new ready for disb
	 */
	public void setReadyForDisb(Integer readyForDisb) {

		this.readyForDisb = readyForDisb;
	}

	/**
	 * Gets the loan assets.
	 *
	 * @return the loan assets
	 */
	public Set<AssetLoan> getLoanAssets() {

		return loanAssets;
	}

	/**
	 * Sets the loan assets.
	 *
	 * @param loanAssets the new loan assets
	 */
	public void setLoanAssets(Set<AssetLoan> loanAssets) {

		this.loanAssets = loanAssets;
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.utils.models.GenericModel#toString()
	 */
	@Override
	public String toString() {

		return "Loan [idLoan=" + idLoan + ", idLoanExtern=" + idLoanExtern + ", idAccountExtern="
				+ idAccountExtern + ", accountNumberExtern=" + accountNumberExtern
				+ ", processInstanceId=" + processInstanceId + ", portfolioId=" + portfolioId
				+ ", statut=" + statut + ", productDescription=" + productDescription
				+ ", productCode=" + productCode + ", productId=" + productId + ", customerId="
				+ customerId + ", customerName=" + customerName + ", applyAmountTotal="
				+ applyAmountTotal + ", approvelAmount=" + approvelAmount + ", currencySymbol="
				+ currencySymbol + ", currencyDecimalPlaces=" + currencyDecimalPlaces
				+ ", applyDate=" + applyDate + ", portfolioCode=" + portfolioCode
				+ ", portfolioDescription=" + portfolioDescription + ", owner=" + owner
				+ ", ownerName=" + ownerName + ", statutWorkflow=" + statutWorkflow
				+ ", statutLibelle=" + statutLibelle + ", etapeWorkflow=" + etapeWorkflow
				+ ", ihmRoot=" + ihmRoot + ", note=" + note + ", gracePeriod=" + gracePeriod
				+ ", industryCode=" + industryCode + ", industryCodeDescription="
				+ industryCodeDescription + ", issueDate=" + issueDate + ", creationDate="
				+ creationDate + ", termPeriodNum=" + termPeriodNum + ", paymentFreq=" + paymentFreq
				+ ", issueFeeAmount=" + issueFeeAmount + ", productRate=" + productRate
				+ ", loanReasonCode=" + loanReasonCode + ", loanReasonDescription="
				+ loanReasonDescription + ", initialPaymentDate=" + initialPaymentDate
				+ ", normalPayment=" + normalPayment + ", ignoreOddDays=" + ignoreOddDays
				+ ", periodsDeferred=" + periodsDeferred + ", calculateInitialPaymentDate="
				+ calculateInitialPaymentDate + ", termPeriodID=" + termPeriodID + ", branchID="
				+ branchID + ", branchName=" + branchName + ", branchDescription="
				+ branchDescription + ", category=" + category + ", changeDateStatusWorkflow="
				+ changeDateStatusWorkflow + ", customerType=" + customerType + ", parentId="
				+ parentId + ", customer=" + customer + ", processName=" + processName
				+ ", communityCULoanID=" + communityCULoanID + ", guarantorSourceId="
				+ guarantorSourceId + ", sourceOfFundsID=" + sourceOfFundsID
				+ ", refinanceReasonId=" + refinanceReasonId + ", districtCodeId=" + districtCodeId
				+ ", interestFreq=" + interestFreq + ", intPayPeriodNum=" + intPayPeriodNum
				+ ", updateLoan=" + updateLoan + ", assignCustomer=" + assignCustomer
				+ ", loanCalculationMode=" + loanCalculationMode + ", apr=" + apr
				+ ", effectiveIntRate=" + effectiveIntRate + ", loanInstances=" + loanInstances
				+ ", groupOwnerName=" + groupOwnerName + ", groupOwner=" + groupOwner + ", feeAmt1="
				+ feeAmt1 + ", loanApplicationStatus=" + loanApplicationStatus + ", openingBalance="
				+ openingBalance + ", periodsDeferredType=" + periodsDeferredType
				+ ", installmentNumber=" + installmentNumber + ", readyForDisb=" + readyForDisb
				+ ", loanAssets=" + loanAssets + ", token=" + token + ", reviewFrom=" + reviewFrom
				+ ", totalInterest=" + totalInterest + ", personalContribution="
				+ personalContribution + ", workflowCompleted=" + workflowCompleted + ", idIbLoan="
				+ idIbLoan + ", otherInformations=" + otherInformations + "]";
	}

	/**
	 * Gets the review from.
	 *
	 * @return the review from
	 */
	public Long getReviewFrom() {

		return reviewFrom;
	}

	/**
	 * Sets the review from.
	 *
	 * @param reviewFrom the new review from
	 */
	public void setReviewFrom(Long reviewFrom) {

		this.reviewFrom = reviewFrom;
	}

	/**
	 * Gets the total interest.
	 *
	 * @return the totalInterest
	 */
	public BigDecimal getTotalInterest() {

		return totalInterest;
	}

	/**
	 * Sets the total interest.
	 *
	 * @param totalInterest the totalInterest to set
	 */
	public void setTotalInterest(BigDecimal totalInterest) {

		this.totalInterest = totalInterest;
	}

	/**
	 * Gets the personal contribution.
	 *
	 * @return the personalContribution
	 */
	public Integer getPersonalContribution() {

		return personalContribution;
	}

	/**
	 * Sets the personal contribution.
	 *
	 * @param personalContribution the personalContribution to set
	 */
	public void setPersonalContribution(Integer personalContribution) {

		this.personalContribution = personalContribution;
	}

	/**
	 * Gets the workflow completed.
	 *
	 * @return the workflow completed
	 */
	public Boolean getWorkflowCompleted() {

		return workflowCompleted;
	}

	/**
	 * Sets the workflow completed.
	 *
	 * @param workflowCompleted the new workflow completed
	 */
	public void setWorkflowCompleted(Boolean workflowCompleted) {

		this.workflowCompleted = workflowCompleted;
	}

	/**
	 * Gets the id ib loan.
	 *
	 * @return the id ib loan
	 */
	public Long getIdIbLoan() {

		return idIbLoan;
	}

	/**
	 * Sets the id ib loan.
	 *
	 * @param idIbLoan the new id ib loan
	 */
	public void setIdIbLoan(Long idIbLoan) {

		this.idIbLoan = idIbLoan;
	}

	/**
	 * Gets the other informations.
	 *
	 * @return the other informations
	 */
	public String getOtherInformations() {

		return otherInformations;
	}

	/**
	 * Sets the other informations.
	 *
	 * @param otherInformations the new other informations
	 */
	public void setOtherInformations(String otherInformations) {

		this.otherInformations = otherInformations;
	}

	/**
	 * Gets the max installment.
	 *
	 * @return the max installment
	 */
	public BigDecimal getMaxInstallment() {

		return maxInstallment;
	}

	/**
	 * Sets the max installment.
	 *
	 * @param maxInstallment the new max installment
	 */
	public void setMaxInstallment(BigDecimal maxInstallment) {

		this.maxInstallment = maxInstallment;
	}

	/**
	 * Gets the sign contarct validation.
	 *
	 * @return the sign contarct validation
	 */
	public String getSignContarctValidation() {

		return signContarctValidation;
	}

	/**
	 * Sets the sign contarct validation.
	 *
	 * @param signContarctValidation the new sign contarct validation
	 */
	public void setSignContarctValidation(String signContarctValidation) {

		this.signContarctValidation = signContarctValidation;
	}

	/**
	 * Gets the bank information.
	 *
	 * @return the bank information
	 */
	public String getBankInformation() {

		return bankInformation;
	}

	/**
	 * Sets the bank information.
	 *
	 * @param bankInformation the new bank information
	 */
	public void setBankInformation(String bankInformation) {

		this.bankInformation = bankInformation;
	}

}
