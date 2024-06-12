/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.utils.dtos;

import java.io.Serializable;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.Date;
import java.util.List;
import java.util.stream.Collectors;

import org.dozer.Mapping;

// TODO: Auto-generated Javadoc
/**
 * {@link LoanDTO} class.
 *
 * @author YesserSomai
 * @since 0.2.0
 */
public class LoanDTO extends GenericDTO implements Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = -3611187028204195378L;

	/** The loanid. */
	@Mapping("idLoan")
	private Long loanId;

	/** The applyamounttotal. */
	private BigDecimal applyAmountTotal;

	/** The approvel amount. */
	private BigDecimal approvelAmount;

	/** The loan approval level. */
	private Integer loanApprovalLevel;

	/** The approvel amount groupe. */
	private BigDecimal approvelAmountGroupe;

	/** The applydate. */
	private Date applyDate;

	/** The culoanrefinancereasonid. */
	private Integer cuLoanReFinanceReason;

	/** The currencyid. */
	private Integer currencyId;

	/** The graceperiod. */
	private Integer gracePeriod;

	/** The industrycodeid. */
	private String industryCode;

	/** The industry code description. */
	private String industryCodeDescription;

	/** The issuedate. */
	private Date issueDate;

	/** The loansourceoffundsid. */
	private Integer loanSourceOffunds;

	/** The portfolioid. */
	private Long portfolioId;

	/** The productid. */
	private Integer productId;

	/** The process instance id. */
	private String processInstanceId;

	/** The id loan extern. */
	private Long idLoanExtern;

	/** The statut. */
	private Integer statut;

	/** The statut libelle. */
	private String statutLibelle;

	/** The statut workflow. */
	private Integer statutWorkflow;

	/** The etape workflow. */
	private Integer etapeWorkflow;

	/** The workflow next action. */
	private String workflowNextAction;

	/** The id account extern. */
	private Long idAccountExtern;

	/** The account number. */
	@Mapping("accountNumberExtern")
	private String accountNumber;

	/** The creation date. */
	private Date creationDate;

	/** The term period num. */
	private Integer termPeriodNum;

	/** The payment period. */
	private Integer paymentPeriod;

	/** The payment freq. */
	private Integer paymentFreq;

	/** The issue fee amount. */
	private BigDecimal issueFeeAmount;

	/** The product code. */
	private String productCode;

	/** The product description. */
	private String productDescription;

	/** The product rate. */
	private BigDecimal productRate;

	/** The customer ID. */
	private Long customerId;

	/** The customer name. */
	private String customerName;

	/** The customer name no pipe. */
	private String customerNameNoPipe;

	/** The loan reason id. */
	private String loanReasonId;

	/** The loan reason code. */
	private String loanReasonCode;

	/** The loan reason description. */
	private String loanReasonDescription;

	/** The portfolio code. */
	private String portfolioCode;

	/** The portfolio description. */
	private String portfolioDescription;

	/** The currency symbol. */
	private String currencySymbol;

	/** The currency decimal places. */
	private Integer currencyDecimalPlaces;

	/** The owner. */
	private String owner;

	/** The owner name. */
	private String ownerName;

	/** The pourcentage. */
	private double pourcentage;

	/** The ihmRoot. */
	private String ihmRoot;

	/** The note. */
	private String note;

	/** The last updated date. */
	private Date dateLastUpdate;

	/** The loan calculation mode. */
	private Integer loanCalculationMode;

	/** The apr. */
	private BigDecimal apr;

	/** The effective int rate. */
	private BigDecimal effectiveIntRate;

	/** The initial payment date. */
	private Date initialPaymentDate;

	/** The normal payment. */
	private Long normalPayment;

	/** The ignore odd days. */
	private Boolean ignoreOddDays;

	/** The periods deferred. */
	private Integer periodsDeferred;

	/** The periods deferred type. */
	private Integer periodsDeferredType;

	/** The calculate initial payment date. */
	private Boolean calculateInitialPaymentDate;

	/** The term period ID. */
	private Long termPeriodID;

	/** The code extern motif rejet. */
	private Integer codeExternMotifRejet;

	/** The contact date. */
	private Date contactDateCustomerDecision;

	/** The comments. */
	private String commentsCustomerDecision;

	/** The confirm: confirm checkbox reject/review/approve. */
	private Boolean confirm;

	/** The branchID. */
	private Integer branchID;

	/** The branch name. */
	private String branchName;

	/** The branch description. */
	private String branchDescription;

	/** The category. */
	private String category;

	/** The change date status workflow. */
	private Date changeDateStatusWorkflow;

	/** The customer DTO. */
	@Mapping("customer")
	private CustomerDTO customerDTO;

	/** The amountWord. */
	private String amountWord;

	/** The process name. */
	private String processName;

	/** The customer type. */
	private String customerType;

	/** The parentid. */
	private Long parentId;

	/** The child missing info. */
	private Boolean childMissingInfo;

	/** The community CU loan ID. */
	private Long communityCULoanID;

	/** The guarantor source id. */
	private Integer guarantorSourceId;

	/** The source of funds ID. */
	private Integer sourceOfFundsID;

	/** The refinance reason id. */
	private Integer refinanceReasonId;

	/** The district code id. */
	private Integer districtCodeId;

	/** The interest freq. */
	private Integer interestFreq;

	/** The int pay period num. */
	private Integer intPayPeriodNum;

	/** The loan update. */
	private Boolean updateLoan;

	/** The product DTO (USED ONLY in saveToAbacus () method). */
	private ProductDTO productDTO;

	/** The loan instances dtos. */
	@Mapping("loanInstances")
	private List<LoanInstanceDTO> loanInstancesDtos = new ArrayList<>();

	/** The user defined fields links DT os. */
	private List<UserDefinedFieldsLinksDTO> userDefinedFieldsLinksDTOs;

	/** The list missing data. */
	private List<String> listMissingData;

	/** The guarantors. */
	private List<CustomerDTO> guarantors;

	/** The loan assigned to customer. */
	private Boolean assignCustomer;

	/** The is not from workflow. */
	private Boolean isNotFromWorkflow;

	/** The last approval date (USE in Reporting). */
	private Date lastApprovalDate;

	/** The customer groupe number (USE in Reporting). */
	private String customerGroupeNumber;

	/** The customer role (USE in Reporting). */
	private String customerRole;

	/** The changed. */
	private Boolean changed;

	/** The updated by. */
	private String updatedBy;

	/** The group owner name. */
	private String groupOwnerName;

	/** The group owner. */
	private String groupOwner;

	/** The collaterals. */
	private List<AcmCollateralDTO> collaterals;

	/** The fee amt 1. Fix Fees (application fees) */
	private BigDecimal feeAmt1;

	/** The update loan abacus. */
	private Boolean updateLoanAbacus;

	/**
	 * The assigned to one user. used to check whether send notification to all group of users or to
	 * one user
	 */
	private Boolean assignedToOneUser;

	/** The loan application status. */
	private String loanApplicationStatus;

	/** The opening balance. */
	private BigDecimal openingBalance;

	/** The enabled. */
	private Boolean enabled;

	/** The work flow action. */
	private String workFlowAction;

	/** The installment number. */
	private Integer installmentNumber;

	/** The setting topup DTos. */
	private List<SettingTopupDTO> settingTopupDTOs;

	/** The loan Assets dtos. */
	@Mapping("loanAssets")
	private List<AssetLoanDTO> loanAssetsDtos = new ArrayList<>();

	/** The supplier quantity Count. */
	private Integer quantitySupplier;

	/** The supplier balance. */
	private BigDecimal balanceSupplier;

	/** The review only selected step. */
	private Boolean reviewOnlySelectedStep;

	/** The review from. */
	private Long reviewFrom;

	/** The total interest. */
	private BigDecimal totalInterest;

	/** The personal contribution. */
	private Integer personalContribution;

	/** The workflow completed. */
	private Boolean workflowCompleted;

	/** The owner email. */
	private String ownerEmail;

	/** The is ib loan. */
	private Long idIbLoan;

	/** The loan schedules. */
	private List<ScheduleDTO> loanSchedules;

	/** The other informations. */
	private String otherInformations;

	/** The step path. */
	private String stepPath;

	/** The send to ib. */
	private Boolean sendToIb;

	/** The custom rate. */
	private BigDecimal customRate;

	/** The max installment. */
	private BigDecimal maxInstallment;

	/** The sign contarct validation. */
	private String signContarctValidation;

	/** The bank information. */
	private String bankInformation;

	/**
	 * Instantiates a new loan DTO.
	 */
	public LoanDTO() {

		/*
		 * EMPTY
		 */
	}

	/**
	 * Instantiates a new loan DTO.
	 *
	 * @param portfolioId the portfolio id
	 * @param idLoanExtern the id loan extern
	 * @param statut the statut
	 * @param idAccountExtern the id account extern
	 * @param accountNumber the account number extern
	 */
	public LoanDTO(Long portfolioId, Long idLoanExtern, Integer statut, Long idAccountExtern,
			String accountNumber) {

		this.portfolioId = portfolioId;
		this.idLoanExtern = idLoanExtern;
		this.statut = statut;
		this.idAccountExtern = idAccountExtern;
		this.accountNumber = accountNumber;
	}

	/**
	 * Instantiates a new loan DTO (USED in LoanServiceImpl : method count()).
	 *
	 * @param statut the statut
	 * @param parentId the parentId
	 */
	public LoanDTO(Integer statut, Long parentId) {

		this.statut = statut;
		this.parentId = parentId;
	}

	/**
	 * Instantiates a new loan DTO : USED IN BATCH.
	 *
	 * @param portfolioId the portfolio id
	 * @param idLoanExtern the id loan extern
	 * @param idAccountExtern the id account extern
	 * @param accountNumber the account number
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

	public LoanDTO(Long portfolioId, Long idLoanExtern, Long idAccountExtern, String accountNumber,
			Date applyDate, String productCode, String productDescription, String customerName,
			String portfolioCode, String portfolioDescription, String currencySymbol,
			Integer currencyDecimalPlaces, Integer productId, Long customerId,
			BigDecimal applyAmountTotal, Integer gracePeriod, String industryCode,
			String industryCodeDescription, Date issueDate, Date creationDate,
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
		this.accountNumber = accountNumber;
		this.productCode = productCode;
		this.productDescription = productDescription;
		this.customerName = customerName;
		this.portfolioCode = portfolioCode;
		this.portfolioDescription = portfolioDescription;
		this.currencySymbol = currencySymbol;
		this.currencyDecimalPlaces = currencyDecimalPlaces;
		this.applyDate = applyDate;
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
	 * Instantiates a new loan DTO.
	 *
	 * @param portfolioId the portfolio id
	 * @param idLoanExtern the id loan extern
	 * @param idAccountExtern the id account extern
	 * @param accountNumber the account number
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
	 * @param loanApplicationStatus the loan application status
	 */
	public LoanDTO(Long portfolioId, Long idLoanExtern, Long idAccountExtern, String accountNumber,
			Date applyDate, String productCode, String productDescription, String customerName,
			String portfolioCode, String portfolioDescription, String currencySymbol,
			Integer currencyDecimalPlaces, Integer productId, Long customerId,
			BigDecimal applyAmountTotal, Integer gracePeriod, String industryCode,
			String industryCodeDescription, Date issueDate, Date creationDate,
			Integer termPeriodNum, Integer paymentFreq, BigDecimal issueFeeAmount,

			BigDecimal productRate, String loanReasonCode, String loanReasonDescription,

			Date initialPaymentDate, Long normalPayment, Boolean ignoreOddDays,
			Integer periodsDeferred, Boolean calculateInitialPaymentDate, Long termPeriodID,
			Integer branchID, String branchName, String branchDescription, String customerType,

			Long communityCULoanID, Integer guarantorSourceId, Integer sourceOfFundsID,
			Integer refinanceReasonId, Integer districtCodeId, Integer intPayPeriodNum,
			Integer loanCalculationMode, BigDecimal apr, BigDecimal effectiveIntRate,
			String loanApplicationStatus) {

		this.portfolioId = portfolioId;
		this.idLoanExtern = idLoanExtern;
		this.idAccountExtern = idAccountExtern;
		this.accountNumber = accountNumber;
		this.productCode = productCode;
		this.productDescription = productDescription;
		this.customerName = customerName;
		this.portfolioCode = portfolioCode;
		this.portfolioDescription = portfolioDescription;
		this.currencySymbol = currencySymbol;
		this.currencyDecimalPlaces = currencyDecimalPlaces;
		this.applyDate = applyDate;
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
		this.loanApplicationStatus = loanApplicationStatus;

	}

	/**
	 * Instantiates a new loan DTO.
	 *
	 * @param portfolioId the portfolio id
	 * @param idLoanExtern the id loan extern
	 * @param idAccountExtern the id account extern
	 * @param accountNumber the account number
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
	 * @param interestFreq the interest freq
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
	 * @param loanApplicationStatus the loan application status
	 * @param openingBalance the opening balance
	 * @param periodsDeferredType the periods deferred type
	 * @param installmentNumber the installment number
	 * @param userDefinedFieldsLinksDTOs the user defined fields links DT os
	 */
	public LoanDTO(Long portfolioId, Long idLoanExtern, Long idAccountExtern, String accountNumber,
			Date applyDate, String productCode, String productDescription, String customerName,
			String portfolioCode, String portfolioDescription, String currencySymbol,
			Integer currencyDecimalPlaces, Integer productId, Long customerId,
			BigDecimal applyAmountTotal, Integer gracePeriod, String industryCode,
			String industryCodeDescription, Date issueDate, Date creationDate,
			Integer termPeriodNum, Integer interestFreq, Integer paymentFreq,
			BigDecimal issueFeeAmount,

			BigDecimal productRate, String loanReasonCode, String loanReasonDescription,
			Date initialPaymentDate, Long normalPayment, Boolean ignoreOddDays,
			Integer periodsDeferred, Boolean calculateInitialPaymentDate, Long termPeriodID,
			Integer branchID, String branchName, String branchDescription, String customerType,
			Long communityCULoanID, Integer guarantorSourceId, Integer sourceOfFundsID,
			Integer refinanceReasonId, Integer districtCodeId, Integer intPayPeriodNum,
			Integer loanCalculationMode, BigDecimal apr, BigDecimal effectiveIntRate,
			String loanApplicationStatus, BigDecimal openingBalance, Integer periodsDeferredType,
			Integer installmentNumber, List<UserDefinedFieldsLinksDTO> userDefinedFieldsLinksDTOs) {

		this.portfolioId = portfolioId;
		this.idLoanExtern = idLoanExtern;
		this.idAccountExtern = idAccountExtern;
		this.accountNumber = accountNumber;
		this.productCode = productCode;
		this.productDescription = productDescription;
		this.customerName = customerName;
		this.portfolioCode = portfolioCode;
		this.portfolioDescription = portfolioDescription;
		this.currencySymbol = currencySymbol;
		this.currencyDecimalPlaces = currencyDecimalPlaces;
		this.applyDate = applyDate;
		this.productId = productId;
		this.customerId = customerId;

		this.applyAmountTotal = applyAmountTotal;
		this.gracePeriod = gracePeriod;
		this.industryCode = industryCode;
		this.industryCodeDescription = industryCodeDescription;
		this.issueDate = issueDate;
		this.creationDate = creationDate;
		this.termPeriodNum = termPeriodNum;
		this.interestFreq = interestFreq;
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
		this.loanApplicationStatus = loanApplicationStatus;
		this.openingBalance = openingBalance;
		this.periodsDeferredType = periodsDeferredType;
		this.installmentNumber = installmentNumber;
		this.userDefinedFieldsLinksDTOs = userDefinedFieldsLinksDTOs;
	}

	/**
	 * Instantiates a new loan DTO.
	 *
	 * @param loanId the loan id
	 */
	public LoanDTO(Long loanId) {

		this.loanId = loanId;
	}

	/**
	 * Instantiates a new loan DTO => used to init loan object in accept() method in
	 * {@link IBLoanServiceImpl} .
	 *
	 * @param applyAmountTotal the apply amount total
	 * @param applyDate the apply date
	 * @param gracePeriod the grace period
	 * @param issueDate the issue date
	 * @param portfolioId the portfolio id
	 * @param productId the product id
	 * @param productDTO the product DTO
	 * @param statut the statut
	 * @param creationDate the creation date
	 * @param termPeriodNum the term period num
	 * @param paymentFreq the payment freq
	 * @param issueFeeAmount the issue fee amount
	 * @param productCode the product code
	 * @param productDescription the product description
	 * @param customerName the customer name
	 * @param portfolioCode the portfolio code
	 * @param portfolioDescription the portfolio description
	 * @param currencySymbol the currency symbol
	 * @param currencyDecimalPlaces the currency decimal places
	 * @param owner the owner
	 * @param initialPaymentDate the initial payment date
	 * @param normalPayment the normal payment
	 * @param periodsDeferred the periods deferred
	 * @param termPeriodID the term period ID
	 * @param branchID the branch ID
	 * @param branchName the branch name
	 * @param branchDescription the branch description
	 * @param customerDTO the customer DTO
	 * @param amountWord the amount word
	 * @param customerType the customer type
	 * @param approvelAmount the approvel amount
	 */
	public LoanDTO(BigDecimal applyAmountTotal, Date applyDate, Integer gracePeriod, Date issueDate,
			Long portfolioId, Integer productId, ProductDTO productDTO, Integer statut,
			Date creationDate, Integer termPeriodNum, Integer paymentFreq,
			BigDecimal issueFeeAmount, String productCode, String productDescription,
			String customerName, String portfolioCode, String portfolioDescription,
			String currencySymbol, Integer currencyDecimalPlaces, String owner,
			Date initialPaymentDate, Long normalPayment, Integer periodsDeferred, Long termPeriodID,
			Integer branchID, String branchName, String branchDescription, CustomerDTO customerDTO,
			String amountWord, String customerType, BigDecimal approvelAmount) {

		this.applyAmountTotal = applyAmountTotal;
		this.approvelAmount = approvelAmount;
		this.applyDate = applyDate;
		this.gracePeriod = gracePeriod;
		this.issueDate = issueDate;
		this.portfolioId = portfolioId;
		this.productId = productId;
		this.productDTO = productDTO;
		this.statut = statut;
		this.creationDate = creationDate;
		this.termPeriodNum = termPeriodNum;
		this.paymentFreq = paymentFreq;
		this.issueFeeAmount = issueFeeAmount;
		this.productCode = productCode;
		this.productDescription = productDescription;
		this.customerName = customerName;
		this.portfolioCode = portfolioCode;
		this.portfolioDescription = portfolioDescription;
		this.currencySymbol = currencySymbol;
		this.currencyDecimalPlaces = currencyDecimalPlaces;
		this.owner = owner;
		this.initialPaymentDate = initialPaymentDate;
		this.normalPayment = normalPayment;
		this.periodsDeferred = periodsDeferred;
		this.termPeriodID = termPeriodID;
		this.branchID = branchID;
		this.branchName = branchName;
		this.branchDescription = branchDescription;
		this.customerDTO = customerDTO;
		this.amountWord = amountWord;
		this.customerType = customerType;
	}

	/**
	 * Instantiates a new loan DTO.
	 *
	 * @param customerDTO the customer DTO
	 * @param statutWorkflow the statut workflow
	 */
	public LoanDTO(CustomerDTO customerDTO, Integer statutWorkflow) {

		this.customerDTO = customerDTO;
		this.statutWorkflow = statutWorkflow;
	}

	/**
	 * Gets the loan id.
	 *
	 * @return the loanId
	 */
	public Long getLoanId() {

		return loanId;
	}

	/**
	 * Sets the loan id.
	 *
	 * @param loanId the loanId to set
	 */
	public void setLoanId(Long loanId) {

		this.loanId = loanId;
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
	 * @param statut the statut to set
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
	 * Gets the cu loan re finance reason.
	 *
	 * @return the cuLoanReFinanceReason
	 */
	public Integer getCuLoanReFinanceReason() {

		return cuLoanReFinanceReason;
	}

	/**
	 * Sets the cu loan re finance reason.
	 *
	 * @param cuLoanReFinanceReason the cuLoanReFinanceReason to set
	 */
	public void setCuLoanReFinanceReason(Integer cuLoanReFinanceReason) {

		this.cuLoanReFinanceReason = cuLoanReFinanceReason;
	}

	/**
	 * Gets the currency.
	 *
	 * @return the currencyId
	 */
	public Integer getCurrencyId() {

		return currencyId;
	}

	/**
	 * Sets the currency.
	 *
	 * @param currencyId the currency to set
	 */
	public void setCurrencyId(Integer currencyId) {

		this.currencyId = currencyId;
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
	 * Gets the loan source offunds.
	 *
	 * @return the loanSourceOffunds
	 */
	public Integer getLoanSourceOffunds() {

		return loanSourceOffunds;
	}

	/**
	 * Sets the loan source offunds.
	 *
	 * @param loanSourceOffunds the loanSourceOffunds to set
	 */
	public void setLoanSourceOffunds(Integer loanSourceOffunds) {

		this.loanSourceOffunds = loanSourceOffunds;
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
	 * Gets the payment period.
	 *
	 * @return the paymentPeriod
	 */
	public Integer getPaymentPeriod() {

		return paymentPeriod;
	}

	/**
	 * Sets the payment period.
	 *
	 * @param paymentPeriod the paymentPeriod to set
	 */
	public void setPaymentPeriod(Integer paymentPeriod) {

		this.paymentPeriod = paymentPeriod;
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
	 * @param statutLibelle the statutLibelle to set
	 */
	public void setStatutLibelle(String statutLibelle) {

		this.statutLibelle = statutLibelle;
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
	 * Sets the etape workflow.
	 *
	 * @param etapeWorkflow the etapeWorkflow to set
	 */
	public void setEtapeWorkflow(Integer etapeWorkflow) {

		this.etapeWorkflow = etapeWorkflow;
	}

	/**
	 * Gets the pourcentage.
	 *
	 * @return the pourcentage
	 */
	public double getPourcentage() {

		if (etapeWorkflow != null) {
			this.pourcentage = ((double) etapeWorkflow * 100) / 13;
			return Math.round(pourcentage * 10) / 10.0;
		}
		return pourcentage;
	}

	/**
	 * Sets the pourcentage.
	 *
	 * @param pourcentage the pourcentage to set
	 */
	public void setPourcentage(double pourcentage) {

		this.pourcentage = pourcentage;
	}

	/**
	 * Gets the workflow next action.
	 *
	 * @return the workflowNextAction
	 */
	public String getWorkflowNextAction() {

		return workflowNextAction;
	}

	/**
	 * Sets the workflow next action.
	 *
	 * @param workflowNextAction the workflowNextAction to set
	 */
	public void setWorkflowNextAction(String workflowNextAction) {

		this.workflowNextAction = workflowNextAction;
	}

	/**
	 * Gets the ihmRoot.
	 *
	 * @return the ihmRoot
	 */
	public String getIhmRoot() {

		return ihmRoot;
	}

	/**
	 * Sets the ihmRoot.
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
	 * Gets the dateLastUpdate.
	 *
	 * @return the dateLastUpdate
	 */
	public Date getDateLastUpdate() {

		return dateLastUpdate;
	}

	/**
	 * Sets the dateLastUpdate.
	 *
	 * @param dateLastUpdate the dateLastUpdate to set
	 */
	public void setDateLastUpdate(Date dateLastUpdate) {

		this.dateLastUpdate = dateLastUpdate;
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
	 * Gets the code extern motif rejet.
	 *
	 * @return the codeExternMotifRejet
	 */
	public Integer getCodeExternMotifRejet() {

		return codeExternMotifRejet;
	}

	/**
	 * Sets the code extern motif rejet.
	 *
	 * @param codeExternMotifRejet the codeExternMotifRejet to set
	 */
	public void setCodeExternMotifRejet(Integer codeExternMotifRejet) {

		this.codeExternMotifRejet = codeExternMotifRejet;
	}

	/**
	 * Gets the contact date customer decision.
	 *
	 * @return the contactDateCustomerDecision
	 */
	public Date getContactDateCustomerDecision() {

		return contactDateCustomerDecision;
	}

	/**
	 * Sets the contact date customer decision.
	 *
	 * @param contactDateCustomerDecision the contactDateCustomerDecision to set
	 */
	public void setContactDateCustomerDecision(Date contactDateCustomerDecision) {

		this.contactDateCustomerDecision = contactDateCustomerDecision;
	}

	/**
	 * Gets the comments customer decision.
	 *
	 * @return the commentsCustomerDecision
	 */
	public String getCommentsCustomerDecision() {

		return commentsCustomerDecision;
	}

	/**
	 * Sets the comments customer decision.
	 *
	 * @param commentsCustomerDecision the commentsCustomerDecision to set
	 */
	public void setCommentsCustomerDecision(String commentsCustomerDecision) {

		this.commentsCustomerDecision = commentsCustomerDecision;
	}

	/**
	 * Gets the confirm.
	 *
	 * @return the confirm
	 */
	public Boolean getConfirm() {

		return confirm;
	}

	/**
	 * Sets the confirm.
	 *
	 * @param confirm the confirm to set
	 */
	public void setConfirm(Boolean confirm) {

		this.confirm = confirm;
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
	 * Gets the list missing data.
	 * 
	 * @return the listMissingData
	 */
	public List<String> getListMissingData() {

		return listMissingData;
	}

	/**
	 * Sets the list missing data.
	 * 
	 * @param listMissingData the listMissingData to set
	 */
	public void setListMissingData(List<String> listMissingData) {

		this.listMissingData = listMissingData;
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
	 * Gets the customer DTO.
	 *
	 * @return the customerDTO
	 */
	public CustomerDTO getCustomerDTO() {

		return customerDTO;
	}

	/**
	 * Sets the customer DTO.
	 *
	 * @param customerDTO the customerDTO to set
	 */
	public void setCustomerDTO(CustomerDTO customerDTO) {

		this.customerDTO = customerDTO;
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
	 * Gets the amount word.
	 * 
	 * @return the amountWord
	 */
	public String getAmountWord() {

		return amountWord;
	}

	/**
	 * Sets the amount word.
	 * 
	 * @param amountWord the amountWord to set
	 */
	public void setAmountWord(String amountWord) {

		this.amountWord = amountWord;
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
	 * Gets the approvel amount groupe.
	 * 
	 * @return the approvelAmountGroupe
	 */
	public BigDecimal getApprovelAmountGroupe() {

		return approvelAmountGroupe;
	}

	/**
	 * Sets the approvel amount groupe.
	 * 
	 * @param approvelAmountGroupe the approvelAmountGroupe to set
	 */
	public void setApprovelAmountGroupe(BigDecimal approvelAmountGroupe) {

		this.approvelAmountGroupe = approvelAmountGroupe;
	}

	/**
	 * Gets the child missing info.
	 * 
	 * @return the childMissingInfo
	 */
	public Boolean getChildMissingInfo() {

		return childMissingInfo;
	}

	/**
	 * Sets the child missing info.
	 * 
	 * @param childMissingInfo the childMissingInfo to set
	 */
	public void setChildMissingInfo(Boolean childMissingInfo) {

		this.childMissingInfo = childMissingInfo;
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

	/*
	 * (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {

		return "LoanDTO [loanId=" + loanId + ", applyAmountTotal=" + applyAmountTotal
				+ ", approvelAmount=" + approvelAmount + ", loanApprovalLevel=" + loanApprovalLevel
				+ ", approvelAmountGroupe=" + approvelAmountGroupe + ", applyDate=" + applyDate
				+ ", cuLoanReFinanceReason=" + cuLoanReFinanceReason + ", currencyId=" + currencyId
				+ ", gracePeriod=" + gracePeriod + ", industryCode=" + industryCode
				+ ", industryCodeDescription=" + industryCodeDescription + ", issueDate="
				+ issueDate + ", loanSourceOffunds=" + loanSourceOffunds + ", portfolioId="
				+ portfolioId + ", productId=" + productId + ", processInstanceId="
				+ processInstanceId + ", idLoanExtern=" + idLoanExtern + ", statut=" + statut
				+ ", statutLibelle=" + statutLibelle + ", statutWorkflow=" + statutWorkflow
				+ ", etapeWorkflow=" + etapeWorkflow + ", workflowNextAction=" + workflowNextAction
				+ ", idAccountExtern=" + idAccountExtern + ", accountNumber=" + accountNumber
				+ ", creationDate=" + creationDate + ", termPeriodNum=" + termPeriodNum
				+ ", paymentPeriod=" + paymentPeriod + ", paymentFreq=" + paymentFreq
				+ ", issueFeeAmount=" + issueFeeAmount + ", productCode=" + productCode
				+ ", productDescription=" + productDescription + ", productRate=" + productRate
				+ ", customerId=" + customerId + ", customerName=" + customerName
				+ ", customerNameNoPipe=" + customerNameNoPipe + ", loanReasonId=" + loanReasonId
				+ ", loanReasonCode=" + loanReasonCode + ", loanReasonDescription="
				+ loanReasonDescription + ", portfolioCode=" + portfolioCode
				+ ", portfolioDescription=" + portfolioDescription + ", currencySymbol="
				+ currencySymbol + ", currencyDecimalPlaces=" + currencyDecimalPlaces + ", owner="
				+ owner + ", ownerName=" + ownerName + ", pourcentage=" + pourcentage + ", ihmRoot="
				+ ihmRoot + ", note=" + note + ", dateLastUpdate=" + dateLastUpdate
				+ ", loanCalculationMode=" + loanCalculationMode + ", apr=" + apr
				+ ", effectiveIntRate=" + effectiveIntRate + ", initialPaymentDate="
				+ initialPaymentDate + ", normalPayment=" + normalPayment + ", ignoreOddDays="
				+ ignoreOddDays + ", periodsDeferred=" + periodsDeferred + ", periodsDeferredType="
				+ periodsDeferredType + ", calculateInitialPaymentDate="
				+ calculateInitialPaymentDate + ", termPeriodID=" + termPeriodID
				+ ", codeExternMotifRejet=" + codeExternMotifRejet
				+ ", contactDateCustomerDecision=" + contactDateCustomerDecision
				+ ", commentsCustomerDecision=" + commentsCustomerDecision + ", confirm=" + confirm
				+ ", branchID=" + branchID + ", branchName=" + branchName + ", branchDescription="
				+ branchDescription + ", category=" + category + ", changeDateStatusWorkflow="
				+ changeDateStatusWorkflow + ", customerDTO=" + customerDTO + ", amountWord="
				+ amountWord + ", processName=" + processName + ", customerType=" + customerType
				+ ", parentId=" + parentId + ", childMissingInfo=" + childMissingInfo
				+ ", communityCULoanID=" + communityCULoanID + ", guarantorSourceId="
				+ guarantorSourceId + ", sourceOfFundsID=" + sourceOfFundsID
				+ ", refinanceReasonId=" + refinanceReasonId + ", districtCodeId=" + districtCodeId
				+ ", interestFreq=" + interestFreq + ", intPayPeriodNum=" + intPayPeriodNum
				+ ", updateLoan=" + updateLoan + ", productDTO=" + productDTO
				+ ", loanInstancesDtos=" + loanInstancesDtos + ", userDefinedFieldsLinksDTOs="
				+ userDefinedFieldsLinksDTOs + ", listMissingData=" + listMissingData
				+ ", guarantors=" + guarantors + ", assignCustomer=" + assignCustomer
				+ ", isNotFromWorkflow=" + isNotFromWorkflow + ", lastApprovalDate="
				+ lastApprovalDate + ", customerGroupeNumber=" + customerGroupeNumber
				+ ", customerRole=" + customerRole + ", changed=" + changed + ", updatedBy="
				+ updatedBy + ", groupOwnerName=" + groupOwnerName + ", groupOwner=" + groupOwner
				+ ", collaterals=" + collaterals + ", feeAmt1=" + feeAmt1 + ", updateLoanAbacus="
				+ updateLoanAbacus + ", assignedToOneUser=" + assignedToOneUser
				+ ", loanApplicationStatus=" + loanApplicationStatus + ", openingBalance="
				+ openingBalance + ", enabled=" + enabled + ", workFlowAction=" + workFlowAction
				+ ", installmentNumber=" + installmentNumber + ", settingTopupDTOs="
				+ settingTopupDTOs + ", loanAssetsDtos=" + loanAssetsDtos + ", quantitySupplier="
				+ quantitySupplier + ", balanceSupplier=" + balanceSupplier
				+ ", reviewOnlySelectedStep=" + reviewOnlySelectedStep + ", reviewFrom="
				+ reviewFrom + ", totalInterest=" + totalInterest + ", personalContribution="
				+ personalContribution + ", workflowCompleted=" + workflowCompleted
				+ ", ownerEmail=" + ownerEmail + ", idIbLoan=" + idIbLoan + ", loanSchedules="
				+ loanSchedules + ", otherInformations=" + otherInformations + "]";
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
	 * Gets the assigned to one user.
	 *
	 * @return the assigned to one user
	 */
	public Boolean getAssignedToOneUser() {

		return assignedToOneUser;
	}

	/**
	 * Sets the assigned to one user.
	 *
	 * @param assignedToOneUser the new assigned to one user
	 */
	public void setAssignedToOneUser(Boolean assignedToOneUser) {

		this.assignedToOneUser = assignedToOneUser;
	}

	/**
	 * Gets the collaterals.
	 *
	 * @return the collaterals
	 */
	public List<AcmCollateralDTO> getCollaterals() {

		return collaterals;
	}

	/**
	 * Sets the collaterals.
	 *
	 * @param collaterals the new collaterals
	 */
	public void setCollaterals(List<AcmCollateralDTO> collaterals) {

		this.collaterals = collaterals;
	}

	/**
	 * Gets the fee amt 1.
	 *
	 * @return the fee amt 1
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
	public BigDecimal getOpeningBalance() {

		return openingBalance;
	}

	/**
	 * Sets the opening balance.
	 *
	 * @param openingBalance the new opening balance
	 */
	public void setOpeningBalance(BigDecimal openingBalance) {

		this.openingBalance = openingBalance;
	}

	/**
	 * Gets the enabled.
	 *
	 * @return the enabled
	 */
	public Boolean getEnabled() {

		return enabled;
	}

	/**
	 * Sets the enabled.
	 *
	 * @param enabled the new enabled
	 */
	public void setEnabled(Boolean enabled) {

		this.enabled = enabled;
	}

	/**
	 * Gets the work flow action.
	 *
	 * @return the workFlowAction
	 */
	public String getWorkFlowAction() {

		return workFlowAction;
	}

	/**
	 * Sets the work flow action.
	 *
	 * @param workFlowAction the workFlowAction to set
	 */
	public void setWorkFlowAction(String workFlowAction) {

		this.workFlowAction = workFlowAction;

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
	 * Gets the update loan abacus.
	 *
	 * @return the updateLoanAbacus
	 */
	public Boolean getUpdateLoanAbacus() {

		return updateLoanAbacus;
	}

	/**
	 * Sets the update loan abacus.
	 *
	 * @param updateLoanAbacus the updateLoanAbacus to set
	 */
	public void setUpdateLoanAbacus(Boolean updateLoanAbacus) {

		this.updateLoanAbacus = updateLoanAbacus;
	}

	/**
	 * Gets the loan approval level.
	 *
	 * @return the loanApprovalLevel
	 */
	public Integer getLoanApprovalLevel() {

		return loanApprovalLevel;
	}

	/**
	 * Sets the loan approval level.
	 *
	 * @param loanApprovalLevel the loanApprovalLevel to set
	 */
	public void setLoanApprovalLevel(Integer loanApprovalLevel) {

		this.loanApprovalLevel = loanApprovalLevel;
	}

	/**
	 * Gets the customer name no pipe.
	 *
	 * @return the customerNameNoPipe
	 */
	public String getCustomerNameNoPipe() {

		return customerNameNoPipe;
	}

	/**
	 * Sets the customer name no pipe.
	 *
	 * @param customerNameNoPipe the customerNameNoPipe to set
	 */
	public void setCustomerNameNoPipe(String customerNameNoPipe) {

		this.customerNameNoPipe = customerNameNoPipe;
	}

	/**
	 * Gets the loan reason id.
	 *
	 * @return the loanReasonId
	 */
	public String getLoanReasonId() {

		return loanReasonId;
	}

	/**
	 * Sets the loan reason id.
	 *
	 * @param loanReasonId the loanReasonId to set
	 */
	public void setLoanReasonId(String loanReasonId) {

		this.loanReasonId = loanReasonId;
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
	 * Gets the periods deferred type.
	 *
	 * @return the periodsDeferredType
	 */
	public Integer getPeriodsDeferredType() {

		return periodsDeferredType;
	}

	/**
	 * Sets the periods deferred type.
	 *
	 * @param periodsDeferredType the periodsDeferredType to set
	 */
	public void setPeriodsDeferredType(Integer periodsDeferredType) {

		this.periodsDeferredType = periodsDeferredType;
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
	 * Gets the product DTO.
	 *
	 * @return the productDTO
	 */
	public ProductDTO getProductDTO() {

		return productDTO;
	}

	/**
	 * Sets the product DTO.
	 *
	 * @param productDTO the productDTO to set
	 */
	public void setProductDTO(ProductDTO productDTO) {

		this.productDTO = productDTO;
	}

	/**
	 * Gets the loan instances dtos.
	 *
	 * @return the loanInstancesDtos
	 */
	public List<LoanInstanceDTO> getLoanInstancesDtos() {

		return loanInstancesDtos.stream()
				// only keep enabled row
				.filter(loanInstance -> Boolean.TRUE.equals(loanInstance.getEnabled()))
				// sort by orderEtapeProcess
				.sorted(Comparator.comparingLong(LoanInstanceDTO::getOrderEtapeProcess))
				.collect(Collectors.toList());
	}

	/**
	 * Sets the loan instances dtos.
	 *
	 * @param loanInstancesDtos the loanInstancesDtos to set
	 */
	public void setLoanInstancesDtos(List<LoanInstanceDTO> loanInstancesDtos) {

		this.loanInstancesDtos = loanInstancesDtos;
	}

	/**
	 * Gets the user defined fields links DT os.
	 *
	 * @return the userDefinedFieldsLinksDTOs
	 */
	public List<UserDefinedFieldsLinksDTO> getUserDefinedFieldsLinksDTOs() {

		return userDefinedFieldsLinksDTOs;
	}

	/**
	 * Sets the user defined fields links DT os.
	 *
	 * @param userDefinedFieldsLinksDTOs the userDefinedFieldsLinksDTOs to set
	 */
	public void setUserDefinedFieldsLinksDTOs(
			List<UserDefinedFieldsLinksDTO> userDefinedFieldsLinksDTOs) {

		this.userDefinedFieldsLinksDTOs = userDefinedFieldsLinksDTOs;
	}

	/**
	 * Gets the guarantors.
	 *
	 * @return the guarantors
	 */
	public List<CustomerDTO> getGuarantors() {

		return guarantors;
	}

	/**
	 * Sets the guarantors.
	 *
	 * @param guarantors the guarantors to set
	 */
	public void setGuarantors(List<CustomerDTO> guarantors) {

		this.guarantors = guarantors;
	}

	/**
	 * Gets the assign customer.
	 *
	 * @return the assignCustomer
	 */
	public Boolean getAssignCustomer() {

		return assignCustomer;
	}

	/**
	 * Sets the assign customer.
	 *
	 * @param assignCustomer the assignCustomer to set
	 */
	public void setAssignCustomer(Boolean assignCustomer) {

		this.assignCustomer = assignCustomer;
	}

	/**
	 * Gets the checks if is not from workflow.
	 *
	 * @return the isNotFromWorkflow
	 */
	public Boolean getIsNotFromWorkflow() {

		return isNotFromWorkflow;
	}

	/**
	 * Sets the checks if is not from workflow.
	 *
	 * @param isNotFromWorkflow the isNotFromWorkflow to set
	 */
	public void setIsNotFromWorkflow(Boolean isNotFromWorkflow) {

		this.isNotFromWorkflow = isNotFromWorkflow;
	}

	/**
	 * Gets the last approval date.
	 *
	 * @return the lastApprovalDate
	 */
	public Date getLastApprovalDate() {

		return lastApprovalDate;
	}

	/**
	 * Sets the last approval date.
	 *
	 * @param lastApprovalDate the lastApprovalDate to set
	 */
	public void setLastApprovalDate(Date lastApprovalDate) {

		this.lastApprovalDate = lastApprovalDate;
	}

	/**
	 * Gets the customer groupe number.
	 *
	 * @return the customerGroupeNumber
	 */
	public String getCustomerGroupeNumber() {

		return customerGroupeNumber;
	}

	/**
	 * Sets the customer groupe number.
	 *
	 * @param customerGroupeNumber the customerGroupeNumber to set
	 */
	public void setCustomerGroupeNumber(String customerGroupeNumber) {

		this.customerGroupeNumber = customerGroupeNumber;
	}

	/**
	 * Gets the customer role.
	 *
	 * @return the customerRole
	 */
	public String getCustomerRole() {

		return customerRole;
	}

	/**
	 * Sets the customer role.
	 *
	 * @param customerRole the customerRole to set
	 */
	public void setCustomerRole(String customerRole) {

		this.customerRole = customerRole;
	}

	/**
	 * Gets the changed.
	 *
	 * @return the changed
	 */
	public Boolean getChanged() {

		return changed;
	}

	/**
	 * Sets the changed.
	 *
	 * @param changed the changed to set
	 */
	public void setChanged(Boolean changed) {

		this.changed = changed;
	}

	/**
	 * Gets the updated by.
	 *
	 * @return the updatedBy
	 */
	public String getUpdatedBy() {

		return updatedBy;
	}

	/**
	 * Sets the updated by.
	 *
	 * @param updatedBy the updatedBy to set
	 */
	public void setUpdatedBy(String updatedBy) {

		this.updatedBy = updatedBy;
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
	 * Gets the setting topup DT os.
	 *
	 * @return the setting topup DT os
	 */
	public List<SettingTopupDTO> getSettingTopupDTOs() {

		return settingTopupDTOs;
	}

	/**
	 * Sets the setting topup DT os.
	 *
	 * @param settingTopupDTOs the new setting topup DT os
	 */
	public void setSettingTopupDTOs(List<SettingTopupDTO> settingTopupDTOs) {

		this.settingTopupDTOs = settingTopupDTOs;
	}

	/**
	 * Gets the loan assets dtos.
	 *
	 * @return the loan assets dtos
	 */
	public List<AssetLoanDTO> getLoanAssetsDtos() {

		return loanAssetsDtos;
	}

	/**
	 * Sets the loan assets dtos.
	 *
	 * @param loanAssetsDtos the new loan assets dtos
	 */
	public void setLoanAssetsDtos(List<AssetLoanDTO> loanAssetsDtos) {

		this.loanAssetsDtos = loanAssetsDtos;
	}

	/**
	 * Gets the quantity supplier.
	 *
	 * @return the quantity supplier
	 */
	public Integer getQuantitySupplier() {

		return quantitySupplier;
	}

	/**
	 * Sets the quantity supplier.
	 *
	 * @param quantitySupplier the new quantity supplier
	 */
	public void setQuantitySupplier(Integer quantitySupplier) {

		this.quantitySupplier = quantitySupplier;
	}

	/**
	 * Gets the balance supplier.
	 *
	 * @return the balance supplier
	 */
	public BigDecimal getBalanceSupplier() {

		return balanceSupplier;
	}

	/**
	 * Sets the balance supplier.
	 *
	 * @param balanceSupplier the new balance supplier
	 */
	public void setBalanceSupplier(BigDecimal balanceSupplier) {

		this.balanceSupplier = balanceSupplier;
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
	 * Gets the review only selected step.
	 *
	 * @return the review only selected step
	 */
	public Boolean getReviewOnlySelectedStep() {

		return reviewOnlySelectedStep;
	}

	/**
	 * Sets the review only selected step.
	 *
	 * @param reviewOnlySelectedStep the new review only selected step
	 */
	public void setReviewOnlySelectedStep(Boolean reviewOnlySelectedStep) {

		this.reviewOnlySelectedStep = reviewOnlySelectedStep;
	}

	/**
	 * Gets the owner email.
	 *
	 * @return the owner email
	 */
	public String getOwnerEmail() {

		return ownerEmail;
	}

	/**
	 * Sets the owner email.
	 *
	 * @param ownerEmail the new owner email
	 */
	public void setOwnerEmail(String ownerEmail) {

		this.ownerEmail = ownerEmail;
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
	 * Gets the schedules.
	 *
	 * @return the schedules
	 */
	public List<ScheduleDTO> getSchedules() {

		return loanSchedules;
	}

	/**
	 * Sets the schedules.
	 *
	 * @param schedules the new schedules
	 */
	public void setSchedules(List<ScheduleDTO> schedules) {

		this.loanSchedules = schedules;
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
	 * Gets the loan schedules.
	 *
	 * @return the loan schedules
	 */
	public List<ScheduleDTO> getLoanSchedules() {

		return loanSchedules;
	}

	/**
	 * Sets the loan schedules.
	 *
	 * @param loanSchedules the new loan schedules
	 */
	public void setLoanSchedules(List<ScheduleDTO> loanSchedules) {

		this.loanSchedules = loanSchedules;
	}

	/**
	 * Gets the step path.
	 *
	 * @return the step path
	 */
	public String getStepPath() {

		return stepPath;
	}

	/**
	 * Sets the step path.
	 *
	 * @param stepPath the new step path
	 */
	public void setStepPath(String stepPath) {

		this.stepPath = stepPath;
	}

	/**
	 * Gets the send to ib.
	 *
	 * @return the send to ib
	 */
	public Boolean getSendToIb() {

		return sendToIb;
	}

	/**
	 * Sets the send to ib.
	 *
	 * @param sendToIb the new send to ib
	 */
	public void setSendToIb(Boolean sendToIb) {

		this.sendToIb = sendToIb;
	}

	/**
	 * Gets the custom rate.
	 *
	 * @return the custom rate
	 */
	public BigDecimal getCustomRate() {

		return customRate;
	}

	/**
	 * Sets the custom rate.
	 *
	 * @param customRate the new custom rate
	 */
	public void setCustomRate(BigDecimal customRate) {

		this.customRate = customRate;
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
