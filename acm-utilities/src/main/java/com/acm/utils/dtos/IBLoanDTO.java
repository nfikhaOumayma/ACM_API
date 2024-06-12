/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.utils.dtos;

import java.io.Serializable;
import java.math.BigDecimal;
import java.util.Date;
import java.util.List;

import org.dozer.Mapping;

/**
 * {@link IBLoanDTO} class.
 *
 * @author MoezMhiri
 * @since 1.0.3
 */
public class IBLoanDTO extends GenericDTO implements Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = -8486227440075390744L;

	/** The loanid. */
	private Long id;

	/** The id loan extern. */
	private Long idLoanExtern;

	/** The applyamounttotal. */
	private BigDecimal applyAmountTotal;

	/** The applydate. */
	private Date applyDate;

	/** The graceperiod. */
	private Integer gracePeriod;

	/** The issuedate. */
	private Date issueDate;

	/** The portfolioid. */
	private Long portfolioId;

	/** The productid. */
	private Integer productId;

	/** The product DTO. */
	private ProductDTO productDTO;

	/** The statut. */
	private Integer statut;

	/** The creation date. */
	private Date creationDate;

	/** The term period num. */
	private Integer termPeriodNum;

	/** The payment freq. */
	private Integer paymentFreq;

	/** The issue fee amount. */
	private BigDecimal issueFeeAmount;

	/** The product code. */
	private String productCode;

	/** The product description. */
	private String productDescription;

	/** The customer name. */
	private String customerName;

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

	/** The note. */
	private String note;

	/** The last updated date. */
	private Date dateLastUpdate;

	/** The initial payment date. */
	private Date initialPaymentDate;

	/** The normal payment. */
	private Long normalPayment;

	/** The periods deferred. */
	private Integer periodsDeferred;

	/** The term period ID. */
	private Long termPeriodID;

	/** The branchID. */
	private Integer branchID;

	/** The branch name. */
	private String branchName;

	/** The branch description. */
	private String branchDescription;

	/** The customer DTO. */
	@Mapping("customer")
	private CustomerDTO customerDTO;

	/** The amountWord. */
	private String amountWord;

	/** The loan type. */
	private String loanType;

	/** The date insertion. */
	private Date dateInsertion;

	/** The project description. */
	private String projectDescription;

	/** The acm id loan. */
	private Long acmIdLoan;

	/** The account number. */
	private String accountNumber;

	/** The loan schedules. */
	private List<ScheduleDTO> loanSchedules;

	/**
	 * Instantiates a new IB loan DTO.
	 */
	public IBLoanDTO() {

		/*
		 * EMPTY
		 */
	}

	/**
	 * Instantiates a new IB loan DTO.
	 *
	 * @param portfolioId the portfolio id
	 * @param statut the statut
	 */
	public IBLoanDTO(Long portfolioId, Integer statut) {

		this.portfolioId = portfolioId;
		this.statut = statut;
	}

	/**
	 * Instantiates a new loan DTO (USED in LoanDetailsServiceImpl : method count()).
	 *
	 * @param statut the statut
	 */
	public IBLoanDTO(Integer statut) {

		this.statut = statut;
	}

	/**
	 * Instantiates a new loan DTO.
	 *
	 * @param loanIdIb the loan id ib
	 */
	public IBLoanDTO(Long loanIdIb) {

		this.id = loanIdIb;
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
	 * Gets the loan id ib.
	 * 
	 * @return the loanIdIb
	 */
	public Long getId() {

		return id;
	}

	/**
	 * Sets the loan id ib.
	 * 
	 * @param loanIdIb the loanIdIb to set
	 */
	public void setId(Long loanIdIb) {

		this.id = loanIdIb;
	}

	/**
	 * Gets the loan type.
	 *
	 * @return the loanType
	 */
	public String getLoanType() {

		return loanType;
	}

	/**
	 * Sets the loan type.
	 *
	 * @param loanType the loanType to set
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

	/**
	 * Gets the acm id loan.
	 *
	 * @return the acm id loan
	 */
	public Long getAcmIdLoan() {

		return acmIdLoan;
	}

	/**
	 * Sets the acm id loan.
	 *
	 * @param acmIdLoan the new acm id loan
	 */
	public void setAcmIdLoan(Long acmIdLoan) {

		this.acmIdLoan = acmIdLoan;
	}

	/**
	 * Gets the account number.
	 *
	 * @return the account number
	 */
	public String getAccountNumber() {

		return accountNumber;
	}

	/**
	 * Sets the account number.
	 *
	 * @param accountNumber the new account number
	 */
	public void setAccountNumber(String accountNumber) {

		this.accountNumber = accountNumber;
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
}
