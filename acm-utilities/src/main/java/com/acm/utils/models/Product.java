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
import javax.persistence.FetchType;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.OneToMany;
import javax.persistence.OneToOne;
import javax.persistence.Table;
import javax.persistence.Transient;

/**
 * {@link Product} class.
 *
 * @author HaythemBenizid
 * @since 0.7.0
 */
@Entity
@Table(name = "ACM_PRODUCT")
// @EntityListeners(AuditTrailListener.class)
public class Product extends GenericModel implements Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = -7203814268449840070L;

	/** The id. */
	@Id
	@Column(name = "ID_ACM_PRODUCT", nullable = false)
	private Long id;

	/** The code. */
	@Column(name = "CODE", nullable = false)
	private String code;

	/** The description. */
	@Column(name = "DESCRIPTION", nullable = false)
	private String description;

	/** The product id abacus. */
	@Column(name = "PRODUCT_ID_ABACUS")
	private Long productIdAbacus;

	/** The product type abacus. */
	@Column(name = "PRODUCT_TYPE_ID_ABACUS")
	private Long productTypeAbacus;

	/** The creation date abacus. */
	@Column(name = "CREATION_DATE_ABACUS")
	private Date creationDateAbacus;

	/** The edit date abacus. */
	@Column(name = "EDIT_DATE_ABACUS")
	private Date editDateAbacus;

	/** The product individual. */
	@Column(name = "INDIV")
	private Boolean productIndiv;

	/** The product group. */
	@Column(name = "GRP")
	private Boolean productGrp;

	/** The product organization. */
	@Column(name = "ORG")
	private Boolean productOrg;

	/** The rate start date. */
	@Column(name = "RATE_START_DATE")
	private Date rateStartDate;

	/** The rate end date. */
	@Column(name = "RATE_END_DATE")
	private Date rateEndDate;

	/** The rate. */
	@Column(name = "RATE")
	private BigDecimal rate;

	/** The maximum balance. */
	@Column(name = "MAXIMUM_BALANCE")
	private BigDecimal maximumBalance;

	/** The minimum term. */
	@Column(name = "MINIMUM_TERM")
	private Integer minimumTerm;

	/** The maximum term. */
	@Column(name = "MAXIMUM_TERM")
	private Integer maximumTerm;

	/** The issue fee percentage 1. */
	@Column(name = "ISSUE_FEE_PERCENTAGE_1")
	private BigDecimal issueFeePercentage1;

	/** The issue fee percentage 2. */
	@Column(name = "ISSUE_FEE_PERCENTAGE_2")
	private BigDecimal issueFeePercentage2;

	/** The issue fee percentage 3. */
	@Column(name = "ISSUE_FEE_PERCENTAGE_3")
	private BigDecimal issueFeePercentage3;

	/** The issue fee percentage 4. */
	@Column(name = "ISSUE_FEE_PERCENTAGE_4")
	private BigDecimal issueFeePercentage4;

	/** The use schedule interest. */
	@Column(name = "USE_SCHEDULE_INTEREST")
	private Boolean useScheduleInterest;

	/** The capitalise interest when refinancing. */
	@Column(name = "CAPITALISE_INTEREST_WHEN_REFINANCING")
	private Boolean capitaliseInterestWhenRefinancing;

	/** The decimal. */
	@Column(name = "DECIMAL_PLACES")
	private Integer decimal;

	/** The currency. */
	@Column(name = "CURRENCY")
	private String currency;

	/** The about product. */
	@Column(name = "ABOUT_PRODUCT", length = 5000)
	private String aboutProduct;

	/** The maximum age. */
	@Column(name = "MAXIMUM_AGE")
	private Integer maximumAge;

	/** The minimum age. */
	@Column(name = "MINIMUM_AGE")
	private Integer minimumAge;

	/** The max accounts. */
	@Column(name = "MAX_ACCOUNTS")
	private Integer maxAccounts;

	/** The maximum deferred period. */
	@Column(name = "MAXIMUM_DEFERRED_PERIOD")
	private Integer maximumDeferredPeriod;

	/** The minimum deferred period. */
	@Column(name = "MINIMUM_DEFERRED_PERIOD")
	private Integer minimumDeferredPeriod;

	/** The cu insurance ID. */
	@Column(name = "CU_INSURANCE_ID")
	private Integer cuInsuranceID;

	/** The issue fee VAT 1. */
	@Column(name = "ISSUE_FEE_VAT_1")
	private BigDecimal issueFeeVAT1;

	/** The issue fee VAT 2. */
	@Column(name = "ISSUE_FEE_VAT_2")
	private BigDecimal issueFeeVAT2;

	/** The insurance vat. */
	@Column(name = "INSURANCE_VAT")
	private BigDecimal insuranceVat;

	/** The round type. */
	@Column(name = "ROUND_TYPE")
	private String roundType;

	/** The issue fee amount 1. */
	@Column(name = "ISSUEFEEAMOUNT1")
	private BigDecimal issueFeeAmount1;

	/** The issue fee amount 2. */
	@Column(name = "ISSUEFEEAMOUNT2")
	private BigDecimal issueFeeAmount2;

	/** The flat interest rate. */
	@Column(name = "FLAT_INTEREST_RATE")
	private BigDecimal flatInterestRate;

	/** The product details. */
	@OneToMany(mappedBy = "product")
	private Set<ProductDetails> productDetails = new HashSet<>();

	/** The renewal percentage. */
	@Column(name = "RENEWAL_LOAN_PERCENTAGE")
	private BigDecimal renewalPercentage;

	/** The token. */
	@Transient
	private String token;

	/** The max num days expiry. */
	@Column(name = "ISCORE_NUM_DAYS_EXPIRY_CHECK")
	private Integer maxNumDaysExpiry;

	/** The max score. */
	@Column(name = "ISCORE_MAX_SCORE")
	private Integer maxScore;

	/** The max active loans. */
	@Column(name = "ISCORE_MAX_ACTIVE_LOANS")
	private Integer maxActiveLoans;

	/** The max num days due. */
	@Column(name = "ISCORE_MAX_NUM_DAYS_DUE")
	private Integer maxNumDaysDue;

	/** The aml check pourcentage. */
	@Column(name = "AML_CHECK_POURCENTAGE")
	private BigDecimal amlCheckPourcentage;

	/** The min active loans. */
	@Column(name = "ISCORE_MIN_ACTIVE_LOANS")
	private Integer minActiveLoans;

	/** The min num days due. */
	@Column(name = "ISCORE_MIN_NUM_DAYS_DUE")
	private Integer minNumDaysDue;

	/** The min score. */
	@Column(name = "ISCORE_MIN_SCORE")
	private Integer minScore;

	/** The topup. */
	@Column(name = "TOPUP")
	private Boolean topup;

	/** The refinance. */
	@Column(name = "REFINANCE")
	private Boolean refinance;

	/** The is frequency. */
	@Column(name = "IS_FREQUENCY")
	private Boolean isFrequency;

	/** The setting topup. */
	@OneToOne(fetch = FetchType.EAGER)
	@JoinColumn(name = "ID_ACM_SETTING_TOPUP", referencedColumnName = "ID_ACM_SETTING_TOPUP")
	private SettingTopup settingTopup;

	/** The supplier. */
	@Column(name = "SUPPLIER")
	private Boolean supplier;

	/** The is frequency with deferred periode. */
	@Column(name = "IS_FREQUENCY_WITH_DEFERRED_PEDIODE")
	private Boolean isFrequencyWithDeferredPeriode;

	/** The disburse. */
	@Column(name = "DISBURSE")
	private Boolean disburse;

	/**
	 * Instantiates a new product.
	 */
	public Product() {

		/*
		 * Empty
		 */
	}

	/**
	 * Instantiates a new product.
	 *
	 * @param id the id
	 */
	public Product(Long id) {

		this.id = id;
	}

	/**
	 * Instantiates a new groupe.
	 *
	 * @param code the code
	 */
	public Product(String code) {

		this.code = code;
	}

	/**
	 * Instantiates a new product.
	 *
	 * @param id the id
	 * @param code the code
	 * @param description the description
	 * @param productIdAbacus the product id abacus
	 * @param productTypeAbacus the product type abacus
	 * @param creationDateAbacus the creation date abacus
	 * @param editDateAbacus the edit date abacus
	 */
	public Product(Long id, String code, String description, Long productIdAbacus,
			Long productTypeAbacus, Date creationDateAbacus, Date editDateAbacus) {

		this.id = id;
		this.code = code;
		this.description = description;
		this.productIdAbacus = productIdAbacus;
		this.productTypeAbacus = productTypeAbacus;
		this.creationDateAbacus = creationDateAbacus;
		this.editDateAbacus = editDateAbacus;
	}

	/**
	 * Instantiates a new product.
	 *
	 * @param code the code
	 * @param description the description
	 * @param productIdAbacus the product id abacus
	 * @param productTypeAbacus the product type abacus
	 * @param creationDateAbacus the creation date abacus
	 * @param editDateAbacus the edit date abacus
	 */
	public Product(String code, String description, Long productIdAbacus, Long productTypeAbacus,
			Date creationDateAbacus, Date editDateAbacus) {

		this.code = code;
		this.description = description;
		this.productIdAbacus = productIdAbacus;
		this.productTypeAbacus = productTypeAbacus;
		this.creationDateAbacus = creationDateAbacus;
		this.editDateAbacus = editDateAbacus;
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
	 * Gets the code.
	 *
	 * @return the code
	 */
	public String getCode() {

		return code;
	}

	/**
	 * Sets the code.
	 *
	 * @param code the code to set
	 */
	public void setCode(String code) {

		this.code = code;
	}

	/**
	 * Gets the description.
	 *
	 * @return the description
	 */
	public String getDescription() {

		return description;
	}

	/**
	 * Sets the description.
	 *
	 * @param description the description to set
	 */
	public void setDescription(String description) {

		this.description = description;
	}

	/**
	 * Gets the product id abacus.
	 *
	 * @return the productIdAbacus
	 */
	public Long getProductIdAbacus() {

		return productIdAbacus;
	}

	/**
	 * Sets the product id abacus.
	 *
	 * @param productIdAbacus the productIdAbacus to set
	 */
	public void setProductIdAbacus(Long productIdAbacus) {

		this.productIdAbacus = productIdAbacus;
	}

	/**
	 * Gets the product type abacus.
	 *
	 * @return the productTypeAbacus
	 */
	public Long getProductTypeAbacus() {

		return productTypeAbacus;
	}

	/**
	 * Sets the product type abacus.
	 *
	 * @param productTypeAbacus the productTypeAbacus to set
	 */
	public void setProductTypeAbacus(Long productTypeAbacus) {

		this.productTypeAbacus = productTypeAbacus;
	}

	/**
	 * Gets the creation date abacus.
	 *
	 * @return the creationDateAbacus
	 */
	public Date getCreationDateAbacus() {

		return creationDateAbacus;
	}

	/**
	 * Sets the creation date abacus.
	 *
	 * @param creationDateAbacus the creationDateAbacus to set
	 */
	public void setCreationDateAbacus(Date creationDateAbacus) {

		this.creationDateAbacus = creationDateAbacus;
	}

	/**
	 * Gets the edits the date abacus.
	 *
	 * @return the editDateAbacus
	 */
	public Date getEditDateAbacus() {

		return editDateAbacus;
	}

	/**
	 * Sets the edits the date abacus.
	 *
	 * @param editDateAbacus the editDateAbacus to set
	 */
	public void setEditDateAbacus(Date editDateAbacus) {

		this.editDateAbacus = editDateAbacus;
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
	 * Gets the product indiv.
	 *
	 * @return the productIndiv
	 */
	public Boolean getProductIndiv() {

		return productIndiv;
	}

	/**
	 * Sets the product indiv.
	 *
	 * @param productIndiv the productIndiv to set
	 */
	public void setProductIndiv(Boolean productIndiv) {

		this.productIndiv = productIndiv;
	}

	/**
	 * Gets the product grp.
	 *
	 * @return the productGrp
	 */
	public Boolean getProductGrp() {

		return productGrp;
	}

	/**
	 * Sets the product grp.
	 *
	 * @param productGrp the productGrp to set
	 */
	public void setProductGrp(Boolean productGrp) {

		this.productGrp = productGrp;
	}

	/**
	 * Gets the product org.
	 *
	 * @return the productOrg
	 */
	public Boolean getProductOrg() {

		return productOrg;
	}

	/**
	 * Sets the product org.
	 *
	 * @param productOrg the productOrg to set
	 */
	public void setProductOrg(Boolean productOrg) {

		this.productOrg = productOrg;
	}

	/**
	 * Gets the rate start date.
	 *
	 * @return the rateStartDate
	 */
	public Date getRateStartDate() {

		return rateStartDate;
	}

	/**
	 * Sets the rate start date.
	 *
	 * @param rateStartDate the rateStartDate to set
	 */
	public void setRateStartDate(Date rateStartDate) {

		this.rateStartDate = rateStartDate;
	}

	/**
	 * Gets the rate end date.
	 *
	 * @return the rateEndDate
	 */
	public Date getRateEndDate() {

		return rateEndDate;
	}

	/**
	 * Sets the rate end date.
	 *
	 * @param rateEndDate the rateEndDate to set
	 */
	public void setRateEndDate(Date rateEndDate) {

		this.rateEndDate = rateEndDate;
	}

	/**
	 * Gets the rate.
	 *
	 * @return the rate
	 */
	public BigDecimal getRate() {

		return rate;
	}

	/**
	 * Sets the rate.
	 *
	 * @param rate the rate to set
	 */
	public void setRate(BigDecimal rate) {

		this.rate = rate;
	}

	/**
	 * Gets the maximum balance.
	 *
	 * @return the maximumBalance
	 */
	public BigDecimal getMaximumBalance() {

		return maximumBalance;
	}

	/**
	 * Sets the maximum balance.
	 *
	 * @param maximumBalance the maximumBalance to set
	 */
	public void setMaximumBalance(BigDecimal maximumBalance) {

		this.maximumBalance = maximumBalance;
	}

	/**
	 * Gets the minimum term.
	 *
	 * @return the minimumTerm
	 */
	public Integer getMinimumTerm() {

		return minimumTerm;
	}

	/**
	 * Sets the minimum term.
	 *
	 * @param minimumTerm the minimumTerm to set
	 */
	public void setMinimumTerm(Integer minimumTerm) {

		this.minimumTerm = minimumTerm;
	}

	/**
	 * Gets the maximum term.
	 *
	 * @return the maximumTerm
	 */
	public Integer getMaximumTerm() {

		return maximumTerm;
	}

	/**
	 * Sets the maximum term.
	 *
	 * @param maximumTerm the maximumTerm to set
	 */
	public void setMaximumTerm(Integer maximumTerm) {

		this.maximumTerm = maximumTerm;
	}

	/**
	 * Gets the issue fee percentage 1.
	 *
	 * @return the issueFeePercentage1
	 */
	public BigDecimal getIssueFeePercentage1() {

		return issueFeePercentage1;
	}

	/**
	 * Sets the issue fee percentage 1.
	 *
	 * @param issueFeePercentage1 the issueFeePercentage1 to set
	 */
	public void setIssueFeePercentage1(BigDecimal issueFeePercentage1) {

		this.issueFeePercentage1 = issueFeePercentage1;
	}

	/**
	 * Gets the issue fee percentage 2.
	 *
	 * @return the issueFeePercentage2
	 */
	public BigDecimal getIssueFeePercentage2() {

		return issueFeePercentage2;
	}

	/**
	 * Sets the issue fee percentage 2.
	 *
	 * @param issueFeePercentage2 the issueFeePercentage2 to set
	 */
	public void setIssueFeePercentage2(BigDecimal issueFeePercentage2) {

		this.issueFeePercentage2 = issueFeePercentage2;
	}

	/**
	 * Gets the issue fee percentage 3.
	 *
	 * @return the issueFeePercentage3
	 */
	public BigDecimal getIssueFeePercentage3() {

		return issueFeePercentage3;
	}

	/**
	 * Sets the issue fee percentage 3.
	 *
	 * @param issueFeePercentage3 the issueFeePercentage3 to set
	 */
	public void setIssueFeePercentage3(BigDecimal issueFeePercentage3) {

		this.issueFeePercentage3 = issueFeePercentage3;
	}

	/**
	 * Gets the issue fee percentage 4.
	 *
	 * @return the issueFeePercentage4
	 */
	public BigDecimal getIssueFeePercentage4() {

		return issueFeePercentage4;
	}

	/**
	 * Sets the issue fee percentage 4.
	 *
	 * @param issueFeePercentage4 the issueFeePercentage4 to set
	 */
	public void setIssueFeePercentage4(BigDecimal issueFeePercentage4) {

		this.issueFeePercentage4 = issueFeePercentage4;
	}

	/**
	 * Gets the use schedule interest.
	 *
	 * @return the useScheduleInterest
	 */
	public Boolean getUseScheduleInterest() {

		return useScheduleInterest;
	}

	/**
	 * Sets the use schedule interest.
	 *
	 * @param useScheduleInterest the useScheduleInterest to set
	 */
	public void setUseScheduleInterest(Boolean useScheduleInterest) {

		this.useScheduleInterest = useScheduleInterest;
	}

	/**
	 * Gets the capitalise interest when refinancing.
	 *
	 * @return the capitaliseInterestWhenRefinancing
	 */
	public Boolean getCapitaliseInterestWhenRefinancing() {

		return capitaliseInterestWhenRefinancing;
	}

	/**
	 * Sets the capitalise interest when refinancing.
	 *
	 * @param capitaliseInterestWhenRefinancing the capitaliseInterestWhenRefinancing to set
	 */
	public void setCapitaliseInterestWhenRefinancing(Boolean capitaliseInterestWhenRefinancing) {

		this.capitaliseInterestWhenRefinancing = capitaliseInterestWhenRefinancing;
	}

	/**
	 * Gets the decimal.
	 *
	 * @return the decimal
	 */
	public Integer getDecimal() {

		return decimal;
	}

	/**
	 * Sets the decimal.
	 *
	 * @param decimal the decimal to set
	 */
	public void setDecimal(Integer decimal) {

		this.decimal = decimal;
	}

	/**
	 * Gets the currency.
	 *
	 * @return the currency
	 */
	public String getCurrency() {

		return currency;
	}

	/**
	 * Sets the currency.
	 *
	 * @param currency the currency to set
	 */
	public void setCurrency(String currency) {

		this.currency = currency;
	}

	/**
	 * Gets the about product.
	 *
	 * @return the aboutProduct
	 */
	public String getAboutProduct() {

		return aboutProduct;
	}

	/**
	 * Sets the about product.
	 *
	 * @param aboutProduct the aboutProduct to set
	 */
	public void setAboutProduct(String aboutProduct) {

		this.aboutProduct = aboutProduct;
	}

	/**
	 * Gets the product details.
	 *
	 * @return the productDetails
	 */
	public Set<ProductDetails> getProductDetails() {

		return productDetails;
	}

	/**
	 * Sets the product details.
	 *
	 * @param productDetails the productDetails to set
	 */
	public void setProductDetails(Set<ProductDetails> productDetails) {

		this.productDetails = productDetails;
	}

	/**
	 * Gets the maximum age.
	 *
	 * @return the maximumAge
	 */
	public Integer getMaximumAge() {

		return maximumAge;
	}

	/**
	 * Sets the maximum age.
	 *
	 * @param maximumAge the maximumAge to set
	 */
	public void setMaximumAge(Integer maximumAge) {

		this.maximumAge = maximumAge;
	}

	/**
	 * Gets the minimum age.
	 *
	 * @return the minimumAge
	 */
	public Integer getMinimumAge() {

		return minimumAge;
	}

	/**
	 * Sets the minimum age.
	 *
	 * @param minimumAge the minimumAge to set
	 */
	public void setMinimumAge(Integer minimumAge) {

		this.minimumAge = minimumAge;
	}

	/**
	 * Gets the max accounts.
	 *
	 * @return the maxAccounts
	 */
	public Integer getMaxAccounts() {

		return maxAccounts;
	}

	/**
	 * Sets the max accounts.
	 *
	 * @param maxAccounts the maxAccounts to set
	 */
	public void setMaxAccounts(Integer maxAccounts) {

		this.maxAccounts = maxAccounts;
	}

	/**
	 * Gets the maximum deferred period.
	 *
	 * @return the maximumDeferredPeriod
	 */
	public Integer getMaximumDeferredPeriod() {

		return maximumDeferredPeriod;
	}

	/**
	 * Sets the maximum deferred period.
	 *
	 * @param maximumDeferredPeriod the maximumDeferredPeriod to set
	 */
	public void setMaximumDeferredPeriod(Integer maximumDeferredPeriod) {

		this.maximumDeferredPeriod = maximumDeferredPeriod;
	}

	/**
	 * Gets the minimum deferred period.
	 *
	 * @return the minimumDeferredPeriod
	 */
	public Integer getMinimumDeferredPeriod() {

		return minimumDeferredPeriod;
	}

	/**
	 * Sets the minimum deferred period.
	 *
	 * @param minimumDeferredPeriod the minimumDeferredPeriod to set
	 */
	public void setMinimumDeferredPeriod(Integer minimumDeferredPeriod) {

		this.minimumDeferredPeriod = minimumDeferredPeriod;
	}

	/**
	 * Gets the cu insurance ID.
	 *
	 * @return the cuInsuranceID
	 */
	public Integer getCuInsuranceID() {

		return cuInsuranceID;
	}

	/**
	 * Sets the cu insurance ID.
	 *
	 * @param cuInsuranceID the cuInsuranceID to set
	 */
	public void setCuInsuranceID(Integer cuInsuranceID) {

		this.cuInsuranceID = cuInsuranceID;
	}

	/**
	 * Gets the issue fee VAT 1.
	 *
	 * @return the issueFeeVAT1
	 */
	public BigDecimal getIssueFeeVAT1() {

		return issueFeeVAT1;
	}

	/**
	 * Sets the issue fee VAT 1.
	 *
	 * @param issueFeeVAT1 the issueFeeVAT1 to set
	 */
	public void setIssueFeeVAT1(BigDecimal issueFeeVAT1) {

		this.issueFeeVAT1 = issueFeeVAT1;
	}

	/**
	 * Gets the issue fee VAT 2.
	 *
	 * @return the issueFeeVAT2
	 */
	public BigDecimal getIssueFeeVAT2() {

		return issueFeeVAT2;
	}

	/**
	 * Sets the issue fee VAT 2.
	 *
	 * @param issueFeeVAT2 the issueFeeVAT2 to set
	 */
	public void setIssueFeeVAT2(BigDecimal issueFeeVAT2) {

		this.issueFeeVAT2 = issueFeeVAT2;
	}

	/**
	 * Gets the insurance vat.
	 *
	 * @return the insuranceVat
	 */
	public BigDecimal getInsuranceVat() {

		return insuranceVat;
	}

	/**
	 * Sets the insurance vat.
	 *
	 * @param insuranceVat the insuranceVat to set
	 */
	public void setInsuranceVat(BigDecimal insuranceVat) {

		this.insuranceVat = insuranceVat;
	}

	/**
	 * Gets the round type.
	 *
	 * @return the roundType
	 */
	public String getRoundType() {

		return roundType;
	}

	/**
	 * Sets the round type.
	 *
	 * @param roundType the roundType to set
	 */
	public void setRoundType(String roundType) {

		this.roundType = roundType;
	}

	/**
	 * Gets the issue fee amount 1.
	 *
	 * @return the issue fee amount 1
	 */
	public BigDecimal getIssueFeeAmount1() {

		return issueFeeAmount1;
	}

	/**
	 * Sets the issue fee amount 1.
	 *
	 * @param issueFeeAmount1 the new issue fee amount 1
	 */
	public void setIssueFeeAmount1(BigDecimal issueFeeAmount1) {

		this.issueFeeAmount1 = issueFeeAmount1;
	}

	/**
	 * Gets the issue fee amount 2.
	 *
	 * @return the issue fee amount 2
	 */
	public BigDecimal getIssueFeeAmount2() {

		return issueFeeAmount2;
	}

	/**
	 * Sets the issue fee amount 2.
	 *
	 * @param issueFeeAmount2 the new issue fee amount 2
	 */
	public void setIssueFeeAmount2(BigDecimal issueFeeAmount2) {

		this.issueFeeAmount2 = issueFeeAmount2;
	}

	/**
	 * Gets the flat interest rate.
	 *
	 * @return the flat interest rate
	 */
	public BigDecimal getFlatInterestRate() {

		return flatInterestRate;
	}

	/**
	 * Sets the flat interest rate.
	 *
	 * @param flatInterestRate the new flat interest rate
	 */
	public void setFlatInterestRate(BigDecimal flatInterestRate) {

		this.flatInterestRate = flatInterestRate;
	}

	/**
	 * Gets the renewal percentage.
	 *
	 * @return the renewal percentage
	 */
	public BigDecimal getRenewalPercentage() {

		return renewalPercentage;
	}

	/**
	 * Sets the renewal percentage.
	 *
	 * @param renewalPercentage the new renewal percentage
	 */
	public void setRenewalPercentage(BigDecimal renewalPercentage) {

		this.renewalPercentage = renewalPercentage;
	}

	/**
	 * To string.
	 *
	 * @return the string
	 */
	/*
	 * (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {

		return "Product [id=" + id + ", code=" + code + ", description=" + description
				+ ", productIdAbacus=" + productIdAbacus + ", productTypeAbacus="
				+ productTypeAbacus + ", creationDateAbacus=" + creationDateAbacus
				+ ", editDateAbacus=" + editDateAbacus + ", productIndiv=" + productIndiv
				+ ", productGrp=" + productGrp + ", productOrg=" + productOrg + ", rateStartDate="
				+ rateStartDate + ", rateEndDate=" + rateEndDate + ", rate=" + rate
				+ ", maximumBalance=" + maximumBalance + ", minimumTerm=" + minimumTerm
				+ ", maximumTerm=" + maximumTerm + ", issueFeePercentage1=" + issueFeePercentage1
				+ ", issueFeePercentage2=" + issueFeePercentage2 + ", issueFeePercentage3="
				+ issueFeePercentage3 + ", issueFeePercentage4=" + issueFeePercentage4
				+ ", useScheduleInterest=" + useScheduleInterest
				+ ", capitaliseInterestWhenRefinancing=" + capitaliseInterestWhenRefinancing
				+ ", decimal=" + decimal + ", currency=" + currency + ", token=" + token + "]";
	}

	/**
	 * Gets the max num days expiry.
	 *
	 * @return the maxNumDaysExpiry
	 */
	public Integer getMaxNumDaysExpiry() {

		return maxNumDaysExpiry;
	}

	/**
	 * Sets the max num days expiry.
	 *
	 * @param maxNumDaysExpiry the maxNumDaysExpiry to set
	 */
	public void setMaxNumDaysExpiry(Integer maxNumDaysExpiry) {

		this.maxNumDaysExpiry = maxNumDaysExpiry;
	}

	/**
	 * Gets the max score.
	 *
	 * @return the maxScore
	 */
	public Integer getMaxScore() {

		return maxScore;
	}

	/**
	 * Sets the max score.
	 *
	 * @param maxScore the maxScore to set
	 */
	public void setMaxScore(Integer maxScore) {

		this.maxScore = maxScore;
	}

	/**
	 * Gets the max active loans.
	 *
	 * @return the maxActiveLoans
	 */
	public Integer getMaxActiveLoans() {

		return maxActiveLoans;
	}

	/**
	 * Sets the max active loans.
	 *
	 * @param maxActiveLoans the maxActiveLoans to set
	 */
	public void setMaxActiveLoans(Integer maxActiveLoans) {

		this.maxActiveLoans = maxActiveLoans;
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
	 * Gets the aml check pourcentage.
	 *
	 * @return the aml check pourcentage
	 */
	public BigDecimal getAmlCheckPourcentage() {

		return amlCheckPourcentage;
	}

	/**
	 * Sets the aml check pourcentage.
	 *
	 * @param amlCheckPourcentage the new aml check pourcentage
	 */
	public void setAmlCheckPourcentage(BigDecimal amlCheckPourcentage) {

		this.amlCheckPourcentage = amlCheckPourcentage;
	}

	/**
	 * Gets the min active loans.
	 *
	 * @return the minActiveLoans
	 */
	public Integer getMinActiveLoans() {

		return minActiveLoans;
	}

	/**
	 * Sets the min active loans.
	 *
	 * @param minActiveLoans the minActiveLoans to set
	 */
	public void setMinActiveLoans(Integer minActiveLoans) {

		this.minActiveLoans = minActiveLoans;
	}

	/**
	 * Gets the min num days due.
	 *
	 * @return the minNumDaysDue
	 */
	public Integer getMinNumDaysDue() {

		return minNumDaysDue;
	}

	/**
	 * Sets the min num days due.
	 *
	 * @param minNumDaysDue the minNumDaysDue to set
	 */
	public void setMinNumDaysDue(Integer minNumDaysDue) {

		this.minNumDaysDue = minNumDaysDue;
	}

	/**
	 * Gets the min score.
	 *
	 * @return the minScore
	 */
	public Integer getMinScore() {

		return minScore;
	}

	/**
	 * Sets the min score.
	 *
	 * @param minScore the minScore to set
	 */
	public void setMinScore(Integer minScore) {

		this.minScore = minScore;
	}

	/**
	 * Gets the topup.
	 *
	 * @return the topup
	 */
	public Boolean getTopup() {

		return topup;
	}

	/**
	 * Sets the topup.
	 *
	 * @param topup the new topup
	 */
	public void setTopup(Boolean topup) {

		this.topup = topup;
	}

	/**
	 * Gets the refinance.
	 *
	 * @return the refinance
	 */
	public Boolean getRefinance() {

		return refinance;
	}

	/**
	 * Sets the refinance.
	 *
	 * @param refinance the new refinance
	 */
	public void setRefinance(Boolean refinance) {

		this.refinance = refinance;
	}

	/**
	 * Gets the setting topup.
	 *
	 * @return the setting topup
	 */
	public SettingTopup getSettingTopup() {

		return settingTopup;
	}

	/**
	 * Sets the setting topup.
	 *
	 * @param settingTopup the new setting topup
	 */
	public void setSettingTopup(SettingTopup settingTopup) {

		this.settingTopup = settingTopup;
	}

	/**
	 * <<<<<<< HEAD Gets the supplier.
	 *
	 * @return the supplier
	 */
	public Boolean getSupplier() {

		return supplier;
	}

	/**
	 * Sets the supplier.
	 *
	 * @param supplier the supplier to set
	 */
	public void setSupplier(Boolean supplier) {

		this.supplier = supplier;
	}

	/**
	 * Gets the checks if is frequency.
	 *
	 * @return the isFrequency
	 */
	public Boolean getIsFrequency() {

		return isFrequency;
	}

	/**
	 * Sets the checks if is frequency.
	 *
	 * @param isFrequency the isFrequency to set
	 */
	public void setIsFrequency(Boolean isFrequency) {

		this.isFrequency = isFrequency;
	}

	/**
	 * Gets the checks if is frequency with deferred periode.
	 *
	 * @return the checks if is frequency with deferred periode
	 */
	public Boolean getIsFrequencyWithDeferredPeriode() {

		return isFrequencyWithDeferredPeriode;
	}

	/**
	 * Sets the checks if is frequency with deferred periode.
	 *
	 * @param isFrequencyWithDeferredPeriode the new checks if is frequency with deferred periode
	 */
	public void setIsFrequencyWithDeferredPeriode(Boolean isFrequencyWithDeferredPeriode) {

		this.isFrequencyWithDeferredPeriode = isFrequencyWithDeferredPeriode;
	}

	/**
	 * Gets the disburse.
	 *
	 * @return the disburse
	 */
	public Boolean getDisburse() {

		return disburse;
	}

	/**
	 * Sets the disburse.
	 *
	 * @param disburse the disburse to set
	 */
	public void setDisburse(Boolean disburse) {

		this.disburse = disburse;
	}

}
