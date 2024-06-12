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
import javax.persistence.NamedQuery;
import javax.persistence.Table;

/**
 * {@link AcmCollateral} class.
 *
 * @author mlamloum
 * @since 0.1.0
 */
@Entity
@Table(name = "ACM_COLLATERAL")
@NamedQuery(name = "AcmCollateral.findAll", query = "SELECT l FROM AcmCollateral l")
public class AcmCollateral extends GenericModel implements Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = 1027544933499745310L;

	/** The id. */
	@Id
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	@Column(name = "ID_ACM_COLLATERAL", unique = true, nullable = false)
	private Long idAcmCollateral;

	/** The loan. */
	@ManyToOne(fetch = FetchType.EAGER, cascade = {})
	@JoinColumn(name = "ACM_LOAN_ID")
	private Loan loan;

	/** The id loan extern. */
	@Column(name = "LOAN_ID")
	private Long externLoanId;

	/** The reference. */
	@Column(name = "REFERENCE")
	private String reference;

	/** The description. */
	@Column(name = "DESCRIPTION")
	private String description;

	/** The collateral type. */
	@Column(name = "COLLATERAL_TYPE_ID_EXTERN")
	private Long collateralTypeIdExtern;

	/** The collateral type description. */
	@Column(name = "COLLATERAL_TYPE_DESCRIPTION")
	private String collateralTypeDescription;

	/** The id account extern. */
	@Column(name = "ID_ACCOUNT_EXTERN")
	private Long idAccountExtern;

	/** The customer id. */
	@Column(name = "CUSTOMER_ID")
	private Long externCustomerId;

	/** The original gross value. */
	@Column(name = "ORIGINAL_GROSS_VALUE")
	private BigDecimal originalGrossValue;

	/** The gross value. */
	@Column(name = "GROSS_VALUE")
	private BigDecimal grossValue;

	/** The realised value. */
	@Column(name = "REALISED_VALUE")
	private BigDecimal realisedValue;

	/** The fixed cost. */
	@Column(name = "FIXED_COST")
	private BigDecimal fixedCost;

	/** The net value. */
	@Column(name = "NET_VALUE")
	private BigDecimal netValue;

	/** The value date. */
	@Column(name = "VALUE_DATE")
	private Date valueDate;

	/** The expiry date. */
	@Column(name = "EXPIRY_DATE")
	private Date expiryDate;

	/** The with holding rate. */
	@Column(name = "WITHHOLDING_RATE")
	private BigDecimal withHoldingRate;

	/** The is deleted. */
	@Column(name = "IS_DELETED")
	private Boolean isDeleted;

	/** The valuer id. */
	@Column(name = "VALUER_ID")
	private String valuerId;

	/** The valuer name. */
	@Column(name = "VALUER_NAME")
	private String valuerName;

	/** The enabled. */
	@Column(name = "ACM_ENABLED", nullable = false)
	private Boolean enabled;

	/**
	 * Instantiates a new acm collateral.
	 */
	public AcmCollateral() {

	}

	/**
	 * Instantiates a new acm collateral.
	 *
	 * @param idAcmCollateral the id acm collateral
	 */
	public AcmCollateral(Long idAcmCollateral) {

		this.idAcmCollateral = idAcmCollateral;
	}

	/**
	 * Gets the id acm collateral.
	 *
	 * @return the id acm collateral
	 */
	public Long getIdAcmCollateral() {

		return idAcmCollateral;
	}

	/**
	 * Sets the id acm collateral.
	 *
	 * @param idAcmCollateral the new id acm collateral
	 */
	public void setIdAcmCollateral(Long idAcmCollateral) {

		this.idAcmCollateral = idAcmCollateral;
	}

	/**
	 * Gets the loan.
	 *
	 * @return the loan
	 */
	public Loan getLoan() {

		return loan;
	}

	/**
	 * Sets the loan.
	 *
	 * @param loan the new loan
	 */
	public void setLoan(Loan loan) {

		this.loan = loan;
	}

	/**
	 * Gets the extern loan id.
	 *
	 * @return the extern loan id
	 */
	public Long getExternLoanId() {

		return externLoanId;
	}

	/**
	 * Sets the extern loan id.
	 *
	 * @param externLoanId the new extern loan id
	 */
	public void setExternLoanId(Long externLoanId) {

		this.externLoanId = externLoanId;
	}

	/**
	 * Gets the reference.
	 *
	 * @return the reference
	 */
	public String getReference() {

		return reference;
	}

	/**
	 * Sets the reference.
	 *
	 * @param reference the new reference
	 */
	public void setReference(String reference) {

		this.reference = reference;
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
	 * @param description the new description
	 */
	public void setDescription(String description) {

		this.description = description;
	}

	/**
	 * Gets the collateral type id extern.
	 *
	 * @return the collateral type id extern
	 */
	public Long getCollateralTypeIdExtern() {

		return collateralTypeIdExtern;
	}

	/**
	 * Sets the collateral type id extern.
	 *
	 * @param collateralTypeIdExtern the new collateral type id extern
	 */
	public void setCollateralTypeIdExtern(Long collateralTypeIdExtern) {

		this.collateralTypeIdExtern = collateralTypeIdExtern;
	}

	/**
	 * Gets the id account extern.
	 *
	 * @return the id account extern
	 */
	public Long getIdAccountExtern() {

		return idAccountExtern;
	}

	/**
	 * Sets the id account extern.
	 *
	 * @param idAccountExtern the new id account extern
	 */
	public void setIdAccountExtern(Long idAccountExtern) {

		this.idAccountExtern = idAccountExtern;
	}

	/**
	 * Gets the extern customer id.
	 *
	 * @return the extern customer id
	 */
	public Long getExternCustomerId() {

		return externCustomerId;
	}

	/**
	 * Sets the extern customer id.
	 *
	 * @param externCustomerId the new extern customer id
	 */
	public void setExternCustomerId(Long externCustomerId) {

		this.externCustomerId = externCustomerId;
	}

	/**
	 * Gets the original gross value.
	 *
	 * @return the original gross value
	 */
	public BigDecimal getOriginalGrossValue() {

		return originalGrossValue;
	}

	/**
	 * Sets the original gross value.
	 *
	 * @param originalGrossValue the new original gross value
	 */
	public void setOriginalGrossValue(BigDecimal originalGrossValue) {

		this.originalGrossValue = originalGrossValue;
	}

	/**
	 * Gets the gross value.
	 *
	 * @return the gross value
	 */
	public BigDecimal getGrossValue() {

		return grossValue;
	}

	/**
	 * Sets the gross value.
	 *
	 * @param grossValue the new gross value
	 */
	public void setGrossValue(BigDecimal grossValue) {

		this.grossValue = grossValue;
	}

	/**
	 * Gets the realised value.
	 *
	 * @return the realised value
	 */
	public BigDecimal getRealisedValue() {

		return realisedValue;
	}

	/**
	 * Sets the realised value.
	 *
	 * @param realisedValue the new realised value
	 */
	public void setRealisedValue(BigDecimal realisedValue) {

		this.realisedValue = realisedValue;
	}

	/**
	 * Gets the fixed cost.
	 *
	 * @return the fixed cost
	 */
	public BigDecimal getFixedCost() {

		return fixedCost;
	}

	/**
	 * Sets the fixed cost.
	 *
	 * @param fixedCost the new fixed cost
	 */
	public void setFixedCost(BigDecimal fixedCost) {

		this.fixedCost = fixedCost;
	}

	/**
	 * Gets the net value.
	 *
	 * @return the net value
	 */
	public BigDecimal getNetValue() {

		return netValue;
	}

	/**
	 * Sets the net value.
	 *
	 * @param netValue the new net value
	 */
	public void setNetValue(BigDecimal netValue) {

		this.netValue = netValue;
	}

	/**
	 * Gets the value date.
	 *
	 * @return the value date
	 */
	public Date getValueDate() {

		return valueDate;
	}

	/**
	 * Sets the value date.
	 *
	 * @param valueDate the new value date
	 */
	public void setValueDate(Date valueDate) {

		this.valueDate = valueDate;
	}

	/**
	 * Gets the expiry date.
	 *
	 * @return the expiry date
	 */
	public Date getExpiryDate() {

		return expiryDate;
	}

	/**
	 * Sets the expiry date.
	 *
	 * @param expiryDate the new expiry date
	 */
	public void setExpiryDate(Date expiryDate) {

		this.expiryDate = expiryDate;
	}

	/**
	 * Gets the with holding rate.
	 *
	 * @return the with holding rate
	 */
	public BigDecimal getWithHoldingRate() {

		return withHoldingRate;
	}

	/**
	 * Sets the with holding rate.
	 *
	 * @param withHoldingRate the new with holding rate
	 */
	public void setWithHoldingRate(BigDecimal withHoldingRate) {

		this.withHoldingRate = withHoldingRate;
	}

	/**
	 * Gets the checks if is deleted.
	 *
	 * @return the checks if is deleted
	 */
	public Boolean getIsDeleted() {

		return isDeleted;
	}

	/**
	 * Sets the checks if is deleted.
	 *
	 * @param isDeleted the new checks if is deleted
	 */
	public void setIsDeleted(Boolean isDeleted) {

		this.isDeleted = isDeleted;
	}

	/**
	 * Gets the valuer id.
	 *
	 * @return the valuer id
	 */
	public String getValuerId() {

		return valuerId;
	}

	/**
	 * Sets the valuer id.
	 *
	 * @param valuerId the new valuer id
	 */
	public void setValuerId(String valuerId) {

		this.valuerId = valuerId;
	}

	/**
	 * Gets the valuer name.
	 *
	 * @return the valuer name
	 */
	public String getValuerName() {

		return valuerName;
	}

	/**
	 * Sets the valuer name.
	 *
	 * @param valuerName the new valuer name
	 */
	public void setValuerName(String valuerName) {

		this.valuerName = valuerName;
	}

	/**
	 * Gets the collateral type description.
	 *
	 * @return the collateral type description
	 */
	public String getCollateralTypeDescription() {

		return collateralTypeDescription;
	}

	/**
	 * Sets the collateral type description.
	 *
	 * @param collateralTypeDescription the new collateral type description
	 */
	public void setCollateralTypeDescription(String collateralTypeDescription) {

		this.collateralTypeDescription = collateralTypeDescription;
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.utils.models.GenericModel#getEnabled()
	 */
	@Override
	public Boolean getEnabled() {

		return enabled;
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.utils.models.GenericModel#setEnabled(java.lang.Boolean)
	 */
	@Override
	public void setEnabled(Boolean enabled) {

		this.enabled = enabled;
	}

}
