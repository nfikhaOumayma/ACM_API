/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.utils.dtos;

import java.io.Serializable;
import java.math.BigDecimal;
import java.util.Date;
import java.util.List;

/**
 * {@link AcmCollateralDTO} class.
 *
 * @author mlamloum
 * @since 0.1.0
 */
public class AcmCollateralDTO extends GenericDTO implements Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = 3674450298678458259L;
	/** The id. */

	private Long idAcmCollateral;

	/** The loan. */
	private LoanDTO loan;

	/** The id loan extern. */
	private Long externLoanId;

	/** The reference. */
	private String reference;

	/** The description. */
	private String description;

	/** The collateral type. */
	private Long collateralTypeIdExtern;

	/** The collateral type description. */
	private String collateralTypeDescription;

	/** The id account extern. */
	private Long idAccountExtern;

	/** The customer id. */
	private Long externCustomerId;

	/** The customer. */
	private CustomerDTO customer;

	/** The original gross value. */
	private BigDecimal originalGrossValue;

	/** The gross value. */
	private BigDecimal grossValue;

	/** The realised value. */
	private BigDecimal realisedValue;

	/** The fixed cost. */
	private BigDecimal fixedCost;

	/** The net value. */
	private BigDecimal netValue;

	/** The value date. */
	private Date valueDate;

	/** The expiry date. */
	private Date expiryDate;

	/** The with holding rate. */
	private BigDecimal withHoldingRate;

	/** The is deleted. */
	private Boolean isDeleted;

	/** The valuer id. */
	private String valuerId;

	/** The valuer name. */
	private String valuerName;

	/** The user defined fields links DT os. */
	private List<UserDefinedFieldsLinksDTO> userDefinedFieldsLinksDTOs;

	/** The action. */
	private String action;

	/** The enabled. */
	private Boolean enabled;

	/**
	 * Instantiates a new acm collateral DTO.
	 */
	public AcmCollateralDTO() {

	}

	/**
	 * Instantiates a new acm collateral DTO.
	 *
	 * @param externLoanId the extern loan id
	 * @param reference the reference
	 * @param description the description
	 * @param collateralTypeDescription the collateral type description
	 * @param originalGrossValue the original gross value
	 * @param grossValue the gross value
	 * @param realisedValue the realised value
	 * @param fixedCost the fixed cost
	 * @param netValue the net value
	 * @param customerName the customer name
	 */
	public AcmCollateralDTO(Long externLoanId, String reference, String description,
			String collateralTypeDescription, BigDecimal originalGrossValue, BigDecimal grossValue,
			BigDecimal realisedValue, BigDecimal fixedCost, BigDecimal netValue,
			String customerName) {

		this.externLoanId = externLoanId;
		this.reference = reference;
		this.description = description;
		this.collateralTypeDescription = collateralTypeDescription;
		this.originalGrossValue = originalGrossValue;
		this.grossValue = grossValue;
		this.realisedValue = realisedValue;
		this.fixedCost = fixedCost;
		this.netValue = netValue;
		this.customer = new CustomerDTO(customerName);
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
	public LoanDTO getLoan() {

		return loan;
	}

	/**
	 * Sets the loan.
	 *
	 * @param loan the new loan
	 */
	public void setLoan(LoanDTO loan) {

		this.loan = loan;
	}

	/**
	 * Sets the customer.
	 *
	 * @param customer the new customer
	 */
	public void setCustomer(CustomerDTO customer) {

		this.customer = customer;
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
	 * Gets the customer.
	 *
	 * @return the customer
	 */
	public CustomerDTO getCustomer() {

		return customer;
	}

	/**
	 * Gets the user defined fields links DT os.
	 *
	 * @return the user defined fields links DT os
	 */
	public List<UserDefinedFieldsLinksDTO> getUserDefinedFieldsLinksDTOs() {

		return userDefinedFieldsLinksDTOs;
	}

	/**
	 * Sets the user defined fields links DT os.
	 *
	 * @param userDefinedFieldsLinksDTOs the new user defined fields links DT os
	 */
	public void setUserDefinedFieldsLinksDTOs(
			List<UserDefinedFieldsLinksDTO> userDefinedFieldsLinksDTOs) {

		this.userDefinedFieldsLinksDTOs = userDefinedFieldsLinksDTOs;
	}

	/**
	 * Gets the action.
	 *
	 * @return the action
	 */
	public String getAction() {

		return action;
	}

	/**
	 * Sets the action.
	 *
	 * @param action the new action
	 */
	public void setAction(String action) {

		this.action = action;
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

}
