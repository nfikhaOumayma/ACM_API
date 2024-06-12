/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.utils.dtos;

import java.io.Serializable;
import java.math.BigDecimal;

import com.fasterxml.jackson.annotation.JsonProperty;

/**
 * The Class PurchaseMurabhaApiResponseDTO.
 */
public class PurchaseMurabhaApiResponseDTO extends GenericDTO implements Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = 5568468032400179395L;

	/** The result. */
	@JsonProperty("result")
	private String result;

	/** The result description. */
	@JsonProperty("resultDescription")
	private String resultDescription;

	/** The reference id. */
	@JsonProperty("referenceId")
	private String referenceId;

	/** The quantity. */
	@JsonProperty("quantity")
	private BigDecimal quantity;

	/** The unit of measurement. */
	@JsonProperty("unitOfMeasurement")
	private String unitOfMeasurement;

	/** The unit price. */
	@JsonProperty("unitPrice")
	private BigDecimal unitPrice;

	/** The unit price currency. */
	@JsonProperty("unitPriceCurrency")
	private String unitPriceCurrency;

	/** The commodity. */
	@JsonProperty("commodity")
	private String commodity;

	/** The eiger result code. */
	@JsonProperty("eigerResultCode")
	private int eigerResultCode;

	/**
	 * Gets the result.
	 *
	 * @return the result
	 */
	public String getResult() {

		return result;
	}

	/**
	 * Sets the result.
	 *
	 * @param result the new result
	 */
	public void setResult(String result) {

		this.result = result;
	}

	/**
	 * Gets the result description.
	 *
	 * @return the result description
	 */
	public String getResultDescription() {

		return resultDescription;
	}

	/**
	 * Sets the result description.
	 *
	 * @param resultDescription the new result description
	 */
	public void setResultDescription(String resultDescription) {

		this.resultDescription = resultDescription;
	}

	/**
	 * Gets the reference id.
	 *
	 * @return the reference id
	 */
	public String getReferenceId() {

		return referenceId;
	}

	/**
	 * Sets the reference id.
	 *
	 * @param referenceId the new reference id
	 */
	public void setReferenceId(String referenceId) {

		this.referenceId = referenceId;
	}

	/**
	 * Gets the quantity.
	 *
	 * @return the quantity
	 */
	public BigDecimal getQuantity() {

		return quantity;
	}

	/**
	 * Sets the quantity.
	 *
	 * @param quantity the new quantity
	 */
	public void setQuantity(BigDecimal quantity) {

		this.quantity = quantity;
	}

	/**
	 * Gets the unit of measurement.
	 *
	 * @return the unit of measurement
	 */
	public String getUnitOfMeasurement() {

		return unitOfMeasurement;
	}

	/**
	 * Sets the unit of measurement.
	 *
	 * @param unitOfMeasurement the new unit of measurement
	 */
	public void setUnitOfMeasurement(String unitOfMeasurement) {

		this.unitOfMeasurement = unitOfMeasurement;
	}

	/**
	 * Gets the unit price.
	 *
	 * @return the unit price
	 */
	public BigDecimal getUnitPrice() {

		return unitPrice;
	}

	/**
	 * Sets the unit price.
	 *
	 * @param unitPrice the new unit price
	 */
	public void setUnitPrice(BigDecimal unitPrice) {

		this.unitPrice = unitPrice;
	}

	/**
	 * Gets the unit price currency.
	 *
	 * @return the unit price currency
	 */
	public String getUnitPriceCurrency() {

		return unitPriceCurrency;
	}

	/**
	 * Sets the unit price currency.
	 *
	 * @param unitPriceCurrency the new unit price currency
	 */
	public void setUnitPriceCurrency(String unitPriceCurrency) {

		this.unitPriceCurrency = unitPriceCurrency;
	}

	/**
	 * Instantiates a new purchase murabha api response DTO.
	 */
	public PurchaseMurabhaApiResponseDTO() {

		super();
	}

	/**
	 * Gets the commodity.
	 *
	 * @return the commodity
	 */
	public String getCommodity() {

		return commodity;
	}

	/**
	 * Sets the commodity.
	 *
	 * @param commodity the new commodity
	 */
	public void setCommodity(String commodity) {

		this.commodity = commodity;
	}

	/**
	 * Gets the eiger result code.
	 *
	 * @return the eiger result code
	 */
	public int getEigerResultCode() {

		return eigerResultCode;
	}

	/**
	 * Sets the eiger result code.
	 *
	 * @param eigerResultCode the new eiger result code
	 */
	public void setEigerResultCode(int eigerResultCode) {

		this.eigerResultCode = eigerResultCode;
	}

	/*
	 * (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {

		return "PurchaseMurabhaApiResponseDTO [result=" + result + ", resultDescription="
				+ resultDescription + ", referenceId=" + referenceId + ", quantity=" + quantity
				+ ", unitOfMeasurement=" + unitOfMeasurement + ", unitPrice=" + unitPrice
				+ ", unitPriceCurrency=" + unitPriceCurrency + ", commodity=" + commodity
				+ ", eigerResultCode=" + eigerResultCode + "]";
	}

}
