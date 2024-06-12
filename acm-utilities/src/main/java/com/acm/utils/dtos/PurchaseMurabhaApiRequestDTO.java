/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.utils.dtos;

import java.io.Serializable;
import java.math.BigDecimal;

import com.fasterxml.jackson.annotation.JsonProperty;

/**
 * The Class PurchaseMurabhaApiRequestDTO.
 */
public class PurchaseMurabhaApiRequestDTO extends GenericDTO implements Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = 4252082260069089100L;

	/** The client code. */
	@JsonProperty("clientCode")
	private String clientCode;

	/** The seller. */
	@JsonProperty("seller")
	private String seller;

	/** The product code. */
	@JsonProperty("productCode")
	private String productCode;

	/** The commodity. */
	@JsonProperty("commodity")
	private String commodity;

	/** The purchase price. */
	@JsonProperty("purchasePrice")
	private BigDecimal purchasePrice;

	/** The currency. */
	@JsonProperty("currencyCode")
	private String currencyCode;

	/** The settlement date. */
	@JsonProperty("settlementDate")
	private String settlementDate;

	/** The customer. */
	@JsonProperty("customer")
	private String customer;

	/**
	 * Instantiates a new purchase murabha apir request DTO.
	 */
	public PurchaseMurabhaApiRequestDTO() {

		super();
	}

	/**
	 * Gets the client code.
	 *
	 * @return the client code
	 */
	public String getClientCode() {

		return clientCode;
	}

	/**
	 * Sets the client code.
	 *
	 * @param clientCode the new client code
	 */
	public void setClientCode(String clientCode) {

		this.clientCode = clientCode;
	}

	/**
	 * Gets the seller.
	 *
	 * @return the seller
	 */
	public String getSeller() {

		return seller;
	}

	/**
	 * Sets the seller.
	 *
	 * @param seller the new seller
	 */
	public void setSeller(String seller) {

		this.seller = seller;
	}

	/**
	 * Gets the product code.
	 *
	 * @return the product code
	 */
	public String getProductCode() {

		return productCode;
	}

	/**
	 * Sets the product code.
	 *
	 * @param productCode the new product code
	 */
	public void setProductCode(String productCode) {

		this.productCode = productCode;
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
	 * Gets the purchase price.
	 *
	 * @return the purchase price
	 */
	public BigDecimal getPurchasePrice() {

		return purchasePrice;
	}

	/**
	 * Sets the purchase price.
	 *
	 * @param purchasePrice the new purchase price
	 */
	public void setPurchasePrice(BigDecimal purchasePrice) {

		this.purchasePrice = purchasePrice;
	}

	/**
	 * Gets the currency code.
	 *
	 * @return the currency code
	 */
	public String getCurrencyCode() {

		return currencyCode;
	}

	/**
	 * Sets the currency code.
	 *
	 * @param currencyCode the new currency code
	 */
	public void setCurrencyCode(String currencyCode) {

		this.currencyCode = currencyCode;
	}

	/**
	 * Gets the settlement date.
	 *
	 * @return the settlement date
	 */
	public String getSettlementDate() {

		return settlementDate;
	}

	/**
	 * Sets the settlement date.
	 *
	 * @param settlementDate the new settlement date
	 */
	public void setSettlementDate(String settlementDate) {

		this.settlementDate = settlementDate;
	}

	/**
	 * Gets the customer.
	 *
	 * @return the customer
	 */
	public String getCustomer() {

		return customer;
	}

	/**
	 * Sets the customer.
	 *
	 * @param customer the new customer
	 */
	public void setCustomer(String customer) {

		this.customer = customer;
	}

	/*
	 * (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {

		return "PurchaseMurabhaApiRequestDTO [clientCode=" + clientCode + ", seller=" + seller
				+ ", productCode=" + productCode + ", commodity=" + commodity + ", purchasePrice="
				+ purchasePrice + ", currencyCode=" + currencyCode + ", settlementDate="
				+ settlementDate + ", customer=" + customer + "]";
	}

}
