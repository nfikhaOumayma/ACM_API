/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.utils.dtos;

import java.io.Serializable;
import java.math.BigDecimal;
import java.util.List;

import com.fasterxml.jackson.annotation.JsonProperty;

/**
 * The Class RequestEnquiryNewCustomerSimahApiDTO.
 */
public class RequestEnquiryNewCustomerSimahApiDTO extends GenericDTO implements Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = 4137701647751673711L;

	/** The product type. */
	@JsonProperty("productType")
	private int productType;

	/** The amount. */
	@JsonProperty("amount")
	private BigDecimal amount;

	/** The applicants. */
	@JsonProperty("applicants")
	private List<RequestApplicantSimahApiDTO> applicants;

	/** The accept. */
	@JsonProperty("accept")
	private boolean accept;

	/** The response type. */
	@JsonProperty("responseType")
	private int responseType;

	/**
	 * Instantiates a new request enquiry new customer simah api DTO.
	 */
	public RequestEnquiryNewCustomerSimahApiDTO() {

		super();
	}

	/**
	 * Gets the product type.
	 *
	 * @return the product type
	 */
	public int getProductType() {

		return productType;
	}

	/**
	 * Sets the product type.
	 *
	 * @param productType the new product type
	 */
	public void setProductType(int productType) {

		this.productType = productType;
	}

	/**
	 * Gets the amount.
	 *
	 * @return the amount
	 */
	public BigDecimal getAmount() {

		return amount;
	}

	/**
	 * Sets the amount.
	 *
	 * @param amount the new amount
	 */
	public void setAmount(BigDecimal amount) {

		this.amount = amount;
	}

	/**
	 * Gets the applicants.
	 *
	 * @return the applicants
	 */
	public List<RequestApplicantSimahApiDTO> getApplicants() {

		return applicants;
	}

	/**
	 * Sets the applicants.
	 *
	 * @param applicants the new applicants
	 */
	public void setApplicants(List<RequestApplicantSimahApiDTO> applicants) {

		this.applicants = applicants;
	}

	/**
	 * Checks if is accept.
	 *
	 * @return true, if is accept
	 */
	public boolean isAccept() {

		return accept;
	}

	/**
	 * Sets the accept.
	 *
	 * @param accept the new accept
	 */
	public void setAccept(boolean accept) {

		this.accept = accept;
	}

	/**
	 * Gets the response type.
	 *
	 * @return the response type
	 */
	public int getResponseType() {

		return responseType;
	}

	/**
	 * Sets the response type.
	 *
	 * @param responseType the new response type
	 */
	public void setResponseType(int responseType) {

		this.responseType = responseType;
	}

	/*
	 * (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {

		return "RequestEnquiryNewCustomerSimahApiDTO [productType=" + productType + ", amount="
				+ amount + ", applicants=" + applicants + ", accept=" + accept + ", responseType="
				+ responseType + "]";
	}

}
