/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.utils.dtos;

import java.io.Serializable;

import org.dozer.Mapping;

/**
 * {@link IncentiveSettingDTO} class.
 *
 * @author HaythemBenizid
 * @since 1.0.8
 */
public class IncentiveSettingDTO extends GenericDTO implements Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = -1445834204407785662L;

	/** The id. */
	private Long id;

	/** The product id. */
	private Long productId;

	/** The frequency. */
	@Mapping("frequency")
	private IncentiveSettingConstantDTO frequency;

	/** The category ACTIVE_CUSTOMER / PRODUCTIVITY / RISK_LEVEL / DISCOUNT_FROM_TOTAL. */
	private String category;

	/** The from. */
	private Long from;

	/** The to. */
	private Long to;

	/** The discount. */
	private Long discount;

	/** The ordre. */
	private Long ordre;

	/**
	 * Instantiates a new incentive setting DTO.
	 */
	public IncentiveSettingDTO() {

		/*
		 * EMPTY
		 */
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
	 * Gets the product id.
	 *
	 * @return the productId
	 */
	public Long getProductId() {

		return productId;
	}

	/**
	 * Sets the product id.
	 *
	 * @param productId the productId to set
	 */
	public void setProductId(Long productId) {

		this.productId = productId;
	}

	/**
	 * Gets the frequency.
	 *
	 * @return the frequency
	 */
	public IncentiveSettingConstantDTO getFrequency() {

		return frequency;
	}

	/**
	 * Sets the frequency.
	 *
	 * @param frequency the frequency to set
	 */
	public void setFrequency(IncentiveSettingConstantDTO frequency) {

		this.frequency = frequency;
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
	 * Gets the from.
	 *
	 * @return the from
	 */
	public Long getFrom() {

		return from;
	}

	/**
	 * Sets the from.
	 *
	 * @param from the from to set
	 */
	public void setFrom(Long from) {

		this.from = from;
	}

	/**
	 * Gets the to.
	 *
	 * @return the to
	 */
	public Long getTo() {

		return to;
	}

	/**
	 * Sets the to.
	 *
	 * @param to the to to set
	 */
	public void setTo(Long to) {

		this.to = to;
	}

	/**
	 * Gets the discount.
	 *
	 * @return the discount
	 */
	public Long getDiscount() {

		return discount;
	}

	/**
	 * Sets the discount.
	 *
	 * @param discount the discount to set
	 */
	public void setDiscount(Long discount) {

		this.discount = discount;
	}

	/**
	 * Gets the ordre.
	 *
	 * @return the ordre
	 */
	public Long getOrdre() {

		return ordre;
	}

	/**
	 * Sets the ordre.
	 *
	 * @param ordre the ordre to set
	 */
	public void setOrdre(Long ordre) {

		this.ordre = ordre;
	}

	/*
	 * (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {

		return "IncentiveSetting [" + (id != null ? "id=" + id + ", " : "")
				+ (productId != null ? "productId=" + productId + ", " : "")
				+ (frequency != null ? "frequency=" + frequency + ", " : "")
				+ (category != null ? "category=" + category + ", " : "")
				+ (from != null ? "from=" + from + ", " : "")
				+ (to != null ? "to=" + to + ", " : "")
				+ (discount != null ? "discount=" + discount + ", " : "")
				+ (ordre != null ? "ordre=" + ordre : "") + "]";
	}

}
