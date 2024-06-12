/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.utils.models;

import java.io.Serializable;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.Table;

/**
 * {@link ProductDetails} class.
 *
 * @author MoezMhiri
 * @since 1.0.9
 */
@Entity
@Table(name = "ACM_PRODUCT_DETAILS")
public class ProductDetails extends GenericModel implements Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = 7033157536974352277L;

	/** The id. */
	@Id
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	@Column(name = "ID_ACM_PRODUCT_DETAILS", nullable = false)
	private Long id;

	/** The product. */
	@ManyToOne(fetch = FetchType.EAGER)
	@JoinColumn(name = "ID_ACM_PRODUCT")
	private Product product;

	/** The minimum amount. */
	@Column(name = "AMT_MIN", nullable = false)
	private Long minimumAmount;

	/** The maximum amount. */
	@Column(name = "AMT_MAX", nullable = false)
	private Long maximumAmount;

	/** The minimum term. */
	@Column(name = "TERM_MIN")
	private Integer minimumTerm;

	/** The maximum term. */
	@Column(name = "TERM_MAX")
	private Integer maximumTerm;

	/** The deferred period types. */
	@Column(name = "DEFERRED_PERIOD_TYPES")
	private String deferredPeriodTypes;

	/** The term type. */
	@Column(name = "TERM_TYPE", nullable = false, length = 256)
	private String termType;

	/** The greenRate. */
	@Column(name = "GREEN_RATE")
	private Integer greenRate;

	/** The orangeRate. */
	@Column(name = "ORANGE_RATE")
	private Integer orangeRate;

	/** The redRate. */
	@Column(name = "RED_RATE")
	private Integer redRate;

	/**
	 * Instantiates a new product.
	 */
	public ProductDetails() {

		/*
		 * Empty
		 */
	}

	/**
	 * Instantiates a new product details.
	 *
	 * @param id the id
	 */
	public ProductDetails(Long id) {

		this.id = id;
	}

	/**
	 * Instantiates a new product details.
	 *
	 * @param id the id
	 * @param minimumAmount the minimum amount
	 * @param maximumAmount the maximum amount
	 * @param minimumTerm the minimum term
	 * @param maximumTerm the maximum term
	 * @param termType the term type
	 */
	public ProductDetails(Long id, Long minimumAmount, Long maximumAmount, Integer minimumTerm,
			Integer maximumTerm, String termType) {

		this.id = id;
		this.minimumAmount = minimumAmount;
		this.maximumAmount = maximumAmount;
		this.minimumTerm = minimumTerm;
		this.maximumTerm = maximumTerm;
		this.termType = termType;
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
	 * Gets the product.
	 *
	 * @return the product
	 */
	public Product getProduct() {

		return product;
	}

	/**
	 * Sets the product.
	 *
	 * @param product the product to set
	 */
	public void setProduct(Product product) {

		this.product = product;
	}

	/**
	 * Gets the minimum amount.
	 *
	 * @return the minimumAmount
	 */
	public Long getMinimumAmount() {

		return minimumAmount;
	}

	/**
	 * Sets the minimum amount.
	 *
	 * @param minimumAmount the minimumAmount to set
	 */
	public void setMinimumAmount(Long minimumAmount) {

		this.minimumAmount = minimumAmount;
	}

	/**
	 * Gets the maximum amount.
	 *
	 * @return the maximumAmount
	 */
	public Long getMaximumAmount() {

		return maximumAmount;
	}

	/**
	 * Sets the maximum amount.
	 *
	 * @param maximumAmount the maximumAmount to set
	 */
	public void setMaximumAmount(Long maximumAmount) {

		this.maximumAmount = maximumAmount;
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
	 * Gets the term type.
	 *
	 * @return the termType
	 */
	public String getTermType() {

		return termType;
	}

	/**
	 * Sets the term type.
	 *
	 * @param termType the termType to set
	 */
	public void setTermType(String termType) {

		this.termType = termType;
	}

	/**
	 * Gets the deferred period types.
	 *
	 * @return the deferred period types
	 */
	public String getDeferredPeriodTypes() {

		return deferredPeriodTypes;
	}

	/**
	 * Sets the deferred period types.
	 *
	 * @param deferredPeriodTypes the new deferred period types
	 */
	public void setDeferredPeriodTypes(String deferredPeriodTypes) {

		this.deferredPeriodTypes = deferredPeriodTypes;
	}

	/**
	 * Gets the green rate.
	 *
	 * @return the green rate
	 */
	public Integer getGreenRate() {

		return greenRate;
	}

	/**
	 * Sets the green rate.
	 *
	 * @param greenRate the new green rate
	 */
	public void setGreenRate(Integer greenRate) {

		this.greenRate = greenRate;
	}

	/**
	 * Gets the orange rate.
	 *
	 * @return the orange rate
	 */
	public Integer getOrangeRate() {

		return orangeRate;
	}

	/**
	 * Sets the orange rate.
	 *
	 * @param orangeRate the new orange rate
	 */
	public void setOrangeRate(Integer orangeRate) {

		this.orangeRate = orangeRate;
	}

	/**
	 * Gets the red rate.
	 *
	 * @return the red rate
	 */
	public Integer getRedRate() {

		return redRate;
	}

	/**
	 * Sets the red rate.
	 *
	 * @param redRate the new red rate
	 */
	public void setRedRate(Integer redRate) {

		this.redRate = redRate;
	}

}
