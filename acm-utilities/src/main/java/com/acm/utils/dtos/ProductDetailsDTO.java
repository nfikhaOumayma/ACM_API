/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.utils.dtos;

import java.io.Serializable;
import java.util.List;

import org.dozer.Mapping;

/**
 * {@link ProductDetailsDTO} class.
 *
 * @author MoezMhiri
 * @since 1.0.9
 */
public class ProductDetailsDTO extends GenericDTO implements Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = -3215836754480293384L;

	/** The id. */
	private Long id;

	/** The id product. */
	@Mapping("product.id")
	private Long idProduct;

	/** The minimum amount. */
	private Long minimumAmount;

	/** The maximum amount. */
	private Long maximumAmount;

	/** The minimum term. */
	private Integer minimumTerm;

	/** The maximum term. */
	private Integer maximumTerm;

	/** The term type. */
	private String termType;

	/** The deferred period types. */
	private String deferredPeriodTypes;

	/** The enabled. */
	private Boolean enabled;

	/** The deferred period type DT os. */
	private List<DeferredPeriodTypeDTO> deferredPeriodTypeDTOs;

	/** The green rate. */
	private Integer greenRate;

	/** The orangeRate. */
	private Integer orangeRate;

	/** The redRate. */
	private Integer redRate;

	/**
	 * Instantiates a new product.
	 */
	public ProductDetailsDTO() {

		/*
		 * Empty
		 */
	}

	/**
	 * Instantiates a new product.
	 * 
	 * @param id the id
	 */
	public ProductDetailsDTO(Long id) {

		this.id = id;
	}

	/**
	 * Instantiates a new product details.
	 *
	 * @param id the id
	 * @param idProduct the id product
	 * @param minimumAmount the minimum amount
	 * @param maximumAmount the maximum amount
	 * @param minimumTerm the minimum term
	 * @param maximumTerm the maximum term
	 * @param termType the term type
	 * @param enabled the enabled
	 */
	public ProductDetailsDTO(Long id, Long idProduct, Long minimumAmount, Long maximumAmount,
			Integer minimumTerm, Integer maximumTerm, String termType, Boolean enabled) {

		this.id = id;
		this.idProduct = idProduct;
		this.minimumAmount = minimumAmount;
		this.maximumAmount = maximumAmount;
		this.minimumTerm = minimumTerm;
		this.maximumTerm = maximumTerm;
		this.termType = termType;
		this.enabled = enabled;
	}

	/**
	 * Instantiates a new product details DTO : Used in setting Product.
	 *
	 * @param idProduct the id product
	 * @param minimumAmount the minimum amount
	 * @param maximumAmount the maximum amount
	 * @param minimumTerm the minimum term
	 * @param maximumTerm the maximum term
	 * @param termType the term type
	 */
	public ProductDetailsDTO(Long idProduct, Long minimumAmount, Long maximumAmount,
			Integer minimumTerm, Integer maximumTerm, String termType) {

		this.idProduct = idProduct;
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
	 * Gets the id product.
	 *
	 * @return the idProduct
	 */
	public Long getIdProduct() {

		return idProduct;
	}

	/**
	 * Sets the id product.
	 *
	 * @param idProduct the idProduct to set
	 */
	public void setIdProduct(Long idProduct) {

		this.idProduct = idProduct;
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
	 * @param enabled the enabled to set
	 */
	public void setEnabled(Boolean enabled) {

		this.enabled = enabled;
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
	 * Gets the deferred period type DT os.
	 *
	 * @return the deferred period type DT os
	 */
	public List<DeferredPeriodTypeDTO> getDeferredPeriodTypeDTOs() {

		return deferredPeriodTypeDTOs;
	}

	/**
	 * Sets the deferred period type DT os.
	 *
	 * @param deferredPeriodTypeDTOs the new deferred period type DT os
	 */
	public void setDeferredPeriodTypeDTOs(List<DeferredPeriodTypeDTO> deferredPeriodTypeDTOs) {

		this.deferredPeriodTypeDTOs = deferredPeriodTypeDTOs;
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

	/*
	 * (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {

		return "ProductDetailsDTO [id=" + id + ", idProduct=" + idProduct + ", minimumAmount="
				+ minimumAmount + ", maximumAmount=" + maximumAmount + ", minimumTerm="
				+ minimumTerm + ", maximumTerm=" + maximumTerm + ", termType=" + termType
				+ ", enabled=" + enabled + "]";
	}
}
