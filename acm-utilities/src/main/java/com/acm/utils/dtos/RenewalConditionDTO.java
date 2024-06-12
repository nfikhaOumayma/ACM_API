/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.utils.dtos;

import java.io.Serializable;

/**
 * {@link RenewalConditionDTO} class.
 *
 * @author idridi
 * @since 1.0.8
 */
public class RenewalConditionDTO extends GenericDTO implements Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = -7865867410277825517L;

	/** The id. */
	private Long id;

	/** The year. */
	private Integer year;

	/** The min amount. */
	private Long minAmount;

	/** The max amount. */
	private Long maxAmount;

	/** The pourcentage. */
	private Integer pourcentage;

	/** The ordre. */
	private Long ordre;

	/**
	 * Instantiates a new renewal condition DTO.
	 *
	 * @param id the id
	 * @param year the year
	 * @param minAmount the min amount
	 * @param maxAmount the max amount
	 * @param pourcentage the pourcentage
	 */
	public RenewalConditionDTO(Long id, Integer year, Long minAmount, Long maxAmount,
			Integer pourcentage) {

		super();
		this.id = id;
		this.year = year;
		this.minAmount = minAmount;
		this.maxAmount = maxAmount;
		this.pourcentage = pourcentage;
	}

	/**
	 * Instantiates a new renewal condition DTO.
	 */
	public RenewalConditionDTO() {

		super();
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
	 * @param id the new id
	 */
	public void setId(Long id) {

		this.id = id;
	}

	/**
	 * Gets the year.
	 *
	 * @return the year
	 */
	public Integer getYear() {

		return year;
	}

	/**
	 * Sets the year.
	 *
	 * @param year the new year
	 */
	public void setYear(Integer year) {

		this.year = year;
	}

	/**
	 * Gets the min amount.
	 *
	 * @return the min amount
	 */
	public Long getMinAmount() {

		return minAmount;
	}

	/**
	 * Sets the min amount.
	 *
	 * @param minAmount the new min amount
	 */
	public void setMinAmount(Long minAmount) {

		this.minAmount = minAmount;
	}

	/**
	 * Gets the max amount.
	 *
	 * @return the max amount
	 */
	public Long getMaxAmount() {

		return maxAmount;
	}

	/**
	 * Sets the max amount.
	 *
	 * @param maxAmount the new max amount
	 */
	public void setMaxAmount(Long maxAmount) {

		this.maxAmount = maxAmount;
	}

	/**
	 * Gets the pourcentage.
	 *
	 * @return the pourcentage
	 */
	public Integer getPourcentage() {

		return pourcentage;
	}

	/**
	 * Sets the pourcentage.
	 *
	 * @param pourcentage the new pourcentage
	 */
	public void setPourcentage(Integer pourcentage) {

		this.pourcentage = pourcentage;
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
	 * @param ordre the new ordre
	 */
	public void setOrdre(Long ordre) {

		this.ordre = ordre;
	}

}
